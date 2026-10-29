#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
StatArcboxplot <- ggproto("StatArcboxplot", Stat,
                          required_aes = c("y|x"),
                          non_missing_aes = "weight",
                          # either the x or y aesthetic will get dropped during
                          # statistical transformation, depending on the orientation
                          dropped_aes = c("x", "y", "weight"),
                          setup_data = function(self, data, params) {
                            data <- flip_data(data, params$flipped_aes)
                            data$x <- data$x %||% 0
                            data <- remove_missing(
                              data,
                              na.rm = params$na.rm,
                              vars = "x",
                              name = "stat_boxplot"
                            )
                            # print(data)
                            flip_data(data, params$flipped_aes)
                          },

                          setup_params = function(self, data, params) {
                            params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = TRUE,
                                                                  group_has_equal = TRUE,
                                                                  main_is_optional = TRUE)
                            data <- flip_data(data, params$flipped_aes)

                            has_x <- !(is.null(data$x) && is.null(params$x))
                            has_y <- !(is.null(data$y) && is.null(params$y))
                            if (!has_x && !has_y) {
                              cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
                            }

                            params$width <- params$width %||% (resolution(data$x %||% 0) * 0.75)

                            if (!is_mapped_discrete(data$x) && is.double(data$x) && !has_groups(data) && any(data$x != data$x[1L])) {
                              cli::cli_warn(c(
                                "Continuous {.field {flipped_names(params$flipped_aes)$x}} aesthetic",
                                "i" = "did you forget {.code aes(group = ...)}?"
                              ))
                            }

                            params
                          },

                          extra_params = c("na.rm", "orientation"),

                          compute_group = function(data, scales, width = NULL, na.rm = FALSE, coef = 1.5, flipped_aes = FALSE) {
                            data <- flip_data(data, flipped_aes)
                            qs <- c(0, 0.25, 0.5, 0.75, 1)

                            if (!is.null(data$weight)) {
                              if (!requireNamespace("quantreg", quietly = TRUE)) {
                                cli::cli_abort(c(
                                  "Weighted quantiles require the {.pkg quantreg} package.",
                                  "i" = "Install it with {.run install.packages(\"quantreg\")} or drop the {.field weight} aesthetic."
                                ))
                              }
                              mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
                              stats <- as.numeric(stats::coef(mod))
                            } else {
                              stats <- as.numeric(stats::quantile(data$y, qs))
                            }
                            names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
                            iqr <- diff(stats[c(2, 4)])

                            outliers <- data$y < (stats[2] - coef * iqr) | data$y > (stats[4] + coef * iqr)
                            if (any(outliers)) {
                              stats[c(1, 5)] <- range(c(stats[2:4], data$y[!outliers]), na.rm = TRUE)
                            }

                            if (vctrs::vec_unique_count(data$x) > 1)
                              width <- diff(range(data$x)) * 0.9

                            df <- data_frame0(!!!as.list(stats))
                            df$outliers <- list(data$y[outliers])

                            if (is.null(data$weight)) {
                              n <- sum(!is.na(data$y))
                            } else {
                              # Sum up weights for non-NA positions of y and weight
                              n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
                            }

                            df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
                            df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)

                            df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
                            df$width <- width
                            df$relvarwidth <- sqrt(n)
                            df$flipped_aes <- flipped_aes
                            flip_data(df, flipped_aes)
                          }
)



#' @param geom The geometric object to use to display the data.
#' @param coef Length of the whiskers as a multiple of the inter-quartile range.
#' @export
#' @rdname geom_arcboxplot
#' @import ggplot2
stat_arcboxplot <- function(mapping = NULL, data = NULL,
                            geom = "arcboxplot", position = "dodge2",
                            ...,
                            coef = 1.5,
                            na.rm = FALSE,
                            orientation = NA,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatArcboxplot,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      orientation = orientation,
      coef = coef,
      ...
    )
  )
}
