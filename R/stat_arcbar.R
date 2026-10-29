#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
StatArcbar <- ggproto("StatArcbar", Stat,
                      required_aes = "x|y",

                      default_aes = aes(x = after_stat(count),
                                        y = after_stat(count),
                                        weight = 1),

                      setup_params = function(self, data, params) {
                        params$flipped_aes <- has_flipped_aes(data, params, main_is_orthogonal = FALSE)

                        has_x <- !(is.null(data$x) && is.null(params$x))
                        has_y <- !(is.null(data$y) && is.null(params$y))
                        if (!has_x && !has_y) {
                          cli::cli_abort("{.fn {snake_class(self)}} requires an {.field x} or {.field y} aesthetic.")
                        }
                        # Both x and y may be present: x groups the bars and y gives
                        # their height (geom_col semantics), in addition to the plain
                        # count mode where only one of x/y is mapped (geom_bar semantics).
                        params$both_xy <- has_x && has_y

                        if (is.null(params$width)) {
                          x <- if (params$flipped_aes) "y" else "x"
                          params$width <- resolution(data[[x]], discrete = TRUE) * 0.9
                        }

                        params
                      },

                      extra_params = c("na.rm", "orientation"),

                      compute_group = function(self, data, scales, width = NULL,
                                               flipped_aes = FALSE, both_xy = FALSE) {
                        data <- flip_data(data, flipped_aes)
                        x <- data$x
                        weight <- data$weight %||% rep(1, length(x))

                        if (both_xy) {
                          # geom_col semantics: x groups, y provides the bar height.
                          y <- data$y
                          val <- as.numeric(tapply(y, x, sum, na.rm = TRUE))
                        } else {
                          # geom_bar semantics: count (weighted) per x level.
                          val <- as.numeric(tapply(weight, x, sum, na.rm = TRUE))
                        }
                        val[is.na(val)] <- 0

                        bars <- data_frame0(
                          count = val,
                          prop = val / sum(abs(val)),
                          x = sort(unique0(x)),
                          width = width,
                          flipped_aes = flipped_aes,
                          .size = length(val)
                        )

                        # In geom_col mode the y aesthetic is consumed by the stat and
                        # must be re-emitted so the geom can draw the bar heights.
                        if (both_xy) {
                          bars$y <- val
                        }

                        # print(scales)

                        flip_data(bars, flipped_aes)
                      },

                      dropped_aes = "weight"

)



#' @param geom The geometric object to use to display the data.
#' @param width Bar width.
#' @param orientation The orientation of the layer.
#' @import ggplot2
#' @export
#' @rdname geom_arcbar
stat_arcbar <- function(mapping = NULL, data = NULL,
                        geom = "arcbar",
                        position = "stack",
                        width = NULL,orientation = NA,
                        na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  layer(
    stat = StatArcbar,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(na.rm = na.rm,
                   orientation = orientation,
                   width = width,
                   ...)
  )
}

