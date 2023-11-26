#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomArclinerange <- ggproto("GeomArclinerange", Geom,
                            required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),

                            default_aes = aes(start = 0,end = 180,r0 = 0.5,r1 = 1,
                                              clock.wise = FALSE,
                                              colour = "black",fill = "black",
                                              linetype = 1,
                                              alpha = NA,linewidth = .5,
                                              sector.bg.fill = "grey95",
                                              sector.bg.col = "black",
                                              sector.bg.lty = 1,
                                              sector.bg.lwd = 0.5),

                            draw_key = draw_key_linerange,

                            setup_params = function(data, params) {
                              params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = TRUE)
                              # if flipped_aes == TRUE then y, xmin, xmax is present
                              if (!(params$flipped_aes || all(c("x", "ymin", "ymax") %in% c(names(data), names(params))))) {
                                cli::cli_abort("Either, {.field x}, {.field ymin}, and {.field ymax} {.emph or} {.field y}, {.field xmin}, and {.field xmax} must be supplied")
                              }
                              params
                            },

                            extra_params = c("na.rm", "orientation"),

                            setup_data = function(data, params) {
                              data$flipped_aes <- params$flipped_aes
                              data
                            },

                            draw_panel = function(data, panel_params,coord,
                                                  extend.xscale = 0.05,
                                                  extend.yscale = 0.05,
                                                  add.bg = TRUE,
                                                  sector.bg.extend = 0.025,
                                                  add.xaxis = TRUE,
                                                  add.yaxis = TRUE,
                                                  xAxis.params = list(),
                                                  yAxis.params = list(),
                                                  lineend = "butt", linejoin = "round",
                                                  flipped_aes = FALSE, na.rm = FALSE) {
                              data <- flip_data(data, flipped_aes)
                              data <- transform(data, xend = x, y = ymin, yend = ymax)
                              data <- flip_data(data, flipped_aes)
                              data <- transform(data,
                                                xmin = x,xmax = xend,
                                                ymin = y,ymax = yend)
                              # print(data)

                              ggname("geom_arclinerange",
                                     gTree(children = gList(GeomArcsegment$draw_panel(data, panel_params, coord,
                                                                                      na.rm = na.rm,
                                                                                      linejoin = linejoin,
                                                                                      lineend = lineend,
                                                                                      extend.xscale = extend.xscale,
                                                                                      extend.yscale = extend.yscale,
                                                                                      add.bg = add.bg,
                                                                                      sector.bg.extend = sector.bg.extend,
                                                                                      add.xaxis = add.xaxis,
                                                                                      add.yaxis = add.yaxis,
                                                                                      xAxis.params = xAxis.params,
                                                                                      yAxis.params = yAxis.params
                                     ))))

                            },

                            rename_size = TRUE
)



#' Create a range of line segments with options for line style and orientation.
#'
#' \code{geom_arclinerange} is used to create a range of line segments with options for line style and orientation.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default: "identity").
#' @param position The position adjustment to be applied (default: "identity").
#' @param na.rm Should missing values be removed? (default: FALSE)
#' @param orientation Orientation of the line segments (default: NA).
#' @param show.legend Should the legend be shown? (default: NA).
#' @param inherit.aes Should aesthetics be inherited? (default: TRUE)
#' @param lineend Line end style (default: "butt").
#' @param linejoin Line join style (default: "round").
#' @param extend.xscale Extension factor for x-axis scale (default: 0.05).
#' @param extend.yscale Extension factor for y-axis scale (default: 0.05).
#' @param add.bg Should a background be added? (default: TRUE)
#' @param sector.bg.extend Extension factor for sector background (default: 0.025).
#' @param add.xaxis Should x-axis be added? (default: TRUE)
#' @param add.yaxis Should y-axis be added? (default: TRUE)
#' @param xAxis.params A list of parameters for x-axis (default: list()).
#' @param yAxis.params A list of parameters for y-axis (default: list()).
#' @param ... Additional parameters to be passed to the \code{GeomArclinerange}.
#'
#' @return A layer to be added to a ggplot object.
#'
#' @seealso \code{\link[ggplot2]{geom_linerange}}
#'
#' @import ggplot2
#'
#' @export
geom_arclinerange <- function(mapping = NULL, data = NULL,
                              stat = "identity", position = "identity",
                              na.rm = FALSE,
                              orientation = NA,
                              show.legend = NA,
                              inherit.aes = TRUE,
                              lineend = "butt", linejoin = "round",
                              extend.xscale = 0.05,
                              extend.yscale = 0.05,
                              add.bg = TRUE,
                              sector.bg.extend = 0.025,
                              add.xaxis = TRUE,
                              add.yaxis = TRUE,
                              xAxis.params = list(),
                              yAxis.params = list(),
                              ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArclinerange,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      lineend = lineend,
      linejoin = linejoin,
      extend.xscale = extend.xscale,
      extend.yscale = extend.yscale,
      add.bg = add.bg,
      sector.bg.extend = sector.bg.extend,
      add.xaxis = add.xaxis,
      add.yaxis = add.yaxis,
      xAxis.params = xAxis.params,
      yAxis.params = yAxis.params,
      ...
    )
  )
}
