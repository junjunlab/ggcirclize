#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomArctile <- ggproto("GeomArctile", GeomArcrect,
                       required_aes = c("x","y",
                                        "start","end","r0","r1","clock.wise"),

                       default_aes = aes(
                         start = 0,end = 180,r0 = 0.5,r1 = 1,
                         clock.wise = FALSE,
                         colour = "black",fill = "grey90",
                         linetype = 1,
                         alpha = NA,linewidth = .5,
                         width = 1, height = 1,
                         sector.bg.fill = "grey95",
                         sector.bg.col = "black",
                         sector.bg.lty = 1,
                         sector.bg.lwd = 0.5
                       ),

                       setup_data = function(data, params) {
                         data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
                         data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)

                         trans_data <- transform(data,
                                                 xmin = x - width/2,xmax = x + width/2,  width = NULL,
                                                 ymin = y - height/2,ymax = y + height/2, height = NULL
                         )

                         trans_data
                       },

                       draw_panel = function(self, data, panel_params, coord,
                                             extend.xscale = 0.05,
                                             extend.yscale = 0.05,
                                             add.bg = TRUE,
                                             sector.bg.extend = 0.025,
                                             add.xaxis = TRUE,
                                             add.yaxis = TRUE,
                                             xAxis.params = list(),
                                             yAxis.params = list(),
                                             lineend = "butt", linejoin = "round") {

                         # get xlim and ylim range
                         xlim <- range(data$x - unique(data$width)/2,data$x + unique(data$width)/2)
                         ylim <- range(data$y - unique(data$width)/2,data$y + unique(data$width)/2)

                         # add tile x labels
                         if(panel_params$x$is_discrete()){
                           tile.label.x <- panel_params$x$limits

                           minor.ticks.n.x = 0
                           breaks.x = 1:length(tile.label.x)
                           breaks.label.x = tile.label.x
                         }else{
                           minor.ticks.n.x = 5
                           breaks.x = NULL
                           breaks.label.x = NULL
                         }

                         # add tile y labels
                         if(panel_params$y$is_discrete()){
                           tile.label.y <- panel_params$y$limits

                           minor.ticks.n.y = 0
                           breaks.y = 1:length(tile.label.y)
                           breaks.label.y = tile.label.y
                         }else{
                           minor.ticks.n.y = 5
                           breaks.y = NULL
                           breaks.label.y = NULL
                         }

                         # update xaxis params
                         arcxAxisGrob.params <- modifyList(list(xscale = xlim,
                                                                minor.ticks.n = minor.ticks.n.x,
                                                                breaks = breaks.x,
                                                                breaks.label = breaks.label.x),
                                                           xAxis.params)
                         arcyAxisGrob.params <- modifyList(list(yscale = ylim,
                                                                minor.ticks.n = minor.ticks.n.y,
                                                                breaks = breaks.y,
                                                                breaks.label = breaks.label.y),
                                                           yAxis.params)
                         # print(arcxAxisGrob.params)
                         # print(arcyAxisGrob.params)

                         # Hack to ensure that width is detected as a parameter
                         ggproto_parent(GeomArcrect, self)$draw_panel(
                           data,
                           panel_params,
                           coord,
                           linejoin = linejoin,
                           lineend = lineend,
                           extend.xscale = extend.xscale,
                           extend.yscale = extend.yscale,
                           add.bg = add.bg,
                           sector.bg.extend = sector.bg.extend,
                           add.xaxis = add.xaxis,
                           add.yaxis = add.yaxis,
                           xAxis.params = arcxAxisGrob.params,
                           yAxis.params = arcyAxisGrob.params
                         )

                       },
                       rename_size = TRUE,
                       draw_key = draw_key_rect
)


# ==============================================================================

#' Create a tiled arc in a polar plot.
#'
#' \code{geom_arctile} is used to create a tiled arc in a polar plot.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default: "identity").
#' @param position The position adjustment to be applied (default: "identity").
#' @param na.rm Should missing values be removed? (default: FALSE).
#' @param show.legend Should the legend be shown? (default: NA).
#' @param inherit.aes Should aesthetics be inherited? (default: TRUE).
#' @param linejoin Type of line join (default: "mitre").
#' @param lineend Type of line end (default: "butt").
#' @param extend.xscale Extension factor for x-axis scale (default: 0.05).
#' @param extend.yscale Extension factor for y-axis scale (default: 0.05).
#' @param add.bg Should a background be added? (default: TRUE).
#' @param sector.bg.extend Extension factor for sector background (default: 0.025).
#' @param add.xaxis Should x-axis be added? (default: TRUE).
#' @param add.yaxis Should y-axis be added? (default: TRUE).
#' @param xAxis.params A list of parameters for x-axis (default: list()).
#' @param yAxis.params A list of parameters for y-axis (default: list()).
#' @param ... Additional parameters to be passed to the \code{GeomArctile}.
#'
#' @return A layer to be added to a ggplot object.
#'
#' @seealso \code{\link[ggplot2]{geom_tile}}
#'
#' @import ggplot2
#'
#' @export
geom_arctile <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE,
                         linejoin = "mitre",lineend = "butt",
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
    geom = GeomArctile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  linejoin = linejoin,
                  lineend = lineend,
                  extend.xscale = extend.xscale,
                  extend.yscale = extend.yscale,
                  add.bg = add.bg,
                  sector.bg.extend = sector.bg.extend,
                  add.xaxis = add.xaxis,
                  add.yaxis = add.yaxis,
                  xAxis.params = xAxis.params,
                  yAxis.params = yAxis.params,
                  ...)
  )
}
