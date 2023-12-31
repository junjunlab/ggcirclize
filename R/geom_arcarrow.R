#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomArcarrow <- ggproto("GeomArcarrow", Geom,
                        required_aes = c("xmin","xmax","y",
                                         "start","end","r0","r1","clock.wise"),
                        default_aes = aes(
                          start = 0,end = 180,r0 = 0.5,r1 = 1,
                          clock.wise = FALSE,
                          colour = "black",fill = "grey90",
                          linetype = 1,
                          alpha = NA,linewidth = .5,
                          sector.bg.fill = "grey95",
                          sector.bg.col = "black",
                          sector.bg.lty = 1,
                          sector.bg.lwd = 0.5),

                        draw_panel = function(data, panel_params, coord,
                                              width = 0.5,
                                              start.arrow.len = 6,
                                              end.arrow.len = 6,
                                              arrow.start.width = 0.5,
                                              arrow.end.width = 0.5,
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
                          xlim <- range(data$xmin,data$xmax)
                          ylim <- range(data$y - width,data$y + width)


                          # update xaxis params
                          arcxAxisGrob.params <- modifyList(list(xscale = xlim),xAxis.params)
                          arcyAxisGrob.params <- modifyList(list(yscale = ylim),yAxis.params)

                          # Transform the data
                          # coords <- coord$transform(data, panel_params)
                          coords <- data

                          # draw grobs
                          if(add.bg == TRUE){
                            sector.grob <- arcSectorGrob(start = unique(coords$start),
                                                         end = unique(coords$end),
                                                         r0 = unique(coords$r0),r1 = unique(coords$r1),
                                                         extend.xscale = extend.xscale - sector.bg.extend,
                                                         extend.yscale = extend.yscale - sector.bg.extend,
                                                         xaxis.extend.xscale = extend.xscale,
                                                         xaxis.extend.yscale = extend.yscale - sector.bg.extend,
                                                         yaxis.extend.xscale = extend.xscale - sector.bg.extend,
                                                         yaxis.extend.yscale = extend.yscale,
                                                         add.xaxis = add.xaxis,add.yaxis = add.yaxis,
                                                         arcxAxisGrob.params = arcxAxisGrob.params,
                                                         arcyAxisGrob.params = arcyAxisGrob.params,
                                                         sector.gp = gpar(col = coords$sector.bg.col,
                                                                          fill = coords$sector.bg.fill,
                                                                          lwd = coords$sector.bg.lwd,
                                                                          lty = coords$sector.bg.lty),
                                                         clock.wise = unique(coords$clock.wise))
                          }else{
                            sector.grob <- nullGrob()
                          }


                          arrow.grob <- arcArrowGrob(xmin = coords$xmin,
                                                     xmax = coords$xmax,
                                                     y = coords$y,
                                                     width = width,
                                                     start.arrow.len = start.arrow.len,
                                                     end.arrow.len = end.arrow.len,
                                                     arrow.start.width = arrow.start.width,
                                                     arrow.end.width = arrow.end.width,
                                                     start = unique(coords$start),
                                                     end = unique(coords$end),
                                                     r0 = unique(coords$r0),r1 = unique(coords$r1),
                                                     polygon.gp = gpar(
                                                       col = alpha(coords$colour, coords$alpha),
                                                       fill = alpha(coords$fill, coords$alpha),
                                                       lwd = unique(coords$linewidth),
                                                       lty = unique(coords$linetype),
                                                       lineend = lineend,
                                                       linejoin = linejoin),
                                                     xscale = arcxAxisGrob.params$xscale,
                                                     yscale = arcyAxisGrob.params$yscale,
                                                     extend.xscale = extend.xscale,
                                                     extend.yscale = extend.yscale,
                                                     clock.wise = unique(coords$clock.wise))



                          ggname("geom_arcarrow",
                                 grid::gTree(children = gList(sector.grob,arrow.grob)))
                        },

                        draw_key = draw_key_rect
)


# ==============================================================================

#' Create rectangles based on arcs.
#'
#' \code{geom_arcrect} is used to create rectangles based on arcs in a polar plot.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default: "identity").
#' @param position The position adjustment to be applied (default: "identity").
#' @param na.rm Should missing values be removed? (default: FALSE).
#' @param show.legend Should the legend be shown? (default: NA).
#' @param inherit.aes Should aesthetics be inherited? (default: TRUE).
#' @param linejoin Line join style (default: "mitre").
#' @param lineend Line end style (default: "butt").
#' @param width Width of the arrow (default: 0.5).
#' @param start.arrow.len Length of the starting arrow (default: 6).
#' @param end.arrow.len Length of the ending arrow (default: 6).
#' @param arrow.start.width Width of the starting arrow (default: 0.5).
#' @param arrow.end.width Width of the ending arrow (default: 0.5).
#' @param extend.xscale Extension factor for x-axis scale (default: 0.05).
#' @param extend.yscale Extension factor for y-axis scale (default: 0.05).
#' @param add.bg Should a background be added? (default: TRUE).
#' @param sector.bg.extend Extension factor for sector background (default: 0.025).
#' @param add.xaxis Should x-axis be added? (default: TRUE).
#' @param add.yaxis Should y-axis be added? (default: TRUE).
#' @param xAxis.params A list of parameters for x-axis (default: list()).
#' @param yAxis.params A list of parameters for y-axis (default: list()).
#' @param ... Additional parameters to be passed to the \code{GeomArcrect}.
#'
#' @return A layer to be added to a ggplot object.
#'
#' @seealso \code{\link[ggplot2]{geom_rect}}
#'
#' @import ggplot2
#'
#' @export
geom_arcarrow <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,
                          linejoin = "mitre",lineend = "butt",
                          width = 0.5,
                          start.arrow.len = 6,
                          end.arrow.len = 6,
                          arrow.start.width = 0.5,
                          arrow.end.width = 0.5,
                          extend.xscale = 0.05,
                          extend.yscale = 0.05,
                          add.bg = TRUE,
                          sector.bg.extend = 0.025,
                          add.xaxis = TRUE,
                          add.yaxis = TRUE,
                          xAxis.params = list(),
                          yAxis.params = list(),...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArcarrow,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  linejoin = linejoin,
                  lineend = lineend,
                  width = width,
                  start.arrow.len = start.arrow.len,
                  end.arrow.len = end.arrow.len,
                  arrow.start.width = arrow.start.width,
                  arrow.end.width = arrow.end.width,
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
