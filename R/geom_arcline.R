#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomArcline <- ggproto("GeomArcline", Geom,
                       required_aes = c("x","y","start","end","r0","r1","clock.wise"),
                       default_aes = aes(
                         start = 0,end = 180,r0 = 0.5,r1 = 1,
                         clock.wise = FALSE,
                         colour = "black",linewidth = .5,
                         linetype = 1,fill = NA,
                         alpha = NA,
                         sector.bg.fill = "grey95",
                         sector.bg.col = "black",
                         sector.bg.lty = 1,
                         sector.bg.lwd = 0.5),

                       # draw_group = function(data, panel_params, coord, ...) {
                       draw_panel = function(data, panel_params, coord,
                                             extend.xscale = 0.05,
                                             extend.yscale = 0.05,
                                             add.bg = TRUE,
                                             sector.bg.extend = 0.025,
                                             add.xaxis = TRUE,
                                             add.yaxis = TRUE,
                                             xAxis.params = list(),
                                             yAxis.params = list(),
                                             arrow = NULL,
                                             lineend = "butt", linejoin = "round", linemitre = 10) {

                         # get xlim and ylim range
                         xlim <- range(data$x)
                         ylim <- range(data$y)

                         # print(ylim)

                         # update xaxis params
                         arcxAxisGrob.params <- modifyList(list(xscale = xlim),xAxis.params)
                         arcyAxisGrob.params <- modifyList(list(yscale = ylim),yAxis.params)

                         # Transform the data
                         # coords <- coord$transform(data, panel_params)
                         coords <- data

                         # order data
                         coords <- coords[order(coords$group,coords$x), , drop = FALSE]

                         # col and fill
                         col.d <- unique(coords[,c("fill","colour","group")])

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


                         line.grob <- arcLinesGrob(x = coords$x,y = coords$y,
                                                   start = unique(coords$start),
                                                   end = unique(coords$end),
                                                   r0 = unique(coords$r0),r1 = unique(coords$r1),
                                                   arrow = arrow,
                                                   id = coords$group,
                                                   xscale = arcxAxisGrob.params$xscale,
                                                   yscale = arcyAxisGrob.params$yscale,
                                                   lines.gp = gpar(
                                                     col = alpha(col.d$colour, unique(coords$alpha)),
                                                     fill = alpha(col.d$fill, unique(coords$alpha)),
                                                     # col = alpha(coords$colour, unique(coords$alpha)),
                                                     # fill = alpha(coords$fill, unique(coords$alpha)),
                                                     lwd = unique(coords$linewidth),
                                                     lty = unique(coords$linetype),
                                                     lineend = lineend,
                                                     linejoin = linejoin,
                                                     linemitre = linemitre),
                                                   extend.xscale = extend.xscale,
                                                   extend.yscale = extend.yscale,
                                                   clock.wise = unique(coords$clock.wise))


                         ggname("geom_arcline",
                                grid::gTree(children = gList(sector.grob,line.grob)))
                       },

                       draw_key = draw_key_path
)



#' Create a line segment layer with options for arrowheads and line style.
#'
#' \code{geom_arcline} is used to create a line segment layer with options for arrowheads and line style.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default: "identity").
#' @param position The position adjustment to be applied (default: "identity").
#' @param na.rm Should missing values be removed? (default: FALSE)
#' @param show.legend Should the legend be shown? (default: NA).
#' @param inherit.aes Should aesthetics be inherited? (default: TRUE)
#' @param arrow A list of arrow properties (default: NULL).
#' @param lineend Line end style (default: "butt").
#' @param linejoin Line join style (default: "round").
#' @param linemitre Line mitre limit (default: 10).
#' @param extend.xscale Extension factor for x-axis scale (default: 0.05).
#' @param extend.yscale Extension factor for y-axis scale (default: 0.05).
#' @param add.bg Should a background be added? (default: TRUE)
#' @param sector.bg.extend Extension factor for sector background (default: 0.025).
#' @param add.xaxis Should x-axis be added? (default: TRUE)
#' @param add.yaxis Should y-axis be added? (default: TRUE)
#' @param xAxis.params A list of parameters for x-axis (default: list()).
#' @param yAxis.params A list of parameters for y-axis (default: list()).
#' @param ... Additional parameters to be passed to the \code{GeomArcline}.
#'
#' @return A layer to be added to a ggplot object.
#'
#' @seealso \code{\link[ggcirclize]{arcLinesGrob}}
#'
#' @import ggplot2
#' @importFrom rlang %||%
#'
#' @export
geom_arcline <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE,arrow = NULL,
                         lineend = "butt", linejoin = "round", linemitre = 10,
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
    geom = GeomArcline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  lineend = lineend,
                  linejoin = linejoin,
                  linemitre = linemitre,
                  arrow = arrow,
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
