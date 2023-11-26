#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomArctext <- ggproto("GeomArctext", Geom,
                       required_aes = c("x", "y", "label"),

                       non_missing_aes = "angle",

                       default_aes = aes(
                         colour = "black", size = 8, alpha = NA,
                         family = "", fontface = 1,
                         start = 0,end = 180,r0 = 0.5,r1 = 1,
                         clock.wise = FALSE
                       ),

                       draw_panel = function(data, panel_params, coord,
                                             curved.label = TRUE,
                                             shift = 0.65,
                                             na.rm = FALSE,
                                             extend.xscale = 0.05,
                                             extend.yscale = 0.05,
                                             add.bg = TRUE,
                                             sector.bg.extend = 0.025,
                                             add.xaxis = TRUE,
                                             add.yaxis = TRUE,
                                             xAxis.params = list(),
                                             yAxis.params = list()) {

                         # get xlim and ylim range
                         xlim <- range(data$x)
                         ylim <- range(data$y)

                         # update xaxis params
                         arcxAxisGrob.params <- modifyList(list(xscale = xlim),xAxis.params)
                         arcyAxisGrob.params <- modifyList(list(yscale = ylim),yAxis.params)

                         # color for labels
                         if(curved.label == TRUE){
                           text.col <- rep(data$colour ,nchar(data$label))
                         }else{
                           text.col <- data$colour
                         }

                         # grob
                         arcTextGrob(x = data$x,y = data$y,
                                     labels = data$label,
                                     shift = shift,
                                     curved.label = curved.label,
                                     start = unique(data$start),
                                     end = unique(data$end),
                                     r0 = unique(data$r0),r1 = unique(data$r1),
                                     xscale = arcxAxisGrob.params$xscale,
                                     yscale = arcyAxisGrob.params$yscale,
                                     text.gp = gpar(
                                       col = alpha(text.col, unique(data$alpha)),
                                       fontsize = data$size,
                                       fontfamily = data$family,
                                       fontface = data$fontface),
                                     extend.xscale = extend.xscale,
                                     extend.yscale = extend.yscale,
                                     clock.wise = unique(data$clock.wise))


                       },

                       draw_key = draw_key_point
)



#' Create text labels along arcs.
#'
#' \code{geom_arctext} is used to create text labels along arcs in a polar plot.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default: "identity").
#' @param position The position adjustment to be applied (default: "identity").
#' @param na.rm Should missing values be removed? (default: FALSE).
#' @param show.legend Should the legend be shown? (default: NA).
#' @param inherit.aes Should aesthetics be inherited? (default: TRUE).
#' @param curved.label Should labels be curved along the arc? (default: TRUE).
#' @param shift Shift factor for label placement along the arc (default: 0.65).
#' @param extend.xscale Extension factor for x-axis scale (default: 0.05).
#' @param extend.yscale Extension factor for y-axis scale (default: 0.05).
#' @param add.bg Should a background be added? (default: TRUE).
#' @param sector.bg.extend Extension factor for sector background (default: 0.025).
#' @param add.xaxis Should x-axis be added? (default: TRUE).
#' @param add.yaxis Should y-axis be added? (default: TRUE).
#' @param xAxis.params A list of parameters for x-axis (default: list()).
#' @param yAxis.params A list of parameters for y-axis (default: list()).
#' @param ... Additional parameters to be passed to the \code{GeomArctext}.
#'
#' @return A layer to be added to a ggplot object.
#'
#' @seealso \code{\link[ggplot2]{geom_text}}
#'
#' @import ggplot2
#'
#' @export
geom_arctext <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         curved.label = TRUE,
                         shift = 0.65,
                         extend.xscale = 0.05,
                         extend.yscale = 0.05,
                         add.bg = TRUE,
                         sector.bg.extend = 0.025,
                         add.xaxis = TRUE,
                         add.yaxis = TRUE,
                         xAxis.params = list(),
                         yAxis.params = list(),...){
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArctext,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      curved.label = curved.label,
      shift = shift,
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
