#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomArcpolygon <- ggproto("GeomArcpolygon", Geom,
                          required_aes = c("x", "y",
                                           "start","end","r0","r1","clock.wise"),

                          default_aes = aes(start = 0,end = 180,r0 = 0.5,r1 = 1,
                                            clock.wise = FALSE,
                                            colour = "black",fill = "grey90",
                                            linetype = 1,
                                            alpha = NA,linewidth = .5,
                                            sector.bg.fill = "grey95",
                                            sector.bg.col = "black",
                                            sector.bg.lty = 1,
                                            sector.bg.lwd = 0.5,id = 1),


                          draw_panel = function(data, panel_params, coord,
                                                polar.every = TRUE,
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
                            xlim <- range(data$x)
                            ylim <- range(data$y)

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

                            # Transform the data
                            # coords <- coord$transform(data, panel_params)
                            coords <- data

                            # sort by grup
                            coords <- coords[order(coords$group),]

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

                            polygon.grob <- arcPolygonGrob(x = coords$x,y = coords$y,id =  coords$group,
                                                           polar.every = polar.every,
                                                           start = unique(coords$start),
                                                           end = unique(coords$end),
                                                           r0 = unique(coords$r0),r1 = unique(coords$r1),
                                                           xscale = arcxAxisGrob.params$xscale,
                                                           yscale = arcyAxisGrob.params$yscale,
                                                           polygon.gp = gpar(
                                                             col = alpha(col.d$colour, unique(coords$alpha)),
                                                             fill = alpha(col.d$fill, unique(coords$alpha)),
                                                             lwd = unique(coords$linewidth),
                                                             lty = unique(coords$linetype),
                                                             lineend = lineend,
                                                             linejoin = linejoin),
                                                           extend.xscale = extend.xscale,
                                                           extend.yscale = extend.yscale,
                                                           clock.wise = unique(coords$clock.wise))


                            ggname("geom_arcpolygon",
                                   grid::gTree(children = gList(sector.grob,polygon.grob)))

                          },



                          draw_key = draw_key_polygon,

                          rename_size = TRUE
)



#' Create polygons based on arc sectors.
#'
#' \code{geom_arcpolygon} is used to create polygons based on arc sectors in a polar plot.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default: "identity").
#' @param position The position adjustment to be applied (default: "identity").
#' @param na.rm Should missing values be removed? (default: FALSE).
#' @param show.legend Should the legend be shown? (default: NA).
#' @param inherit.aes Should aesthetics be inherited? (default: TRUE).
#' @param polar.every Should polygons be created for every data point? (default: TRUE).
#' @param extend.xscale Extension factor for x-axis scale (default: 0.05).
#' @param extend.yscale Extension factor for y-axis scale (default: 0.05).
#' @param add.bg Should a background be added? (default: TRUE).
#' @param sector.bg.extend Extension factor for sector background (default: 0.025).
#' @param add.xaxis Should x-axis be added? (default: TRUE).
#' @param add.yaxis Should y-axis be added? (default: TRUE).
#' @param xAxis.params A list of parameters for x-axis (default: list()).
#' @param yAxis.params A list of parameters for y-axis (default: list()).
#' @param lineend Line end style (default: "butt").
#' @param linejoin Line join style (default: "round").
#' @param ... Additional parameters to be passed to the \code{GeomArccolygon}.
#'
#' @return A layer to be added to a ggplot object.
#'
#' @seealso \code{\link[ggplot2]{geom_polygon}}
#'
#' @import ggplot2
#'
#' @export
geom_arcpolygon <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            polar.every = TRUE,
                            extend.xscale = 0.05,
                            extend.yscale = 0.05,
                            add.bg = TRUE,
                            sector.bg.extend = 0.025,
                            add.xaxis = TRUE,
                            add.yaxis = TRUE,
                            xAxis.params = list(),
                            yAxis.params = list(),
                            lineend = "butt", linejoin = "round",
                            ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArcpolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      lineend = lineend,
      linejoin = linejoin,
      polar.every = polar.every,
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
