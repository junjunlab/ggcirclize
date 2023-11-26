#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomArcrossbar <- ggproto("GeomArcrossbar", Geom,
                          required_aes = c("x", "y", "ymin|xmin", "ymax|xmax"),

                          default_aes = aes(start = 0,end = 180,r0 = 0.5,r1 = 1,
                                            clock.wise = FALSE,
                                            colour = "black",fill = NA,
                                            linetype = 1,
                                            alpha = NA,linewidth = .5,
                                            sector.bg.fill = "grey95",
                                            sector.bg.col = "black",
                                            sector.bg.lty = 1,
                                            sector.bg.lwd = 0.5),

                          setup_params = function(data, params) {
                            GeomErrorbar$setup_params(data, params)
                          },

                          extra_params = c("na.rm", "orientation"),

                          setup_data = function(data, params) {
                            GeomErrorbar$setup_data(data, params)
                          },


                          draw_key = draw_key_crossbar,

                          draw_panel = function(self, data, panel_params, coord,
                                                extend.xscale = 0.05,
                                                extend.yscale = 0.05,
                                                add.bg = TRUE,
                                                sector.bg.extend = 0.025,
                                                add.xaxis = TRUE,
                                                add.yaxis = TRUE,
                                                xAxis.params = list(),
                                                yAxis.params = list(),
                                                lineend = "butt", linejoin = "round",
                                                fatten = 2.5, width = NULL,flipped_aes = FALSE) {
                            data <- check_linewidth(data, snake_class(self))
                            data <- flip_data(data, flipped_aes)

                            middle <- transform(data, x = xmin, xend = xmax, yend = y,
                                                linewidth = linewidth * fatten, alpha = NA)

                            has_notch <- suppressWarnings(!is.null(data$ynotchlower) && !is.null(data$ynotchupper) &&
                                                            !is.na(data$ynotchlower) && !is.na(data$ynotchupper))

                            if (has_notch) {
                              if (data$ynotchlower < data$ymin  ||  data$ynotchupper > data$ymax)
                                cli::cli_inform(c(
                                  "Notch went outside hinges",
                                  i = "Do you want {.code notch = FALSE}?"
                                ))

                              notchindent <- (1 - data$notchwidth) * (data$xmax - data$xmin) / 2

                              middle$x <- middle$x + notchindent
                              middle$xend <- middle$xend - notchindent

                              box <- data_frame0(
                                x = c(
                                  data$xmin, data$xmin, data$xmin + notchindent, data$xmin, data$xmin,
                                  data$xmax, data$xmax, data$xmax - notchindent, data$xmax, data$xmax,
                                  data$xmin
                                ),
                                y = c(
                                  data$ymax, data$ynotchupper, data$y, data$ynotchlower, data$ymin,
                                  data$ymin, data$ynotchlower, data$y, data$ynotchupper, data$ymax,
                                  data$ymax
                                ),
                                alpha = rep(data$alpha, 11),
                                colour = rep(data$colour, 11),
                                linewidth = rep(data$linewidth, 11),
                                linetype = rep(data$linetype, 11),
                                fill = rep(data$fill, 11),
                                group = rep(seq_len(nrow(data)), 11),
                                start = rep(data$start, 11),
                                end = rep(data$end, 11),
                                r0 = rep(data$r0, 11),
                                r1 = rep(data$r1, 11),
                                clock.wise = rep(data$clock.wise, 11),
                                sector.bg.fill = rep(data$sector.bg.fill, 11),
                                sector.bg.col = rep(data$sector.bg.col, 11),
                                sector.bg.lty = rep(data$sector.bg.lty, 11),
                                sector.bg.lwd = rep(data$sector.bg.lwd, 11)
                              )
                            } else {
                              # No notch
                              box <- data_frame0(
                                x = c(data$xmin, data$xmin, data$xmax, data$xmax, data$xmin),
                                y = c(data$ymax, data$ymin, data$ymin, data$ymax, data$ymax),
                                alpha = rep(data$alpha, 5),
                                colour = rep(data$colour, 5),
                                linewidth = rep(data$linewidth, 5),
                                linetype = rep(data$linetype, 5),
                                fill = rep(data$fill, 5),
                                group = rep(seq_len(nrow(data)), 5), # each bar forms it's own group
                                start = rep(data$start, 5),
                                end = rep(data$end, 5),
                                r0 = rep(data$r0, 5),
                                r1 = rep(data$r1, 5),
                                clock.wise = rep(data$clock.wise, 5),
                                sector.bg.fill = rep(data$sector.bg.fill, 5),
                                sector.bg.col = rep(data$sector.bg.col, 5),
                                sector.bg.lty = rep(data$sector.bg.lty, 5),
                                sector.bg.lwd = rep(data$sector.bg.lwd, 5)
                              )
                            }

                            box <- flip_data(box, flipped_aes)
                            box <- transform(box,x1 = x,y1 = y,id = group)
                            box <- box[order(box$group),]

                            middle <- flip_data(middle, flipped_aes)
                            # print(middle)
                            if(unique(middle$flipped_aes) == FALSE){
                              middle <- transform(middle,
                                                  ymin = y,ymax = y,
                                                  linewidth = linewidth*2)

                              if(has_notch){
                                middle <- transform(middle,
                                                    xmin = x,xmax = xend)
                              }
                            }else{
                              middle <- transform(middle,
                                                  xmin = x,xmax = x,
                                                  linewidth = linewidth*2)
                              if(has_notch){
                                middle <- transform(middle,
                                                    ymin = x,ymax = xend)
                              }
                            }

                            # ====================================================
                            # get xlim and ylim range
                            data <- flip_data(data, flipped_aes)
                            xlim <- range(data$xmin,data$xmax)
                            ylim <- range(data$ymin,data$ymax)

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

                            # combine grobs
                            ggname("geom_arcrossbar",
                                   gTree(children = gList(
                                     GeomArcpolygon$draw_panel(box, panel_params, coord,
                                                               linejoin = linejoin,
                                                               lineend = lineend,
                                                               extend.xscale = extend.xscale,
                                                               extend.yscale = extend.yscale,
                                                               add.bg = add.bg,
                                                               sector.bg.extend = sector.bg.extend,
                                                               add.xaxis = add.xaxis,
                                                               add.yaxis = add.yaxis,
                                                               xAxis.params = arcxAxisGrob.params,
                                                               yAxis.params = arcyAxisGrob.params),

                                     GeomArcsegment$draw_panel(middle, panel_params, coord,
                                                               linejoin = linejoin,
                                                               lineend = lineend,
                                                               extend.xscale = extend.xscale,
                                                               extend.yscale = extend.yscale,
                                                               add.bg = FALSE,
                                                               sector.bg.extend = sector.bg.extend,
                                                               add.xaxis = add.xaxis,
                                                               add.yaxis = add.yaxis,
                                                               xAxis.params = arcxAxisGrob.params,
                                                               yAxis.params = arcyAxisGrob.params)
                                   )))
                          },

                          rename_size = TRUE
)


#' Create crossbars based on arcs.
#'
#' \code{geom_arcrossbar} is used to create crossbars based on arcs in a polar plot.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default: "identity").
#' @param position The position adjustment to be applied (default: "identity").
#' @param fatten Fattening factor for crossbars (default: 2.5).
#' @param na.rm Should missing values be removed? (default: FALSE).
#' @param orientation Orientation of the crossbars (default: NA).
#' @param show.legend Should the legend be shown? (default: NA).
#' @param inherit.aes Should aesthetics be inherited? (default: TRUE).
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
#' @param ... Additional parameters to be passed to the \code{GeomArcrossbar}.
#'
#' @return A layer to be added to a ggplot object.
#'
#' @seealso \code{\link[ggplot2]{geom_crossbar}}
#'
#' @import ggplot2
#'
#' @export
geom_arcrossbar <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            fatten = 2.5,
                            na.rm = FALSE,
                            orientation = NA,
                            show.legend = NA,
                            inherit.aes = TRUE,
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
    geom = GeomArcrossbar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fatten = fatten,
      na.rm = na.rm,
      orientation = orientation,
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
      ...
    )
  )
}
