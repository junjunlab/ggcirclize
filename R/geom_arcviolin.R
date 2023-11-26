#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomArcviolin <- ggproto("GeomArcviolin", Geom,
                         required_aes = c("x", "y"),

                         default_aes = aes(weight = 1, width = 0.9,
                                           start = 0,end = 180,r0 = 0.5,r1 = 1,
                                           clock.wise = FALSE,
                                           colour = "black",linewidth = .5,
                                           linetype = 1,fill = "white",
                                           alpha = NA,
                                           sector.bg.fill = "grey95",
                                           sector.bg.col = "black",
                                           sector.bg.lty = 1,
                                           sector.bg.lwd = 0.5),

                         draw_key = draw_key_polygon,

                         setup_params = function(data, params) {
                           params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
                           params
                         },

                         extra_params = c("na.rm", "orientation",
                                          "lineend", "linejoin", "linemitre"),

                         setup_data = function(data, params) {
                           data$flipped_aes <- params$flipped_aes
                           data <- flip_data(data, params$flipped_aes)
                           data$width <- data$width %||%
                             params$width %||% (resolution(data$x, FALSE) * 0.9)

                           # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
                           data <- dapply(data, "group", transform,
                                          xmin = x - width / 2,
                                          xmax = x + width / 2)

                           # flip_data(data, params$flipped_aes)
                         },

                         draw_panel = function(self, data, panel_params, coord,
                                               polar.every = FALSE,
                                               draw_quantiles = NULL,
                                               flipped_aes = FALSE,
                                               quantiles.colour = "black",
                                               quantiles.linewidth = 1,
                                               quantiles.linetype = 1,
                                               extend.xscale = 0.05,
                                               extend.yscale = 0.05,
                                               add.bg = TRUE,
                                               sector.bg.extend = 0.025,
                                               add.xaxis = TRUE,
                                               add.yaxis = TRUE,
                                               xAxis.params = list(),
                                               yAxis.params = list(),
                                               lineend = "butt", linejoin = "round") {
                           data <- flip_data(data, flipped_aes)

                           if(unique(data$flipped_aes) == "FALSE"){
                             # Find the points for the line to go all the way around
                             data <- transform(data,
                                               xminv = x - violinwidth * (x - xmin),
                                               xmaxv = x + violinwidth * (xmax - x)
                             )

                             # Make sure it's sorted properly to draw the outline
                             newdata <- vec_rbind0(
                               transform(data, x = xminv)[order(data$y), ],
                               transform(data, x = xmaxv)[order(data$y, decreasing = TRUE), ]
                             )
                           }else{
                             # Find the points for the line to go all the way around
                             data <- transform(data,
                                               yminv = y - violinwidth * (y - ymin),
                                               ymaxv = y + violinwidth * (ymax - y)
                             )

                             # Make sure it's sorted properly to draw the outline
                             newdata <- vec_rbind0(
                               transform(data, y = yminv)[order(data$x), ],
                               transform(data, y = ymaxv)[order(data$x, decreasing = TRUE), ]
                             )
                           }

                           # Close the polygon: set first and last point the same
                           # Needed for coord_polar and such
                           newdata <- vec_rbind0(newdata, newdata[1,])
                           # newdata <- flip_data(newdata, flipped_aes)

                           # =============================================================================
                           # draw grobs
                           # get xlim and ylim range
                           xlim <- range(newdata$x)
                           ylim <- range(newdata$y)


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

                           # Hack to ensure that width is detected as a parameter
                           polygon_grob <- ggproto_parent(GeomArcpolygon, self)$draw_panel(
                             newdata,panel_params,coord,
                             polar.every = polar.every,
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

                           # ====================================================================
                           # Draw quantiles if requested, so long as there is non-zero y range
                           if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                             if (!(all(draw_quantiles >= 0) && all(draw_quantiles <= 1))) {
                               cli::cli_abort("{.arg draw_quantiles} must be between 0 and 1")
                             }

                             # Compute the quantile segments and combine with existing aesthetics
                             gp <- unique(data$group)

                             lapply(seq_along(gp), function(x){
                               tmp.data <- data[which(data$group == gp[x]),]
                               quantiles <- create_quantile_segment_frame(tmp.data, draw_quantiles)
                               aesthetics <- tmp.data[
                                 rep(1, nrow(quantiles)),
                                 setdiff(names(tmp.data), c("x", "y", "group")),
                                 drop = FALSE
                               ]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- vec_cbind(quantiles, aesthetics)
                               both <- both[!is.na(both$group), , drop = FALSE]
                               both <- flip_data(both, flipped_aes)
                             }) %>% do.call("rbind",.) -> quantiles.both

                             quantile_grob <- if (nrow(quantiles.both) == 0) {
                               zeroGrob()
                             } else {

                               quantiles.both$id <- rep(1:(nrow(quantiles.both)/2),each = 2)
                               arcLinesGrob(x = quantiles.both$x,
                                            y = quantiles.both$y,
                                            polar.every = polar.every,
                                            start = unique(quantiles.both$start),
                                            end = unique(quantiles.both$end),
                                            r0 = unique(quantiles.both$r0),
                                            r1 = unique(quantiles.both$r1),
                                            id = quantiles.both$id,
                                            xscale = range(newdata$x),
                                            yscale = range(newdata$y),
                                            lines.gp = gpar(
                                              col = quantiles.colour,
                                              fill = quantiles.colour,
                                              lwd = quantiles.linewidth,
                                              lty = quantiles.linetype),
                                            extend.xscale = extend.xscale,
                                            extend.yscale = extend.yscale,
                                            clock.wise = unique(quantiles.both$clock.wise))
                             }


                             # combine grobs
                             ggname("geom_arcviolin", grobTree(
                               # GeomArcpolygon$draw_panel(newdata, panel_params, coord),
                               polygon_grob,quantile_grob)
                             )
                           } else {
                             ggname("geom_arcviolin",
                                    # GeomArcpolygon$draw_panel(newdata, panel_params, coord)
                                    polygon_grob
                             )
                           }
                         },

                         rename_size = TRUE
)



#' Create a violin-like arc density plot in a polar coordinate system.
#'
#' \code{geom_arcviolin} is used to create a violin-like arc density plot in a polar coordinate system.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default: "arcydensity").
#' @param position The position adjustment to be applied (default: "dodge").
#' @param draw_quantiles Should quantiles be drawn? (default: NULL).
#' @param trim Should the violin be trimmed? (default: TRUE).
#' @param bounds The bounds for the density calculation (default: c(-Inf, Inf)).
#' @param scale The scaling method (default: "area").
#' @param na.rm Should missing values be removed? (default: FALSE).
#' @param orientation Orientation of the violin plot (default: NA).
#' @param show.legend Should the legend be shown? (default: NA).
#' @param inherit.aes Should aesthetics be inherited? (default: TRUE).
#' @param polar.every Should polar coordinate be used for every data point? (default: FALSE).
#' @param quantiles.colour Colour of quantile lines (default: "black").
#' @param quantiles.linewidth Line width of quantile lines (default: 1).
#' @param quantiles.linetype Line type of quantile lines (default: 1).
#' @param extend.xscale Extension factor for x-axis scale (default: 0.05).
#' @param extend.yscale Extension factor for y-axis scale (default: 0.05).
#' @param add.bg Should a background be added? (default: TRUE).
#' @param sector.bg.extend Extension factor for sector background (default: 0.025).
#' @param add.xaxis Should x-axis be added? (default: TRUE).
#' @param add.yaxis Should y-axis be added? (default: TRUE).
#' @param xAxis.params A list of parameters for x-axis (default: list()).
#' @param yAxis.params A list of parameters for y-axis (default: list()).
#' @param lineend Type of line end (default: "butt").
#' @param linejoin Type of line join (default: "round").
#' @param ... Additional parameters to be passed to the \code{GeomArcviolin}.
#'
#' @return A layer to be added to a ggplot object.
#'
#' @seealso \code{\link[ggplot2]{geom_violin}}
#'
#' @import ggplot2
#'
#' @export
geom_arcviolin <- function(mapping = NULL, data = NULL,
                           stat = "ydensity", position = "dodge",
                           draw_quantiles = NULL,
                           trim = TRUE,
                           bounds = c(-Inf, Inf),
                           scale = "area",
                           na.rm = FALSE,
                           orientation = NA,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           polar.every = FALSE,
                           quantiles.colour = "black",
                           quantiles.linewidth = 1,
                           quantiles.linetype = 1,
                           extend.xscale = 0.05,
                           extend.yscale = 0.05,
                           add.bg = TRUE,
                           sector.bg.extend = 0.025,
                           add.xaxis = TRUE,
                           add.yaxis = TRUE,
                           xAxis.params = list(),
                           yAxis.params = list(),
                           lineend = "butt", linejoin = "round",...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArcviolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      draw_quantiles = draw_quantiles,
      na.rm = na.rm,
      orientation = orientation,
      bounds = bounds,
      polar.every = polar.every,
      quantiles.colour = quantiles.colour,
      quantiles.linewidth = quantiles.linewidth,
      quantiles.linetype = quantiles.linetype,
      extend.xscale = extend.xscale,
      extend.yscale = extend.yscale,
      add.bg = add.bg,
      sector.bg.extend = sector.bg.extend,
      add.xaxis = add.xaxis,
      add.yaxis = add.yaxis,
      xAxis.params = xAxis.params,
      yAxis.params = yAxis.params,
      lineend = lineend, linejoin = linejoin,
      ...
    )
  )
}





# Returns a data.frame with info needed to draw quantile segments.
create_quantile_segment_frame <- function(data, draw_quantiles) {
  dens <- cumsum(data$density) / sum(data$density)
  ecdf <- stats::approxfun(dens, data$y, ties = "ordered")
  ys <- ecdf(draw_quantiles) # these are all the y-values for quantiles

  # Get the violin bounds for the requested quantiles.
  violin.xminvs <- (stats::approxfun(data$y, data$xminv))(ys)
  violin.xmaxvs <- (stats::approxfun(data$y, data$xmaxv))(ys)

  # We have two rows per segment drawn. Each segment gets its own group.
  data_frame0(
    x = interleave(violin.xminvs, violin.xmaxvs),
    y = rep(ys, each = 2),
    group = rep(ys, each = 2)
  )
}




# Interleave (or zip) multiple units into one vector
interleave <- function(...) UseMethod("interleave")

#' @export
interleave.unit <- function(...) {
  units <- lapply(list(...), as.list)
  interleaved_list <- interleave.default(!!!units)
  inject(grid::unit.c(!!!interleaved_list))
}

#' @export
interleave.default <- function(...) {
  vec_interleave(...)
}
