#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomArcribbon <- ggproto("GeomArcribbon", Geom,
                         required_aes = c("x|y", "ymin|xmin", "ymax|xmax"),

                         default_aes = aes(start = 0,end = 180,r0 = 0.5,r1 = 1,
                                           clock.wise = FALSE,
                                           colour = NA,fill = "grey80",
                                           linetype = 1,
                                           alpha = NA,linewidth = .5,
                                           sector.bg.fill = "transparent",
                                           sector.bg.col = "black",
                                           sector.bg.lty = 1,
                                           sector.bg.lwd = 0.5),

                         draw_key = draw_key_polygon,

                         setup_params = function(data, params) {
                           params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = TRUE)
                           params
                         },

                         extra_params = c("na.rm", "orientation"),

                         setup_data = function(data, params) {
                           data$flipped_aes <- params$flipped_aes
                           data <- flip_data(data, params$flipped_aes)

                           if (is.null(data$ymin) && is.null(data$ymax)) {
                             cli::cli_abort("Either {.field {flipped_names(params$flipped_aes)$ymin}} or {.field {flipped_names(params$flipped_aes)$ymax}} must be given as an aesthetic.")
                           }
                           data <- data[order(data$PANEL, data$group, data$x), , drop = FALSE]
                           data$y <- data$ymin %||% data$ymax
                           flip_data(data, params$flipped_aes)
                         },


                         handle_na = function(data, params) {
                           data
                         },

                         # draw_group = function(self, data, panel_params, coord,
                         draw_panel = function(self, data, panel_params, coord,
                                               polar.every = TRUE,
                                               extend.xscale = 0.05,
                                               extend.yscale = 0.05,
                                               add.bg = TRUE,
                                               sector.bg.extend = 0.025,
                                               add.xaxis = TRUE,
                                               add.yaxis = TRUE,
                                               xAxis.params = list(),
                                               yAxis.params = list(),
                                               lineend = "butt",linejoin = "round", linemitre = 10,
                                               na.rm = FALSE,flipped_aes = FALSE, outline.type = "both") {
                           data <- check_linewidth(data, snake_class(self))
                           data <- flip_data(data, flipped_aes)
                           if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
                           data <- data[order(data$group), ]

                           tmpdata <- data

                           # col and fill
                           col.d <- unique(tmpdata[,c("fill","colour","group")])

                           # ===================================================
                           # loop calculate xy
                           group <- unique(tmpdata$group)
                           lapply(seq_along(group), function(x){
                             loop.data <- tmpdata[which(tmpdata$group == group[x]),]

                             # Check that aesthetics are constant
                             aes <- unique0(loop.data[names(loop.data) %in% c("colour", "fill", "linewidth", "linetype", "alpha")])
                             if (nrow(aes) > 1) {
                               cli::cli_abort("Aesthetics can not vary along a ribbon")
                             }
                             aes <- as.list(aes)

                             # Instead of removing NA values from the data and plotting a single
                             # polygon, we want to "stop" plotting the polygon whenever we're
                             # missing values and "start" a new polygon as soon as we have new
                             # values.  We do this by creating an id vector for polygonGrob that
                             # has distinct polygon numbers for sequences of non-NA values and NA
                             # for NA values in the original data.  Example: c(NA, 2, 2, 2, NA, NA,
                             # 4, 4, 4, NA)
                             missing_pos <- !stats::complete.cases(loop.data[c("x", "ymin", "ymax")])
                             ids <- cumsum(missing_pos) + 1
                             ids[missing_pos] <- NA

                             loop.data <- unclass(loop.data) #for faster indexing

                             # In case the data comes from stat_align
                             upper_keep <- if (is.null(loop.data$align_padding)) TRUE else !loop.data$align_padding

                             # The upper line and lower line need to processed separately (#4023)
                             positions_upper <- data_frame0(
                               x = loop.data$x[upper_keep],
                               y = loop.data$ymax[upper_keep],
                               id = ids[upper_keep]
                             )

                             positions_lower <- data_frame0(
                               x = rev(loop.data$x),
                               y = rev(loop.data$ymin),
                               id = rev(ids)
                             )

                             positions_upper <- flip_data(positions_upper, flipped_aes)
                             positions_lower <- flip_data(positions_lower, flipped_aes)

                             munched_upper <- coord_munch(coord, positions_upper, panel_params)
                             munched_lower <- coord_munch(coord, positions_lower, panel_params)

                             munched_poly <- vec_rbind0(munched_upper, munched_lower)

                             # is_full_outline <- identical(outline.type, "full")

                             munched_poly <- transform(munched_poly,
                                                       alpha = unique(loop.data$alpha),
                                                       colour = unique(loop.data$colour),
                                                       linewidth = unique(loop.data$linewidth),
                                                       linetype = unique(loop.data$linetype),
                                                       fill = unique(loop.data$fill),
                                                       group = unique(loop.data$group),
                                                       start = unique(loop.data$start),
                                                       end = unique(loop.data$end),
                                                       r0 = unique(loop.data$r0),
                                                       r1 = unique(loop.data$r1),
                                                       clock.wise = unique(loop.data$clock.wise),
                                                       sector.bg.fill = unique(loop.data$sector.bg.fill),
                                                       sector.bg.col = unique(loop.data$sector.bg.col),
                                                       sector.bg.lty = unique(loop.data$sector.bg.lty),
                                                       sector.bg.lwd = unique(loop.data$sector.bg.lwd))

                             # ==================================================================================================
                             # Increment the IDs of the lower line so that they will be drawn as separate lines
                             munched_lower$id <- munched_lower$id + max(ids, na.rm = TRUE)

                             munched_lines <- switch(outline.type,
                                                     both = vec_rbind0(munched_upper, munched_lower),
                                                     upper = munched_upper,
                                                     lower = munched_lower,
                                                     cli::cli_abort(c(
                                                       "invalid {.arg outline.type}: {.val {outline.type}}",
                                                       "i" = "use either {.val upper}, {.val lower}, or {.val both}"
                                                     ))
                             )

                             munched_lines <- transform(munched_lines,
                                                        alpha = unique(loop.data$alpha),
                                                        colour = unique(loop.data$colour),
                                                        linewidth = unique(loop.data$linewidth),
                                                        linetype = unique(loop.data$linetype),
                                                        fill = unique(loop.data$fill),
                                                        group = unique(loop.data$group),
                                                        start = unique(loop.data$start),
                                                        end = unique(loop.data$end),
                                                        r0 = unique(loop.data$r0),
                                                        r1 = unique(loop.data$r1),
                                                        clock.wise = unique(loop.data$clock.wise),
                                                        sector.bg.fill = unique(loop.data$sector.bg.fill),
                                                        sector.bg.col = unique(loop.data$sector.bg.col),
                                                        sector.bg.lty = unique(loop.data$sector.bg.lty),
                                                        sector.bg.lwd = unique(loop.data$sector.bg.lwd))

                             res <- list(munched_poly,munched_lines)
                           }) -> res

                           is_full_outline <- identical(outline.type, "full")

                           munched_poly <- Reduce("rbind",sapply(res, "[",1))
                           munched_lines <- Reduce("rbind",sapply(res, "[",2))

                           # ================================================================================================
                           # get xlim and ylim range
                           if(unique(tmpdata$flipped_aes) == FALSE){
                             xlim <- range(tmpdata$x)
                             ylim <- range(tmpdata$ymin,tmpdata$ymax)
                           }else{
                             xlim <- range(tmpdata$ymin,tmpdata$ymax)
                             ylim <- range(tmpdata$x)
                           }


                           # update xaxis params
                           arcxAxisGrob.params <- modifyList(list(xscale = xlim),xAxis.params)
                           arcyAxisGrob.params <- modifyList(list(yscale = ylim),yAxis.params)


                           # draw grobs
                           if(add.bg == TRUE){
                             sector.grob <- arcSectorGrob(start = unique(tmpdata$start),
                                                          end = unique(tmpdata$end),
                                                          r0 = unique(tmpdata$r0),r1 = unique(tmpdata$r1),
                                                          extend.xscale = extend.xscale - sector.bg.extend,
                                                          extend.yscale = extend.yscale - sector.bg.extend,
                                                          xaxis.extend.xscale = extend.xscale,
                                                          xaxis.extend.yscale = extend.yscale - sector.bg.extend,
                                                          yaxis.extend.xscale = extend.xscale - sector.bg.extend,
                                                          yaxis.extend.yscale = extend.yscale,
                                                          add.xaxis = add.xaxis,add.yaxis = add.yaxis,
                                                          arcxAxisGrob.params = arcxAxisGrob.params,
                                                          arcyAxisGrob.params = arcyAxisGrob.params,
                                                          sector.gp = gpar(col = tmpdata$sector.bg.col,
                                                                           fill = tmpdata$sector.bg.fill,
                                                                           lwd = tmpdata$sector.bg.lwd,
                                                                           lty = tmpdata$sector.bg.lty),
                                                          clock.wise = unique(tmpdata$clock.wise))
                           }else{
                             sector.grob <- nullGrob()
                           }

                           g_poly <- arcPolygonGrob(
                             # x = munched_poly$x,
                             # y = munched_poly$y,
                             x = rescale(munched_poly$x,to = xlim),
                             y = rescale(munched_poly$y,to = ylim),
                             id =  munched_poly$group,
                             polar.every = polar.every,
                             start = unique(munched_poly$start),
                             end = unique(munched_poly$end),
                             r0 = unique(munched_poly$r0),r1 = unique(munched_poly$r1),
                             xscale = arcxAxisGrob.params$xscale,
                             yscale = arcyAxisGrob.params$yscale,
                             polygon.gp = gpar(
                               col = alpha(col.d$colour, unique(munched_poly$alpha)),
                               fill = alpha(col.d$fill, unique(munched_poly$alpha)),
                               lwd = unique(munched_poly$linewidth),
                               lty = unique(munched_poly$linetype),
                               lineend = lineend,
                               linejoin = linejoin),
                             extend.xscale = extend.xscale,
                             extend.yscale = extend.yscale,
                             clock.wise = unique(munched_poly$clock.wise))

                           if (is_full_outline) {
                             return(ggname("geom_ribbon", g_poly))
                           }


                           # print(munched_lines)

                           g_lines <- arcLinesGrob(
                             # x = munched_lines$x,
                             # y = munched_lines$y,
                             x = rescale(munched_lines$x,to = xlim),
                             y = rescale(munched_lines$y,to = ylim),
                             start = unique(munched_lines$start),
                             end = unique(munched_lines$end),
                             r0 = unique(munched_lines$r0),r1 = unique(munched_lines$r1),
                             id = munched_lines$group,
                             xscale = arcxAxisGrob.params$xscale,
                             yscale = arcyAxisGrob.params$yscale,
                             lines.gp = gpar(
                               col = alpha(col.d$colour, unique(munched_lines$alpha)),
                               fill = alpha(col.d$fill, unique(munched_lines$alpha)),
                               lwd = unique(munched_lines$linewidth),
                               lty = unique(munched_lines$linetype),
                               lineend = lineend,
                               linejoin = linejoin,
                               linemitre = linemitre),
                             extend.xscale = extend.xscale,
                             extend.yscale = extend.yscale,
                             clock.wise = unique(munched_lines$clock.wise))


                           ggname("geom_arcribbon", grobTree(sector.grob,g_poly, g_lines))
                         },

                         rename_size = TRUE
)


#' Create ribbons based on arcs.
#'
#' \code{geom_arcribbon} is used to create ribbons based on arcs in a polar plot.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default: "identity").
#' @param position The position adjustment to be applied (default: "identity").
#' @param na.rm Should missing values be removed? (default: FALSE).
#' @param orientation Orientation of the ribbons (default: NA).
#' @param show.legend Should the legend be shown? (default: NA).
#' @param inherit.aes Should aesthetics be inherited? (default: TRUE).
#' @param outline.type Type of outline to be drawn, one of "both", "upper", "lower", or "full" (default: "both").
#' @param lineend Line end style (default: "butt").
#' @param linejoin Line join style (default: "round").
#' @param linemitre Line mitre limit (default: 10).
#' @param polar.every Should ribbons extend across every polar sector? (default: TRUE).
#' @param extend.xscale Extension factor for x-axis scale (default: 0.05).
#' @param extend.yscale Extension factor for y-axis scale (default: 0.05).
#' @param add.bg Should a background be added? (default: TRUE).
#' @param sector.bg.extend Extension factor for sector background (default: 0.025).
#' @param add.xaxis Should x-axis be added? (default: TRUE).
#' @param add.yaxis Should y-axis be added? (default: TRUE).
#' @param xAxis.params A list of parameters for x-axis (default: list()).
#' @param yAxis.params A list of parameters for y-axis (default: list()).
#' @param ... Additional parameters to be passed to the \code{GeomArcribbon}.
#'
#' @return A layer to be added to a ggplot object.
#'
#' @seealso \code{\link[ggplot2]{geom_ribbon}}
#'
#' @import ggplot2
#'
#' @export
geom_arcribbon <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           na.rm = FALSE,
                           orientation = NA,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           outline.type = "both",
                           lineend = "butt", linejoin = "round", linemitre = 10,
                           polar.every = TRUE,
                           extend.xscale = 0.05,
                           extend.yscale = 0.05,
                           add.bg = TRUE,
                           sector.bg.extend = 0.025,
                           add.xaxis = TRUE,
                           add.yaxis = TRUE,
                           xAxis.params = list(),
                           yAxis.params = list(),...) {
  outline.type <-  rlang::arg_match0(outline.type, c("both", "upper", "lower", "full"))

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArcribbon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      outline.type = outline.type,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
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
