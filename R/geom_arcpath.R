#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomArcpath <- ggproto("GeomArcpath", Geom,
                       required_aes = c("x", "y"),

                       default_aes = aes(start = 0,end = 180,r0 = 0.5,r1 = 1,
                                         clock.wise = FALSE,
                                         colour = "black",linewidth = .5,
                                         linetype = 1,fill = NA,
                                         alpha = NA,
                                         sector.bg.fill = "grey95",
                                         sector.bg.col = "black",
                                         sector.bg.lty = 1,
                                         sector.bg.lwd = 0.5,id = 1),

                       handle_na = function(self, data, params) {
                         # Drop missing values at the start or end of a line - can't drop in the
                         # middle since you expect those to be shown by a break in the line
                         complete <- stats::complete.cases(data[names(data) %in% c("x", "y", "linewidth", "colour", "linetype")])
                         kept <- stats::ave(complete, data$group, FUN = keep_mid_true)
                         data <- data[kept, ]

                         if (!all(kept) && !params$na.rm) {
                           cli::cli_warn(paste0(
                             "Removed {sum(!kept)} row{?s} containing missing values or values ",
                             "outside the scale range ({.fn {snake_class(self)}})."
                           ))
                         }

                         data
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
                                             arrow = NULL,
                                             lineend = "butt", linejoin = "round", linemitre = 10,
                                             na.rm = FALSE) {
                         data <- check_linewidth(data, snake_class(self))
                         if (!anyDuplicated(data$group)) {
                           cli::cli_inform(c(
                             "{.fn {snake_class(self)}}: Each group consists of only one observation.",
                             i = "Do you need to adjust the {.field group} aesthetic?"
                           ))
                         }

                         # must be sorted on group
                         data <- data[order(data$group), , drop = FALSE]
                         munched <- coord_munch(coord, data, panel_params)

                         # Silently drop lines with less than two points, preserving order
                         rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
                         munched <- munched[rows >= 2, ]
                         if (nrow(munched) < 2) return(zeroGrob())

                         # Work out whether we should use lines or segments
                         attr <- dapply(munched, "group", function(df) {
                           linetype <- unique0(df$linetype)
                           data_frame0(
                             solid = identical(linetype, 1) || identical(linetype, "solid"),
                             constant = nrow(unique0(df[, names(df) %in% c("alpha", "colour", "linewidth", "linetype")])) == 1,
                             .size = 1
                           )
                         })
                         solid_lines <- all(attr$solid)
                         constant = T

                         # constant <- all(attr$constant)
                         # if (!solid_lines && !constant) {
                         #   cli::cli_abort("{.fn {snake_class(self)}} can't have varying {.field colour}, {.field linewidth}, and/or {.field alpha} along the line when {.field linetype} isn't solid")
                         # }

                         # Work out grouping variables for grobs
                         n <- nrow(munched)
                         group_diff <- munched$group[-1] != munched$group[-n]
                         start <- c(TRUE, group_diff)
                         end <-   c(group_diff, TRUE)

                         # =====================================================================================
                         # get xlim and ylim range
                         xlim <- range(data$x)
                         ylim <- range(data$y)

                         # update xaxis params
                         arcxAxisGrob.params <- modifyList(list(xscale = xlim),xAxis.params)
                         arcyAxisGrob.params <- modifyList(list(yscale = ylim),yAxis.params)

                         # draw grobs
                         if(add.bg == TRUE){
                           sector.grob <- arcSectorGrob(start = unique(munched$start),
                                                        end = unique(munched$end),
                                                        r0 = unique(munched$r0),r1 = unique(munched$r1),
                                                        extend.xscale = extend.xscale - sector.bg.extend,
                                                        extend.yscale = extend.yscale - sector.bg.extend,
                                                        xaxis.extend.xscale = extend.xscale,
                                                        xaxis.extend.yscale = extend.yscale - sector.bg.extend,
                                                        yaxis.extend.xscale = extend.xscale - sector.bg.extend,
                                                        yaxis.extend.yscale = extend.yscale,
                                                        add.xaxis = add.xaxis,add.yaxis = add.yaxis,
                                                        arcxAxisGrob.params = arcxAxisGrob.params,
                                                        arcyAxisGrob.params = arcyAxisGrob.params,
                                                        sector.gp = gpar(col = munched$sector.bg.col,
                                                                         fill = munched$sector.bg.fill,
                                                                         lwd = munched$sector.bg.lwd,
                                                                         lty = munched$sector.bg.lty),
                                                        clock.wise = unique(munched$clock.wise))
                         }else{
                           sector.grob <- nullGrob()
                         }

                         # =====================================================
                         # which type grob to draw
                         if (!constant) {
                           arrow <- repair_segment_arrow(arrow, munched$group)

                           line.grob <- arcSegmentsGrob(xmin = rescale(munched$x[!end],to = xlim),
                                                        ymin = rescale(munched$y[!end],to = ylim),
                                                        xmax = rescale(munched$x[!start],to = xlim),
                                                        ymax = rescale(munched$y[!start],to = ylim),
                                                        start = unique(munched$start),
                                                        end = unique(munched$end),
                                                        r0 = unique(munched$r0),r1 = unique(munched$r1),
                                                        arrow = arrow,
                                                        # id = coords$group,
                                                        xscale = arcxAxisGrob.params$xscale,
                                                        yscale = arcyAxisGrob.params$yscale,
                                                        polyline.gp = gpar(
                                                          col = alpha(munched$colour, unique(munched$alpha)),
                                                          fill = alpha(munched$colour, unique(munched$alpha)),
                                                          lwd = unique(munched$linewidth),
                                                          lty = unique(munched$linetype),
                                                          lineend = lineend,
                                                          linejoin = linejoin,
                                                          linemitre = linemitre),
                                                        extend.xscale = extend.xscale,
                                                        extend.yscale = extend.yscale,
                                                        clock.wise = unique(munched$clock.wise))
                         } else {
                           id <- match(munched$group, unique0(munched$group))

                           # col and fill
                           col.d <- unique(munched[,c("fill","colour","group")])

                           line.grob <- arcLinesGrob(x = rescale(munched$x,to = xlim),
                                                     y = rescale(munched$y,to = ylim),
                                                     start = unique(munched$start),
                                                     end = unique(munched$end),
                                                     r0 = unique(munched$r0),r1 = unique(munched$r1),
                                                     arrow = arrow,
                                                     id = id,
                                                     xscale = arcxAxisGrob.params$xscale,
                                                     yscale = arcyAxisGrob.params$yscale,
                                                     lines.gp = gpar(
                                                       col = alpha(col.d$colour, unique(munched$alpha)),
                                                       fill = alpha(col.d$colour, unique(munched$alpha)),
                                                       lwd = unique(munched$linewidth),
                                                       lty = munched$linetype[start],
                                                       lineend = lineend,
                                                       linejoin = linejoin,
                                                       linemitre = linemitre),
                                                     extend.xscale = extend.xscale,
                                                     extend.yscale = extend.yscale,
                                                     clock.wise = unique(munched$clock.wise))
                         }

                         ggname("geom_arcpath",
                                grid::gTree(children = gList(sector.grob,line.grob)))
                       },

                       draw_key = draw_key_path,

                       rename_size = TRUE
)



#' Create a path of arcs with options for line style and arrowheads.
#'
#' \code{geom_arcpath} is used to create a path of arcs with options for line style, arrowheads, and other visual properties.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default: "identity").
#' @param position The position adjustment to be applied (default: "identity").
#' @param na.rm Should missing values be removed? (default: FALSE)
#' @param arrow Arrowhead specifications (default: NULL).
#' @param show.legend Should the legend be shown? (default: NA).
#' @param inherit.aes Should aesthetics be inherited? (default: TRUE)
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
#' @param ... Additional parameters to be passed to the \code{GeomArcpath}.
#'
#' @return A layer to be added to a ggplot object.
#'
#' @seealso \code{\link[ggplot2]{geom_path}}
#'
#' @import ggplot2
#'
#' @export
geom_arcpath <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         na.rm = FALSE,arrow = NULL,
                         show.legend = NA,
                         inherit.aes = TRUE,
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
    geom = GeomArcpath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      na.rm = na.rm,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
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



# Trim false values from left and right: keep all values from
# first TRUE to last TRUE
keep_mid_true <- function(x) {
  first <- match(TRUE, x) - 1
  if (is.na(first)) {
    return(rep(FALSE, length(x)))
  }

  last <- length(x) - match(TRUE, rev(x)) + 1
  c(
    rep(FALSE, first),
    rep(TRUE, last - first),
    rep(FALSE, length(x) - last)
  )
}


repair_segment_arrow <- function(arrow, group) {
  # Early exit if there is no arrow
  if (is.null(arrow)) {
    return(arrow)
  }

  # Get group parameters
  rle       <- vec_group_rle(group) # handles NAs better than base::rle()
  n_groups  <- length(rle)
  rle_len   <- field(rle, "length") - 1 # segments have 1 member less than lines
  rle_end   <- cumsum(rle_len)
  rle_start <- rle_end - rle_len + 1

  # Recycle ends and lengths
  ends <- rep(rep(arrow$ends,   length.out = n_groups), rle_len)
  len  <- rep(rep(arrow$length, length.out = n_groups), rle_len)

  # Repair ends
  # Convert 'both' ends to first/last in multi-member groups
  is_both <- which(ends == 3)
  ends[setdiff(intersect(rle_start, is_both), rle_end)] <- 1L
  ends[setdiff(intersect(rle_end, is_both), rle_start)] <- 2L
  arrow$ends <- ends

  # Repair lengths
  zero <- unit(0, "mm")
  # Set length of first segment to zero when ends is 'last'
  len[intersect(setdiff(rle_start, rle_end), which(ends == 2))] <- zero
  # Set length of last segment to zero when ends is 'first'
  len[intersect(setdiff(rle_end, rle_start), which(ends == 1))] <- zero
  # Set length of middle pieces to zero
  len[setdiff(seq_along(len), c(rle_start, rle_end))] <- zero
  arrow$length <- len

  return(arrow)
}
