#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomArcboxplot <- ggproto("GeomArcboxplot", Geom,
                          required_aes = c("x|y", "lower|xlower", "upper|xupper", "middle|xmiddle", "ymin|xmin", "ymax|xmax"),

                          draw_key = draw_key_boxplot,

                          default_aes = aes(weight = 1, shape = 19,
                                            start = 0,end = 180,r0 = 0.5,r1 = 1,
                                            clock.wise = FALSE,
                                            colour = "black",linewidth = .5,
                                            linetype = 1,fill = "white",
                                            alpha = NA,
                                            sector.bg.fill = "grey95",
                                            sector.bg.col = "black",
                                            sector.bg.lty = 1,
                                            sector.bg.lwd = 0.5),



                          # need to declare `width` here in case this geom is used with a stat that
                          # doesn't have a `width` parameter (e.g., `stat_identity`).
                          extra_params = c("na.rm", "width", "orientation", "outliers"),

                          setup_params = function(data, params) {
                            params$flipped_aes <- has_flipped_aes(data, params)
                            params
                          },

                          setup_data = function(data, params) {
                            data$flipped_aes <- params$flipped_aes
                            data <- flip_data(data, params$flipped_aes)
                            data$width <- data$width %||%
                              params$width %||% (resolution(data$x, FALSE) * 0.9)

                            if (isFALSE(params$outliers)) {
                              data$outliers <- NULL
                            }

                            if (!is.null(data$outliers)) {
                              suppressWarnings({
                                out_min <- vapply(data$outliers, min, numeric(1))
                                out_max <- vapply(data$outliers, max, numeric(1))
                              })

                              data$ymin_final  <- pmin(out_min, data$ymin)
                              data$ymax_final  <- pmax(out_max, data$ymax)
                            }

                            # if `varwidth` not requested or not available, don't use it
                            if (is.null(params) || is.null(params$varwidth) || !params$varwidth || is.null(data$relvarwidth)) {
                              data$xmin <- data$x - data$width / 2
                              data$xmax <- data$x + data$width / 2
                            } else {
                              # make `relvarwidth` relative to the size of the largest group
                              data$relvarwidth <- data$relvarwidth / max(data$relvarwidth)
                              data$xmin <- data$x - data$relvarwidth * data$width / 2
                              data$xmax <- data$x + data$relvarwidth * data$width / 2
                            }
                            data$width <- NULL
                            if (!is.null(data$relvarwidth)) data$relvarwidth <- NULL

                            # transform(data,
                            #           all_ymin = min(data$ymin_final),
                            #           all_ymax = max(data$ymax_final),
                            #           all_xmin = min(data$xmin),
                            #           all_xmax = max(data$xmax))

                            flip_data(data, params$flipped_aes)
                            # print(flip_data(data, params$flipped_aes))
                          },

                          draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                                                linejoin = "mitre", fatten = 2,
                                                outlier.colour = NULL,
                                                outlier.fill = NULL,
                                                outlier.shape = 19,
                                                outlier.size = 1.5,
                                                outlier.stroke = 0.5,
                                                outlier.alpha = NULL,
                                                notch = FALSE, notchwidth = 0.5,
                                                staplewidth = 0, varwidth = FALSE, flipped_aes = FALSE,
                                                extend.xscale = 0.05,
                                                extend.yscale = 0.05,
                                                add.bg = TRUE,
                                                sector.bg.extend = 0.025,
                                                add.xaxis = TRUE,
                                                add.yaxis = TRUE,
                                                xAxis.params = list(),
                                                yAxis.params = list()) {

                            data <- check_linewidth(data, snake_class(self))
                            data <- flip_data(data, flipped_aes)

                            # print(flip_data(data, flipped_aes))
                            # this may occur when using geom_boxplot(stat = "identity")
                            # if (nrow(data) != 1) {
                            #   cli::cli_abort(c(
                            #     "Can only draw one boxplot per group",
                            #     "i"= "Did you forget {.code aes(group = ...)}?"
                            #   ))
                            # }

                            # ==================================================
                            common_fun <- function(data){
                              common <- list(
                                colour = data$colour,
                                linewidth = data$linewidth,
                                linetype = data$linetype,
                                fill = alpha(data$fill, data$alpha),
                                group = data$group,
                                start = data$start,
                                end = data$end,
                                r0 = data$r0,
                                r1 = data$r1,
                                clock.wise = data$clock.wise,
                                sector.bg.fill = data$sector.bg.fill,
                                sector.bg.col = data$sector.bg.col,
                                sector.bg.lty = data$sector.bg.lty,
                                sector.bg.lwd = data$sector.bg.lwd
                              )
                            }

                            # loop combine data
                            gp <- unique(data$group)

                            lapply(seq_along(gp), function(g){
                              data <- data[which(data$group == gp[g]),]

                              whiskers <- data_frame0(
                                xmin = c(data$x, data$x),
                                xmax = c(data$x, data$x),
                                ymin = c(data$upper, data$lower),
                                ymax = c(data$ymax, data$ymin),
                                alpha = c(NA_real_, NA_real_),
                                !!!common_fun(data),
                                .size = 2
                              )

                              whiskers <- flip_data(whiskers, flipped_aes)

                              # =====================================================
                              box <- data_frame0(
                                xmin = data$xmin,
                                xmax = data$xmax,
                                ymin = data$lower,
                                y = data$middle,
                                ymax = data$upper,
                                ynotchlower = ifelse(notch, data$notchlower, NA),
                                ynotchupper = ifelse(notch, data$notchupper, NA),
                                notchwidth = notchwidth,
                                alpha = data$alpha,
                                !!!common_fun(data)
                              )
                              box <- flip_data(box, flipped_aes)

                              cmb <- list(whiskers,box)
                              return(cmb)
                            }) -> res_data

                            whiskers <- Reduce("rbind",sapply(res_data, "[",1))
                            box <- Reduce("rbind",sapply(res_data, "[",2))

                            # process global data
                            t_data <- flip_data(data, flipped_aes)

                            # ==================================================
                            # get xlim and ylim range
                            if(unique(t_data$flipped_aes) == "TRUE"){
                              xlim <- range(t_data$xmin_final,t_data$xmax_final)
                              ylim <- range(t_data$ymin,t_data$ymax)
                            }else{
                              xlim <- range(t_data$xmin,t_data$xmax)
                              ylim <- range(t_data$ymin_final,t_data$ymax_final)
                            }

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

                            # =====================================================
                            # loop get data
                            lapply(seq_along(gp), function(g){
                              data <- data[which(data$group == gp[g]),]
                              # print(data)
                              if (suppressWarnings(!is.null(data$outliers) && length(data$outliers[[1]]) >= 1)) {
                                outliers <- data_frame0(
                                  y = data$outliers[[1]],
                                  x = data$x[1],
                                  colour = outlier.colour %||% data$colour[1],
                                  fill = outlier.fill %||% data$fill[1],
                                  shape = outlier.shape %||% data$shape[1],
                                  size = outlier.size %||% data$size[1],
                                  stroke = outlier.stroke %||% data$stroke[1],
                                  fill = NA,
                                  alpha = outlier.alpha %||% data$alpha[1],
                                  .size = length(data$outliers[[1]]),
                                  !!!common_fun(data)
                                )
                                # print(outliers)
                                outliers <- flip_data(outliers, flipped_aes)
                              } else {
                                # outliers <- NULL
                              }

                            }) %>% Reduce("rbind",.) -> outliers

                            # outliers_grob <- NULL
                            # print(outliers)

                            outliers_grob <- GeomArcpoint$draw_panel(outliers, panel_params, coord,
                                                                     extend.xscale = extend.xscale,
                                                                     extend.yscale = extend.yscale,
                                                                     add.bg = FALSE,
                                                                     sector.bg.extend = sector.bg.extend,
                                                                     add.xaxis = FALSE,
                                                                     add.yaxis = FALSE,
                                                                     xAxis.params = arcxAxisGrob.params,
                                                                     yAxis.params = arcyAxisGrob.params)
                            # =====================================================
                            if (staplewidth != 0) {
                              # loop get data
                              lapply(seq_along(gp), function(g){
                                data <- data[which(data$group == gp[g]),]
                                print(data)
                                staples <- data_frame0(
                                  xmin    = rep((data$xmin - data$x) * staplewidth + data$x, 2),
                                  xmax = rep((data$xmax - data$x) * staplewidth + data$x, 2),
                                  ymin    = c(data$ymax, data$ymin),
                                  ymax = c(data$ymax, data$ymin),
                                  alpha = c(NA_real_, NA_real_),
                                  !!!common,
                                  .size = 2
                                )

                                staples <- flip_data(staples, flipped_aes)
                              }) %>% Reduce("rbind",.) -> staples



                              staple_grob <- GeomArcsegment$draw_panel(staples, panel_params, coord,
                                                                       extend.xscale = extend.xscale,
                                                                       extend.yscale = extend.yscale,
                                                                       add.bg = FALSE,
                                                                       sector.bg.extend = sector.bg.extend,
                                                                       add.xaxis = FALSE,
                                                                       add.yaxis = FALSE,
                                                                       xAxis.params = arcxAxisGrob.params,
                                                                       yAxis.params = arcyAxisGrob.params)
                            } else {
                              staple_grob <- NULL
                            }

                            # =====================================================
                            # combine grobs
                            # =====================================================
                            if(unique(data$flipped_aes) == "FALSE"){
                              box <- transform(box,x = (xmin + xmax)/2)
                            }else{
                              box <- transform(box,y = (ymin + ymax)/2)
                            }

                            box <- transform(box,flipped_aes = unique(data$flipped_aes))

                            # draw grobs
                            if(add.bg == TRUE){
                              sector.grob <- arcSectorGrob(start = unique(data$start),
                                                           end = unique(data$end),
                                                           r0 = unique(data$r0),r1 = unique(data$r1),
                                                           extend.xscale = extend.xscale - sector.bg.extend,
                                                           extend.yscale = extend.yscale - sector.bg.extend,
                                                           xaxis.extend.xscale = extend.xscale,
                                                           xaxis.extend.yscale = extend.yscale - sector.bg.extend,
                                                           yaxis.extend.xscale = extend.xscale - sector.bg.extend,
                                                           yaxis.extend.yscale = extend.yscale,
                                                           add.xaxis = add.xaxis,add.yaxis = add.yaxis,
                                                           arcxAxisGrob.params = arcxAxisGrob.params,
                                                           arcyAxisGrob.params = arcyAxisGrob.params,
                                                           sector.gp = gpar(col = data$sector.bg.col,
                                                                            fill = data$sector.bg.fill,
                                                                            lwd = data$sector.bg.lwd,
                                                                            lty = data$sector.bg.lty),
                                                           clock.wise = unique(data$clock.wise))
                            }else{
                              sector.grob <- nullGrob()
                            }

                            ggname("geom_arcboxplot", grobTree(
                              sector.grob,
                              outliers_grob,
                              staple_grob,
                              GeomArcsegment$draw_panel(whiskers, panel_params, coord,
                                                        extend.xscale = extend.xscale,
                                                        extend.yscale = extend.yscale,
                                                        add.bg = FALSE,
                                                        sector.bg.extend = sector.bg.extend,
                                                        add.xaxis = FALSE,
                                                        add.yaxis = FALSE,
                                                        xAxis.params = arcxAxisGrob.params,
                                                        yAxis.params = arcyAxisGrob.params),


                              GeomArcrossbar$draw_panel(box,fatten = fatten,panel_params,coord,
                                                        extend.xscale = extend.xscale,
                                                        extend.yscale = extend.yscale,
                                                        add.bg = FALSE,
                                                        sector.bg.extend = sector.bg.extend,
                                                        add.xaxis = FALSE,
                                                        add.yaxis = FALSE,
                                                        xAxis.params = arcxAxisGrob.params,
                                                        yAxis.params = arcyAxisGrob.params,
                                                        lineend = lineend,
                                                        linejoin = linejoin,
                                                        flipped_aes = flipped_aes)
                            ))
                          },

                          rename_size = TRUE
)



#' Create an arc boxplot layer for ggplot2.
#'
#' \code{geom_arcboxplot} is used to create an arc boxplot layer for ggplot2.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default: "arcboxplot").
#' @param position The position adjustment to be applied (default: "dodge2").
#' @param outliers Should outliers be displayed? (default: TRUE)
#' @param outlier.colour Color of the outlier points (default: NULL).
#' @param outlier.color Color of the outlier points (default: NULL).
#' @param outlier.fill Fill color of the outlier points (default: NULL).
#' @param outlier.shape Shape of the outlier points (default: 19).
#' @param outlier.size Size of the outlier points (default: 1.5).
#' @param outlier.stroke Stroke width of the outlier points (default: 0.5).
#' @param outlier.alpha Alpha transparency of the outlier points (default: NULL).
#' @param notch Should notches be added to the boxplots? (default: FALSE)
#' @param notchwidth Width of notches (default: 0.5).
#' @param staplewidth Width of staple lines (default: 0).
#' @param varwidth Should box widths vary with sample sizes? (default: FALSE)
#' @param na.rm Should missing values be removed? (default: FALSE)
#' @param orientation Orientation of the arc boxplot (default: NA).
#' @param show.legend Should the legend be shown? (default: NA).
#' @param inherit.aes Should aesthetics be inherited? (default: TRUE)
#' @param extend.xscale Extension factor for x-axis scale (default: 0.05).
#' @param extend.yscale Extension factor for y-axis scale (default: 0.05).
#' @param add.bg Should a background be added? (default: TRUE)
#' @param sector.bg.extend Extension factor for sector background (default: 0.025).
#' @param add.xaxis Should x-axis be added? (default: TRUE)
#' @param add.yaxis Should y-axis be added? (default: TRUE)
#' @param xAxis.params A list of parameters for x-axis (default: list()).
#' @param yAxis.params A list of parameters for y-axis (default: list()).
#' @param ... Additional parameters to be passed to the \code{GeomArcboxplot}.
#'
#' @return A layer to be added to a ggplot object.
#'
#' @seealso \code{\link[ggplot2]{geom_boxplot}}
#'
#' @import ggplot2
#' @importFrom rlang %||%
#'
#' @export
geom_arcboxplot <- function(mapping = NULL, data = NULL,
                            stat = "boxplot", position = "dodge2",
                            outliers = TRUE,
                            outlier.colour = NULL,
                            outlier.color = NULL,
                            outlier.fill = NULL,
                            outlier.shape = 19,
                            outlier.size = 1.5,
                            outlier.stroke = 0.5,
                            outlier.alpha = NULL,
                            notch = FALSE,
                            notchwidth = 0.5,
                            staplewidth = 0,
                            varwidth = FALSE,
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
                            yAxis.params = list(),...) {

  # varwidth = TRUE is not compatible with preserve = "total"
  if (is.character(position)) {
    if (varwidth == TRUE) position <- position_dodge2(preserve = "single")
  } else {
    if (identical(position$preserve, "total") & varwidth == TRUE) {
      cli::cli_warn("Can't preserve total widths when {.code varwidth = TRUE}.")
      position$preserve <- "single"
    }
  }

  check_number_decimal(staplewidth)
  check_bool(outliers)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArcboxplot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outliers = outliers,
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      notch = notch,
      notchwidth = notchwidth,
      staplewidth = staplewidth,
      varwidth = varwidth,
      na.rm = na.rm,
      orientation = orientation,
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

is_mapped_discrete <- function(x) inherits(x, "mapped_discrete")
