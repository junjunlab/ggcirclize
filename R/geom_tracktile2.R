#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @include geom_arctile.R
#' @export
GeomTracktile2 <- ggproto("GeomTracktile2", GeomArctile,

                          default_aes = aes(
                            start = 0,end = 180,r0 = 0.5,r1 = 1,
                            mark = NULL,
                            clock.wise = FALSE,
                            colour = "black",fill = "grey90",
                            linetype = 1,size = 10,
                            alpha = NA,linewidth = .5,
                            width = 1, height = 1,
                            sector.bg.fill = "grey95",
                            sector.bg.col = "black",
                            sector.bg.lty = 1,
                            sector.bg.lwd = 0.5
                          ),

                          draw_panel = function(self,data, panel_params, coord,
                                                mark.col = "black",
                                                mark.pos = c("bottom","top"),
                                                mark.shift = 0.05,
                                                mark.mat = FALSE,
                                                strip.label = TRUE,
                                                strip.label.pos = c("top","bottom"),
                                                strip.label.space = 0.15,
                                                strip.label.fontface = "bold",
                                                strip.label.col = "black",
                                                strip.label.size = 10,
                                                scales = c("fixed","free","free_x","free_y"),
                                                space = c("free_x","fixed"),
                                                sector.gap = 3,
                                                extend.xscale = 0.05,
                                                extend.yscale = 0.05,
                                                add.bg = TRUE,
                                                sector.bg.extend = 0.025,
                                                add.xaxis = TRUE,
                                                add.yaxis = TRUE,
                                                xAxis.params = list(),
                                                yAxis.params = list()) {
                            # check scale type
                            strip.label.pos <- match.arg(strip.label.pos,c("top","bottom"))
                            scales <- match.arg(scales,c("fixed","free","free_x","free_y"))
                            space <- match.arg(space,c("free_x","fixed"))
                            mark.pos <- match.arg(mark.pos,c("bottom","top"))

                            # calculate sector theta
                            sec_df = facet_sector(data = data,sector.gap = sector.gap,space = space)

                            if(length(unique(data$sector)) == 1){
                              sec_df <- sec_df[1,]
                            }

                            # label for different facets
                            if(strip.label.pos == "top"){
                              label.r = unique(data$r1) + strip.label.space
                            }else{
                              label.r = unique(data$r0) - strip.label.space
                            }

                            # whether add xy axis
                            if(length(add.xaxis) == 1){
                              add.xaxis <- rep(add.xaxis,nrow(sec_df))
                            }else{
                              add.xaxis <- add.xaxis
                            }

                            if(length(add.yaxis) == 1){
                              add.yaxis <- rep(add.yaxis,nrow(sec_df))
                            }else{
                              add.yaxis <- add.yaxis
                            }
                            # ====================================================
                            # loop draw grobs
                            gp <- sec_df$sector_name
                            all_limits.x <- panel_params$x$limits
                            all_limits.y <- panel_params$y$limits
                            track_glist <- gList()

                            for (sec in seq_along(gp)) {
                              tmp_data <- data[which(data$sector == gp[sec]),]

                              # check sub-panel scales
                              if(scales == "fixed"){
                                new_panel_params <- panel_params

                                if(!panel_params$x$is_discrete()){
                                  xAxis.params <- modifyList(list(xscale = range(data$x)),xAxis.params)
                                  yAxis.params <- modifyList(list(yscale = range(data$y)),yAxis.params)
                                }else{
                                  xAxis.params <- modifyList(list(xscale = range(data$x - unique(data$width)/2,
                                                                                 data$x + unique(data$width)/2),
                                                                  breaks = 1:length(new_panel_params$x$breaks),
                                                                  breaks.label = new_panel_params$x$limits),
                                                             xAxis.params)
                                  yAxis.params <- modifyList(list(yscale = range(data$y - unique(data$width)/2,
                                                                                 data$y + unique(data$width)/2),
                                                                  breaks = 1:length(new_panel_params$y$breaks),
                                                                  breaks.label = new_panel_params$y$limits),
                                                             yAxis.params)
                                }
                              }else if(scales == "free_x"){
                                tmp_data <- tmp_data[order(tmp_data$x),]
                                new_panel_params <- panel_params

                                if(panel_params$x$is_discrete()){
                                  new_panel_params$x$limits <- all_limits.x[unique(tmp_data$x)]
                                  tmp_data$x <- rep(1:length(unique(tmp_data$x)),
                                                    table(tmp_data$x))
                                  tmp_data$xmin <- tmp_data$x - tmp_data$width/2
                                  tmp_data$xmax <- tmp_data$x + tmp_data$width/2
                                  new_panel_params$x.range <- range(tmp_data$x)

                                  yAxis.params <- modifyList(list(yscale = range(data$y - unique(data$width)/2,
                                                                                 data$y + unique(data$width)/2)),
                                                             yAxis.params)
                                }else{
                                  new_panel_params$x.range <- range(tmp_data$x)
                                  yAxis.params <- modifyList(list(yscale = range(data$y - unique(data$width)/2,
                                                                                 data$y + unique(data$width)/2)),
                                                             yAxis.params)
                                }
                              }else if(scales == "free_y"){
                                tmp_data <- tmp_data[order(tmp_data$y),]
                                new_panel_params <- panel_params

                                if(panel_params$y$is_discrete()){
                                  new_panel_params$y$limits <- all_limits.y[unique(tmp_data$y)]
                                  tmp_data$y <- rep(1:length(unique(tmp_data$y)),
                                                    table(tmp_data$y))
                                  tmp_data$ymin <- tmp_data$y - tmp_data$width/2
                                  tmp_data$ymax <- tmp_data$y + tmp_data$width/2
                                  new_panel_params$y.range <- range(tmp_data$y)

                                  xAxis.params <- modifyList(list(xscale = range(data$x - unique(data$width)/2,
                                                                                 data$x + unique(data$width)/2)),
                                                             xAxis.params)
                                }else{
                                  new_panel_params$y.range <- range(tmp_data$y)
                                  xAxis.params <- modifyList(list(xscale = range(data$x - unique(data$width)/2,
                                                                                 data$x + unique(data$width)/2)),
                                                             xAxis.params)
                                }
                              }else if(scales == "free"){
                                tmp_data <- tmp_data[order(tmp_data$x),]
                                new_panel_params <- panel_params

                                if(panel_params$x$is_discrete()){
                                  new_panel_params$x$limits <- all_limits.x[unique(tmp_data$x)]
                                  tmp_data$x <- rep(1:length(unique(tmp_data$x)),
                                                    table(tmp_data$x))
                                  tmp_data$xmin <- tmp_data$x - tmp_data$width/2
                                  tmp_data$xmax <- tmp_data$x + tmp_data$width/2
                                  new_panel_params$x.range <- range(tmp_data$x)
                                }else{
                                  new_panel_params$x.range <- range(tmp_data$x - tmp_data$width/2,
                                                                    tmp_data$x + tmp_data$width/2)
                                }

                                tmp_data <- tmp_data[order(tmp_data$y),]
                                if(panel_params$y$is_discrete()){
                                  new_panel_params$y$limits <- all_limits.y[unique(tmp_data$y)]
                                  tmp_data$y <- rep(1:length(unique(tmp_data$y)),
                                                    table(tmp_data$y))
                                  tmp_data$ymin <- tmp_data$y - tmp_data$width/2
                                  tmp_data$ymax <- tmp_data$y + tmp_data$width/2
                                  new_panel_params$y.range <- range(tmp_data$y)

                                }else{
                                  new_panel_params$y.range <- range(tmp_data$y - tmp_data$width/2,
                                                                    tmp_data$y + tmp_data$width/2)
                                }
                              }


                              tmp_data <- transform(tmp_data,
                                                    start = sec_df$sector_start[sec],
                                                    end = sec_df$sector_end[sec])


                              if(strip.label == TRUE){
                                plabel <- arcTextGrob(x = 1,y = 1,
                                                      labels = sec_df$sector_name[sec],
                                                      shift = 0.7,
                                                      curved.label = FALSE,
                                                      start = sec_df$sector_start[sec],
                                                      end = sec_df$sector_end[sec],
                                                      r0 = label.r,r1 = label.r,
                                                      # xscale = xAxis.params$xscale,
                                                      # yscale = yAxis.params$yscale,
                                                      text.gp = gpar(
                                                        col = strip.label.col,
                                                        fontsize = strip.label.size,
                                                        # fontfamily = data$family,
                                                        fontface = strip.label.fontface),
                                                      extend.xscale = extend.xscale,
                                                      extend.yscale = extend.yscale,
                                                      clock.wise = unique(tmp_data$clock.wise))
                              }else{
                                plabel <- zeroGrob()
                              }


                              # =======================================================================
                              # add mark layers
                              if("mark" %in% colnames(tmp_data)){
                                mark_data <- unique(subset(tmp_data,select = c(-fill,-y,-ymin,-ymax)))

                                # check mark position
                                if(mark.pos == "bottom"){
                                  mark.r0 <- unique(tmp_data$r0) - mark.shift
                                  mark.r1 <- mark.r0
                                }else{
                                  mark.r0 <- unique(tmp_data$r1) + mark.shift
                                  mark.r1 <- mark.r0
                                }

                                # check mark data type
                                if(mark.mat == TRUE){
                                  mark.r0 <- unique(tmp_data$r0)
                                  mark.r1 <- unique(tmp_data$r1)

                                  yscale = range(tmp_data$ymin,tmp_data$ymax)
                                }else{
                                  yscale = range(0,1)
                                }

                                mark_grob <-
                                  arcPointGrob(x = tmp_data$x,y = tmp_data$y,
                                               start = unique(tmp_data$start),
                                               end = unique(tmp_data$end),
                                               r0 = mark.r0,
                                               r1 = mark.r1,
                                               pch = tmp_data$mark,
                                               size = tmp_data$size,
                                               xscale = range(tmp_data$xmin,tmp_data$xmax),
                                               yscale = yscale,
                                               point.gp = gpar(
                                                 col = mark.col,
                                                 fill = mark.col,
                                                 fontsize = unique(tmp_data$size)),
                                               extend.xscale = extend.xscale,
                                               extend.yscale = extend.yscale,
                                               clock.wise = unique(tmp_data$clock.wise))
                              }else{
                                mark_grob <- zeroGrob()
                              }

                              # ====================================================================
                              if(mark.mat == TRUE){
                                ptack <- zeroGrob()
                              }else{
                                ptack <-
                                  ggproto_parent(GeomArctile, self)$draw_panel(
                                    tmp_data,new_panel_params,coord,
                                    extend.xscale = extend.xscale,
                                    extend.yscale = extend.yscale,
                                    add.bg = add.bg,
                                    sector.bg.extend = sector.bg.extend,
                                    add.xaxis = add.xaxis[sec],
                                    add.yaxis = add.yaxis[sec],
                                    xAxis.params = xAxis.params,
                                    yAxis.params = yAxis.params
                                  )
                              }

                              track_glist <- gList(track_glist,ptack,plabel,mark_grob)
                            }

                            ggname("geom_tracktile2",
                                   grid::gTree(children = gList(track_glist)))
                          },

                          draw_key = draw_key_rect
)


# ==============================================================================

#' geom_tracktile2 Function
#'
#' This is a custom geom function used to draw a track tile plot.
#'
#' @param mapping Mapping rules for mapping data to visual aesthetics.
#' @param data A data frame containing the plotting data.
#' @param stat The name of the statistical transformation.
#' @param position The name of positioning adjustment.
#' @param na.rm Whether to remove missing values (default = FALSE).
#' @param show.legend Whether to display the legend (default = NA).
#' @param inherit.aes Whether to inherit aesthetics from the parent layer (default = TRUE).
#' @param mark.col The color of mark (default: "black").
#' @param mark.pos The position to put the mark layer (default: c("bottom", "top")).
#' @param mark.shift The space to the margin of tile bottom or top (default: 0.05).
#' @param mark.mat Whether mark data is a mark matrix (default: FALSE).
#' @param strip.label Whether to display track labels (default = TRUE).
#' @param strip.label.pos The position of track labels ("top" or "bottom") (default = "top").
#' @param strip.label.space The spacing between track labels and track rectangles (default = 0.15).
#' @param strip.label.fontface The font style of track labels (default = "bold").
#' @param strip.label.col The color of track labels (default = "black").
#' @param strip.label.size The size of track labels (default = 10).
#' @param scales Specifies the scaling type for x and y axes (default = "fixed").
#' @param space Specifies the spatial type for track rectangles (default = "free_x").
#' @param sector.gap The gap between track rectangles (default = 3).
#' @param extend.xscale The scaling factor for the x-axis (default = 0.05).
#' @param extend.yscale The scaling factor for the y-axis (default = 0.05).
#' @param add.bg Whether to add a background (default = TRUE).
#' @param sector.bg.extend The background extension factor (default = 0.025).
#' @param add.xaxis Whether to add the x-axis (default = TRUE).
#' @param add.yaxis Whether to add the y-axis (default = TRUE).
#' @param xAxis.params Parameters for the x-axis (default = list()).
#' @param yAxis.params Parameters for the y-axis (default = list()).
#' @param ... Other parameters to be passed to the function.
#'
#' @return A ggplot2 layer.
#'
#' @import ggplot2
#' @export
geom_tracktile2 <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE,
                            mark.col = "black",
                            mark.pos = c("bottom","top"),
                            mark.shift = 0.05,
                            mark.mat = FALSE,
                            strip.label = TRUE,
                            strip.label.pos = c("top","bottom"),
                            strip.label.space = 0.15,
                            strip.label.fontface = "bold",
                            strip.label.col = "black",
                            strip.label.size = 10,
                            scales = c("fixed","free","free_x","free_y"),
                            space = c("free_x","fixed"),
                            sector.gap = 3,
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
    geom = GeomTracktile2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  mark.col = mark.col,
                  mark.pos = mark.pos,
                  mark.shift = mark.shift,
                  mark.mat = mark.mat,
                  strip.label = strip.label,
                  strip.label.pos = strip.label.pos,
                  strip.label.space = strip.label.space,
                  strip.label.fontface = strip.label.fontface,
                  strip.label.col = strip.label.col,
                  strip.label.size = strip.label.size,
                  scales = scales,
                  space = space,
                  sector.gap = sector.gap,
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
