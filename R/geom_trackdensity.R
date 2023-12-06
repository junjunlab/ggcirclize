#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @include geom_arcdensity.R
#' @export
GeomTrackdensity <- ggproto("GeomTrackdensity", GeomArcdensity,

                            draw_panel = function(self,data, panel_params, coord,
                                                  strip.label = TRUE,
                                                  strip.label.pos = c("top","bottom"),
                                                  strip.label.space = 0.15,
                                                  strip.label.fontface = "bold",
                                                  strip.label.col = "black",
                                                  strip.label.size = 10,
                                                  scales = c("fixed","free","free_x","free_y"),
                                                  space = c("free_x","fixed"),
                                                  sector.gap = 3,
                                                  polar.every = FALSE,
                                                  extend.xscale = 0.05,
                                                  extend.yscale = 0.05,
                                                  add.bg = TRUE,
                                                  sector.bg.extend = 0.025,
                                                  add.xaxis = TRUE,
                                                  add.yaxis = TRUE,
                                                  xAxis.params = list(),
                                                  yAxis.params = list(),
                                                  width = NULL, flipped_aes = FALSE) {
                              # check scale type
                              strip.label.pos <- match.arg(strip.label.pos,c("top","bottom"))
                              scales <- match.arg(scales,c("fixed","free","free_x","free_y"))
                              space <- match.arg(space,c("free_x","fixed"))

                              xlim <- range(data$x)
                              ylim <- range(data$y)

                              if(scales == "fixed"){
                                xAxis.params <- modifyList(list(xscale = xlim),xAxis.params)
                                yAxis.params <- modifyList(list(yscale = ylim),yAxis.params)
                              }else if(scales == "free_x"){
                                yAxis.params <- modifyList(list(yscale = ylim),yAxis.params)
                              }else if(scales == "free_y"){
                                xAxis.params <- modifyList(list(xscale = xlim),xAxis.params)
                              }

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

                              track_glist <- gList()
                              for (sec in seq_along(gp)) {
                                tmp_data <- data[which(data$sector == gp[sec]),]

                                tmp_data <- transform(tmp_data,
                                                      start = sec_df$sector_start[sec],
                                                      end = sec_df$sector_end[sec])

                                if(strip.label == TRUE){
                                  plabel <- arcTextGrob(x = 1,y = 1,
                                                        labels = sec_df$sector_name[sec],
                                                        # shift = 5,
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


                                ptack <-
                                  ggproto_parent(GeomArcdensity, self)$draw_panel(
                                    tmp_data,panel_params,coord,
                                    polar.every = polar.every,
                                    extend.xscale = extend.xscale,
                                    extend.yscale = extend.yscale,
                                    add.bg = add.bg,
                                    sector.bg.extend = sector.bg.extend,
                                    add.xaxis = add.xaxis[sec],
                                    add.yaxis = add.yaxis[sec],
                                    xAxis.params = xAxis.params,
                                    yAxis.params = yAxis.params
                                  )

                                track_glist <- gList(track_glist,ptack,plabel)
                              }

                              ggname("geom_trackdensity",
                                     grid::gTree(children = gList(track_glist)))
                            },

                            draw_key = draw_key_rect
)


# ==============================================================================

#' geom_trackdensity Function
#'
#' This is a custom geom function used to draw track density plots.
#'
#' @param mapping Mapping rules for mapping data to visual aesthetics.
#' @param data A data frame containing the plotting data.
#' @param stat The name of the statistical transformation.
#' @param position The name of positioning adjustment.
#' @param na.rm Whether to remove missing values (default = FALSE).
#' @param show.legend Whether to display the legend (default = NA).
#' @param inherit.aes Whether to inherit aesthetics from the parent layer (default = TRUE).
#' @param strip.label Whether to display track labels (default = TRUE).
#' @param strip.label.pos The position of track labels ("top" or "bottom") (default = "top").
#' @param strip.label.space The spacing between track labels and track density plots (default = 0.15).
#' @param strip.label.fontface The font style of track labels (default = "bold").
#' @param strip.label.col The color of track labels (default = "black").
#' @param strip.label.size The size of track labels (default = 10).
#' @param scales Specifies the scaling type for x and y axes (default = "fixed").
#' @param space Specifies the spatial type for track density plots (default = "free_x").
#' @param sector.gap The gap between track density plots (default = 3).
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
geom_trackdensity <- function(mapping = NULL, data = NULL,
                              stat = "arcdensity", position = "identity",
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE,
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
    geom = GeomTrackdensity,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
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
