#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @include geom_arcpoint.R
#' @export
GeomTrackgenomicpoint <- ggproto("GeomTrackgenomicpoint", GeomArcpoint,

                                 required_aes = c("chr","gstart","gend","value",
                                                  "start","end","r0","r1","clock.wise"),

                                 draw_panel = function(self,data, panel_params, coord,
                                                       chrom_data = NULL,
                                                       keep.all.chrom = FALSE,
                                                       strip.label = TRUE,
                                                       strip.label.pos = c("top","bottom"),
                                                       strip.label.space = 0.1,
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
                                                       add.yaxis = FALSE,
                                                       xAxis.params = list(),
                                                       yAxis.params = list()) {
                                   # check scale type
                                   strip.label.pos <- match.arg(strip.label.pos,c("top","bottom"))
                                   scales <- match.arg(scales,c("fixed","free","free_x","free_y"))
                                   space <- match.arg(space,c("free_x","fixed"))

                                   # fetch genome size data
                                   if("genome" %in% colnames(data) & is.null(chrom_data)){
                                     gm <- unique(data$genome)
                                     if(gm %in% c("hg38","hg19","mm39","mm10","mm9")){
                                       chrom_data <- get_chrom_data(genome = gm)
                                     }else if(is.data.frame(get(gm))){
                                       chrom_data <- get(gm)
                                     }else{
                                       message("Please supply a data frame includes chromsome length or
                                               genome version(hg38,hg19,mm39,mm10,mm9).")
                                     }
                                   }

                                   # add start and end
                                   chrom_data <- transform(chrom_data,
                                                           start = unique(data$start),
                                                           end = unique(data$end))

                                   if(keep.all.chrom == FALSE){
                                     chrom_data <- chrom_data[which(chrom_data[,1] %in% unique(data$chr)),]
                                   }

                                   # calculate sector theta
                                   sec_df = facet_sector(data = chrom_data,
                                                         data.type = "genomic",
                                                         sector.gap = sector.gap,
                                                         space = space)

                                   if(length(unique(sec_df$sector_name)) == 1){
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
                                   data <- transform(data,
                                                     x = (gstart + gend)/2,y = value)

                                   # loop draw grobs
                                   gp <- sec_df$sector_name
                                   track_glist <- gList()

                                   yAxis.params <- modifyList(list(yscale = range(data$y)),yAxis.params)

                                   for (sec in seq_along(gp)) {
                                     tmp_data <- data[which(data$chr == gp[sec]),]

                                     if(nrow(tmp_data) > 0){
                                       tmp_data <- transform(tmp_data,
                                                             start = sec_df$sector_start[sec],
                                                             end = sec_df$sector_end[sec])

                                       # setting x axis
                                       x.range <- range(0,sec_df$length[sec])
                                       breaks <- grid.pretty(x.range,n = 3)
                                       breaks.label <- paste0(breaks/10^6,"Mb")
                                       xAxis.params.tmp <- list(xscale = x.range,
                                                                breaks = breaks,
                                                                breaks.label = breaks.label,
                                                                nice.facing = TRUE)
                                       xAxis.params <- modifyList(xAxis.params,xAxis.params.tmp)

                                       if(strip.label == TRUE){
                                         plabel <- arcTextGrob(x = 1,y = 1,
                                                               labels = sec_df$sector_name[sec],
                                                               shift = 0.7,
                                                               curved.label = FALSE,
                                                               start = sec_df$sector_start[sec],
                                                               end = sec_df$sector_end[sec],
                                                               r0 = label.r,r1 = label.r,
                                                               text.gp = gpar(
                                                                 col = strip.label.col,
                                                                 fontsize = strip.label.size,
                                                                 fontface = strip.label.fontface),
                                                               extend.xscale = extend.xscale,
                                                               extend.yscale = extend.yscale,
                                                               clock.wise = unique(tmp_data$clock.wise))
                                       }else{
                                         plabel <- zeroGrob()
                                       }


                                       ptack <-
                                         ggproto_parent(GeomArcpoint, self)$draw_panel(
                                           tmp_data,panel_params,coord,
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
                                     }else{
                                       track_glist <- gList(track_glist,zeroGrob())
                                     }

                                   }

                                   ggname("geom_trackgenomicpoint",
                                          grid::gTree(children = gList(track_glist)))
                                 },

                                 draw_key = draw_key_point
)


# ==============================================================================

#' geom_trackgenomicpoint Function
#'
#' This is a custom geom function used to draw track genomic points plots.
#'
#' @param mapping Mapping rules for mapping data to visual aesthetics.
#' @param data A data frame containing the plotting data.
#' @param stat The name of the statistical transformation.
#' @param position The name of positioning adjustment.
#' @param na.rm Whether to remove missing values (default = FALSE).
#' @param show.legend Whether to display the legend (default = NA).
#' @param inherit.aes Whether to inherit aesthetics from the parent layer (default = TRUE).
#' @param chrom_data Data frame with 2 columns containing chromosome size information (default = NULL).
#' @param keep.all.chrom Whether to keep all chromosomes in the plot (default = FALSE).
#' @param strip.label Whether to display track labels (default = TRUE).
#' @param strip.label.pos The position of track labels ("top" or "bottom") (default = "top").
#' @param strip.label.space The spacing between track labels and track genomic area plots (default = 0.15).
#' @param strip.label.fontface The font style of track labels (default = "bold").
#' @param strip.label.col The color of track labels (default = "black").
#' @param strip.label.size The size of track labels (default = 10).
#' @param scales Specifies the scaling type for x and y axes (default = "fixed": unused arg now).
#' @param space Specifies the spatial type for track genomic area plots (default = "free_x").
#' @param sector.gap The gap between track genomic area plots (default = 3).
#' @param extend.xscale The scaling factor for the x-axis (default = 0.05).
#' @param extend.yscale The scaling factor for the y-axis (default = 0.05).
#' @param add.bg Whether to add a background (default = TRUE).
#' @param sector.bg.extend The background extension factor (default = 0.025).
#' @param add.xaxis Whether to add the x-axis (default = TRUE).
#' @param add.yaxis Whether to add the y-axis (default = FALSE).
#' @param xAxis.params Parameters for the x-axis (default = list()).
#' @param yAxis.params Parameters for the y-axis (default = list()).
#' @param ... Other parameters to be passed to the function.
#'
#' @return A ggplot2 layer.
#'
#' @import ggplot2
#' @export
geom_trackgenomicpoint <- function(mapping = NULL, data = NULL,
                                   stat = "identity", position = "identity",
                                   na.rm = FALSE, show.legend = NA,
                                   inherit.aes = TRUE,
                                   chrom_data = NULL,
                                   keep.all.chrom = FALSE,
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
                                   add.yaxis = FALSE,
                                   xAxis.params = list(),
                                   yAxis.params = list(),
                                   ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTrackgenomicpoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  chrom_data = chrom_data,
                  keep.all.chrom = keep.all.chrom,
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
