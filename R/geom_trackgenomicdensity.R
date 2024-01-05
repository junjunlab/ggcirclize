#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @importFrom circlize genomicDensity
#' @include geom_arcpolygon.R
#' @export
GeomTrackgenomicdensity <- ggproto("GeomTrackgenomicdensity", GeomArcpolygon,

                                   required_aes = c("chr","gstart","gend",
                                                    "start","end","r0","r1","clock.wise"),

                                   draw_panel = function(self,data, panel_params, coord,
                                                         chrom_data = NULL,
                                                         keep.all.chrom = FALSE,
                                                         window.size = 1e7,
                                                         count_by = c("percent", "number"),
                                                         polar.every = FALSE,
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
                                     count_by <- match.arg(count_by,c("percent", "number"))

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
                                     # calculate rainfall data
                                     chr <- unique(data$chr)

                                     lapply(seq_along(chr), function(x){
                                       tmp <- data[which(data$chr %in% chr[x]),]

                                       data_raw <- tmp[,c("chr","gstart","gend")]
                                       rd <- circlize::genomicDensity(region = data_raw,
                                                                      window.size = window.size,
                                                                      count_by = count_by)

                                       tmp <- tmp[1,]
                                       tmp2 <- tmp[rep(seq_len(nrow(tmp)), nrow(rd)), ]

                                       tmp2$x <- (rd$start + rd$end)/2
                                       tmp2$ymax <- rd$value
                                       tmp2$ymin <- 0
                                       tmp2$flipped_aes <- FALSE

                                       return(tmp2)
                                     }) %>% Reduce("rbind",.) -> data

                                     yAxis.params <- modifyList(list(yscale = range(data$ymin,data$ymax)),yAxis.params)

                                     # loop draw grobs
                                     gp <- sec_df$sector_name
                                     track_glist <- gList()

                                     for (sec in seq_along(gp)) {
                                       tmp_data <- data[which(data$chr == gp[sec]),]

                                       if(nrow(tmp_data) > 0){
                                         tmp_data <- transform(tmp_data,
                                                               start = sec_df$sector_start[sec],
                                                               end = sec_df$sector_end[sec])

                                         # setting x axis
                                         x.range <- range(0,sec_df$length[sec])
                                         breaks <- grid.pretty(x.range,n = 3)
                                         # breaks.label <- paste0(breaks/10^6,"Mb")
                                         breaks.label <- addsuffix(breaks)
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

                                         ptack <- ggproto_parent(GeomArcribbon, self)$draw_panel(
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
                                       }else{
                                         track_glist <- gList(track_glist,zeroGrob())
                                       }

                                     }

                                     ggname("geom_trackgenomicdensity",
                                            grid::gTree(children = gList(track_glist)))
                                   },


                                   draw_key = draw_key_polygon
)


# ==============================================================================

#' geom_trackgenomicdensity Function
#'
#' This is a custom geom function used to draw track genomic density plots.
#'
#' @param mapping Aesthetic mapping (required).
#' @param data The data frame containing the data (required).
#' @param stat The statistical transformation to apply (default: "identity").
#' @param position The position adjustment method (default: "identity").
#' @param na.rm A logical value indicating whether to remove NA values (default: FALSE).
#' @param show.legend A logical value indicating whether to show the legend (default: NA).
#' @param inherit.aes A logical value indicating whether to inherit aesthetics
#' from the parent plot (default: TRUE).
#' @param chrom_data Data frame with 2 columns containing chromosome information (default: NULL).
#' @param keep.all.chrom A logical value indicating whether to keep all chromosomes
#' in the plot (default: FALSE).
#' @param window.size The size of the genomic window (default: 1e7).
#' @param count_by The counting method, either "percent" or "number" (default: "percent").
#' @param polar.every A logical value indicating whether to use polar coordinates
#' for every chromosome (default: FALSE).
#' @param strip.label A logical value indicating whether to add strip labels (default: TRUE).
#' @param strip.label.pos Position of strip labels ("top" or "bottom") (default: c("top", "bottom")).
#' @param strip.label.space Space between strip labels (default: 0.15).
#' @param strip.label.fontface Fontface for strip labels (default: "bold").
#' @param strip.label.col Color of strip labels (default: "black").
#' @param strip.label.size Size of strip labels (default: 10).
#' @param scales Type of scales for x and y axes ("fixed", "free", "free_x", "free_y")
#'  (default: c("fixed","free","free_x","free_y"): unused arg now).
#' @param space Type of space scales ("free_x", "fixed") (default: c("free_x","fixed")).
#' @param sector.gap Gap between sectors in polar coordinates (default: 3).
#' @param extend.xscale Extension of x-axis scale (default: 0.05).
#' @param extend.yscale Extension of y-axis scale (default: 0.05).
#' @param add.bg A logical value indicating whether to add background to the plot (default: TRUE).
#' @param sector.bg.extend Extension of sector background (default: 0.025).
#' @param add.xaxis A logical value indicating whether to add x-axis (default: TRUE).
#' @param add.yaxis A logical value indicating whether to add y-axis (default: FALSE).
#' @param xAxis.params A list of parameters for customizing the x-axis (default: list()).
#' @param yAxis.params A list of parameters for customizing the y-axis (default: list()).
#' @param ... Additional arguments to be passed to the underlying layer function.
#'
#' @return A ggplot2 layer.
#'
#' @import ggplot2
#'
#' @export
geom_trackgenomicdensity <- function(mapping = NULL, data = NULL,
                                     stat = "identity", position = "identity",
                                     na.rm = FALSE, show.legend = NA,
                                     inherit.aes = TRUE,
                                     chrom_data = NULL,
                                     keep.all.chrom = FALSE,
                                     window.size = 1e7,
                                     count_by = c("percent", "number"),
                                     polar.every = FALSE,
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
    geom = GeomTrackgenomicdensity,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  chrom_data = chrom_data,
                  keep.all.chrom = keep.all.chrom,
                  window.size = window.size,
                  count_by = count_by,
                  polar.every = polar.every,
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
