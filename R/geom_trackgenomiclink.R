#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomTrackgenomiclink <- ggproto("GeomTrackgenomiclink", Geom,

                                # required_aes = c("chr0","gstart0","gend0",
                                #                  "chr1","gstart1","gend1",
                                #                  "start","end","r","clock.wise"),

                                required_aes = c("chr0","gstart0|gend0",
                                                 "chr1","gstart1|gend1",
                                                 "start","end","r","clock.wise"),

                                default_aes = aes(
                                  start = 0,end = 360,r = 0.5,
                                  clock.wise = FALSE,
                                  colour = "black",linewidth = .5,
                                  linetype = 1,fill = "black",alpha = NA),

                                draw_panel = function(self,data, panel_params, coord,
                                                      chrom_data = NULL,
                                                      keep.all.chrom = FALSE,
                                                      start.arrow = FALSE,
                                                      end.arrow = FALSE,
                                                      curve.arrow = NULL,
                                                      arrow.len = 0.05,
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
                                    chrom_data <- chrom_data[which(chrom_data[,1] %in% c(unique(data$chr0),
                                                                                         unique(data$chr1))),]
                                  }

                                  # calculate sector theta
                                  sec_df = facet_sector(data = chrom_data,
                                                        data.type = "genomic",
                                                        sector.gap = sector.gap,
                                                        space = space)

                                  if(length(unique(sec_df$sector_name)) == 1){
                                    sec_df <- sec_df[1,]
                                  }

                                  # extend secdf theta
                                  sec_df <- transform(sec_df,
                                                      sector_start = sector_start + abs(sector_end - sector_start)*extend.xscale,
                                                      sector_end = sector_end - abs(sector_end - sector_start)*extend.xscale)

                                  # label for different facets
                                  if(strip.label.pos == "top"){
                                    label.r = unique(data$r1) + strip.label.space
                                  }else{
                                    label.r = unique(data$r0) - strip.label.space
                                  }

                                  # ====================================================

                                  # loop draw grobs
                                  track_glist <- gList()

                                  # yAxis.params <- modifyList(list(yscale = range(data$y)),yAxis.params)

                                  for (sec in 1:nrow(data)) {
                                    tmp_cor <- data[sec,]
                                    col_name <- colnames(tmp_cor)

                                    chr0_region <- sec_df[which(sec_df$sector_name == tmp_cor$chr0),]
                                    chr0_th <- rev(c(chr0_region$sector_start,chr0_region$sector_end))

                                    if("gstart0" %in% col_name){
                                      tmp_cor$gstart0_th <- rescale(tmp_cor$gstart0,to = chr0_th,from = c(0,chr0_region$length))
                                    }
                                    if("gend0" %in% col_name){
                                      tmp_cor$gend0_th <- rescale(tmp_cor$gend0,to = chr0_th,from = c(0,chr0_region$length))
                                    }


                                    chr1_region <- sec_df[which(sec_df$sector_name == tmp_cor$chr1),]
                                    chr1_th <- rev(c(chr1_region$sector_start,chr1_region$sector_end))

                                    if("gstart1" %in% col_name){
                                      tmp_cor$gstart1_th <- rescale(tmp_cor$gstart1,to = chr1_th,from = c(0,chr1_region$length))
                                    }
                                    if("gend1" %in% col_name){
                                      tmp_cor$gend1_th <- rescale(tmp_cor$gend1,to = chr1_th,from = c(0,chr1_region$length))
                                    }

                                    # check link start and end theta
                                    if(all(c("gstart0","gend0") %in% col_name)){
                                      link_start <- c(tmp_cor$gstart0_th,tmp_cor$gend0_th)
                                    }else if("gstart0" %in% col_name){
                                      link_start <- c(tmp_cor$gstart0_th)
                                    }else{
                                      link_start <- c(tmp_cor$gend0_th)
                                    }

                                    if(all(c("gstart1","gend1") %in% col_name)){
                                      link_end <- c(tmp_cor$gstart1_th,tmp_cor$gend1_th)
                                    }else if("gstart0" %in% col_name){
                                      link_end <- c(tmp_cor$gstart1_th)
                                    }else{
                                      link_end <- c(tmp_cor$gend1_th)
                                    }

                                    # LINK GROB
                                    link_grob <- arcLinksGrob(start = link_start,
                                                              end = link_end,
                                                              r = tmp_cor$r,
                                                              curve.height = unique(tmp_cor$r),
                                                              start.arrow = start.arrow,
                                                              end.arrow = end.arrow,
                                                              arrow.len = arrow.len,
                                                              bezierCurve.gp =
                                                                gpar(fill = alpha(tmp_cor$fill, tmp_cor$alpha),
                                                                     col = alpha(tmp_cor$colour, tmp_cor$alpha),
                                                                     lwd = tmp_cor$linewidth,
                                                                     lty = tmp_cor$linetype),
                                                              bezierPolygon.gp =
                                                                gpar(fill = alpha(tmp_cor$fill, tmp_cor$alpha),
                                                                     col = alpha(tmp_cor$colour, tmp_cor$alpha),
                                                                     lwd = tmp_cor$linewidth,
                                                                     lty = tmp_cor$linetype),
                                                              curve.arrow = curve.arrow)

                                    track_glist <- gList(track_glist,link_grob)
                                  }


                                  # ============================================
                                  gp <- sec_df$sector_name
                                  label_glist <- gList()

                                  for (sec in seq_along(gp)) {

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
                                                            clock.wise = unique(data$clock.wise))
                                    }else{
                                      plabel <- zeroGrob()
                                    }

                                    label_glist <- gList(label_glist,plabel)
                                  }


                                  ggname("geom_trackgenomiclink",
                                         grid::gTree(children = gList(track_glist,label_glist)))
                                },

                                draw_key = draw_key_polygon
)


# ==============================================================================

#' Track Genomic Link Plot
#'
#' This function creates a genomic link plot using track-based visualization.
#'
#' @param mapping Aesthetic mapping (required).
#' @param data The data frame containing the data (required).
#' @param stat The statistical transformation to apply (default: "identity").
#' @param position The position adjustment method (default: "identity").
#' @param na.rm A logical value indicating whether to remove NA values (default: FALSE).
#' @param show.legend A logical value indicating whether to show the legend (default: NA).
#' @param inherit.aes A logical value indicating whether to inherit aesthetics from the parent plot (default: TRUE).
#' @param chrom_data Data frame with 2 columns containing chromosome information (default: NULL).
#' @param keep.all.chrom A logical value indicating whether to keep all chromosomes in the plot (default: FALSE).
#' @param start.arrow A logical value indicating whether to add start arrows to links (default: FALSE).
#' @param end.arrow A logical value indicating whether to add end arrows to links (default: FALSE).
#' @param curve.arrow The curvature of arrow lines (default: NULL).
#' @param arrow.len Length of arrows (default: 0.05).
#' @param strip.label A logical value indicating whether to add strip labels (default: TRUE).
#' @param strip.label.pos Position of strip labels ("top" or "bottom") (default: c("top", "bottom")).
#' @param strip.label.space Space between strip labels (default: 0.15).
#' @param strip.label.fontface Fontface for strip labels (default: "bold").
#' @param strip.label.col Color of strip labels (default: "black").
#' @param strip.label.size Size of strip labels (default: 10).
#' @param scales Type of scales for x and y axes ("fixed", "free", "free_x", "free_y")
#' (default: c("fixed","free","free_x","free_y"): unused arg now).
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
#' @export
geom_trackgenomiclink <- function(mapping = NULL, data = NULL,
                                  stat = "identity", position = "identity",
                                  na.rm = FALSE, show.legend = NA,
                                  inherit.aes = TRUE,
                                  chrom_data = NULL,
                                  keep.all.chrom = FALSE,
                                  start.arrow = FALSE,
                                  end.arrow = FALSE,
                                  curve.arrow = NULL,
                                  arrow.len = 0.05,
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
    geom = GeomTrackgenomiclink,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  chrom_data = chrom_data,
                  keep.all.chrom = keep.all.chrom,
                  start.arrow = start.arrow,
                  end.arrow = end.arrow,
                  curve.arrow = curve.arrow,
                  arrow.len = arrow.len,
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
