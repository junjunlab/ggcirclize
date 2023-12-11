#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomTrackgenomiclabel <- ggproto("GeomTrackgenomiclabel", Geom,

                                 required_aes = c("chr","gstart","gend","label",
                                                  "start","end","r0","r1","clock.wise"),

                                 draw_panel = function(self,data, panel_params, coord,
                                                       chrom_data = NULL,
                                                       keep.all.chrom = FALSE,
                                                       add_link = TRUE,
                                                       link_pos = c("top","bottom"),
                                                       link_r = 0.1,
                                                       link_col = NULL,
                                                       label.col = NULL,
                                                       label.size = 3,
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
                                                       add.xaxis = FALSE,
                                                       add.yaxis = FALSE,
                                                       xAxis.params = list(),
                                                       yAxis.params = list(),
                                                       size.unit = "mm") {

                                   link_pos <- match.arg(link_pos,c("top","bottom"))

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

                                   size.unit <- resolve_text_unit(size.unit)
                                   # ====================================================
                                   # link color
                                   if(is.null(link_col)){
                                     link_col <- rep("black",length(unique(data$chr)))
                                   }else{
                                     link_col <- link_col
                                   }

                                   if(is.null(label.col)){
                                     label.col <- rep("black",length(unique(data$chr)))
                                   }else{
                                     label.col <- label.col
                                   }

                                   # loop draw grobs
                                   gp <- sec_df$sector_name
                                   track_glist <- gList()

                                   # loop
                                   for (sec in seq_along(gp)) {
                                     tmp_data <- data[which(data$chr == gp[sec]),]

                                     if(nrow(tmp_data) > 0){
                                       tmp_data <- transform(tmp_data,
                                                             start = sec_df$sector_start[sec],
                                                             end = sec_df$sector_end[sec])


                                       # ========================================================================
                                       # generate link data
                                       if(add_link == TRUE){
                                         range.x <- range(0,sec_df$length[sec])
                                         range.th <- c(sec_df$sector_end[sec],sec_df$sector_start[sec])

                                         link_df <- unique(tmp_data[,c("chr","gstart","gend","label")])
                                         link_df <- transform(link_df,mid = (gstart + gend)/2)
                                         link_df <- link_df[order(link_df$mid),]
                                         link_df$ht.x <- 1:nrow(link_df)
                                         link_df$ht.x.th <- rescale(link_df$ht.x,to = rev(range.th),
                                                                    from = c(min(link_df$ht.x) - 0.5,
                                                                             max(link_df$ht.x) + 0.5))

                                         link_df$mid.th <- rescale(link_df$mid,to = rev(range.th),
                                                                   from = range.x)

                                         # check link position
                                         if(link_pos == "top"){
                                           r.y <- rev(seq(unique(tmp_data$r1),unique(tmp_data$r1) + link_r,length = 4))
                                           arcymin <- rep(1,nrow(link_df))
                                           arcymax <- rep(0,nrow(link_df))
                                         }else{
                                           r.y <- rev(seq(unique(tmp_data$r0),unique(tmp_data$r0) - link_r,length = 4))
                                           arcymin <- rep(0,nrow(link_df))
                                           arcymax <- rep(1,nrow(link_df))
                                         }

                                         # generate grobs
                                         vuline <- arcSegmentsGrob(xmin = link_df$mid,
                                                                   xmax = link_df$mid,
                                                                   ymin = rep(0,nrow(link_df)),
                                                                   ymax = rep(1,nrow(link_df)),
                                                                   start = range.th[2],end = range.th[1],
                                                                   r0 = r.y[2],r1 = r.y[1],
                                                                   x0 = 0,y0 = 0,
                                                                   polyline.gp = gpar(col = link_col[sec]),
                                                                   clock.wise = unique(tmp_data$clock.wise),
                                                                   extend.xscale = extend.xscale,
                                                                   extend.yscale = 0,
                                                                   xscale = range.x)

                                         vbline <- arcSegmentsGrob(xmin = link_df$ht.x,
                                                                   xmax = link_df$ht.x,
                                                                   ymin = rep(0,nrow(link_df)),
                                                                   ymax = rep(1,nrow(link_df)),
                                                                   start = range.th[2],end = range.th[1],
                                                                   r0 = r.y[4],r1 = r.y[3],
                                                                   x0 = 0,y0 = 0,
                                                                   polyline.gp = gpar(col = link_col[sec]),
                                                                   clock.wise = unique(tmp_data$clock.wise),
                                                                   extend.xscale = extend.xscale,
                                                                   extend.yscale = 0,
                                                                   xscale = c(min(link_df$ht.x) - 0.5,
                                                                              max(link_df$ht.x) + 0.5))

                                         # ===========================================
                                         arcline <- arcSegmentsGrob(xmin = link_df$mid.th,
                                                                    xmax = link_df$ht.x.th,
                                                                    ymin = arcymin,
                                                                    ymax = arcymax,
                                                                    start = range.th[2],end = range.th[1],
                                                                    r0 = r.y[2],r1 = r.y[3],
                                                                    x0 = 0,y0 = 0,
                                                                    polyline.gp = gpar(col = link_col[sec]),
                                                                    clock.wise = unique(tmp_data$clock.wise),
                                                                    extend.xscale = extend.xscale,
                                                                    extend.yscale = 0,
                                                                    xscale = rev(range.th))


                                         up_line <- arcSegmentsGrob(xmin = link_df$gstart,
                                                                    xmax = link_df$gend,
                                                                    ymin = rep(0,nrow(link_df)),
                                                                    ymax = rep(1,nrow(link_df)),
                                                                    start = range.th[2],end = range.th[1],
                                                                    r0 = r.y[1],r1 = r.y[1],
                                                                    x0 = 0,y0 = 0,
                                                                    polyline.gp = gpar(col = link_col[sec]),
                                                                    arrow = NULL,
                                                                    clock.wise = unique(tmp_data$clock.wise),
                                                                    extend.xscale = extend.xscale,
                                                                    extend.yscale = extend.yscale,
                                                                    xscale = range.x)
                                       }else{
                                         up_line <- zeroGrob()
                                         vuline <- zeroGrob()
                                         arcline <- zeroGrob()
                                         vbline <- zeroGrob()
                                       }

                                       # =============================================================

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

                                       # label position
                                       if(link_pos == "top"){
                                         label.r0 <- r.y[4] - 0.025
                                         label.r1 <- r.y[4] - 0.025
                                         inward <- TRUE
                                       }else{
                                         label.r0 <- r.y[4] + 0.025
                                         label.r1 <- r.y[4] + 0.025
                                         inward <- FALSE
                                       }


                                       ptack <- arcTextGrob(x = link_df$ht.x,
                                                            y = rep(0.5,nrow(link_df)),
                                                            labels = link_df$label,
                                                            start = sec_df$sector_start[sec],
                                                            end = sec_df$sector_end[sec],
                                                            r0 = label.r0,
                                                            r1 = label.r1,
                                                            curved.label = FALSE,
                                                            nice.facing = FALSE,
                                                            inward = inward,
                                                            text.gp = gpar(col = label.col[sec],
                                                                           fontsize = label.size * size.unit),
                                                            extend.xscale = extend.xscale,
                                                            extend.yscale = extend.yscale,
                                                            clock.wise = unique(tmp_data$clock.wise),
                                                            xscale = c(min(link_df$ht.x) - 0.5,
                                                                       max(link_df$ht.x) + 0.5))


                                       track_glist <- gList(track_glist,plabel,ptack,
                                                            up_line,vuline,arcline,vbline)
                                     }else{
                                       track_glist <- gList(track_glist,zeroGrob())
                                     }

                                   }

                                   ggname("geom_trackgenomiclabel",
                                          grid::gTree(children = gList(track_glist)))
                                 },

                                 draw_key = draw_key_point
)


# ==============================================================================

#' Track Genomic Label Plot
#'
#' This function creates a genomic label plot using track-based visualization.
#'
#' @param mapping Aesthetic mapping (required).
#' @param data The data frame containing the data (required).
#' @param stat The statistical transformation to apply (default: "identity").
#' @param position The position adjustment method (default: "identity").
#' @param na.rm A logical value indicating whether to remove NA values (default: FALSE).
#' @param show.legend A logical value indicating whether to show the legend (default: NA).
#' @param inherit.aes A logical value indicating whether to inherit aesthetics from the parent plot (default: TRUE).
#' @param chrom_data Data frame 2 columns containing chromosome information (default: NULL).
#' @param keep.all.chrom A logical value indicating whether to keep all
#' chromosomes in the plot (default: FALSE).
#' @param add_link A logical value indicating whether to add links between labels (default: TRUE).
#' @param link_pos Position of links ("top" or "bottom") (default: c("top", "bottom")).
#' @param link_r Radius of links (default: 0.1).
#' @param link_col Color of links (default: NULL).
#' @param label.col Color of labels (default: NULL).
#' @param label.size Size of labels (default: 10).
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
#' @param add.xaxis A logical value indicating whether to add x-axis (default: FALSE).
#' @param add.yaxis A logical value indicating whether to add y-axis (default: FALSE).
#' @param xAxis.params A list of parameters for customizing the x-axis (default: list()).
#' @param yAxis.params A list of parameters for customizing the y-axis (default: list()).
#' @param ... Additional arguments to be passed to the underlying layer function.
#'
#' @return A ggplot2 layer.
#'
#' @import ggplot2
#' @export
geom_trackgenomiclabel <- function(mapping = NULL, data = NULL,
                                   stat = "identity", position = "identity",
                                   na.rm = FALSE, show.legend = NA,
                                   inherit.aes = TRUE,
                                   chrom_data = NULL,
                                   keep.all.chrom = FALSE,
                                   add_link = TRUE,
                                   link_pos = c("top","bottom"),
                                   link_r = 0.1,
                                   link_col = NULL,
                                   label.col = NULL,
                                   label.size = 3,
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
                                   add.xaxis = FALSE,
                                   add.yaxis = FALSE,
                                   xAxis.params = list(),
                                   yAxis.params = list(),
                                   ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTrackgenomiclabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  chrom_data = chrom_data,
                  keep.all.chrom = keep.all.chrom,
                  add_link = add_link,
                  link_pos = link_pos,
                  link_r = link_r,
                  link_col = link_col,
                  label.col = label.col,
                  label.size = label.size,
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
