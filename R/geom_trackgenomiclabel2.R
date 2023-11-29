#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomTrackgenomiclabel2 <- ggproto("GeomTrackgenomiclabel2", Geom,

                                  required_aes = c("chr","gstart","gend","label",
                                                   "start","end","r0","r1","clock.wise"),

                                  default_aes = aes(
                                    start = 0,end = 180,r0 = 0.5,r1 = 1,
                                    clock.wise = FALSE,
                                    colour = "black",linewidth = .5,
                                    linetype = 1,size = 10,
                                    sector.bg.fill = "grey95",
                                    sector.bg.col = "black",
                                    sector.bg.lty = 1,
                                    sector.bg.lwd = 0.5),


                                  draw_panel = function(self,data, panel_params, coord,
                                                        chrom_data = NULL,
                                                        keep.all.chrom = FALSE,
                                                        link_pos = c("top","bottom"),
                                                        link_r = 0.1,
                                                        link_col = NULL,
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
                                                        yAxis.params = list()) {

                                    link_pos <- match.arg(link_pos,c("top","bottom"))

                                    # check scale type
                                    strip.label.pos <- match.arg(strip.label.pos,c("top","bottom"))
                                    scales <- match.arg(scales,c("fixed","free","free_x","free_y"))
                                    space <- match.arg(space,c("free_x","fixed"))

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

                                    # ====================================================
                                    # link color
                                    if(is.null(link_col)){
                                      link_col <- rep("black",nrow(data))
                                      data$link_col <- link_col
                                    }else{
                                      link_col <- link_col
                                      data$link_col <- link_col
                                    }

                                    # loop draw grobs
                                    gp <- sec_df$sector_name

                                    # =======================================================================
                                    # get theta
                                    lapply(seq_along(gp), function(x){
                                      tmp_data <- data[which(data$chr == gp[x]),]

                                      range.x <- range(0,sec_df$length[x])
                                      range.th <- c(sec_df$sector_end[x],sec_df$sector_start[x])

                                      tmp_data <- transform(tmp_data,mid = (gstart + gend)/2)
                                      tmp_data <- tmp_data[order(tmp_data$mid),]

                                      tmp_data$mid.th <- rescale(tmp_data$mid,to = range.th,from = range.x)
                                      tmp_data$gstart.th <- rescale(tmp_data$gstart,to = range.th,from = range.x)
                                      tmp_data$gend.th <- rescale(tmp_data$gend,to = range.th,from = range.x)

                                      return(tmp_data)
                                    }) %>% Reduce("rbind",.) -> df_data

                                    # ==================================================================
                                    # adjusted the label position
                                    pushViewport(viewport(xscale = c(0,1),
                                                          yscale = c(unique(data$start),unique(data$end))
                                    )
                                    )

                                    lb <- textGrob(label = df_data$label[1],
                                                   default.units = "native",
                                                   gp = gpar(fontsize = unique(data$size)))

                                    label_height <- convertHeight(grobHeight(lb),unitTo = "native",valueOnly = T)

                                    x1 <- df_data$mid.th - label_height/2
                                    x2 <- df_data$mid.th + label_height/2

                                    label_pos_new <- ComplexHeatmap::smartAlign2(start = x1,end = x2,
                                                                                 range = c(unique(data$start),unique(data$end)),
                                                                                 plot = F)

                                    df_data$mid.th.new <- (label_pos_new[,1] + label_pos_new[,2])/2


                                    # =======================================================================
                                    # check link position
                                    if(link_pos == "top"){
                                      r.y <- rev(seq(unique(df_data$r1),unique(df_data$r1) + link_r,length = 4))
                                    }else{
                                      r.y <- rev(seq(unique(df_data$r0),unique(df_data$r0) - link_r,length = 4))
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

                                    # genrate connection grobs
                                    connection_list <- gList()
                                    for (r in 1:nrow(df_data)) {
                                      tmp <- df_data[r,]

                                      # gpar for link lines
                                      link_gp = gpar(col = tmp$link_col,
                                                     lwd = tmp$linewidth,
                                                     lty = tmp$linetype)

                                      th <- seq(tmp$gstart.th,tmp$gend.th,length = 100)
                                      xuh <- r.y[1]*cos(as.radian(th))
                                      yuh <- r.y[1]*sin(as.radian(th))
                                      xuh <- scales::rescale(xuh,to = c(0,1),from = c(-1,1))
                                      yuh <- scales::rescale(yuh,to = c(0,1),from = c(-1,1))

                                      top_hline <- polylineGrob(x = xuh,y = yuh,
                                                                gp = link_gp,
                                                                default.units = "native")

                                      rup <- seq(r.y[2],r.y[1],length = 100)
                                      xup <- rup*cos(as.radian(tmp$mid.th))
                                      yup <- rup*sin(as.radian(tmp$mid.th))
                                      xup <- scales::rescale(xup,to = c(0,1),from = c(-1,1))
                                      yup <- scales::rescale(yup,to = c(0,1),from = c(-1,1))

                                      top_vline <- polylineGrob(x = xup,y = yup,
                                                                gp = link_gp,
                                                                default.units = "native")

                                      rarc <- seq(r.y[3],r.y[2],length = 100)
                                      th <- rev(seq(tmp$mid.th,tmp$mid.th.new,length = 100))
                                      xarc <- rarc*cos(as.radian(th))
                                      yarc <- rarc*sin(as.radian(th))
                                      xarc <- scales::rescale(xarc,to = c(0,1),from = c(-1,1))
                                      yarc <- scales::rescale(yarc,to = c(0,1),from = c(-1,1))

                                      arcline <- polylineGrob(x = xarc,y = yarc,
                                                              gp = link_gp,
                                                              default.units = "native")

                                      rbt <- seq(r.y[4],r.y[3],length = 100)
                                      xbt <- rbt*cos(as.radian(tmp$mid.th.new))
                                      ybt <- rbt*sin(as.radian(tmp$mid.th.new))
                                      xbt <- scales::rescale(xbt,to = c(0,1),from = c(-1,1))
                                      ybt <- scales::rescale(ybt,to = c(0,1),from = c(-1,1))

                                      bottom_vline <- polylineGrob(x = xbt,y = ybt,
                                                                   gp = link_gp,
                                                                   default.units = "native")


                                      label_grob <- arcTextGrob(x = 0.5,y = 0.5,
                                                                labels = tmp$label,
                                                                r0 = label.r0,r1 = label.r1,
                                                                curved.label = FALSE,
                                                                inward = inward,
                                                                nice.facing = FALSE,
                                                                text.gp = gpar(col = tmp$colour,
                                                                               fontsize = tmp$size),
                                                                start = tmp$mid.th.new,end = tmp$mid.th.new)

                                      connection_list <- gList(connection_list,
                                                               top_hline,top_vline,arcline,bottom_vline,label_grob)
                                    }
                                    # =======================================================================

                                    # loop
                                    track_glist <- gList()
                                    for (sec in seq_along(gp)) {
                                      tmp_data <- df_data[which(df_data$chr == gp[sec]),]

                                      if(nrow(tmp_data) > 0){
                                        tmp_data <- transform(tmp_data,
                                                              start = sec_df$sector_start[sec],
                                                              end = sec_df$sector_end[sec])

                                        # ========================================================================
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


                                        track_glist <- gList(track_glist,plabel)
                                      }else{
                                        track_glist <- gList(track_glist,zeroGrob())
                                      }

                                    }

                                    # ==========================================================================
                                    # combine grobs
                                    ggname("geom_trackgenomiclabel2",
                                           grid::gTree(children = gList(connection_list,track_glist)))
                                  },

                                  draw_key = function(data, params, size) {
                                    zeroGrob()
                                  }
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
#' @param link_pos Position of links ("top" or "bottom") (default: c("top", "bottom")).
#' @param link_r Radius of links (default: 0.1).
#' @param link_col Color of links (default: NULL).
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
geom_trackgenomiclabel2 <- function(mapping = NULL, data = NULL,
                                    stat = "identity", position = "identity",
                                    na.rm = FALSE, show.legend = FALSE,
                                    inherit.aes = TRUE,
                                    chrom_data = NULL,
                                    keep.all.chrom = FALSE,
                                    link_pos = c("top","bottom"),
                                    link_r = 0.1,
                                    link_col = NULL,
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
    geom = GeomTrackgenomiclabel2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  chrom_data = chrom_data,
                  keep.all.chrom = keep.all.chrom,
                  link_pos = link_pos,
                  link_r = link_r,
                  link_col = link_col,
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
