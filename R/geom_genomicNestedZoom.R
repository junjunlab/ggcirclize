#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomGenomicNestedZoom <- ggproto("GeomGenomicNestedZoom", Geom,

                                 required_aes = c("chr","gstart","gend",
                                                  "start","end","r0","r1","clock.wise"),

                                 default_aes = aes(
                                   start = 0,end = 360,r0 = 0.5,r1 = 1,
                                   clock.wise = FALSE,
                                   # ===========================================
                                   value = NULL,
                                   ymin = NULL,ymax = NULL,
                                   x = NULL,y = NULL,
                                   width = 1, height = 1,
                                   # ===========================================
                                   stroke = 1,
                                   colour = "black",linewidth = .5,
                                   size = 5,linetype = 1,
                                   shape = 19,fill = NA,
                                   alpha = NA),

                                 draw_panel = function(self,data, panel_params, coord,
                                                       chrom_data = NULL,
                                                       keep.all.chrom = FALSE,
                                                       zoom_data = NULL,
                                                       geom = c("point","line","rect","tile","area","rainfall","density"),
                                                       extend.start = 0,
                                                       extend.end = 0,
                                                       nested.r = 0.02,
                                                       panel.gap = 0.02,
                                                       zoom.panel.height = NULL,
                                                       sub.zoom.gap = 1,
                                                       zoom_pos = c("bottom","top"),n = 100,
                                                       nested_fill = NULL,
                                                       nested_col = "black",
                                                       nested_lty = "solid",
                                                       nested_linewidth = 0.5,
                                                       nested_alpha = c(0.2,0.5),
                                                       zoom.free.sec = FALSE,
                                                       zoom.panel.extend.xscale = 0.05,
                                                       # ================================================
                                                       mode = c("min", "max", "mean", "left", "right"),
                                                       normalize_to_width = FALSE,
                                                       window.size = 1e7,
                                                       count_by = c("percent", "number"),
                                                       polar.every = FALSE,
                                                       # ================================================
                                                       strip.label = TRUE,
                                                       strip.label.pos = c("top","bottom"),
                                                       strip.label.space = 0.05,
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
                                   geom <- match.arg(geom,c("point","line","rect","tile","area","rainfall","density"))

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

                                   # extend secdf theta
                                   sec_df <- transform(sec_df,
                                                       sector_start1 = sector_start + abs(sector_end - sector_start)*extend.xscale,
                                                       sector_end1 = sector_end - abs(sector_end - sector_start)*extend.xscale)

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
                                   # calculate zoom sector theta
                                   if(zoom.free.sec == TRUE){
                                     zoom_chrom_data <- zoom_data
                                     zoom_chrom_data$width <- abs(zoom_chrom_data$end - zoom_chrom_data$start)
                                     zoom_chrom_data <- zoom_chrom_data %>%
                                       dplyr::group_by(chr) %>% dplyr::summarise(len = sum(width))
                                     zoom_chrom_data <- zoom_chrom_data[match(sec_df$sector_name,zoom_chrom_data$chr),]
                                     zoom_chrom_data <- transform(zoom_chrom_data,
                                                                  start = unique(data$start),end = unique(data$end))

                                     # print(sec_df$sector_name)
                                     # print(head(zoom_chrom_data))
                                     # break

                                     sec_df2 = facet_sector(data = zoom_chrom_data,
                                                            data.type = "genomic",
                                                            sector.gap = sub.zoom.gap,
                                                            space = space)

                                   }
                                   # ====================================================
                                   # add required variables and process data according to geom
                                   if(geom %in% c("point","line")){
                                     data <- transform(data,
                                                       x = (gstart + gend)/2,y = value)
                                   }else if(geom == "area"){
                                     data <- transform(data,
                                                       x = (gstart + gend)/2,ymax = value,
                                                       ymin = 0,flipped_aes = FALSE)
                                   }else if(geom == "rect"){
                                     if("ymin" %in% colnames(data) & "ymax" %in% colnames(data)){
                                       data <- transform(data,
                                                         xmin = gstart,xmax = gend)
                                     }else{
                                       data <- transform(data,
                                                         xmin = gstart,xmax = gend,
                                                         ymin = unique(data$r0),ymax = unique(data$r1))
                                     }
                                   }else if(geom == "rainfall"){
                                     # calculate rainfall data
                                     chr <- unique(data$chr)

                                     lapply(seq_along(chr), function(x){
                                       tmp <- data[which(data$chr %in% chr[x]),]

                                       data_raw <- tmp[,c("chr","gstart","gend")]
                                       rf <- circlize::rainfallTransform(region = data_raw,
                                                                         mode = mode,
                                                                         normalize_to_width = normalize_to_width)

                                       tmp <- transform(tmp,x = (gstart + gend)/2)

                                       if(normalize_to_width == TRUE){
                                         tmp$y <- log10(rf$dist)
                                       }else{
                                         tmp$y <- log10(rf$dist + 1)
                                       }

                                       return(tmp)
                                     }) %>% Reduce("rbind",.) -> data
                                   }else if(geom == "density"){
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
                                   }else{
                                     data <- data
                                   }

                                   # ====================================================
                                   # loop draw grobs
                                   # yAxis.params <- modifyList(list(yscale = range(data$y)),yAxis.params)

                                   zoom_chr <- unique(zoom_data$chr)

                                   # nested panel fill
                                   if(is.null(nested_fill)){
                                     zoom_data$nested_fill <- rep("white",nrow(zoom_data))
                                   }else{
                                     zoom_data$nested_fill <- rep(nested_fill,table(zoom_data$chr))
                                   }

                                   # generate grobs
                                   grob.list <- gList()
                                   for (sec in seq_along(zoom_chr)) {
                                     sec_tmp <- subset(sec_df,sector_name == zoom_chr[sec])
                                     zoom_chr_data <- zoom_data[which(zoom_data$chr == zoom_chr[sec]),]

                                     # calculate zoom panel sector theta position
                                     zoom_chr_data$ed_th <- rescale(zoom_chr_data$start,
                                                                    to = c(sec_tmp$sector_end1,sec_tmp$sector_start1),
                                                                    from = c(0,sec_tmp$length))
                                     zoom_chr_data$st_th <- rescale(zoom_chr_data$end,
                                                                    to = c(sec_tmp$sector_end1,sec_tmp$sector_start1),
                                                                    from = c(0,sec_tmp$length))


                                     tmp_data <- data[which(data$chr == zoom_chr[sec]),]

                                     # print(zoom_chr_data)
                                     # print(sec_tmp)
                                     # print(head(tmp_data))
                                     # break

                                     # ==============================================================================
                                     # args
                                     # ==============================================================================
                                     zoom_pos <- match.arg(zoom_pos,c("bottom","top"))

                                     zoom.start <- rev(zoom_chr_data$st_th)
                                     zoom.end <- rev(zoom_chr_data$ed_th)

                                     if(zoom.free.sec == TRUE){
                                       sec_tmp <- subset(sec_df2,sector_name == zoom_chr[sec])

                                       start <- sec_tmp$sector_start
                                       end <- sec_tmp$sector_end
                                     }else{
                                       start <- sec_tmp$sector_start1
                                       end <- sec_tmp$sector_end1
                                     }

                                     r0 <- unique(data$r0)
                                     r1 <- unique(data$r1)
                                     extend.radias <- abs(r1 - r0)*extend.yscale
                                     r0 <- r0 + extend.radias
                                     r1 <- r1 - extend.radias

                                     clock.wise <- unique(data$clock.wise)
                                     # ==============================================================================
                                     # coords into theta

                                     start.ed <- start - extend.start
                                     end.ed <- end + extend.end


                                     # check position
                                     if(is.null(zoom.panel.height)){
                                       zoom.panel.height <- abs(r1 - r0)
                                     }

                                     if(zoom_pos == "bottom"){
                                       r01 = r0 - panel.gap
                                       r2 = r01 - nested.r
                                       r3 = r01 - nested.r*2
                                       r4 = r01 - nested.r*3
                                       r5 = r01 - nested.r*4
                                       r6 = r5 - zoom.panel.height
                                     }else{
                                       r01 = r1 + panel.gap
                                       r2 = r01 + nested.r
                                       r3 = r01 + nested.r*2
                                       r4 = r01 + nested.r*3
                                       r5 = r01 + nested.r*4
                                       r6 = r5 + zoom.panel.height
                                     }


                                     # calculate sub panel coords
                                     theta.0 <- abs(end.ed - start.ed) - (length(zoom.start) - 1)*sub.zoom.gap
                                     sec <- (theta.0/sum(abs(zoom.end - zoom.start)))*(abs(zoom.end - zoom.start))

                                     sec.sum <- cumsum(c(start.ed,sec + c(0,rep(sub.zoom.gap,length(zoom.start) - 1))))
                                     sec.start <- sec.sum[1:(length(sec.sum) - 1)] + c(0,rep(sub.zoom.gap,length(zoom.start) - 1))
                                     sec.end <- sec.sum[2:length(sec.sum)]

                                     # check direction
                                     if(clock.wise == TRUE){
                                       sec.start <- 360 - sec.start
                                       sec.end <- 360 - sec.end

                                       zoom.start <- 360 - zoom.start
                                       zoom.end <- 360 - zoom.end

                                       start.ed <- 360 - start.ed
                                       end.ed <- 360 - end.ed
                                     }

                                     # ==============================================================================
                                     # genreate grobs
                                     # ==============================================================================

                                     # loop to draw
                                     # ii = 1
                                     # grob.list <- gList()

                                     for (ii in seq_along(zoom.start)) {
                                       # ============================================================================
                                       # origin panel highlight
                                       # ============================================================================
                                       thata.raw.u <- seq(as.radian(zoom.start[ii]),as.radian(zoom.end[ii]),length = n)

                                       xp.raw.up <- x0 + r1*cos(thata.raw.u)
                                       yp.raw.up <- y0 + r1*sin(thata.raw.u)

                                       thata.raw.b <- seq(as.radian(zoom.end[ii]),as.radian(zoom.start[ii]),length = n)

                                       xp.raw.b <- x0 + r0*cos(thata.raw.b)
                                       yp.raw.b <- y0 + r0*sin(thata.raw.b)

                                       xu <- scales::rescale(c(xp.raw.up,xp.raw.b),to = c(0,1),from = c(-1,1))
                                       yu <- scales::rescale(c(yp.raw.up,yp.raw.b),to = c(0,1),from = c(-1,1))

                                       upper.panel <- polygonGrob(x = xu,
                                                                  y = yu,
                                                                  gp = gpar(fill = alpha(zoom_chr_data$nested_fill,nested_alpha[1]),
                                                                            col = nested_col,
                                                                            lty = nested_lty,
                                                                            lwd = nested_linewidth),
                                                                  default.units = "npc")
                                       # ============================================================================
                                       # zoom linked panel
                                       # ============================================================================
                                       thata.1 <- seq(as.radian(zoom.start[ii]),as.radian(zoom.end[ii]),length = n)

                                       xp0 <- x0 + r01*cos(thata.1)
                                       yp0 <- y0 + r01*sin(thata.1)

                                       thata.23 <- seq(as.radian(sec.end[ii]),as.radian(zoom.end[ii]),length = n)
                                       r23 <- seq(r3,r2,length = n)

                                       xp23 <- x0 + r23*cos(thata.23)
                                       yp23 <- y0 + r23*sin(thata.23)

                                       thata.4 <- seq(as.radian(sec.end[ii]),as.radian(sec.start[ii]),length = n)

                                       xp4 <- x0 + r4*cos(thata.4)
                                       yp4 <- y0 + r4*sin(thata.4)


                                       thata.34 <- rev(seq(as.radian(sec.start[ii]),as.radian(zoom.start[ii]),length = n))
                                       r34 <- seq(r2,r3,length = n)

                                       xp34 <- x0 + r34*cos(thata.34)
                                       yp34 <- y0 + r34*sin(thata.34)

                                       xp <- scales::rescale(c(xp0,rev(xp23),xp4,rev(xp34)),to = c(0,1),from = c(-1,1))
                                       yp <- scales::rescale(c(yp0,rev(yp23),yp4,rev(yp34)),to = c(0,1),from = c(-1,1))

                                       middle.panel <- polygonGrob(x = xp,
                                                                   y = yp,
                                                                   gp = gpar(col = nested_col,
                                                                             lty = nested_lty,
                                                                             lwd = nested_linewidth,
                                                                             fill = alpha(zoom_chr_data$nested_fill,nested_alpha[2])),
                                                                   default.units = "npc")
                                       # ============================================================================
                                       # nested zoom panel
                                       # ============================================================================
                                       thata.zoom.u <- seq(as.radian(sec.start[ii]),as.radian(sec.end[ii]),length = n)

                                       xp.up <- x0 + r5*cos(thata.zoom.u)
                                       yp.up <- y0 + r5*sin(thata.zoom.u)

                                       thata.zoom.b <- seq(as.radian(sec.end[ii]),as.radian(sec.start[ii]),length = n)

                                       xp.b <- x0 + r6*cos(thata.zoom.b)
                                       yp.b <- y0 + r6*sin(thata.zoom.b)

                                       xb <- scales::rescale(c(xp.up,xp.b),to = c(0,1),from = c(-1,1))
                                       yb <- scales::rescale(c(yp.up,yp.b),to = c(0,1),from = c(-1,1))

                                       bottom.panel <- polygonGrob(x = xb,
                                                                   y = yb,
                                                                   gp = gpar(col = nested_col,
                                                                             lty = nested_lty,
                                                                             lwd = nested_linewidth,
                                                                             fill = alpha(zoom_chr_data$nested_fill,nested_alpha[2])),
                                                                   default.units = "npc")
                                       # ============================================================================
                                       # nested zoom panel add graphics
                                       # ============================================================================
                                       # filter data
                                       zoom_df <- zoom_chr_data[ii,]
                                       raw_df <- subset(tmp_data,gstart >= zoom_df$start & gend <= zoom_df$end)

                                       # check data and geom layers
                                       if(nrow(raw_df) == 0){
                                         graph.grob <- zeroGrob()
                                       }else{
                                         if(geom %in% c("point","rainfall")){
                                           stroke_size <- raw_df$stroke
                                           stroke_size[is.na(stroke_size)] <- 0

                                           graph.grob <- arcPointGrob(x = raw_df$x,y = raw_df$y,
                                                                      start = rev(sec.start)[ii],
                                                                      end = rev(sec.end)[ii],
                                                                      r0 = r5,r1 = r6,
                                                                      pch = raw_df$shape,
                                                                      size = raw_df$size,
                                                                      xscale = range(raw_df$x),
                                                                      yscale = range(data$y),
                                                                      point.gp = gpar(
                                                                        col = alpha(raw_df$colour, raw_df$alpha),
                                                                        fill = alpha(raw_df$fill, raw_df$alpha),
                                                                        # Stroke is added around the outside of the point
                                                                        fontsize = raw_df$size * .pt + stroke_size * .stroke / 2,
                                                                        lwd = raw_df$stroke * .stroke / 2),
                                                                      extend.xscale = zoom.panel.extend.xscale,
                                                                      extend.yscale = extend.yscale,
                                                                      clock.wise = unique(raw_df$clock.wise))
                                         }else if(geom == "line"){
                                           # col and fill
                                           col.d <- unique(raw_df[,c("fill","colour","group")])

                                           graph.grob <- arcLinesGrob(x = raw_df$x,y = raw_df$y,
                                                                      start = rev(sec.start)[ii],
                                                                      end = rev(sec.end)[ii],
                                                                      r0 = r5,r1 = r6,
                                                                      id = raw_df$group,
                                                                      xscale = range(raw_df$x),
                                                                      yscale = range(data$y),
                                                                      lines.gp = gpar(
                                                                        col = alpha(col.d$colour, unique(raw_df$alpha)),
                                                                        fill = alpha(col.d$fill, unique(raw_df$alpha)),
                                                                        lwd = unique(raw_df$linewidth),
                                                                        lty = unique(raw_df$linetype),
                                                                        lineend = "butt", linejoin = "round", linemitre = 10),
                                                                      extend.xscale = zoom.panel.extend.xscale,
                                                                      extend.yscale = extend.yscale,
                                                                      clock.wise = unique(raw_df$clock.wise))
                                         }else if(geom %in% c("area","density")){
                                           # col and fill
                                           col.d <- unique(raw_df[,c("fill","colour","group")])

                                           graph.grob <- arcPolygonGrob(x = c(raw_df$x,rev(raw_df$x)),
                                                                        y = c(raw_df$ymax,raw_df$ymin),
                                                                        id = c(raw_df$group,rev(raw_df$group)),
                                                                        polar.every = polar.every,
                                                                        start = rev(sec.start)[ii],
                                                                        end = rev(sec.end)[ii],
                                                                        r0 = r5,r1 = r6,
                                                                        xscale = range(raw_df$x),
                                                                        yscale = range(data$ymin,data$ymax),
                                                                        polygon.gp = gpar(
                                                                          col = alpha(col.d$colour, unique(raw_df$alpha)),
                                                                          fill = alpha(col.d$fill, unique(raw_df$alpha)),
                                                                          lwd = unique(raw_df$linewidth),
                                                                          lty = unique(raw_df$linetype),
                                                                          lineend = "butt", linejoin = "round"),
                                                                        extend.xscale = zoom.panel.extend.xscale,
                                                                        extend.yscale = extend.yscale,
                                                                        clock.wise = unique(raw_df$clock.wise))
                                         }else if(geom %in% c("rect","tile")){
                                           if(geom == "tile"){
                                             raw_df <- transform(raw_df,
                                                                 xmin = x - width/2,xmax = x + width/2,
                                                                 ymin = y - height/2,ymax = y + height/2)
                                           }

                                           graph.grob <- arcRectGrob(xmin = raw_df$xmin,ymin = raw_df$ymin,
                                                                     xmax = raw_df$xmax,ymax = raw_df$ymax,
                                                                     start = rev(sec.start)[ii],
                                                                     end = rev(sec.end)[ii],
                                                                     r0 = r5,r1 = r6,
                                                                     xscale = range(raw_df$xmin,raw_df$xmax),
                                                                     yscale = range(raw_df$ymin,raw_df$ymax),
                                                                     polygon.gp = gpar(
                                                                       col = alpha(raw_df$colour, raw_df$alpha),
                                                                       fill = alpha(raw_df$fill, raw_df$alpha),
                                                                       lwd = unique(raw_df$linewidth),
                                                                       lty = unique(raw_df$linetype),
                                                                       lineend = "butt", linejoin = "round"),
                                                                     extend.xscale = zoom.panel.extend.xscale,
                                                                     extend.yscale = extend.yscale,
                                                                     clock.wise = unique(raw_df$clock.wise))
                                         }
                                       }


                                       # ============================================================================
                                       # save in glist
                                       grob.list <- gList(grob.list,upper.panel,middle.panel,bottom.panel,graph.grob)
                                     }

                                   }

                                   # ============================================================================
                                   # combine grobs
                                   # ============================================================================
                                   ggname("geom_genomicNestedZoom",
                                          grid::gTree(children = gList(grob.list)))
                                 },

                                 draw_key = draw_key_point
)


# ==============================================================================

#' Create a genomic nested zoom plot
#'
#' This function creates a genomic nested zoom plot, allowing you to visualize
#' nested genomic regions and zoom in on specific sections of the data. It can be
#' used with various geoms such as points, lines, rectangles, areas, rainfall,
#' and density.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to apply to the data. Default is "identity".
#' @param position The position adjustment to be applied to the data. Default is "identity".
#' @param na.rm Should missing values be removed? Default is FALSE.
#' @param show.legend Should a legend be shown? Default is NA.
#' @param inherit.aes Should aesthetics be inherited? Default is TRUE.
#' @param chrom_data Data containing chromosome information.
#' @param keep.all.chrom Should all chromosomes be kept? Default is FALSE.
#' @param zoom_data Data for zoomed-in regions which includes 3 columns("chr","start","end").
#' @param geom Type of geom to be used. Default is "point".
#' @param extend.start Extend the start of the zoom panel. Default is 0.
#' @param extend.end Extend the end of the zoom panel. Default is 0.
#' @param nested.r Radius of nested genomic regions. Default is 0.02.
#' @param panel.gap Gap between zoom panels. Default is 0.02.
#' @param zoom.panel.height Height of the zoom panel. Default is NULL.
#' @param sub.zoom.gap Gap between sub-zoom panels. Default is 1.
#' @param zoom_pos Position of zoom panels (bottom or top). Default is c("bottom", "top").
#' @param n Number of points for create plot. Default is 100.
#' @param nested_fill Fill color for nested regions. Default is NULL.
#' @param nested_col Color for nested regions' outline. Default is "black".
#' @param nested_lty Line type for nested regions' outline. Default is "solid".
#' @param nested_linewidth Line width for nested regions' outline. Default is 0.5.
#' @param nested_alpha Alpha transparency for nested regions. Default is c(0.2, 0.5).
#' @param zoom.free.sec Allow free scaling for zoomed-in sections. Default is FALSE.
#' @param zoom.panel.extend.xscale Extend the x-scale for zoom panels. Default is 0.05.
#' @param mode Details see circlize::rainfallTransform.
#' @param normalize_to_width Details see circlize::rainfallTransform.
#' @param window.size Size of the genomic window. Default is 1e7.
#' Details see circlize::genomicDensity.
#' @param count_by Method for counting data (percent or number). Details see circlize::genomicDensity.
#' @param polar.every Display sectors at fixed angles (for circular plots). Default is FALSE.
#' @param space Space type for panels (free_x or fixed). Default is c("free_x", "fixed").
#' @param sector.gap Gap between sectors (for circular plots). Default is 3.
#' @param ... Additional arguments to be passed to the underlying layer function.
#'
#' @return A ggplot2 plot object.
#'
#' @export
geom_genomicNestedZoom <- function(mapping = NULL, data = NULL,
                                   stat = "identity", position = "identity",
                                   na.rm = FALSE, show.legend = NA,
                                   inherit.aes = TRUE,
                                   chrom_data = NULL,
                                   keep.all.chrom = FALSE,
                                   zoom_data = NULL,
                                   geom = c("point","line","rect","tile","area","rainfall","density"),
                                   extend.start = 0,
                                   extend.end = 0,
                                   nested.r = 0.02,
                                   panel.gap = 0.02,
                                   zoom.panel.height = NULL,
                                   sub.zoom.gap = 1,
                                   zoom_pos = c("bottom","top"),n = 100,
                                   nested_fill = NULL,
                                   nested_col = "black",
                                   nested_lty = "solid",
                                   nested_linewidth = 0.5,
                                   nested_alpha = c(0.2,0.5),
                                   zoom.free.sec = FALSE,
                                   zoom.panel.extend.xscale = 0.05,
                                   # ================================================
                                   mode = c("min", "max", "mean", "left", "right"),
                                   normalize_to_width = FALSE,
                                   window.size = 1e7,
                                   count_by = c("percent", "number"),
                                   polar.every = FALSE,
                                   # ================================================
                                   space = c("free_x","fixed"),
                                   sector.gap = 3,
                                   ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGenomicNestedZoom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  chrom_data = chrom_data,
                  keep.all.chrom = keep.all.chrom,
                  zoom_data = zoom_data,
                  geom = geom,
                  extend.start = extend.start,
                  extend.end = extend.end,
                  nested.r = nested.r,
                  panel.gap = panel.gap,
                  zoom.panel.height = zoom.panel.height,
                  sub.zoom.gap = sub.zoom.gap,
                  zoom_pos = zoom_pos,n = n,
                  nested_fill = nested_fill,
                  nested_col = nested_col,
                  nested_lty = nested_lty,
                  nested_linewidth = nested_linewidth,
                  nested_alpha = nested_alpha,
                  zoom.free.sec = zoom.free.sec,
                  zoom.panel.extend.xscale = zoom.panel.extend.xscale,
                  # ================================================
                  mode = mode,
                  normalize_to_width = normalize_to_width,
                  window.size = window.size,
                  count_by = count_by,
                  polar.every = polar.every,
                  # ================================================
                  space = space,
                  sector.gap = sector.gap,
                  ...)
  )
}
