#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @importFrom dplyr filter mutate group_by summarise ungroup
#' @export
GeomChordDiagram <- ggproto("GeomChordDiagram", Geom,

                            required_aes = c("from","to","value",
                                             "start","end","r","clock.wise"),

                            default_aes = aes(
                              start = 0,end = 360,r = 0.5,
                              clock.wise = FALSE,
                              colour = NA,linewidth = .5,
                              linetype = 1,fill = "grey80",alpha = 0.5,
                              sector.bg.col = "black",
                              sector.bg.lty = 1,
                              sector.bg.lwd = 0.5),

                            draw_panel = function(self,data, panel_params, coord,
                                                  start.arrow = FALSE,
                                                  end.arrow = FALSE,
                                                  curve.arrow = NULL,
                                                  arrow.len = 0.01,
                                                  link.sector.space = 0.05,
                                                  sector.height = 0.1,
                                                  nice.facing = TRUE,
                                                  sector.fill = NULL,
                                                  sector.order = NULL,
                                                  vp.angle = NULL,
                                                  link.sort = TRUE,
                                                  link.decreasing = c(TRUE,TRUE),
                                                  directional = 0,
                                                  diffHeight = 0,
                                                  curve.height.low = 0.6,
                                                  link2line = FALSE,
                                                  big.gap = 15,
                                                  strip.label = TRUE,
                                                  strip.label.space = 0.15,
                                                  strip.label.fontface = "bold",
                                                  strip.label.col = "black",
                                                  strip.label.size = 10,
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
                              space <- match.arg(space,c("free_x","fixed"))

                              if(is.null(vp.angle)){
                                vp.angle <- big.gap
                              }

                              # prepare length for each sector
                              from_sum <- data %>% group_by(from) %>% summarise(sm = sum(abs(value)))
                              to_sum <- data %>% group_by(to) %>% summarise(sm = sum(abs(value)))

                              size_info <- data.frame(name = c(from_sum$from,to_sum$to),
                                                      size = c(from_sum$sm,to_sum$sm))
                              size_info$start <- unique(data$start)
                              size_info$end <- unique(data$end)

                              # combine duplicate
                              size_info <- size_info %>% group_by(name,start,end) %>%
                                summarise(size = sum(size)) %>%
                                ungroup()

                              size_info <- size_info[,c("name","size","start","end")]

                              # sector order
                              if(!is.null(sector.order)){
                                size_info <- size_info[match(sector.order,size_info$name),]
                              }else{
                                sector.order <- unique(c(unique(data$from),unique(data$to)))
                                size_info <- size_info[match(sector.order,size_info$name),]
                              }

                              # calculate sector theta
                              if(any(unique(data$from) %in% unique(data$to))){
                                sector.gap.new <- sector.gap
                              }else{
                                sector.gap.new <- c(rep(sector.gap,length(unique(data$from)) - 1),
                                                    big.gap,
                                                    rep(sector.gap,length(unique(data$to)) - 1),
                                                    big.gap)
                              }


                              sec_df <- unique(facet_sector(data = size_info,
                                                            data.type = "genomic",
                                                            sector.gap = sector.gap.new,
                                                            space = space))

                              if(length(unique(sec_df$sector_name)) == 1){
                                sec_df <- sec_df[1,]
                              }

                              # label for different facets
                              label.r = unique(data$r) + strip.label.space + link.sector.space + sector.height

                              data <- transform(data,value = abs(value))
                              # ====================================================
                              # check whether the names duplicate in from and to
                              if(any(unique(data$from) %in% unique(data$to))){
                                # process from
                                lapply(sec_df$sector_name, function(x){
                                  sec_th <- sec_df[which(sec_df$sector_name == x),]

                                  tmp.from <- subset(data,from %in% x | to %in% x)
                                  tmp.from$mk <- "f"

                                  # check source
                                  if(x %in% unique(data$from)){
                                    tmp_data <- tmp.from %>%
                                      filter(as.character(from) == as.character(to)) %>%
                                      mutate(mk = "t")

                                    if(nrow(tmp_data) > 0){
                                      tmp.from <- rbind(tmp.from,tmp_data)
                                    }else{
                                      tmp.from
                                    }

                                    tmp1 <- subset(tmp.from,from %in% x & mk == "f") %>% unique()

                                    order_sector <- get_order_name(from = x,
                                                                   all_name = sec_df$sector_name,
                                                                   all_from = unique(data$from),
                                                                   shift = 1)
                                    order_sector <- order_sector[order_sector %in% tmp1$to]

                                    tmp1 <- tmp1[match(order_sector,tmp1$to),]

                                    # =========================================================
                                    tmp2 <- subset(tmp.from,to %in% x & mk == "f") %>% unique()
                                    order_sector2 <- order_sector[order_sector %in% tmp2$from]
                                    tmp2 <- tmp2[match(order_sector2,tmp2$from),]

                                    tmp.from <- rbind(tmp1,tmp2)

                                  }

                                  cum <- c(0,cumsum(tmp.from$value))
                                  cum_th <- rescale(cum,to = rev(c(sec_th$sector_start,sec_th$sector_end)))

                                  tmp.from$start0 <- cum_th[1:(length(cum_th) - 1)]
                                  tmp.from$end0 <- cum_th[2:length(cum_th)]

                                  return(tmp.from)
                                }) %>% do.call("rbind",.) -> f.df

                                # process clean connection
                                lapply(sec_df$sector_name, function(x){
                                  tmp.from <- subset(f.df,from %in% x)

                                  lapply(unique(tmp.from$to), function(x){
                                    tmp_df <- subset(tmp.from,to %in% x)

                                    tmp_1 <- tmp_df[1,]
                                    tmp_2 <- tmp_df[2,]

                                    tmp_1$start1 <- tmp_2$start0
                                    tmp_1$end1 <- tmp_2$end0

                                    return(tmp_1)
                                  }) %>% Reduce("rbind",.) -> clean_df

                                  return(clean_df)
                                }) %>% do.call("rbind",.) -> mer

                              }else{
                                # =======================================================================================
                                # process from
                                lapply(unique(data$from), function(x){
                                  sec_th <- sec_df[which(sec_df$sector_name == x),]
                                  tmp.from <- subset(data,from == x)

                                  if(link.sort == TRUE){
                                    tmp.from <- tmp.from[order(tmp.from$to,tmp.from$value,decreasing = link.decreasing[1]),]
                                  }

                                  cum <- c(0,cumsum(tmp.from$value))
                                  cum_th <- rescale(cum,to = c(sec_th$sector_start,sec_th$sector_end))

                                  tmp.from$start0 <- cum_th[1:(length(cum_th) - 1)]
                                  tmp.from$end0 <- cum_th[2:length(cum_th)]

                                  return(tmp.from)
                                }) %>% do.call("rbind",.) -> f.df

                                # process to
                                lapply(unique(data$to), function(x){
                                  sec_th <- sec_df[which(sec_df$sector_name == x),]
                                  tmp.to <- subset(data,to == x)

                                  if(link.sort == TRUE){
                                    tmp.to <- tmp.to[order(tmp.to$from,tmp.to$value,decreasing = link.decreasing[2]),]
                                  }

                                  cum <- c(0,cumsum(tmp.to$value))
                                  cum_th <- rescale(cum,to = c(sec_th$sector_start,sec_th$sector_end))

                                  tmp.to$start1 <- cum_th[1:(length(cum_th) - 1)]
                                  tmp.to$end1 <- cum_th[2:length(cum_th)]

                                  tmp.to <- tmp.to[,c("from","to","value","start1","end1")]

                                  return(tmp.to)
                                }) %>% do.call("rbind",.) -> t.df

                                # merge
                                mer <- merge(f.df,t.df,by = c("from","to"))
                              }

                              # =======================================================================================
                              # generate grobs
                              # =======================================================================================
                              track_glist <- gList()
                              for (i in 1:nrow(mer)) {
                                tmp <- mer[i,]

                                # check links radius
                                if(directional == 1){
                                  if(any(unique(data$from) %in% unique(data$to))){
                                    idx_raw <- match(tmp$from,sec_df$sector_name)
                                    idx <- match(tmp$to,sec_df$sector_name)

                                    if(idx >= idx_raw){
                                      r0 <- tmp$r - sector.height/2 - link.sector.space - diffHeight
                                      r1 <- tmp$r
                                    }else{
                                      r0 <- tmp$r
                                      r1 <- tmp$r - sector.height/2 - link.sector.space - diffHeight
                                    }
                                  }else{
                                    r0 <- tmp$r - sector.height/2 - link.sector.space - diffHeight
                                    r1 <- tmp$r
                                  }

                                }else if(directional == -1){
                                  if(any(unique(data$from) %in% unique(data$to))){
                                    idx_raw <- match(tmp$from,sec_df$sector_name)
                                    idx <- match(tmp$to,sec_df$sector_name)

                                    if(idx >= idx_raw){
                                      r0 <- tmp$r
                                      r1 <- tmp$r - sector.height/2 - link.sector.space - diffHeight
                                    }else{
                                      r0 <- tmp$r - sector.height/2 - link.sector.space - diffHeight
                                      r1 <- tmp$r
                                    }

                                  }else{
                                    r0 <- tmp$r
                                    r1 <- tmp$r - sector.height/2 - link.sector.space - diffHeight
                                  }
                                }else{
                                  r0 <- tmp$r
                                  r1 <- tmp$r
                                }

                                # check link type
                                if(link2line == TRUE){
                                  link_start <- (tmp$start0 + tmp$end0)/2
                                  link_end <- (tmp$start1 + tmp$end1)/2
                                }else{
                                  link_start <- c(tmp$start0,tmp$end0)
                                  link_end <- c(tmp$start1,tmp$end1)
                                }

                                # curve height
                                h <- get_height(start = min(link_start),
                                                end = max(link_end),
                                                r0 = curve.height.low,r1 = tmp$r)

                                # link grobs
                                link_grob <- arcLinks2Grob(start = link_start,
                                                           end = link_end,
                                                           r = tmp$r,r0 = r0,r1 = r1,
                                                           # curve.height = tmp$r,
                                                           curve.height = h,
                                                           start.arrow = start.arrow,
                                                           end.arrow = end.arrow,
                                                           arrow.len = arrow.len,
                                                           bezierCurve.gp =
                                                             gpar(fill = alpha(tmp$fill, tmp$alpha),
                                                                  col = alpha(tmp$colour, tmp$alpha),
                                                                  lwd = tmp$linewidth,
                                                                  lty = tmp$linetype),
                                                           bezierPolygon.gp =
                                                             gpar(fill = alpha(tmp$fill, tmp$alpha),
                                                                  col = alpha(tmp$colour, tmp$alpha),
                                                                  lwd = tmp$linewidth,
                                                                  lty = tmp$linetype),
                                                           curve.arrow = curve.arrow)


                                track_glist <- gList(track_glist,link_grob)
                              }

                              # ==================================================================
                              # labels for each sector
                              gp <- sec_df$sector_name

                              label_glist <- gList()

                              # loop for grobs
                              if(is.null(sector.fill)){
                                sec_df$col <- circlize::rand_color(n = nrow(sec_df))
                              }else{
                                sec_df$col <- sector.fill
                              }

                              # loop
                              for (sec in seq_along(gp)) {
                                xAxis.params.tmp <- list(xscale = c(0,sec_df$length[sec]),nice.facing = nice.facing)
                                xAxis.params <- modifyList(xAxis.params,xAxis.params.tmp)

                                sector_grob <- arcSectorGrob(start = sec_df$sector_start[sec],
                                                             end = sec_df$sector_end[sec],
                                                             r0 = unique(data$r) + link.sector.space,
                                                             r1 = unique(data$r) + link.sector.space + sector.height,
                                                             extend.xscale = 0,
                                                             extend.yscale = extend.yscale,
                                                             sector.gp = gpar(fill = sec_df$col[sec]),
                                                             add.xaxis = add.xaxis,
                                                             arcxAxisGrob.params = xAxis.params,
                                                             add.yaxis = FALSE,
                                                             clock.wise = unique(data$clock.wise))

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

                                label_glist <- gList(label_glist,plabel,sector_grob)
                              }

                              # =========================================================================
                              # generateing direction grobs
                              if(!is.null(directional)){
                                if(directional == 1){
                                  if(any(unique(data$from) %in% unique(data$to))){
                                    sec_direction <- mer

                                    direct_list <- gList()
                                    for (f in unique(sec_direction$from)) {
                                      sub_direc <- subset(sec_direction,from %in% f) %>%
                                        dplyr::left_join(y = sec_df[,c("sector_name","col")],
                                                         by = c("to" = "sector_name"))

                                      # reorder
                                      order_sector <- get_order_name(from = f,
                                                                     all_name = sec_df$sector_name,
                                                                     all_from = unique(data$from),
                                                                     shift = 1)

                                      sub_direc <- sub_direc[match(order_sector,sub_direc$to),]

                                      # direct_list <- gList()
                                      for (i in 1:nrow(sub_direc)) {
                                        tmp_dirc <- sub_direc[i,]

                                        idx_raw <- match(tmp_dirc$from,sec_df$sector_name)
                                        idx <- match(tmp_dirc$to,sec_df$sector_name)

                                        if(idx >= idx_raw){
                                          sector_start <- tmp_dirc$start0
                                          sector_end <- tmp_dirc$end0
                                        }else{
                                          sector_start <- tmp_dirc$start1
                                          sector_end <- tmp_dirc$end1
                                        }


                                        direc_grob <- arcSectorGrob(start = sector_start,
                                                                    end = sector_end,
                                                                    r1 = unique(data$r),
                                                                    r0 = unique(data$r) - sector.height/2,
                                                                    extend.xscale = 0,
                                                                    extend.yscale = 0,
                                                                    sector.gp = gpar(fill = tmp_dirc$col,
                                                                                     col = tmp_dirc$colour),
                                                                    add.xaxis = FALSE,
                                                                    add.yaxis = FALSE,
                                                                    clock.wise = unique(data$clock.wise))

                                        direct_list <- gList(direct_list,direc_grob)
                                      }
                                    }

                                  }else{
                                    sec_direction <- subset(sec_df,sector_name %in% unique(data$from))

                                    direct_list <- gList()
                                    for (i in 1:nrow(sec_direction)) {
                                      tmp_dirc <- sec_direction[i,]
                                      direc_grob <- arcSectorGrob(start = tmp_dirc$sector_start,
                                                                  end = tmp_dirc$sector_end,
                                                                  r1 = unique(data$r),
                                                                  r0 = unique(data$r) - sector.height/2,
                                                                  extend.xscale = 0,
                                                                  extend.yscale = 0,
                                                                  sector.gp = gpar(fill = "grey80"),
                                                                  add.xaxis = FALSE,
                                                                  add.yaxis = FALSE,
                                                                  clock.wise = unique(data$clock.wise))

                                      direct_list <- gList(direct_list,direc_grob)
                                    }
                                  }

                                }else if(directional == -1){
                                  if(any(unique(data$from) %in% unique(data$to))){
                                    sec_direction <- mer

                                    direct_list <- gList()
                                    for (f in unique(sec_direction$to)) {
                                      sub_direc <- subset(sec_direction,to %in% f) %>%
                                        dplyr::left_join(y = sec_df[,c("sector_name","col")],
                                                         by = c("from" = "sector_name"))

                                      # loop create sectors
                                      for (i in 1:nrow(sub_direc)) {
                                        tmp_dirc <- sub_direc[i,]

                                        idx_raw <- match(tmp_dirc$to,sec_df$sector_name)
                                        idx <- match(tmp_dirc$from,sec_df$sector_name)

                                        if(idx <= idx_raw){
                                          sector_start <- tmp_dirc$start1
                                          sector_end <- tmp_dirc$end1
                                        }else{
                                          sector_start <- tmp_dirc$start0
                                          sector_end <- tmp_dirc$end0
                                        }


                                        direc_grob <- arcSectorGrob(start = sector_start,
                                                                    end = sector_end,
                                                                    r1 = unique(data$r),
                                                                    r0 = unique(data$r) - sector.height/2,
                                                                    extend.xscale = 0,
                                                                    extend.yscale = 0,
                                                                    sector.gp = gpar(fill = tmp_dirc$col,
                                                                                     col = tmp_dirc$colour),
                                                                    add.xaxis = FALSE,
                                                                    add.yaxis = FALSE,
                                                                    clock.wise = unique(data$clock.wise))

                                        direct_list <- gList(direct_list,direc_grob)
                                      }
                                    }
                                  }else{
                                    sec_direction <- mer

                                    direct_list <- gList()
                                    for (i in 1:nrow(sec_direction)) {
                                      tmp_dirc <- sec_direction[i,]
                                      direc_grob <- arcSectorGrob(start = tmp_dirc$start1,
                                                                  end = tmp_dirc$end1,
                                                                  r1 = unique(data$r),
                                                                  r0 = unique(data$r) - sector.height/2,
                                                                  extend.xscale = 0,
                                                                  extend.yscale = 0,
                                                                  sector.gp = gpar(fill = tmp_dirc$fill,
                                                                                   col = tmp_dirc$colour),
                                                                  add.xaxis = FALSE,
                                                                  add.yaxis = FALSE,
                                                                  clock.wise = unique(data$clock.wise))

                                      direct_list <- gList(direct_list,direc_grob)
                                    }
                                  }

                                }else{
                                  direct_list <- zeroGrob()
                                }

                              }else{
                                direct_list <- zeroGrob()
                              }

                              ggname("geom_chordDiagram",
                                     grid::gTree(children = gList(track_glist,label_glist,direct_list),
                                                 vp = viewport(angle = vp.angle)))
                            },

                            # ==================================================================
                            # legend
                            draw_key = draw_key_rect

)


# ==============================================================================

#' Create a chord diagram plot.
#'
#' This function creates a chord diagram plot using the specified data and
#' mapping.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default is "identity").
#' @param position The position adjustment method to be used (default is "identity").
#' @param na.rm Logical value indicating whether to remove NA values (default is FALSE).
#' @param show.legend Logical value indicating whether to display the legend (default is NA).
#' @param inherit.aes Logical value indicating whether to inherit aesthetics (default is TRUE).
#' @param start.arrow Logical value indicating whether to include arrows at the start of links (default is FALSE).
#' @param end.arrow Logical value indicating whether to include arrows at the end of links (default is FALSE).
#' @param curve.arrow The curvature of the arrows (default is NULL).
#' @param arrow.len The length of arrows (default is 0.01).
#' @param link.sector.space The space between links and sectors (default is 0.05).
#' @param sector.height The height of sectors (default is 0.1).
#' @param nice.facing Logical value indicating whether to use nice facing for sectors (default is TRUE).
#' @param sector.fill Fill color for sectors (default is NULL).
#' @param sector.order Order of sectors (default is NULL).
#' @param vp.angle Angle of the viewing perspective (default is NULL).
#' @param link.sort Logical value indicating whether to sort links (default is TRUE).
#' @param link.decreasing Logical vector indicating whether links should be decreasing in size (default is c(TRUE, TRUE)).
#' @param directional Numeric value indicating directionality (default is 0).
#' @param diffHeight Numeric value indicating height difference (default is 0).
#' @param curve.height.low Numeric value indicating the low point of the curve (default is 0.6).
#' @param link2line Logical value indicating whether to convert links to lines (default is FALSE).
#' @param big.gap Numeric value indicating the size of a big gap (default is 15).
#' @param strip.label Logical value indicating whether to display strip labels (default is TRUE).
#' @param strip.label.space Numeric value indicating the space for strip labels (default is 0.15).
#' @param strip.label.fontface Font face for strip labels (default is "bold").
#' @param strip.label.col Color for strip labels (default is "black").
#' @param strip.label.size Size of strip labels (default is 10).
#' @param space Space setting for x and y scales (default is c("free_x", "fixed")).
#' @param sector.gap Numeric value indicating the gap between sectors (default is 3).
#' @param extend.xscale Numeric value indicating the extension of the x-scale (default is 0.05).
#' @param extend.yscale Numeric value indicating the extension of the y-scale (default is 0.05).
#' @param add.bg Logical value indicating whether to add a background (default is TRUE).
#' @param sector.bg.extend Numeric value indicating the extension of the sector background (default is 0.025).
#' @param add.xaxis Logical value indicating whether to add an x-axis (default is TRUE).
#' @param add.yaxis Logical value indicating whether to add a y-axis (default is FALSE).
#' @param xAxis.params List of parameters for the x-axis (default is list()).
#' @param yAxis.params List of parameters for the y-axis (default is list()).
#' @param ... Additional parameters.
#'
#' @return A chord diagram plot.
#'
#' @import ggplot2
#'
#' @export
geom_chordDiagram <- function(mapping = NULL, data = NULL,
                              stat = "identity", position = "identity",
                              na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE,
                              start.arrow = FALSE,
                              end.arrow = FALSE,
                              curve.arrow = NULL,
                              arrow.len = 0.01,
                              link.sector.space = 0.05,
                              sector.height = 0.1,
                              nice.facing = TRUE,
                              sector.fill = NULL,
                              sector.order = NULL,
                              vp.angle = NULL,
                              link.sort = TRUE,
                              link.decreasing = c(TRUE,TRUE),
                              directional = 0,
                              diffHeight = 0,
                              curve.height.low = 0.6,
                              link2line = FALSE,
                              big.gap = 15,
                              strip.label = TRUE,
                              strip.label.space = 0.15,
                              strip.label.fontface = "bold",
                              strip.label.col = "black",
                              strip.label.size = 10,
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
    geom = GeomChordDiagram,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  start.arrow = start.arrow,
                  end.arrow = end.arrow,
                  curve.arrow = curve.arrow,
                  arrow.len = arrow.len,
                  link.sector.space = link.sector.space,
                  sector.height = sector.height,
                  nice.facing = nice.facing,
                  sector.fill = sector.fill,
                  sector.order = sector.order,
                  vp.angle = vp.angle,
                  link.sort = link.sort,
                  link.decreasing = link.decreasing,
                  directional = directional,
                  diffHeight = diffHeight,
                  curve.height.low = curve.height.low,
                  link2line = link2line,
                  big.gap = big.gap,
                  strip.label = strip.label,
                  strip.label.space = strip.label.space,
                  strip.label.fontface = strip.label.fontface,
                  strip.label.col = strip.label.col,
                  strip.label.size = strip.label.size,
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
