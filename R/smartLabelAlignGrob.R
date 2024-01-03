#' Smart Label Align Grob
#'
#' This function creates a graphical object that aligns a labeled mark with a set of labels on a plot.
#'
#' @param all.label A vector of all labels.
#' @param mark.label The label to be aligned.
#' @param x The x-coordinate for the alignment.
#' @param y The y-coordinate for the alignment.
#' @param use.smartAlign2 Logical, whether to use smart alignment.
#' @param link.line.length Length of the connecting lines.
#' @param link.label.space Spacing between the label and connecting lines.
#' @param mark.scale A numeric vector specifying the scale for mark positioning.
#' @param link.label.gp A graphical parameter for the label appearance.
#' @param link.line.gp A graphical parameter for the connecting lines.
#' @param link.circle.start.gp A graphical parameter for the start circle (if used).
#' @param link.circle.end.gp A graphical parameter for the end circle (if used).
#' @param link.start.type Type of shape at the start of the connecting lines ("line", "circle", or "arrow").
#' @param link.end.type Type of shape at the end of the connecting lines ("line", "circle", or "arrow").
#' @param circle.arrow.size A numeric vector specifying the size of circles or arrows.
#' @param pos Alignment position ("right", "left", "top", or "bottom").
#' @param name A name for the graphical object.
#' @param gp A graphical parameter for the entire grob.
#' @param vp A viewport for the grob.
#'
#' @return A graphical object for smart label alignment.
#'
#' @export
smartLabelAlignGrob <- function(all.label = NULL,
                                mark.label = NULL,
                                x = 0.5,y = 0.5,
                                use.smartAlign2 = FALSE,
                                link.line.length = 0.025,
                                link.label.space = 0.02,
                                mark.scale = c(0.01,0.99),
                                link.label.gp = gpar(fontsize = 10),
                                link.line.gp = gpar(fill = "black",col = "black"),
                                link.circle.start.gp = gpar(fill = "black",col = "black"),
                                link.circle.end.gp = gpar(fill = "black",col = "black"),
                                link.start.type = c("line","circle","arrow"),
                                link.end.type = c("line","circle","arrow"),
                                circle.arrow.size = c(0.01,0.01),
                                pos = c("right","left","top","bottom"),
                                name = NULL,
                                gp = NULL, vp = NULL){

  lst <- list(all.label = all.label,
              mark.label = mark.label,
              x = x,y = y,
              use.smartAlign2 = use.smartAlign2,
              link.line.length = link.line.length,
              link.label.space = link.label.space,
              mark.scale = mark.scale,
              link.label.gp = link.label.gp,
              link.line.gp = link.line.gp,
              link.circle.start.gp = link.circle.start.gp,
              link.circle.end.gp = link.circle.end.gp,
              link.start.type = link.start.type,
              link.end.type = link.end.type,
              circle.arrow.size = circle.arrow.size,
              pos = pos,
              name = name, gp = gp, vp = vp,
              cl = "smartLabelAlignGrob")

  do.call(gTree,lst)
}



#' @method makeContent smartLabelAlignGrob
#' @export
makeContent.smartLabelAlignGrob <- function(x){
  g <- .smartLabelAlignGrob(all.label = x$all.label,
                            mark.label = x$mark.label,
                            x = x$x,y = x$y,
                            use.smartAlign2 = x$use.smartAlign2,
                            link.line.length = x$link.line.length,
                            link.label.space = x$link.label.space,
                            mark.scale = x$mark.scale,
                            link.label.gp = x$link.label.gp,
                            link.line.gp = x$link.line.gp,
                            link.circle.start.gp = x$link.circle.start.gp,
                            link.circle.end.gp = x$link.circle.end.gp,
                            link.start.type = x$link.start.type,
                            link.end.type = x$link.end.type,
                            circle.arrow.size = x$circle.arrow.size,
                            pos = x$pos,
                            name = x$name, gp = x$gp, vp = x$vp)
  grid::setChildren(x, children = g$children)
}


#' @noRd
.smartLabelAlignGrob <- function(all.label = NULL,
                                 mark.label = NULL,
                                 x = 0.5,y = 0.5,
                                 use.smartAlign2 = FALSE,
                                 link.line.length = 0.025,
                                 link.label.space = 0.02,
                                 mark.scale = c(0.01,0.99),
                                 link.label.gp = gpar(fontsize = 10),
                                 link.line.gp = gpar(fill = "black",col = "black"),
                                 link.circle.start.gp = gpar(fill = "black",col = "black"),
                                 link.circle.end.gp = gpar(fill = "black",col = "black"),
                                 link.start.type = c("line","circle","arrow"),
                                 link.end.type = c("line","circle","arrow"),
                                 circle.arrow.size = c(0.01,0.01),
                                 pos = c("right","left","top","bottom"),
                                 name = NULL,
                                 gp = NULL, vp = NULL){
  # check args
  link.start.type <- match.arg(link.start.type,c("line","circle","arrow"))
  link.end.type <- match.arg(link.end.type,c("line","circle","arrow"))
  pos <- match.arg(pos,c("right","left","top","bottom"))

  # ============================================================================
  # get mark label index and position
  idx <- match(mark.label,all.label)
  pos.raw <- scales::rescale(idx,to = c(0,1),from = c(0.5,length(all.label) + 0.5))
  pos.pos <- scales::rescale(1:length(idx),to = mark.scale)
  pos.idx <- match(idx,sort(idx))
  pos.new <- pos.pos[pos.idx]

  # ============================================================================
  # smartAlign2 adjusted
  if(use.smartAlign2 == TRUE){
    lapply(seq_along(idx), function(ii){
      if(pos %in% c("left","right")){
        # pushViewport(viewport(xscale = c(0,1),
        #                       yscale = c(0.5,length(all.label) + 0.5)
        # ))

        label.g <- textGrob(label = mark.label[ii],
                            x = x,y = idx[ii],
                            gp = link.label.gp,
                            default.units = "native")

        h <- convertHeight(grobHeight(label.g),"native")
        y <- convertY(grobY(label.g,0),"native")

        ymin <- y - h*0.5
        ymax <- y + h*0.5

        data.frame(ymin = as.numeric(ymin),ymax = as.numeric(ymax))
      }else if(pos %in% c("top","bottom")){
        # pushViewport(viewport(yscale = c(0,1),
        #                       xscale = c(0.5,length(all.label) + 0.5)
        # ))
        label.g <- textGrob(label = mark.label[ii],
                            x = idx[ii],y = y,
                            gp = link.label.gp,
                            rot = 90,
                            default.units = "native")

        h <- convertWidth(grobWidth(label.g),"native")
        y <- convertX(grobX(label.g,0),"native")

        ymin <- y - h*0.5
        ymax <- y + h*0.5

        data.frame(ymin = as.numeric(ymin),ymax = as.numeric(ymax))
      }


    }) %>% do.call("rbind",.) -> pos.label

    # calculate adjusted pos
    pos.ajusted <- data.frame(ComplexHeatmap::smartAlign2(pos.label,
                                                          range = c(0.5,length(all.label) + 0.5),
                                                          # range = c(0,1),
                                                          plot = F))
    colnames(pos.ajusted) <- c("ymin","ymax")
    pos.ajusted$ymid <- (pos.ajusted$ymin + pos.ajusted$ymax)/2

    pos.new <- scales::rescale(pos.ajusted$ymid,to = mark.scale)
  }
  # ============================================================================

  # viewport
  vp <- viewport(yscale = mark.scale,xscale = c(0,1),clip = "off")

  # coordinate
  seg.x0 <- x
  seg.y0 <- pos.raw

  seg.x1 <- x + link.line.length
  seg.y1 <- pos.raw
  seg.x2 <- seg.x1 + link.line.length
  seg.y2 <- pos.new
  seg.x3 <- seg.x2 + link.line.length
  seg.y3 <- pos.new
  label.x <- x + 3*link.line.length + link.label.space

  # ============================================================================
  # check direction
  # pos <- c("right","left","top","bottom")
  if(pos == "left"){
    seg.x1 <- 2*x - seg.x1
    seg.x2 <- 2*x - seg.x2
    seg.x3 <- 2*x - seg.x3
    label.x <- 2*x - label.x

    hjust <- 1
  }else if(pos == "right"){
    hjust <- 0
  }else if(pos == "top"){
    seg.y0 <- y
    seg.x0 <- pos.raw

    seg.y1 <- y + link.line.length
    seg.x1 <- pos.raw
    seg.y2 <- seg.y1 + link.line.length
    seg.x2 <- pos.new
    seg.y3 <- seg.y2 + link.line.length
    seg.x3 <- pos.new
    label.x <- y + 3*link.line.length + link.label.space

    hjust <- 0
  }else if(pos == "bottom"){
    seg.y0 <- y
    seg.x0 <- pos.raw

    seg.y1 <- 2*y - (y + link.line.length)
    seg.x1 <- pos.raw
    seg.y2 <- seg.y1 - link.line.length
    seg.x2 <- pos.new
    seg.y3 <- seg.y2 - link.line.length
    seg.x3 <- pos.new
    label.x <- 2*y - (y + 3*link.line.length + link.label.space)

    hjust <- 1
  }

  # ============================================================================
  # check link start shape
  if(link.start.type %in% c("circle","arrow")){
    if(pos == "left"){
      seg.x1 <- seg.x1 - circle.arrow.size[1]
      seg.x2 <- seg.x2 - circle.arrow.size[1]
      seg.x3 <- seg.x3 - circle.arrow.size[1]
      label.x <- label.x - circle.arrow.size[1]
    }else if(pos == "right"){
      seg.x1 <- seg.x1 + circle.arrow.size[1]
      seg.x2 <- seg.x2 + circle.arrow.size[1]
      seg.x3 <- seg.x3 + circle.arrow.size[1]
      label.x <- label.x + circle.arrow.size[1]
    }else if(pos == "top"){
      seg.y1 <- seg.y1 + circle.arrow.size[1]
      seg.y2 <- seg.y2 + circle.arrow.size[1]
      seg.y3 <- seg.y3 + circle.arrow.size[1]
      label.x <- label.x + circle.arrow.size[1]
    }else if(pos == "bottom"){
      seg.y1 <- seg.y1 - circle.arrow.size[1]
      seg.y2 <- seg.y2 - circle.arrow.size[1]
      seg.y3 <- seg.y3 - circle.arrow.size[1]
      label.x <- label.x - circle.arrow.size[1]
    }


    # check
    if(link.start.type == "circle"){
      start.circle.grob <- circleGrob(x = seg.x0,y = seg.y0,
                                      r = circle.arrow.size[1],
                                      vp = vp,
                                      gp = link.circle.start.gp,
                                      name = "start.circle",
                                      default.units = "npc")

      start.arrow <- NULL
    }else if(link.start.type == "arrow"){
      start.arrow <- arrow(ends = "first",type = "closed",
                           length = unit(circle.arrow.size[1]*2,"npc"))

      start.circle.grob <- nullGrob()
    }
  }else{
    start.arrow <- NULL
    start.circle.grob <- nullGrob()
  }

  # check link end shape
  if(link.end.type %in% c("circle","arrow")){
    if(pos == "left"){
      seg.x3 <- seg.x3 - circle.arrow.size[2]
      label.x <- label.x - circle.arrow.size[2] - circle.arrow.size[2]*0.5
    }else if(pos == "right"){
      seg.x3 <- seg.x3 + circle.arrow.size[2]
      label.x <- label.x + circle.arrow.size[2] + circle.arrow.size[2]*0.5
    }else if(pos == "top"){
      seg.y3 <- seg.y3 + circle.arrow.size[2]
      label.x <- label.x + circle.arrow.size[2] + circle.arrow.size[2]*0.5
    }else if(pos == "bottom"){
      seg.y3 <- seg.y3 - circle.arrow.size[2]
      label.x <- label.x - circle.arrow.size[2] - circle.arrow.size[2]*0.5
    }


    # check
    if(link.end.type == "circle"){
      end.circle.grob <- circleGrob(x = seg.x3,y = seg.y3,
                                    r = circle.arrow.size[2],
                                    vp = vp,
                                    gp = link.circle.end.gp,
                                    name = "end.circle",
                                    default.units = "npc")

      end.arrow <- NULL
    }else if(link.end.type == "arrow"){
      end.arrow <- arrow(ends = "last",type = "closed",
                         length = unit(circle.arrow.size[2]*2,"npc"))

      end.circle.grob <- nullGrob()
    }
  }else{
    end.arrow <- NULL
    end.circle.grob <- nullGrob()
  }

  # ============================================================================
  # label x,y
  if(pos %in% c("left","right")){
    text.xpos <- label.x
    text.ypos <- pos.new
    text.rot <- 0
    line1.y1 <- seg.y0
    line3.y0 <- seg.y3
  }else if(pos %in% c("top","bottom")){
    text.xpos <- pos.new
    text.ypos <- label.x
    text.rot <- 90
    line1.y1 <- seg.y1
    line3.y0 <- seg.y2
  }
  # ============================================================================
  # grobs
  link.1.grob <- segmentsGrob(x0 = seg.x0,x1 = seg.x1,
                              y0 = seg.y0,y1 = line1.y1,
                              vp = vp,
                              arrow = start.arrow,
                              gp = link.line.gp,
                              name = "line1",
                              default.units = "npc")

  link.2.grob <- segmentsGrob(x0 = seg.x1,x1 = seg.x2,
                              y0 = seg.y1,y1 = seg.y2,
                              vp = vp,
                              gp = link.line.gp,
                              name = "line2",
                              default.units = "npc")

  link.3.grob <- segmentsGrob(x0 = seg.x2,x1 = seg.x3,
                              y0 = line3.y0,y1 = seg.y3,
                              vp = vp,
                              arrow = end.arrow,
                              gp = link.line.gp,
                              name = "line3",
                              default.units = "npc")


  mark.label.grob <- textGrob(x = text.xpos,
                              y = text.ypos,
                              vp = vp,
                              gp = link.label.gp,
                              label = mark.label,
                              hjust = hjust,
                              name = "mark.label",
                              rot = text.rot)
  # ============================================================================
  # combine grobs
  # ============================================================================
  grid::gTree(children = grid::gList(link.1.grob,link.2.grob,link.3.grob,
                                     mark.label.grob,
                                     start.circle.grob,end.circle.grob),
              name = "smartLabelAlignGrob")
}
