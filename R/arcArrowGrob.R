#' Create an arc arrow grob
#'
#' This function creates a graphical object (grob) representing an arrow-shaped arc.
#'
#' @param xmin Minimum x-coordinate for the arrow.
#' @param xmax Maximum x-coordinate for the arrow.
#' @param y Y-coordinate for the arrow.
#' @param width Width of the arrow.
#' @param start.arrow.len Length of the starting arrow.
#' @param end.arrow.len Length of the ending arrow.
#' @param arrow.start.width Width of the starting arrow.
#' @param arrow.end.width Width of the ending arrow.
#' @param start Starting angle for the arc (in degrees).
#' @param end Ending angle for the arc (in degrees).
#' @param r0 Inner radius of the arc.
#' @param r1 Outer radius of the arc.
#' @param x0 X-coordinate of the center of the arc.
#' @param y0 Y-coordinate of the center of the arc.
#' @param polygon.gp Graphic parameters for the arc.
#' @param n Number of points to use for drawing the arc.
#' @param clock.wise If TRUE, draw the arc in a clockwise direction.
#' @param extend.xscale Scale factor for extending the x-axis range.
#' @param extend.yscale Scale factor for extending the y-axis range.
#' @param xscale Optional custom x-scale.
#' @param yscale Optional custom y-scale.
#' @param ... Additional arguments (not used).
#' @param name Name for the grob.
#' @param gp Graphic parameters for the grob.
#' @param vp Viewport for the grob.
#'
#' @return An arc arrow grob.
#'
#' @export
arcArrowGrob <- function(xmin = NULL,xmax = NULL,
                         y = NULL,width = 0.5,
                         start.arrow.len = 6,
                         end.arrow.len = 6,
                         arrow.start.width = 0.5,
                         arrow.end.width = 0.5,
                         start = 0,end = 360,
                         r0 = 0.5,r1 = 1,
                         x0 = 0,y0 = 0,
                         polygon.gp = NULL,
                         n = 100,clock.wise = FALSE,
                         extend.xscale = 0.05,
                         extend.yscale = 0.05,
                         xscale = NULL,
                         yscale = NULL,
                         ...,
                         name = NULL,
                         gp = NULL, vp = NULL){

  lst <- list(xmin = xmin,xmax = xmax,
              y = y,width = width,
              start.arrow.len = start.arrow.len,
              end.arrow.len = end.arrow.len,
              arrow.start.width = arrow.start.width,
              arrow.end.width = arrow.end.width,
              start = start,end = end,
              r0 = r0,r1 = r1,
              x0 = x0,y0 = y0,n = n,
              clock.wise = clock.wise,
              polygon.gp = polygon.gp,
              extend.xscale = extend.xscale,
              extend.yscale = extend.yscale,
              xscale = xscale,
              yscale = yscale,
              name = name, gp = gp, vp = vp,
              cl = "arcArrowGrob")

  do.call(gTree,lst)
}



#' @method makeContent arcArrowGrob
#' @export
makeContent.arcArrowGrob <- function(x){
  g <- .arcArrowGrob(xmin = x$xmin,xmax = x$xmax,
                     y = x$y,width = x$width,
                     start.arrow.len = x$start.arrow.len,
                     end.arrow.len = x$end.arrow.len,
                     arrow.start.width = x$arrow.start.width,
                     arrow.end.width = x$arrow.end.width,
                     start = x$start,end = x$end,
                     r0 = x$r0,r1 = x$r1,
                     x0 = x$x0,y0 = x$y0,n = x$n,
                     clock.wise = x$clock.wise,
                     polygon.gp = x$polygon.gp,
                     extend.xscale = x$extend.xscale,
                     extend.yscale = x$extend.yscale,
                     xscale = x$xscale,
                     yscale = x$yscale,
                     name = x$name, gp = x$gp, vp = x$vp)
  grid::setChildren(x, children = g$children)
}



#' @noRd
.arcArrowGrob <- function(xmin = NULL,xmax = NULL,
                          y = NULL,width = 0.5,
                          start.arrow.len = 6,
                          end.arrow.len = 6,
                          arrow.start.width = 0.5,
                          arrow.end.width = 0.5,
                          start = 0,end = 360,
                          r0 = 0.5,r1 = 1,
                          x0 = 0,y0 = 0,
                          polygon.gp = NULL,
                          n = 100,clock.wise = FALSE,
                          extend.xscale = 0.05,
                          extend.yscale = 0.05,
                          xscale = NULL,
                          yscale = NULL,
                          ...,
                          name = NULL,
                          gp = NULL, vp = NULL){
  # check length
  ymin <- y - width*0.5
  ymax <- y + width*0.5

  if(length(xmin) == 1 & length(xmax) == 1){
    xmin <- rep(xmin,length(ymin))
    xmax <- rep(xmax,length(ymax))
  }

  # extend scale
  extend.theta <- (end - start)*extend.xscale
  start_ed <- start + extend.theta
  end_ed <- end - extend.theta

  extend.radias <- (r1 - r0)*extend.yscale
  r0_ed <- r0 + extend.radias
  r1_ed <- r1 - extend.radias

  # =============================
  # check sector x,y scale
  if(is.null(yscale)){
    r.scale <- range(c(ymin,ymax))
  }else{
    r.scale <- yscale
  }

  if(is.null(xscale)){
    thata.scale <- range(c(xmin,xmax))
  }else{
    thata.scale <- xscale
  }
  # =============================

  # rescale data range
  scale.y <- scales::rescale(c(ymin,ymax),to = range(r0_ed,r1_ed),from = r.scale)
  rect.r0 <- scale.y[1:length(ymin)]
  rect.r1 <- scale.y[(length(ymin) + 1):length(scale.y)]

  # check direction
  if(clock.wise == TRUE){
    scale.x <- scales::rescale(c(xmin,xmax),to = 2*pi - range(as.radian(start_ed),as.radian(end_ed)),
                               from = thata.scale)
  }else{
    scale.x <- scales::rescale(c(xmin,xmax),to = range(as.radian(start_ed),as.radian(end_ed)),
                               from = thata.scale)
  }

  rect.start <- scale.x[1:length(xmin)]
  rect.end <- scale.x[(length(xmin) + 1):length(scale.x)]

  if(clock.wise == TRUE){
    new.start <- rect.start
    new.end <- rect.end
  }else{
    new.start <- as.radian(end) - (rect.start - as.radian(start))
    new.end <- new.start - (rect.end - rect.start)
  }

  # check start.arrow.len and end.arrow.len
  if(length(start.arrow.len) == 1){
    start.arrow.len <- rep(start.arrow.len,length(xmin))
  }

  if(length(end.arrow.len) == 1){
    end.arrow.len <- rep(end.arrow.len,length(xmin))
  }

  # loop create coordinates
  # x = 1
  lapply(seq_along(rect.r0), function(x){
    rin <- rect.r0[x]
    rout <- rect.r1[x]
    wid <- abs(rout - rin)

    # check start.arrow.len and end.arrow.len no more than abs(new.start - new.end)
    max_th <- abs(as.theta(new.start[x]) - as.theta(new.end[x]))

    if(start.arrow.len[x] > max_th){
      start.arrow.len[x] <- max_th
    }

    if(end.arrow.len[x] > max_th){
      end.arrow.len[x] <- max_th
    }

    theta <- seq(new.start[x] - as.radian(start.arrow.len[x]),
                 new.end[x] + as.radian(end.arrow.len[x]), length = n)

    # ==========================================================================
    # start arrow coordinate
    th.start <- new.start[x] - as.radian(start.arrow.len[x])

    if(start.arrow.len[x] == 0) arrow.start.width = 0

    start.arrow.pos.b.x <- x0 + (rin - wid*arrow.start.width)*cos(th.start)
    start.arrow.pos.b.y <- y0 + (rin - wid*arrow.start.width)*sin(th.start)

    start.arrow.pos.t.x <- x0 + (rout + wid*arrow.start.width)*cos(th.start)
    start.arrow.pos.t.y <- y0 + (rout + wid*arrow.start.width)*sin(th.start)

    start.arrow.pos.m.x <- x0 + ((rout + rin)/2)*cos(new.start[x])
    start.arrow.pos.m.y <- y0 + ((rout + rin)/2)*sin(new.start[x])

    # ==========================================================================
    # end arrow coordinate
    th.end <- new.end[x] + as.radian(end.arrow.len[x])

    if(end.arrow.len[x] == 0) arrow.end.width = 0

    end.arrow.pos.b.x <- x0 + (rin - wid*arrow.end.width)*cos(th.end)
    end.arrow.pos.b.y <- y0 + (rin - wid*arrow.end.width)*sin(th.end)

    end.arrow.pos.t.x <- x0 + (rout + wid*arrow.end.width)*cos(th.end)
    end.arrow.pos.t.y <- y0 + (rout + wid*arrow.end.width)*sin(th.end)

    end.arrow.pos.m.x <- x0 + ((rout + rin)/2)*cos(new.end[x])
    end.arrow.pos.m.y <- y0 + ((rout + rin)/2)*sin(new.end[x])


    # ==============================================
    # inner
    # ==============================================
    if(rin == 0){
      xp0 = x0;yp0 = y0
    }else{
      xp0 <- x0 + rin*cos(theta)
      yp0 <- y0 + rin*sin(theta)
    }

    # ==============================================
    # outer
    # ==============================================
    xp1 <- x0 + rout*cos(theta)
    yp1 <- y0 + rout*sin(theta)

    xlast <- c(start.arrow.pos.b.x,
               xp0,
               end.arrow.pos.b.x,end.arrow.pos.m.x,end.arrow.pos.t.x,
               rev(xp1),
               start.arrow.pos.t.x,start.arrow.pos.m.x)

    ylast <- c(start.arrow.pos.b.y,
               yp0,
               end.arrow.pos.b.y,end.arrow.pos.m.y,end.arrow.pos.t.y,
               rev(yp1),
               start.arrow.pos.t.y,start.arrow.pos.m.y)

    rect.df <- data.frame(x = xlast,y = ylast,id = x)

    return(rect.df)
  }) %>% do.call("rbind",.) -> all.df

  xp <- scales::rescale(all.df$x,to = c(0,1),from = c(-1,1))
  yp <- scales::rescale(all.df$y,to = c(0,1),from = c(-1,1))

  # grobs
  polygon.grob <- polygonGrob(x = xp,y = yp,
                              id = all.df$id,
                              gp = polygon.gp,
                              default.units = "npc",
                              name = "polygon")

  # ============================================================================
  # combine grobs
  # ============================================================================
  grid::gTree(children = grid::gList(polygon.grob),
              name = "arcArrowGrob")
}
