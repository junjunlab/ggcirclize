#' Create a Grob for Arc Links
#'
#' This function creates a grob for arc links between two points.
#'
#' @param start Starting angle in degrees (default: 0).
#' @param end Ending angle in degrees (default: 60).
#' @param r0 Inner radius (default: 1).
#' @param r1 Outer radius (default: 1).
#' @param r Radius for the arc (default: 1).
#' @param arrow.len Length of arrows (default: 0.05).
#' @param x0 X-coordinate of the center point (default: 0).
#' @param y0 Y-coordinate of the center point (default: 0).
#' @param curve.arrow Arrow for the curve (default: NULL).
#' @param curve.height Height of the curve (default: 0.5).
#' @param start.arrow Include arrow at the starting point (default: FALSE).
#' @param end.arrow Include arrow at the ending point (default: FALSE).
#' @param bezierCurve.gp Graphics parameters for the curve (default: gpar(fill = "black")).
#' @param bezierPolygon.gp Graphics parameters for the polygon (default: gpar(fill = "black", col = "black")).
#' @param n Number of points for the curve (default: 100).
#' @param clock.wise Direction of the curve (default: FALSE).
#' @param name Name of the grob (default: NULL).
#' @param gp Graphics parameters for the grob (default: NULL).
#' @param vp Viewport for the grob (default: NULL).
#'
#' @return A grob representing the arc link.
#'
#' @export
arcLinks2Grob <- function(start = 0,end = 60,
                          r0 = 1,r1 = 1,r = 1,
                          arrow.len = 0.05,
                          x0 = 0,y0 = 0,
                          curve.arrow = NULL,
                          curve.height = 0.5,
                          start.arrow = FALSE,
                          end.arrow = FALSE,
                          bezierCurve.gp = gpar(fill = "black"),
                          bezierPolygon.gp = gpar(fill = "black",col = "black"),
                          n = 100,clock.wise = FALSE,
                          name = NULL,
                          gp = NULL, vp = NULL){

  lst <- list(start = start,end = end,
              r0 = r0,r1 = r1,r = r,
              arrow.len = arrow.len,
              x0 = x0,y0 = y0,
              curve.arrow = curve.arrow,
              curve.height = curve.height,
              start.arrow = start.arrow,
              end.arrow = end.arrow,
              bezierCurve.gp = bezierCurve.gp,
              bezierPolygon.gp = bezierPolygon.gp,
              n = n,clock.wise = clock.wise,
              name = name, gp = gp, vp = vp,
              cl = "arcLinks2Grob")

  do.call(gTree,lst)
}


#' @method makeContent arcLinks2Grob
#' @export
makeContent.arcLinks2Grob <- function(x){
  g <- .arcLinks2Grob(start = x$start,end = x$end,
                      r0 = x$r0,r1 = x$r1,r = x$r,
                      arrow.len = x$arrow.len,
                      x0 = x$x0,y0 = x$y0,
                      curve.arrow = x$curve.arrow,
                      curve.height = x$curve.height,
                      start.arrow = x$start.arrow,
                      end.arrow = x$end.arrow,
                      bezierCurve.gp = x$bezierCurve.gp,
                      bezierPolygon.gp = x$bezierPolygon.gp,
                      n = x$n,clock.wise = x$clock.wise,
                      name = x$name, gp = x$gp, vp = x$vp)
  grid::setChildren(x, children = g$children)
}



#' @noRd
.arcLinks2Grob <- function(start = 0,end = 60,
                           r0 = 1,r1 = 1,r = 1,
                           arrow.len = 0.05,
                           x0 = 0,y0 = 0,
                           curve.arrow = NULL,
                           curve.height = 0.5,
                           start.arrow = FALSE,
                           end.arrow = FALSE,
                           bezierCurve.gp = gpar(fill = "black"),
                           bezierPolygon.gp = gpar(fill = "black",col = "black"),
                           n = 100,clock.wise = FALSE,
                           name = NULL,
                           gp = NULL, vp = NULL){
  # check direction
  if(clock.wise == TRUE){
    start <- 360 - start
    end <- 360 - end
  }

  # check line or band plot
  if(length(start) == 1 && length(end) == 1){
    theta <- c(as.radian(start),as.radian(end))
    # r <- c(r0,r1)
    xf <- x0 + r0*cos(theta)
    yf <- y0 + r0*sin(theta)

    xf <- scales::rescale(xf,to = c(0,1),from = c(-1,1))
    yf <- scales::rescale(yf,to = c(0,1),from = c(-1,1))

    xt <- x0 + r1*cos(theta)
    yt <- y0 + r1*sin(theta)

    xt <- scales::rescale(xt,to = c(0,1),from = c(-1,1))
    yt <- scales::rescale(yt,to = c(0,1),from = c(-1,1))

    print(xf)
    print(yf)
    print(xt)
    print(yt)
    # curve height with control points
    xm <- x0 + (r - curve.height)*cos(as.radian((start + end)/2))
    ym <- y0 + (r - curve.height)*sin(as.radian((start + end)/2))

    xm <- scales::rescale(xm,to = c(0,1),from = c(-1,1))
    ym <- scales::rescale(ym,to = c(0,1),from = c(-1,1))

    res <- DescTools::DrawBezier(plot = F,
                                 x = c(xf[1],xm,xm,xt[2]),
                                 y = c(yf[1],ym,ym,yt[2]))

    bz.grob <- linesGrob(x = res$x,y = res$y,
                         default.units = "npc",
                         arrow = curve.arrow,
                         gp = bezierCurve.gp,
                         name = "bezierCurve")

  }else{
    if(length(start) != length(end)){
      stop("Please make sure the length of start and end are same.")
    }

    # curve.height = r
    # ==========================================================================
    # calculate coordinates
    # ==========================================================================
    if(start.arrow == TRUE | end.arrow == TRUE){
      bezier.r0 <- r0 - arrow.len*r
      bezier.r1 <- r1 - arrow.len*r
      bezier.r <- r - arrow.len*r
    }else{
      bezier.r0 <- r0
      bezier.r1 <- r1
      bezier.r <- r
    }
    # ==========================================================================
    # arc line1
    theta.arc <- seq(as.radian(start[1]),as.radian(start[2]),length = n)
    x.arc1 <- x0 + r0*cos(theta.arc)
    y.arc1 <- y0 + r0*sin(theta.arc)

    # ==========================================================================
    # get first bezier curves
    theta <- c(as.radian(start[2]),as.radian(end[1]))
    x.f <- x0 + bezier.r0*cos(theta)
    y.f <- y0 + bezier.r0*sin(theta)

    x.t <- x0 + bezier.r1*cos(theta)
    y.t <- y0 + bezier.r1*sin(theta)

    # curve height with control points
    xm <- x0 + (bezier.r - curve.height)*cos(as.radian((start[2] + end[1])/2))
    ym <- y0 + (bezier.r - curve.height)*sin(as.radian((start[2] + end[1])/2))

    # bzgrob <- bezierGrob(c(x[1],x0,x0,x[2]),c(y[1],y0,y0,y[2]),default.units = "native")
    # bezier <- bezierPoints(bzgrob)
    #
    # curve1.x <- convertUnit(bezier$x,unitTo = "native")
    # curve1.y <- convertUnit(bezier$y,unitTo = "native")

    bezier <- DescTools::DrawBezier(plot = F,
                                    x = c(x.f[1],xm,xm,x.t[2]),
                                    y = c(y.f[1],ym,ym,y.t[2]))

    curve1.x <- bezier$x
    curve1.y <- bezier$y

    # ==========================================================================
    # arc line2
    theta.arc <- seq(as.radian(end[1]),as.radian(end[2]),length = n)
    x.arc2 <- x0 + r1*cos(theta.arc)
    y.arc2 <- y0 + r1*sin(theta.arc)

    # ==========================================================================
    # get second bezier curves
    theta <- c(as.radian(end[2]),as.radian(start[1]))
    x.f <- x0 + bezier.r1*cos(theta)
    y.f <- y0 + bezier.r1*sin(theta)

    x.t <- x0 + bezier.r0*cos(theta)
    y.t <- y0 + bezier.r0*sin(theta)

    # curve height with control points
    xm <- x0 + (bezier.r - curve.height)*cos(as.radian((end[2] + start[1])/2))
    ym <- y0 + (bezier.r - curve.height)*sin(as.radian((end[2] + start[1])/2))

    # bzgrob <- bezierGrob(c(x[1],x0,x0,x[2]),c(y[1],y0,y0,y[2]),default.units = "native")
    # bezier <- bezierPoints(bzgrob)

    bezier <- DescTools::DrawBezier(plot = F,
                                    x = c(x.f[1],xm,xm,x.t[2]),
                                    y = c(y.f[1],ym,ym,y.t[2]))

    curve2.x <- bezier$x
    curve2.y <- bezier$y

    # ==========================================================================
    # add arrows
    # arrow.len = 0.05
    if(start.arrow == TRUE && end.arrow == FALSE){
      arc1.mid.x <- x0 + r0*cos(as.radian((start[1] + start[2])/2))
      arc1.mid.y <- y0 + r0*sin(as.radian((start[1] + start[2])/2))

      # newpage()
      # grid.points(x = arc1.mid.x,y = arc1.mid.y)

      link.x <- c(arc1.mid.x,curve1.x,x.arc2,curve2.x,arc1.mid.x)
      link.y <- c(arc1.mid.y,curve1.y,y.arc2,curve2.y,arc1.mid.y)

      # grid.points(x = curve2.x,y = curve2.y)
    }else if(end.arrow == TRUE && start.arrow == FALSE){
      arc2.mid.x <- x0 + r1*cos(as.radian((end[1] + end[2])/2))
      arc2.mid.y <- y0 + r1*sin(as.radian((end[1] + end[2])/2))

      link.x <- c(x.arc1,curve1.x,arc2.mid.x,curve2.x)
      link.y <- c(y.arc1,curve1.y,arc2.mid.y,curve2.y)
    }else if(end.arrow == TRUE && start.arrow == TRUE){
      arc1.mid.x <- x0 + r0*cos(as.radian((start[1] + start[2])/2))
      arc1.mid.y <- y0 + r0*sin(as.radian((start[1] + start[2])/2))
      arc2.mid.x <- x0 + r1*cos(as.radian((end[1] + end[2])/2))
      arc2.mid.y <- y0 + r1*sin(as.radian((end[1] + end[2])/2))

      link.x <- c(arc1.mid.x,curve1.x,arc2.mid.x,curve2.x,arc1.mid.x)
      link.y <- c(arc1.mid.y,curve1.y,arc2.mid.y,curve2.y,arc1.mid.y)
    }else{
      link.x <- c(x.arc1,curve1.x,x.arc2,curve2.x)
      link.y <- c(y.arc1,curve1.y,y.arc2,curve2.y)
    }

    link.x <- scales::rescale(link.x,to = c(0,1),from = c(-1,1))
    link.y <- scales::rescale(link.y,to = c(0,1),from = c(-1,1))

    # ==========================================================================
    # combine x,y
    bz.grob <- polygonGrob(x = link.x,
                           y = link.y,
                           gp = bezierPolygon.gp,
                           default.units = "npc",
                           name = "bezierCurve")
  }
  # ============================================================================
  # combine grobs
  # ============================================================================
  grid::gTree(children = grid::gList(bz.grob),
              name = "arcLinks2Grob")
}
