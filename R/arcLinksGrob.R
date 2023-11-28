#' Create custom arc links graphical object.
#'
#' This function creates a custom arc links graphical object that can be added to
#' a plot in R. The links consist of Bezier curves and arcs connecting points in a polar
#' coordinate system.
#'
#' @param start The starting angle of the arc (in degrees). Default is 0.
#' @param end The ending angle of the arc (in degrees). Default is 60.
#' @param r The radius of the arcs. Default is 1.
#' @param arrow.len The length of arrowheads on the links. Default is 0.05.
#' @param x0 The x-coordinate of the center of the unit circle. Default is 0.
#' @param y0 The y-coordinate of the center of the unit circle. Default is 0.
#' @param curve.arrow A graphical parameter object for the arrowheads on the curves.
#' Default is NULL.
#' @param curve.height The height of the control points for the Bezier curves.
#' Default is 0.5.
#' @param start.arrow A logical value indicating whether to add an arrowhead at
#' the starting point. Default is FALSE.
#' @param end.arrow A logical value indicating whether to add an arrowhead at
#' the ending point. Default is FALSE.
#' @param bezierCurve.gp A graphical parameter object for the Bezier curve appearance.
#' Default is gpar(fill = "black").
#' @param bezierPolygon.gp A graphical parameter object for the Bezier curve
#' polygon appearance. Default is gpar(fill = "black", col = "black").
#' @param n The number of points to generate along the arcs. Default is 100.
#' @param clock.wise A logical value indicating whether the arcs are drawn in a
#' clockwise direction. Default is FALSE.
#' @param name The name of the arc links graphical object. Default is NULL.
#' @param gp A graphical parameter object for the arc links. Default is NULL.
#' @param vp A viewport object for the arc links. Default is NULL.
#'
#' @return A graphical object of class 'grob'.
#'
#' @examples
#' \dontrun{
#' # Create arc links
#' arc_links <- arcLinksGrob(start = 0, end = 120, r = 1, arrow.len = 0.05)
#' # Add the arc links to a plot
#' grid.draw(arc_links)
#' }
#'
#' @export
arcLinksGrob <- function(start = 0,end = 60,
                         r = 1,arrow.len = 0.05,
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
              r = r,arrow.len = arrow.len,
              x0 = x0,y0 = y0,
              curve.arrow = curve.arrow,
              curve.height = curve.height,
              start.arrow = start.arrow,
              end.arrow = end.arrow,
              bezierCurve.gp = bezierCurve.gp,
              bezierPolygon.gp = bezierPolygon.gp,
              n = n,clock.wise = clock.wise,
              name = name, gp = gp, vp = vp,
              cl = "arcLinksGrob")

  do.call(gTree,lst)
}


#' @method makeContent arcLinksGrob
#' @export
makeContent.arcLinksGrob <- function(x){
  g <- .arcLinksGrob(start = x$start,end = x$end,
                     r = x$r,arrow.len = x$arrow.len,
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
.arcLinksGrob <- function(start = 0,end = 60,
                          r = 1,arrow.len = 0.05,
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
    x <- x0 + r*cos(theta)
    y <- y0 + r*sin(theta)

    x <- scales::rescale(x,to = c(0,1),from = c(-1,1))
    y <- scales::rescale(y,to = c(0,1),from = c(-1,1))

    # curve height with control points
    xm <- x0 + abs(r - curve.height)*cos(as.radian((start + end)/2))
    ym <- y0 + abs(r - curve.height)*sin(as.radian((start + end)/2))

    xm <- scales::rescale(xm,to = c(0,1),from = c(-1,1))
    ym <- scales::rescale(ym,to = c(0,1),from = c(-1,1))

    res <- DescTools::DrawBezier(plot = F,
                                 x = c(x[1],xm,xm,x[2]),
                                 y = c(y[1],ym,ym,y[2]))

    bz.grob <- linesGrob(x = res$x,y = res$y,
                         default.units = "npc",
                         arrow = curve.arrow,
                         gp = bezierCurve.gp,
                         name = "bezierCurve")

  }else{
    if(length(start) != length(end)){
      stop("Please make sure the length of start and end are same.")
    }

    # ==========================================================================
    # calculate coordinates
    # ==========================================================================
    if(start.arrow == TRUE || end.arrow == TRUE){
      bezier.r <- r - arrow.len*r
    }else{
      bezier.r <- r
    }
    # ==========================================================================
    # arc line1
    theta.arc <- seq(as.radian(start[1]),as.radian(start[2]),length = n)
    x.arc1 <- x0 + r*cos(theta.arc)
    y.arc1 <- y0 + r*sin(theta.arc)

    # ==========================================================================
    # get first bezier curves
    theta <- c(as.radian(start[2]),as.radian(end[1]))
    x <- x0 + bezier.r*cos(theta)
    y <- y0 + bezier.r*sin(theta)

    # curve height with control points
    xm <- x0 + abs(bezier.r - curve.height)*cos(as.radian((start[2] + end[1])/2))
    ym <- y0 + abs(bezier.r - curve.height)*sin(as.radian((start[2] + end[1])/2))

    # bzgrob <- bezierGrob(c(x[1],x0,x0,x[2]),c(y[1],y0,y0,y[2]),default.units = "native")
    # bezier <- bezierPoints(bzgrob)
    #
    # curve1.x <- convertUnit(bezier$x,unitTo = "native")
    # curve1.y <- convertUnit(bezier$y,unitTo = "native")

    bezier <- DescTools::DrawBezier(plot = F,
                                    x = c(x[1],xm,xm,x[2]),
                                    y = c(y[1],ym,ym,y[2]))

    curve1.x <- bezier$x
    curve1.y <- bezier$y

    # ==========================================================================
    # arc line2
    theta.arc <- seq(as.radian(end[1]),as.radian(end[2]),length = n)
    x.arc2 <- x0 + r*cos(theta.arc)
    y.arc2 <- y0 + r*sin(theta.arc)

    # ==========================================================================
    # get second bezier curves
    theta <- c(as.radian(end[2]),as.radian(start[1]))
    x <- x0 + bezier.r*cos(theta)
    y <- y0 + bezier.r*sin(theta)

    # curve height with control points
    xm <- x0 + abs(bezier.r - curve.height)*cos(as.radian((end[2] + start[1])/2))
    ym <- y0 + abs(bezier.r - curve.height)*sin(as.radian((end[2] + start[1])/2))

    # bzgrob <- bezierGrob(c(x[1],x0,x0,x[2]),c(y[1],y0,y0,y[2]),default.units = "native")
    # bezier <- bezierPoints(bzgrob)

    bezier <- DescTools::DrawBezier(plot = F,
                                    x = c(x[1],xm,xm,x[2]),
                                    y = c(y[1],ym,ym,y[2]))

    curve2.x <- bezier$x
    curve2.y <- bezier$y

    # ==========================================================================
    # add arrows
    # arrow.len = 0.05
    if(start.arrow == TRUE && end.arrow == FALSE){
      arc1.mid.x <- x0 + r*cos(as.radian((start[1] + start[2])/2))
      arc1.mid.y <- y0 + r*sin(as.radian((start[1] + start[2])/2))

      link.x <- c(arc1.mid.x,curve1.x,x.arc2,curve2.x,arc1.mid.x)
      link.y <- c(arc1.mid.y,curve1.y,y.arc2,curve2.y,arc1.mid.y)
    }else if(end.arrow == TRUE && start.arrow == FALSE){
      arc2.mid.x <- x0 + r*cos(as.radian((end[1] + end[2])/2))
      arc2.mid.y <- y0 + r*sin(as.radian((end[1] + end[2])/2))

      link.x <- c(x.arc1,curve1.x,arc2.mid.x,curve2.x)
      link.y <- c(y.arc1,curve1.y,arc2.mid.y,curve2.y)
    }else if(end.arrow == TRUE && start.arrow == TRUE){
      arc1.mid.x <- x0 + r*cos(as.radian((start[1] + start[2])/2))
      arc1.mid.y <- y0 + r*sin(as.radian((start[1] + start[2])/2))
      arc2.mid.x <- x0 + r*cos(as.radian((end[1] + end[2])/2))
      arc2.mid.y <- y0 + r*sin(as.radian((end[1] + end[2])/2))

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
              name = "arcLinksGrob")
}
