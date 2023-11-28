#' Create a custom arc point graphical object.
#'
#' This function creates a custom arc point graphical object that can be added to
#' a plot in R. The point's position is determined by its radius and angle in a
#' polar coordinate system.
#'
#' @param x The numeric values at which the point should be placed along the arc.
#' Default is NULL.
#' @param y The numeric values at which the point should be placed along the arc.
#' Default is NULL.
#' @param start The starting angle of the arc (in degrees). Default is 0.
#' @param end The ending angle of the arc (in degrees). Default is 60.
#' @param r0 The inner radius of the arc. Default is 0.5.
#' @param r1 The outer radius of the arc. Default is 1.
#' @param x0 The x-coordinate of the center of the unit circle. Default is 0.
#' @param y0 The y-coordinate of the center of the unit circle. Default is 0.
#' @param point.gp A graphical parameter object for the point's appearance. Default is NULL.
#' @param size The size of the point. Default is 5.
#' @param pch The plotting character used for the point. Default is 19.
#' @param clock.wise A logical value indicating whether the arc is drawn in a
#'   clockwise direction. Default is FALSE.
#' @param extend.xscale The extension factor for the scaling of the arc. Default is 0.05.
#' @param extend.yscale The extension factor for the scaling of the arc. Default is 0.05.
#' @param xscale The scaling range for the x-axis. Default is NULL.
#' @param yscale The scaling range for the y-axis. Default is NULL.
#' @param name The name of the arc point graphical object. Default is NULL.
#' @param gp A graphical parameter object for the arc point. Default is NULL.
#' @param vp A viewport object for the arc point. Default is NULL.
#' @param ... Additional arguments to be passed to the `pointsGrob` function.
#'
#' @return A graphical object of class 'grob'.
#'
#' @examples
#' \dontrun{
#' # Create an arc point
#' arc_point <- arcPointGrob(x = 30, y = 0.7, start = 0, end = 180, r0 = 0.5, r1 = 1)
#' # Add the arc point to a plot
#' grid.draw(arc_point)
#' }
#'
#' @export
arcPointGrob <- function(x = NULL,y = NULL,
                         start = 0,end = 60,
                         r0 = 0.5,r1 = 1,
                         x0 = 0,y0 = 0,
                         point.gp = NULL,
                         size = 5,pch = 19,
                         clock.wise = FALSE,
                         extend.xscale = 0.05,
                         extend.yscale = 0.05,
                         xscale = NULL,
                         yscale = NULL,
                         ...,
                         name = NULL,
                         gp = NULL, vp = NULL){

  lst <- list(x = x,y = y,
              start = start,end = end,
              r0 = r0,r1 = r1,
              x0 = x0,y0 = y0,
              point.gp = point.gp,
              size = size,pch = pch,
              clock.wise = clock.wise,
              extend.xscale = extend.xscale,
              extend.yscale = extend.yscale,
              xscale = xscale,
              yscale = yscale,
              name = name, gp = gp, vp = vp,
              cl = "arcPointGrob")

  do.call(gTree,lst)
}


#' @method makeContent arcPointGrob
#' @export
makeContent.arcPointGrob <- function(x){
  g <- .arcPointGrob(x = x$x,y = x$y,
                     start = x$start,end = x$end,
                     r0 = x$r0,r1 = x$r1,
                     x0 = x$x0,y0 = x$y0,
                     point.gp = x$point.gp,
                     size = x$size,pch = x$pch,
                     clock.wise = x$clock.wise,
                     extend.xscale = x$extend.xscale,
                     extend.yscale = x$extend.yscale,
                     xscale = x$xscale,
                     yscale = x$yscale,
                     name = x$name, gp = x$gp, vp = x$vp)
  grid::setChildren(x, children = g$children)
}


#' @noRd
.arcPointGrob <- function(x = NULL,y = NULL,
                          start = 0,end = 60,
                          r0 = 0.5,r1 = 1,
                          x0 = 0,y0 = 0,
                          point.gp = NULL,
                          size = 5,pch = 19,
                          clock.wise = FALSE,
                          extend.xscale = 0.05,
                          extend.yscale = 0.05,
                          xscale = NULL,
                          yscale = NULL,
                          name = NULL,
                          gp = NULL, vp = NULL,...){
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
    r.scale <- range(y)
  }else{
    r.scale <- yscale
  }

  if(is.null(xscale)){
    thata.scale <- range(x)
  }else{
    thata.scale <- xscale
  }
  # =============================

  r <- scales::rescale(y,to = range(r0_ed,r1_ed),from = r.scale)


  if(clock.wise == TRUE){
    theta <- scales::rescale(x,to = 2*pi - range(as.radian(start_ed),as.radian(end_ed)),
                             from = thata.scale)
  }else{
    theta <- scales::rescale(x,to = range(as.radian(end_ed),as.radian(start_ed)),
                             from = rev(thata.scale))
  }

  xp0 <- x0 + r*cos(theta)
  yp0 <- y0 + r*sin(theta)

  xp0 <- scales::rescale(xp0,to = c(0,1),from = c(-1,1))
  yp0 <- scales::rescale(yp0,to = c(0,1),from = c(-1,1))

  point.grob <- pointsGrob(x = xp0,y = yp0,
                           pch = pch,size = unit(size, "pt"),
                           gp = point.gp,
                           default.units = "npc",
                           name = "points")


  # ============================================================================
  # combine grobs
  # ============================================================================
  grid::gTree(children = grid::gList(point.grob),
              name = "arcPointGrob")
}
