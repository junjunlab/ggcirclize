#' Create a custom arc rectangle graphical object.
#'
#' This function creates a custom arc rectangle graphical object that can be added
#' to a plot in R. The rectangles are defined by their positions along an arc in
#' a polar coordinate system.
#'
#' @param xmin The minimum x value of the rectangles along the arc. Default is NULL.
#' @param xmax The maximum x value of the rectangles along the arc. Default is NULL.
#' @param ymin The minimum y value of the rectangles along the arc. Default is NULL.
#' @param ymax The maximum y value of the rectangles along the arc. Default is NULL.
#' @param start The starting angle of the arc (in degrees). Default is 0.
#' @param end The ending angle of the arc (in degrees). Default is 360.
#' @param r0 The inner radius of the arc. Default is 0.5.
#' @param r1 The outer radius of the arc. Default is 1.
#' @param x0 The x-coordinate of the center of the unit circle. Default is 0.
#' @param y0 The y-coordinate of the center of the unit circle. Default is 0.
#' @param polygon.gp A graphical parameter object for the rectangles' appearance.
#'   Default is NULL.
#' @param n The number of points used to approximate the rectangles. Default is 100.
#' @param clock.wise A logical value indicating whether the arc is drawn in a
#'   clockwise direction. Default is FALSE.
#' @param extend.xscale The extension factor for the scaling of the arc. Default is 0.05.
#' @param extend.yscale The extension factor for the scaling of the arc. Default is 0.05.
#' @param xscale The scaling range for the x-axis. Default is NULL.
#' @param yscale The scaling range for the y-axis. Default is NULL.
#' @param ... Additional arguments to be passed to the `polygonGrob` function.
#' @param name The name of the arc rectangle graphical object. Default is NULL.
#' @param gp A graphical parameter object for the arc rectangle. Default is NULL.
#' @param vp A viewport object for the arc rectangle. Default is NULL.
#'
#' @return A graphical object of class 'grob'.
#'
#' @examples
#' \dontrun{
#' # Create arc rectangles
#' arc_rectangles <- arcRectGrob(xmin = 0, xmax = 45, ymin = 0.5, ymax = 0.7,
#'                               start = 0, end = 180, r0 = 0.5, r1 = 1)
#' # Add the arc rectangles to a plot
#' grid.draw(arc_rectangles)
#' }
#'
#' @export
arcRectGrob <- function(xmin = NULL,xmax = NULL,
                        ymin = NULL,ymax = NULL,
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
              ymin = ymin,ymax = ymax,
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
              cl = "arcRectGrob")

  do.call(gTree,lst)
}


#' @method makeContent arcRectGrob
#' @export
makeContent.arcRectGrob <- function(x){
  g <- .arcRectGrob(xmin = x$xmin,xmax = x$xmax,
                    ymin = x$ymin,ymax = x$ymax,
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
.arcRectGrob <- function(xmin = NULL,xmax = NULL,
                         ymin = NULL,ymax = NULL,
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
  if(length(ymin) == 1 & length(ymax) == 1){
    ymin <- rep(ymin,length(xmin))
    ymax <- rep(ymax,length(xmax))
  }

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

  # loop create coordinates
  lapply(seq_along(rect.r0), function(x){
    theta <- seq(new.start[x], new.end[x], length = n)

    # ==============================================
    # inner
    # ==============================================
    rin <- rect.r0[x]
    if(rin == 0){
      xp0 = x0;yp0 = y0
    }else{
      xp0 <- x0 + rin*cos(theta)
      yp0 <- y0 + rin*sin(theta)
    }

    # ==============================================
    # outer
    # ==============================================
    rout <- rect.r1[x]
    xp1 <- x0 + rout*cos(theta)
    yp1 <- y0 + rout*sin(theta)

    rect.df <- data.frame(x = c(xp0,rev(xp1)),y = c(yp0,rev(yp1)),
                          id = x,
                          group = rep(c("inner","outer"),c(length(xp0),length(xp1))))

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
              name = "arcRectGrob")
}
