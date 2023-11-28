#' Create custom arc polygon graphical object.
#'
#' This function creates a custom arc polygon graphical object that can be added to
#' a plot in R. The polygon is defined by connecting points along an arc in a polar
#' coordinate system.
#'
#' @param x The x-coordinates of the polygon points. Default is NULL.
#' @param y The y-coordinates of the polygon points. Default is NULL.
#' @param start The starting angle of the arc (in degrees). Default is 0.
#' @param end The ending angle of the arc (in degrees). Default is 60.
#' @param r0 The inner radius of the arc. Default is 0.5.
#' @param r1 The outer radius of the arc. Default is 1.
#' @param x0 The x-coordinate of the center of the unit circle. Default is 0.
#' @param y0 The y-coordinate of the center of the unit circle. Default is 0.
#' @param n The number of points to generate along the arc. Default is 100.
#' @param id An optional vector specifying the ID or group for each point. Default is NULL.
#' @param polar.every A logical value indicating whether to calculate points for every
#'   section of the arc. If FALSE, points will be calculated for the entire arc. Default is TRUE.
#' @param polygon.gp A graphical parameter object for the polygon's appearance.
#'   Default is NULL.
#' @param clock.wise A logical value indicating whether the arc is drawn in a clockwise
#'   direction. Default is FALSE.
#' @param extend.xscale The extension factor for the scaling of the arc. Default is 0.05.
#' @param extend.yscale The extension factor for the scaling of the arc. Default is 0.05.
#' @param xscale The scaling range for the x-axis. Default is NULL.
#' @param yscale The scaling range for the y-axis. Default is NULL.
#' @param name The name of the arc polygon graphical object. Default is NULL.
#' @param gp A graphical parameter object for the arc polygon. Default is NULL.
#' @param vp A viewport object for the arc polygon. Default is NULL.
#' @param ... unused params.
#'
#' @return A graphical object of class 'grob'.
#'
#' @examples
#' \dontrun{
#' # Create arc polygon
#' arc_polygon <- arcPolygonGrob(x = c(0, 10, 20), y = c(0, 0, 0))
#' # Add the arc polygon to a plot
#' grid.draw(arc_polygon)
#' }
#'
#' @export
arcPolygonGrob <- function(x = NULL,y = NULL,
                           start = 0,end = 60,
                           r0 = 0.5,r1 = 1,
                           x0 = 0,y0 = 0,
                           polygon.gp = NULL,
                           n = 100,id = NULL,
                           polar.every = TRUE,
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
              polygon.gp = polygon.gp,
              n = n, id = id,
              polar.every = polar.every,
              clock.wise = clock.wise,
              extend.xscale = extend.xscale,
              extend.yscale = extend.yscale,
              xscale = xscale,
              yscale = yscale,
              name = name, gp = gp, vp = vp,
              cl = "arcPolygonGrob")

  do.call(gTree,lst)
}


#' @method makeContent arcPolygonGrob
#' @export
makeContent.arcPolygonGrob <- function(x){
  g <- .arcPolygonGrob(x = x$x,y = x$y,
                       start = x$start,end = x$end,
                       r0 = x$r0,r1 = x$r1,
                       x0 = x$x0,y0 = x$y0,
                       n = x$n, id = x$id,
                       polar.every = x$polar.every,
                       polygon.gp = x$polygon.gp,
                       clock.wise = x$clock.wise,
                       extend.xscale = x$extend.xscale,
                       extend.yscale = x$extend.yscale,
                       xscale = x$xscale,
                       yscale = x$yscale,
                       name = x$name, gp = x$gp, vp = x$vp)
  grid::setChildren(x, children = g$children)
}



#' @noRd
.arcPolygonGrob <- function(x = NULL,y = NULL,
                            start = 0,end = 60,
                            r0 = 0.5,r1 = 1,
                            x0 = 0,y0 = 0,
                            n = 100, id = NULL,
                            polar.every = TRUE,
                            polygon.gp = NULL,
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
  r.y <- scales::rescale(y,to = range(r0_ed,r1_ed),from = r.scale)

  if(clock.wise == TRUE){
    theta.x <- scales::rescale(x,to = 2*pi - range(as.radian(start_ed),as.radian(end_ed)),
                               from = thata.scale)
  }else{
    theta.x <- scales::rescale(x,to = range(as.radian(end_ed),as.radian(start_ed)),
                               from = rev(thata.scale))
  }

  if(is.null(id)){
    id <- rep(1,length(theta.x))
  }

  # close shape
  id.n <- table(id)
  id.sm <- c(1,cumsum(id.n))
  id.s <- id.sm[1:(length(id.sm) - 1)] + c(0,rep(1,length(id.n) - 1))
  id.p <- id.sm[2:length(id.sm)]

  # loop
  # idx = 1
  # polar.every = F
  if(polar.every == TRUE){
    # ==============================================================
    # calculate for each point to polar
    lapply(seq_along(id.n), function(idx){
      theta.tmp <- c(theta.x[id.s[idx]:id.p[idx]],theta.x[id.s[idx]])
      r.y.tmp <- c(r.y[id.s[idx]:id.p[idx]],r.y[id.s[idx]])

      # generate continues point
      # ii = 1
      lapply(1:(length(theta.tmp) - 1) , function(ii){
        theta <- seq(theta.tmp[ii],theta.tmp[ii + 1],length = n)
        r <- seq(r.y.tmp[ii],r.y.tmp[ii + 1],length = n)

        xp0 <- x0 + r*cos(theta)
        yp0 <- y0 + r*sin(theta)

        data.frame(x = xp0,y = yp0)
      }) %>% do.call("rbind",.) -> coord
    }) %>% do.call("rbind",.) -> coord
  }else{
    xp0 <- x0 + r.y*cos(theta.x)
    yp0 <- y0 + r.y*sin(theta.x)

    coord <- data.frame(x = xp0,y = yp0)
  }

  coord <- transform(coord,
                     x = scales::rescale(x,to = c(0,1),from = c(-1,1)),
                     y = scales::rescale(y,to = c(0,1),from = c(-1,1)))


  # grob
  if(is.null(id)){
    id.polygon <- rep(1,nrow(coord))
  }else{
    if(polar.every == TRUE){
      id.polygon <- rep(id,each = n)
    }else{
      id.polygon <- id
    }

  }

  # check id is numric or character
  if(all(is.character(id.polygon))){
    id.polygon <- as.numeric(factor(id.polygon))
  }

  # draw grobs
  polygon.grob <- polygonGrob(x = coord$x,y = coord$y,
                              gp = polygon.gp,
                              id = id.polygon,
                              name = "polygon",
                              default.units = "npc")

  # ============================================================================
  # combine grobs
  # ============================================================================
  grid::gTree(children = grid::gList(polygon.grob),
              name = "arcPolygonGrob")
}
