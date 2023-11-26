#' Create a custom arc sector graphical object.
#'
#' This function creates a custom arc sector graphical object that can be added to
#' a plot in R. The arc sector is defined by its starting and ending angles, inner
#' and outer radii, and center coordinates.
#'
#' @param start The starting angle of the arc sector (in degrees). Default is 0.
#' @param end The ending angle of the arc sector (in degrees). Default is 360.
#' @param r0 The inner radius of the arc sector. Default is 0.
#' @param r1 The outer radius of the arc sector. Default is 1.
#' @param x0 The x-coordinate of the center of the unit circle. Default is 0.
#' @param y0 The y-coordinate of the center of the unit circle. Default is 0.
#' @param sector.gp A graphical parameter object for the arc sector's appearance.
#'   Default is gpar(fill = "white").
#' @param n The number of points used to approximate the arc sector. Default is 100.
#' @param clock.wise A logical value indicating whether the arc sector should be drawn
#'   in a clockwise direction. Default is FALSE.
#' @param add.xaxis A logical value indicating whether to add an x-axis to the plot.
#'   Default is TRUE.
#' @param add.yaxis A logical value indicating whether to add a y-axis to the plot.
#'   Default is TRUE.
#' @param arcxAxisGrob.params A list of additional parameters to be passed to the
#'   arcxAxisGrob function when adding the x-axis. Default is an empty list.
#' @param arcyAxisGrob.params A list of additional parameters to be passed to the
#'   arcyAxisGrob function when adding the y-axis. Default is an empty list.
#' @param extend.xscale The extension factor for the scaling of the arc sector.
#'   Default is 0.05.
#' @param extend.yscale The extension factor for the scaling of the arc sector.
#'   Default is 0.05.
#' @param xaxis.extend.xscale The extension factor for the scaling of the x-axis.
#'   Default is NULL (uses extend.xscale).
#' @param xaxis.extend.yscale The extension factor for the scaling of the x-axis.
#'   Default is NULL (uses extend.yscale).
#' @param yaxis.extend.xscale The extension factor for the scaling of the y-axis.
#'   Default is NULL (uses extend.xscale).
#' @param yaxis.extend.yscale The extension factor for the scaling of the y-axis.
#'   Default is NULL (uses extend.yscale).
#' @param name The name of the arc sector graphical object. Default is NULL.
#' @param gp A graphical parameter object for the arc sector. Default is NULL.
#' @param vp A viewport object for the arc sector. Default is NULL.
#'
#' @return A graphical object of class 'grob'.
#'
#' @examples
#' \dontrun{
#' # Create an arc sector
#' arc_sector <- arcSectorGrob(start = 0, end = 180, r0 = 0.5, r1 = 1)
#' # Add the arc sector to a plot
#' grid.draw(arc_sector)
#' }
#'
#' @export
arcSectorGrob <- function(start = 0,end = 360,
                          r0 = 0,r1 = 1,
                          x0 = 0,y0 = 0,
                          sector.gp = gpar(fill = "white"),
                          n = 100,clock.wise = FALSE,
                          add.xaxis = TRUE,add.yaxis = TRUE,
                          arcxAxisGrob.params = list(),
                          arcyAxisGrob.params = list(),
                          extend.xscale = 0.05,
                          extend.yscale = 0.05,
                          xaxis.extend.xscale = NULL,
                          xaxis.extend.yscale = NULL,
                          yaxis.extend.xscale = NULL,
                          yaxis.extend.yscale = NULL,
                          name = NULL,
                          gp = NULL, vp = NULL){

  lst <- list(start = start,end = end,
              r0 = r0,r1 = r1,
              x0 = x0,y0 = y0,
              sector.gp = sector.gp,n = n,clock.wise = clock.wise,
              add.xaxis = add.xaxis,add.yaxis = add.yaxis,
              arcxAxisGrob.params = arcxAxisGrob.params,
              arcyAxisGrob.params = arcyAxisGrob.params,
              extend.xscale = extend.xscale,
              extend.yscale = extend.yscale,
              xaxis.extend.xscale = xaxis.extend.xscale,
              xaxis.extend.yscale = xaxis.extend.yscale,
              yaxis.extend.xscale = yaxis.extend.xscale,
              yaxis.extend.yscale = yaxis.extend.yscale,
              name = name, gp = gp, vp = vp,
              cl = "arcSectorGrob")

  do.call(gTree,lst)
}


#' @noRd
#' @export
makeContent.arcSectorGrob <- function(x){
  g <- .arcSectorGrob(start = x$start,end = x$end,
                      r0 = x$r0,r1 = x$r1,
                      x0 = x$x0,y0 = x$y0,
                      sector.gp = x$sector.gp,n = x$n,clock.wise = x$clock.wise,
                      add.xaxis = x$add.xaxis,add.yaxis = x$add.yaxis,
                      arcxAxisGrob.params = x$arcxAxisGrob.params,
                      arcyAxisGrob.params = x$arcyAxisGrob.params,
                      extend.xscale = x$extend.xscale,
                      extend.yscale = x$extend.yscale,
                      xaxis.extend.xscale = x$xaxis.extend.xscale,
                      xaxis.extend.yscale = x$xaxis.extend.yscale,
                      yaxis.extend.xscale = x$yaxis.extend.xscale,
                      yaxis.extend.yscale = x$yaxis.extend.yscale,
                      name = x$name, gp = x$gp, vp = x$vp)
  grid::setChildren(x, children = g$children)
}


#' @noRd
.arcSectorGrob <- function(start = 0,end = 360,
                           r0 = 0,r1 = 1,
                           x0 = 0,y0 = 0,
                           sector.gp = gpar(fill = "white"),
                           n = 100,clock.wise = FALSE,
                           add.xaxis = TRUE,add.yaxis = TRUE,
                           arcxAxisGrob.params = list(),
                           arcyAxisGrob.params = list(),
                           extend.xscale = 0.05,
                           extend.yscale = 0.05,
                           xaxis.extend.xscale = NULL,
                           xaxis.extend.yscale = NULL,
                           yaxis.extend.xscale = NULL,
                           yaxis.extend.yscale = NULL,
                           name = NULL,
                           gp = NULL, vp = NULL){
  # extend scale
  extend.theta <- (end - start)*extend.xscale
  start_ed <- start + extend.theta
  end_ed <- end - extend.theta

  extend.radias <- (r1 - r0)*extend.yscale
  r0_ed <- r0 + extend.radias
  r1_ed <- r1 - extend.radias

  # check direction
  if(clock.wise == TRUE){
    theta <- 2*pi - seq((start_ed/180) * pi, (end_ed/180) * pi, length = n)
  }else{
    theta <- seq((start_ed/180) * pi, (end_ed/180) * pi, length = n)
  }
  # ==============================================
  # inner
  # ==============================================
  if(r0 == 0){
    xp0 = x0;yp0 = y0
  }else{
    xp0 <- x0 + r0_ed*cos(theta)
    yp0 <- y0 + r0_ed*sin(theta)
  }

  xp0 <- scales::rescale(xp0,to = c(0,1),from = c(-1,1))
  yp0 <- scales::rescale(yp0,to = c(0,1),from = c(-1,1))
  # ==============================================
  # outer
  # ==============================================
  xp1 <- x0 + r1_ed*cos(theta)
  yp1 <- y0 + r1_ed*sin(theta)

  xp1 <- scales::rescale(xp1,to = c(0,1),from = c(-1,1))
  yp1 <- scales::rescale(yp1,to = c(0,1),from = c(-1,1))

  df <- data.frame(x = c(xp0,rev(xp1)),y = c(yp0,rev(yp1)),
                   id = "sector",
                   group = rep(c("inner","outer"),c(length(xp0),length(xp1))))

  polygon.grob <- polygonGrob(x = df$x,y = df$y,
                              # id = df$id,
                              gp = sector.gp,
                              default.units = "npc",
                              name = "polygon")

  # check extend scale
  xaxis.extend.xscale <- xaxis.extend.xscale %||% extend.xscale
  xaxis.extend.yscale <- xaxis.extend.yscale %||% extend.yscale
  yaxis.extend.xscale <- yaxis.extend.xscale %||% extend.xscale
  yaxis.extend.yscale <- yaxis.extend.yscale %||% extend.yscale


  if(add.xaxis == TRUE){
    xaxis.grob <- do.call(arcAxisGrob,modifyList(list(start = start,end = end,
                                                      r0 = r0,r1 = r1,
                                                      extend.xscale = xaxis.extend.xscale,
                                                      extend.yscale = xaxis.extend.yscale,
                                                      clock.wise = clock.wise,
                                                      axis.type = "x",pos = "top"),
                                                 arcxAxisGrob.params))
  }else{
    xaxis.grob <- nullGrob()
  }

  if(add.yaxis == TRUE){
    yaxis.grob <- do.call(arcAxisGrob,modifyList(list(start = start,end = end,
                                                      r0 = r0,r1 = r1,
                                                      extend.xscale = yaxis.extend.xscale,
                                                      extend.yscale = yaxis.extend.yscale,
                                                      clock.wise = clock.wise,
                                                      axis.type = "y",pos = "left"),
                                                 arcyAxisGrob.params))
  }else{
    yaxis.grob <- nullGrob()
  }

  # ============================================================================
  # combine grobs
  # ============================================================================
  grid::gTree(children = grid::gList(polygon.grob,xaxis.grob,yaxis.grob),
              name = "arcSectorGrob")
}
