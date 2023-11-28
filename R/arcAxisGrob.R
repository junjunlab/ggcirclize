#' Create a custom arc axis graphical object.
#'
#' This function creates a custom arc axis graphical object that can be added to
#' a plot in R. The arc axis can be used to display data along a curved axis,
#' such as a circular or radial axis.
#'
#' @param start The starting angle of the arc axis (in degrees). Default is 0.
#' @param end The ending angle of the arc axis (in degrees). Default is 60.
#' @param r0 The inner radius of the arc axis. Default is 0.5.
#' @param r1 The outer radius of the arc axis. Default is 1.
#' @param x0 The x-coordinate of the center of the unit circle. Default is 0.
#' @param y0 The y-coordinate of the center of the unit circle. Default is 0.
#' @param ticks.len The length of the tick marks along the arc axis. Default is NULL.
#' @param n The number of ticks to be drawn along the arc axis. Default is 100.
#' @param clock.wise A logical value indicating whether the arc axis should be
#' drawn clockwise. Default is FALSE.
#' @param axis.type A character string specifying the type of the axis ('x' or 'y').
#' Default is 'x'.
#' @param nice.facing A logical value indicating whether tick labels should be
#' nicely aligned. Default is FALSE.
#' @param pos The position of the arc axis ('left', 'right', 'top', or 'bottom').
#' Default is 'left'.
#' @param xscale The scaling factor for the x-axis. Default is NULL.
#' @param yscale The scaling range for the y-axis. Default is c(0, 1).
#' @param breaks.n The number of major tick marks to be drawn. Default is 5.
#' @param minor.ticks.n The number of minor tick marks to be drawn between major
#' ticks. Default is 5.
#' @param breaks A vector specifying the positions of major tick marks. Default is NULL.
#' @param breaks.label A vector of labels for major tick marks. Default is NULL.
#' @param label.space The space between tick labels and the axis. Default is NULL.
#' @param x.gp A graphical parameter object for the x-axis. Default is NULL.
#' @param y.gp A graphical parameter object for the y-axis. Default is NULL.
#' @param x.label.gp A graphical parameter object for x-axis labels. Default is gpar(fontsize = 8).
#' @param y.label.gp A graphical parameter object for y-axis labels. Default is gpar(fontsize = 8).
#' @param extend.xscale The extension factor for the x-axis. Default is 0.05.
#' @param extend.yscale The extension factor for the y-axis. Default is 0.05.
#' @param name The name of the arc axis. Default is NULL.
#' @param gp A graphical parameter object for the arc axis. Default is NULL.
#' @param vp A viewport object for the arc axis. Default is NULL.
#'
#' @return A graphical object of class 'grob'.
#'
#' @import utils ggplot2
#' @importFrom scales rescale
#' @importFrom grid convertWidth gTree gpar grid.pretty linesGrob nullGrob pointsGrob
#' @importFrom grid polygonGrob polylineGrob segmentsGrob stringWidth textGrob unit
#'
#' @examples
#' \dontrun{
#' # Create an arc axis
#' arc_axis <- arcAxisGrob(start = 0, end = 180, r0 = 0.5, r1 = 1, axis.type = "x",pos = "top")
#' # Add the arc axis to a plot
#' grid.draw(arc_axis)
#' }
#'
#' @export
arcAxisGrob <- function(start = 0,end = 60,
                        r0 = 0.5,r1 = 1,
                        x0 = 0,y0 = 0,
                        ticks.len = NULL,
                        n = 100,clock.wise = FALSE,
                        axis.type = c("x","y"),
                        nice.facing = FALSE,
                        pos = c("left","right","top","bottom"),
                        xscale = NULL,yscale = c(0,1),
                        breaks.n = 5,
                        minor.ticks.n = 5,
                        breaks = NULL,
                        breaks.label = NULL,
                        label.space = NULL,
                        x.gp = NULL,y.gp = NULL,
                        x.label.gp = gpar(fontsize = 8),
                        y.label.gp = gpar(fontsize = 8),
                        extend.xscale = 0.05,
                        extend.yscale = 0.05,
                        name = NULL,
                        gp = NULL, vp = NULL){

  axis.type <- match.arg(axis.type,c("x","y"))
  pos <- match.arg(pos,c("left","right","top","bottom"))

  lst <- list(start = start,end = end,
              r0 = r0,r1 = r1,
              x0 = x0,y0 = y0,
              ticks.len = ticks.len,
              n = n,clock.wise = clock.wise,
              axis.type = axis.type,
              nice.facing = nice.facing,
              pos = pos,
              xscale = xscale,yscale = yscale,
              breaks.n = breaks.n,
              minor.ticks.n = minor.ticks.n,
              breaks = breaks,
              breaks.label = breaks.label,
              label.space = label.space,
              x.gp = x.gp,y.gp = y.gp,
              x.label.gp = x.label.gp,
              y.label.gp = y.label.gp,
              extend.xscale = extend.xscale,
              extend.yscale = extend.yscale,
              name = name, gp = gp, vp = vp,
              cl = "arcAxisGrob")

  do.call(gTree,lst)
}



#' @method makeContent arcAxisGrob
#' @export
makeContent.arcAxisGrob <- function(x){
  g <- .arcAxisGrob(start = x$start,end = x$end,
                    r0 = x$r0,r1 = x$r1,
                    x0 = x$x0,y0 = x$y0,
                    ticks.len = x$ticks.len,
                    n = x$n,clock.wise = x$clock.wise,
                    axis.type = x$axis.type,
                    nice.facing = x$nice.facing,
                    pos = x$pos,
                    xscale = x$xscale,yscale = x$yscale,
                    breaks.n = x$breaks.n,
                    minor.ticks.n = x$minor.ticks.n,
                    breaks = x$breaks,
                    breaks.label = x$breaks.label,
                    label.space = x$label.space,
                    x.gp = x$x.gp,y.gp = x$y.gp,
                    x.label.gp = x$x.label.gp,
                    y.label.gp = x$y.label.gp,
                    extend.xscale = x$extend.xscale,
                    extend.yscale = x$extend.yscale,
                    name = x$name, gp = x$gp, vp = x$vp)
  grid::setChildren(x, children = g$children)
}


#' @noRd
.arcAxisGrob <- function(start = 0,end = 60,
                         r0 = 0.5,r1 = 1,
                         x0 = 0,y0 = 0,
                         ticks.len = NULL,
                         n = 100,clock.wise = FALSE,
                         axis.type = c("x","y"),
                         nice.facing = FALSE,
                         pos = c("left","right","top","bottom"),
                         xscale = NULL,yscale = c(0,1),
                         breaks.n = 5,
                         minor.ticks.n = 5,
                         breaks = NULL,
                         breaks.label = NULL,
                         label.space = NULL,
                         x.gp = NULL,y.gp = NULL,
                         x.label.gp = gpar(fontsize = 8),
                         y.label.gp = gpar(fontsize = 8),
                         extend.xscale = 0.05,
                         extend.yscale = 0.05,
                         name = NULL,
                         gp = NULL, vp = NULL){
  # extend scale
  extend.theta <- (end - start)*extend.xscale
  start <- start + extend.theta
  end <- end - extend.theta

  extend.radias <- (r1 - r0)*extend.yscale
  r0 <- r0 + extend.radias
  r1 <- r1 - extend.radias

  # check axis type
  if(axis.type == "x"){
    ticks.len <- ifelse(is.null(ticks.len),0.03,ticks.len)
    label.space <- ifelse(is.null(label.space),0.03,label.space)
  }else{
    ticks.len <- ifelse(is.null(ticks.len),3,ticks.len)
    label.space <- ifelse(is.null(label.space),3,label.space)
  }

  # ============================================================================
  if(axis.type == "x"){
    if(is.null(xscale)){
      range.x <- range(start,end)
    }else{
      range.x <- xscale
    }

    if(is.null(breaks)){
      pretty.at <- grid.pretty(range = range.x,n = breaks.n)
    }else{
      pretty.at <- breaks
    }

    ticks.raw <- scales::rescale(pretty.at,
                                 to = c(end,start),
                                 from = rev(range.x))

    start <- start + end - max(ticks.raw)
    end <- start + (max(ticks.raw) - min(ticks.raw))
  }else{
    range.y <- yscale

    if(is.null(breaks)){
      pretty.at <- grid.pretty(range = range.y,n = breaks.n)
    }else{
      pretty.at <- breaks
    }

    ticks.raw <- scales::rescale(pretty.at,
                                 to = c(r0,r1),
                                 from = range.y)

    r0 <- min(ticks.raw)
    r1 <- max(ticks.raw)
  }

  # ============================================================================

  # check direction
  if(clock.wise == TRUE){
    theta <- 2*pi - seq(as.radian(start), as.radian(end), length = n)
  }else{
    theta <- rev(seq(as.radian(start), as.radian(end), length = n))
  }

  xp0 <- x0 + r0*cos(theta)
  yp0 <- y0 + r0*sin(theta)

  xp1 <- x0 + r1*cos(theta)
  yp1 <- y0 + r1*sin(theta)

  # ============================================================================
  # draw main line
  # ============================================================================
  if(axis.type == "x"){

    if(clock.wise == TRUE){
      # ticks <- 2*pi - seq(as.radian(start), as.radian(end),length = length(pretty.at))

      ticks.raw.st <- ticks.raw[1:(length(ticks.raw) - 1)]
      ticks.raw.sp <- ticks.raw[2:length(ticks.raw)]
      interval <- ticks.raw.sp - ticks.raw.st
      inter.tmp <- unique(c(start + cumsum(c(0, interval)), end))
      ticks <- 2*pi - as.radian(inter.tmp)
    }else{
      # ticks <- rev(seq((start/180) * pi, (end/180) * pi,length = length(pretty.at)))

      ticks.raw.st <- ticks.raw[1:(length(ticks.raw) - 1)]
      ticks.raw.sp <- ticks.raw[2:length(ticks.raw)]
      interval <- rev(ticks.raw.sp - ticks.raw.st)
      inter.tmp <- unique(c(start + cumsum(c(0, interval)), end))
      ticks <- as.radian(rev(inter.tmp))
    }

    # minor positions
    if(!is.null(breaks)){
      if(length(unique(round(interval,digits = 4))) == 1){
        ticks.minor <- rev(seq(as.radian(start),as.radian(end),
                               length = (length(pretty.at) - 1)*minor.ticks.n + 1))
      }else{
        ticks.minor <- 0
      }
    }else{
      ticks.minor <- rev(seq(as.radian(start),as.radian(end),
                             length = (length(pretty.at) - 1)*minor.ticks.n + 1))
    }


    # check direction
    if(clock.wise == TRUE){
      ticks.minor <- 2*pi - ticks.minor
    }

    minor.x0 <- x0 + r0*cos(ticks.minor)
    minor.y0 <- y0 + r0*sin(ticks.minor)

    minor.x1 <- x0 + r1*cos(ticks.minor)
    minor.y1 <- y0 + r1*sin(ticks.minor)

    xp2.0 <- x0 + r0*cos(ticks)
    yp2.0 <- y0 + r0*sin(ticks)

    xp2 <- x0 + r1*cos(ticks)
    yp2 <- y0 + r1*sin(ticks)

    if(pos == "top"){
      xmain <- xp1; ymain <- yp1

      xp3 <- x0 + (r1 + ticks.len)*cos(ticks)
      yp3 <- y0 + (r1 + ticks.len)*sin(ticks)

      minor.x <- x0 + (r1 + ticks.len/2)*cos(ticks.minor)
      minor.y <- x0 + (r1 + ticks.len/2)*sin(ticks.minor)

      # label.space = 0.05
      label.x <- x0 + (r1 + ticks.len + label.space)*cos(ticks)
      label.y <- y0 + (r1 + ticks.len + label.space)*sin(ticks)

      tck.x0 = xp2; tck.x1 = xp3
      tck.y0 = yp2; tck.y1 = yp3

      mior.x0 = minor.x1; mior.x1 = minor.x
      mior.y0 = minor.y1; mior.y1 = minor.y
    }else{
      xmain <- xp0; ymain <- yp0

      xp3 <- x0 + (r0 - ticks.len)*cos(ticks)
      yp3 <- y0 + (r0 - ticks.len)*sin(ticks)

      minor.x <- x0 + (r0 - ticks.len*0.5)*cos(ticks.minor)
      minor.y <- x0 + (r0 - ticks.len*0.5)*sin(ticks.minor)

      # label.space = 0.05
      label.x <- x0 + (r0 - ticks.len - label.space)*cos(ticks)
      label.y <- y0 + (r0 - ticks.len - label.space)*sin(ticks)

      tck.x0 = xp2.0; tck.x1 = xp3
      tck.y0 = yp2.0; tck.y1 = yp3

      mior.x0 = minor.x0; mior.x1 = minor.x
      mior.y0 = minor.y0; mior.y1 = minor.y
    }

    # ==========================================================================
    # draw grobs
    main.grob <- linesGrob(x = rescale(xmain,to = c(0,1),from = c(-1,1)),
                           y = rescale(ymain,to = c(0,1),from = c(-1,1)),
                           default.units = "npc",
                           gp = x.gp,
                           name = "arc.x.main")

    if(length(ticks.minor) != 1){
      minor.grob <- segmentsGrob(x0 = rescale(mior.x0,to = c(0,1),from = c(-1,1)),
                                 x1 = rescale(mior.x1,to = c(0,1),from = c(-1,1)),
                                 y0 = rescale(mior.y0,to = c(0,1),from = c(-1,1)),
                                 y1 = rescale(mior.y1,to = c(0,1),from = c(-1,1)),
                                 default.units = "npc",
                                 gp = x.gp,
                                 name = "arc.x.ticks.minor")
    }else{
      minor.grob <- nullGrob()
    }


    tick.grob <- segmentsGrob(x0 = rescale(tck.x0,to = c(0,1),from = c(-1,1)),
                              x1 = rescale(tck.x1,to = c(0,1),from = c(-1,1)),
                              y0 = rescale(tck.y0,to = c(0,1),from = c(-1,1)),
                              y1 = rescale(tck.y1,to = c(0,1),from = c(-1,1)),
                              default.units = "npc",
                              gp = x.gp,
                              name = "arc.x.ticks")
    # ========================================
    # draw ticks label for arc
    if(is.null(breaks.label)){
      # labels <- paste0(round(seq(start,end,length = breaks.n),digits = 2),"°")
      if(is.null(xscale)){
        labels <- paste0(pretty.at,"\u00b0")
      }else{
        labels <- pretty.at
      }
    }else{
      labels <- breaks.label
    }

    if(clock.wise == TRUE){
      # label.rot <- seq(start,end,length = length(pretty.at))
      label.rot <- inter.tmp

      lapply(seq_along(label.rot), function(x){
        rot <- label.rot[x]
        if(rot >= 0 & rot < 90){
          label.just <- 0
        }else if(rot >= 90 & rot < 180){
          label.just <- 1
        }else if(rot >= 180 & rot < 270){
          label.just <- 1
        }else if(rot >= 270 & rot <= 360){
          label.just <- 0
        }else{
          label.just <- 0
        }
      }) |> unlist() -> label.just

      if(pos == "bottom"){
        label.just <- abs(label.just - 1)
      }

      # ajust label rot
      if(nice.facing == TRUE){
        lapply(seq_along(label.rot), function(x){
          rot <- label.rot[x]
          if(rot >= 0 & rot <= 90){
            new.rot <- 90 - rot
          }else if(rot >= 90 & rot < 180){
            new.rot <- 90 - rot
          }else if(rot >= 180 & rot < 270){
            new.rot <- 270 - rot
          }else if(rot >= 270 & rot <= 360){
            new.rot <- 270 - rot
          }else{
            new.rot <- 270 - rot
          }
          return(new.rot)
        }) |> unlist() |> as.numeric() -> label.rot
        label.just <- 0.5
      }else{
        lapply(seq_along(label.rot), function(x){
          rot <- label.rot[x]
          if(rot >= 0 & rot < 90){
            new.rot <- -rot
          }else if(rot >= 90 & rot < 180){
            new.rot <- 180 - rot
          }else if(rot >= 180 & rot < 270){
            new.rot <- 180 - rot
          }else if(rot >= 270 & rot <= 360){
            new.rot <- 360 - rot
          }else{
            new.rot <- 360 - rot
          }
          return(new.rot)
        }) |> unlist() |> as.numeric() -> label.rot
      }

    }else{
      # label.rot <- rev(seq(start,end,length = length(pretty.at)))
      label.rot <- rev(inter.tmp)

      lapply(seq_along(label.rot), function(x){
        rot <- label.rot[x]
        if(rot >= 0 & rot <= 90){
          label.just <- 0
        }else if(rot >= 90 & rot < 180){
          label.just <- 1
        }else if(rot >= 180 & rot < 270){
          label.just <- 1
        }else if(rot >= 270 & rot <= 360){
          label.just <- 0
        }else{
          label.just <- 0
        }
      }) |> unlist() -> label.just

      if(pos == "bottom"){
        label.just <- abs(label.just - 1)
      }

      # ajust label rot
      # nice facing
      if(nice.facing == TRUE){
        lapply(seq_along(label.rot), function(x){
          rot <- label.rot[x]
          if(rot >= 0 & rot <= 90){
            new.rot <- rot - 90
          }else if(rot >= 90 & rot < 180){
            new.rot <- rot - 90
          }else if(rot >= 180 & rot < 270){
            new.rot <- rot - 270
          }else if(rot >= 270 & rot <= 360){
            new.rot <- rot - 270
          }else{
            new.rot <- rot - 270
          }
          return(new.rot)
        }) |> unlist() |> as.numeric() -> label.rot
        label.just <- 0.5
      }else{
        lapply(seq_along(label.rot), function(x){
          rot <- label.rot[x]
          if(rot >= 0 & rot <= 90){
            new.rot <- rot
          }else if(rot >= 90 & rot < 180){
            new.rot <- rot - 180
          }else if(rot >= 180 & rot < 270){
            new.rot <- rot - 180
          }else if(rot >= 270 & rot <= 360){
            new.rot <- rot - 360
          }else{
            new.rot <- rot - 360
          }
          return(new.rot)
        }) |> unlist() |> as.numeric() -> label.rot
      }

    }

    arc.label.grob <- textGrob(label = labels,
                               x = rescale(label.x,to = c(0,1),from = c(-1,1)),
                               y = rescale(label.y,to = c(0,1),from = c(-1,1)),
                               rot = label.rot,
                               hjust = label.just,
                               default.units = "npc",
                               gp = x.label.gp,
                               name = "arc.x.labels")

  }else{
    # ==========================================================================
    # yaxis
    # ==========================================================================

    # ticks
    # r.y <- seq(r0,r1,length = length(pretty.at))
    # r.y.minor <- seq(r0,r1,length = (length(pretty.at) - 1)*minor.ticks.n + 1)

    ticks.raw.st <- ticks.raw[1:(length(ticks.raw) - 1)]
    ticks.raw.sp <- ticks.raw[2:length(ticks.raw)]
    interval <- ticks.raw.sp - ticks.raw.st
    r.y <- unique(c(r0 + cumsum(c(0, interval)), r1))

    if(!is.null(breaks)){
      if(length(unique(round(interval,digits = 4))) == 1){
        r.y.minor <- seq(r0,r1,length = (length(pretty.at) - 1)*minor.ticks.n + 1)
      }else{
        r.y.minor <- 0
      }
      # r.y.minor <- seq(r0,r1,length = ((r1 - r0)/min(interval) + 0)*minor.ticks.n)
    }else{
      r.y.minor <- seq(r0,r1,length = (length(pretty.at) - 1)*minor.ticks.n + 1)
    }
    # ========================================================================
    if(pos == "left"){
      xmain0 <- xp0[1]; xmain1 <- xp1[1]
      ymain0 <- yp0[1]; ymain1 <- yp1[1]

      if(clock.wise == TRUE){
        theta.y <- 2*pi - seq(as.radian(start), as.radian(start - ticks.len), length = n)
      }else{
        theta.y <- seq(as.radian(end), as.radian(end + ticks.len), length = n)
      }

      if(clock.wise == TRUE){
        # theta.y.label <- 2*pi - as.radian(start - ticks.len - label.space)
        theta.y.label <- 2*pi - as.radian(seq(start - ticks.len - label.space,start - label.space,
                                              length = length(pretty.at)))
      }else{
        # theta.y.label <- as.radian(end + ticks.len + label.space)
        theta.y.label <- as.radian(seq(end + ticks.len + label.space,end + label.space,
                                       length = length(pretty.at)))
      }


      # ========================================================================
      # minors
      if(clock.wise == TRUE){
        theta.y.minor <- 2*pi - seq(as.radian(start), as.radian(start - ticks.len/2), length = n)
      }else{
        theta.y.minor <- seq(as.radian(end), as.radian((end + ticks.len/2)), length = n)
      }

    }else{
      xmain0 <- tail(xp0,1); xmain1 <- tail(xp1,1)
      ymain0 <- tail(yp0,1); ymain1 <- tail(yp1,1)

      # ========================================================================
      # ticks
      if(clock.wise == TRUE){
        theta.y <- 2*pi - seq(as.radian(end), as.radian((end + ticks.len)), length = n)
      }else{
        theta.y <- seq(as.radian(start), as.radian(start - ticks.len), length = n)
      }

      if(clock.wise == TRUE){
        # theta.y.label <- 2*pi - as.radian(end + ticks.len + label.space)
        theta.y.label <- 2*pi - as.radian(seq(end + ticks.len + label.space,end + label.space,
                                              length = length(pretty.at)))
      }else{
        # theta.y.label <- as.radian(start - ticks.len - label.space)
        theta.y.label <- as.radian(seq(start - ticks.len - label.space,start - label.space,
                                       length = length(pretty.at)))
      }

      # ========================================================================
      # minors
      if(clock.wise == TRUE){
        theta.y.minor <- 2*pi - seq(as.radian(end), as.radian(end + ticks.len/2), length = n)
      }else{
        theta.y.minor <- seq(as.radian(start), as.radian((start - ticks.len/2)), length = n)
      }
    }


    # ========================================================================
    # labels
    # yaxis label position
    xp.y.label <- x0 + r.y*cos(theta.y.label)
    yp.y.label <- y0 + r.y*sin(theta.y.label)

    if(is.null(breaks.label)){
      label <- pretty.at
    }else{
      label <- breaks.label
    }

    # label rot
    if(pos == "right"){
      if(clock.wise == TRUE){
        label.rot <- 90 - end + ticks.len + label.space
        label.just <- 1

      }else{
        label.rot <- start - ticks.len - label.space - 90
        label.just <- 0
      }

    }else{
      if(clock.wise == TRUE){
        label.rot <- 90 - start - ticks.len - label.space

        label.just <- 0
      }else{
        label.rot <- end + ticks.len + label.space - 90

        label.just <- 1
      }

    }

    arc.label.grob <- textGrob(label = label,
                               x = rescale(xp.y.label,to = c(0,1),from = c(-1,1)),
                               y = rescale(yp.y.label,to = c(0,1),from = c(-1,1)),
                               rot = label.rot,
                               hjust = label.just,
                               gp = y.label.gp,
                               default.units = "npc")

    # ========================================================================
    # main ticks
    # x = 1
    lapply(seq_along(r.y), function(x){
      xp.y <- x0 + r.y[x]*cos(theta.y)
      yp.y <- y0 + r.y[x]*sin(theta.y)

      coord.y <- data.frame(x = xp.y,y = yp.y,id = x)

      return(coord.y)
    }) %>% Reduce("rbind",.) -> coord.y.df


    tick.grob <- polylineGrob(x = rescale(coord.y.df$x,to = c(0,1),from = c(-1,1)),
                              y = rescale(coord.y.df$y,to = c(0,1),from = c(-1,1)),
                              id = coord.y.df$id,
                              gp = y.gp,
                              default.units = "npc")

    # ==========================================================================
    # minors
    # x = 1
    lapply(seq_along(r.y.minor), function(x){
      xp.y <- x0 + r.y.minor[x]*cos(theta.y.minor)
      yp.y <- y0 + r.y.minor[x]*sin(theta.y.minor)

      coord.y <- data.frame(x = xp.y,y = yp.y,id = x)

      return(coord.y)
    }) %>% Reduce("rbind",.) -> coord.y.minor.df

    if(length(r.y.minor) != 1){
      minor.grob <- polylineGrob(x = rescale(coord.y.minor.df$x,to = c(0,1),from = c(-1,1)),
                                 y = rescale(coord.y.minor.df$y,to = c(0,1),from = c(-1,1)),
                                 id = coord.y.minor.df$id,
                                 default.units = "npc")
    }else{
      minor.grob <- nullGrob()
    }



    main.grob <- segmentsGrob(x0 = rescale(xmain0,to = c(0,1),from = c(-1,1)),
                              x1 = rescale(xmain1,to = c(0,1),from = c(-1,1)),
                              y0 = rescale(ymain0,to = c(0,1),from = c(-1,1)),
                              y1 = rescale(ymain1,to = c(0,1),from = c(-1,1)),
                              gp = y.gp,
                              default.units = "npc",
                              name = "arc.y.main")

  }

  # ============================================================================
  # combine grobs
  # ============================================================================
  grid::gTree(children = grid::gList(main.grob,tick.grob,
                                     minor.grob,arc.label.grob),
              name = "arcAxisGrob")
}

