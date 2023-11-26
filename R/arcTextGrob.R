#' Create custom arc text labels graphical object.
#'
#' This function creates a custom arc text labels graphical object that can be added
#' to a plot in R. The text labels are positioned along an arc in a polar coordinate system.
#'
#' @param x The x-coordinates of the text labels. Default is NULL.
#' @param y The y-coordinates of the text labels. Default is NULL.
#' @param labels The text labels to be displayed. Default is NULL.
#' @param shift The factor by which the labels are shifted along the arc. Default is 0.5.
#' @param start The starting angle of the arc (in degrees). Default is 0.
#' @param end The ending angle of the arc (in degrees). Default is 60.
#' @param r0 The inner radius of the arc. Default is 0.5.
#' @param r1 The outer radius of the arc. Default is 1.
#' @param x0 The x-coordinate of the center of the unit circle. Default is 0.
#' @param y0 The y-coordinate of the center of the unit circle. Default is 0.
#' @param text.gp A graphical parameter object for the text labels' appearance.
#'   Default is NULL.
#' @param nice.facing A logical value indicating whether the labels should face in
#'   a "nice" direction. Default is TRUE.
#' @param inward A logical value indicating whether the labels should face inward
#'   or outward from the center of the arc. Default is FALSE.
#' @param clock.wise A logical value indicating whether the arc is drawn in a
#'   clockwise direction. Default is FALSE.
#' @param curved.label A logical value indicating whether the labels should be curved
#'   along the arc. Default is TRUE.
#' @param extend.xscale The extension factor for the scaling of the arc. Default is 0.05.
#' @param extend.yscale The extension factor for the scaling of the arc. Default is 0.05.
#' @param xscale The scaling range for the x-axis. Default is NULL.
#' @param yscale The scaling range for the y-axis. Default is NULL.
#' @param name The name of the arc text labels graphical object. Default is NULL.
#' @param gp A graphical parameter object for the arc text labels. Default is NULL.
#' @param vp A viewport object for the arc text labels. Default is NULL.
#'
#' @return A graphical object of class 'grob'.
#'
#' @examples
#' \dontrun{
#' # Create arc text labels
#' arc_text_labels <- arcTextGrob(x = c(0, 10, 20), y = c(0, 0, 0), labels = c("A", "B", "C"))
#' # Add the arc text labels to a plot
#' grid.draw(arc_text_labels)
#' }
#'
#' @export
arcTextGrob <- function(x = NULL,y = NULL,
                        labels = NULL,shift = 0.5,
                        start = 0,end = 60,
                        r0 = 0.5,r1 = 1,
                        x0 = 0,y0 = 0,
                        text.gp = NULL,
                        nice.facing = TRUE,
                        inward = FALSE,
                        clock.wise = FALSE,
                        curved.label = TRUE,
                        extend.xscale = 0.05,
                        extend.yscale = 0.05,
                        xscale = NULL,
                        yscale = NULL,
                        name = NULL,
                        gp = NULL, vp = NULL){

  lst <- list(x = x,y = y,
              labels = labels,shift = shift,
              start = start,end = end,
              r0 = r0,r1 = r1,
              x0 = x0,y0 = y0,
              text.gp = text.gp,
              nice.facing = nice.facing,
              inward = inward,
              clock.wise = clock.wise,
              curved.label = curved.label,
              extend.xscale = extend.xscale,
              extend.yscale = extend.yscale,
              xscale = xscale,
              yscale = yscale,
              name = name, gp = gp, vp = vp,
              cl = "arcTextGrob")

  do.call(gTree,lst)
}


#' @noRd
#' @export
makeContent.arcTextGrob <- function(x){
  g <- .arcTextGrob(x = x$x,y = x$y,
                    labels = x$labels,shift = x$shift,
                    start = x$start,end = x$end,
                    r0 = x$r0,r1 = x$r1,
                    x0 = x$x0,y0 = x$y0,
                    text.gp = x$text.gp,
                    nice.facing = x$nice.facing,
                    inward = x$inward,
                    clock.wise = x$clock.wise,
                    curved.label = x$curved.label,
                    extend.xscale = x$extend.xscale,
                    extend.yscale = x$extend.yscale,
                    xscale = x$xscale,
                    yscale = x$yscale,
                    name = x$name, gp = x$gp, vp = x$vp)
  grid::setChildren(x, children = g$children)
}



#' @noRd
.arcTextGrob <- function(x = NULL,y = NULL,
                         labels = NULL,shift = 0.5,
                         start = 0,end = 60,
                         r0 = 0.5,r1 = 1,
                         x0 = 0,y0 = 0,
                         text.gp = NULL,
                         nice.facing = TRUE,
                         inward = FALSE,
                         clock.wise = FALSE,
                         curved.label = TRUE,
                         extend.xscale = 0.05,
                         extend.yscale = 0.05,
                         xscale = NULL,
                         yscale = NULL,
                         name = NULL,
                         gp = NULL, vp = NULL){
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

  # ============================================================================
  # whether use curved text
  if(curved.label == TRUE){
    # ii = 1
    lapply(seq_along(x), function(ii){
      # split labels
      label <- labels[ii]
      label.len <- nchar(label)
      char <- unlist(strsplit(label,split = ""))

      str.width <- as.numeric(convertWidth(stringWidth(label),unitTo = "native"))

      theta.label <- as.theta(str.width)
      start.ll <- as.theta(theta[ii]) - theta.label*shift
      end.ll <- as.theta(theta[ii]) + theta.label*shift

      # check direction
      if(clock.wise == TRUE){
        theta <- scales::rescale(1:label.len,to = 2*pi - range(as.radian(start.ll),as.radian(end.ll)))
        label.rot <- scales::rescale(1:label.len,to = 360 - range(start.ll,end.ll)) - 90
      }else{
        theta <- rev(scales::rescale(1:label.len,to = range(as.radian(start.ll),as.radian(end.ll))))
        label.rot <- rev(scales::rescale(1:label.len,to = range(start.ll,end.ll))) - 90
      }

      label <- char
      theta.r <- rep(r[ii],length(char))

      res <- list(label,theta,theta.r,label.rot)
    }) -> curve.list

    labels <- Reduce("c",sapply(curve.list,"[",1))
    theta.c <- Reduce("c",sapply(curve.list,"[",2))
    theta.r.c <- Reduce("c",sapply(curve.list,"[",3))
    label.rot <- Reduce("c",sapply(curve.list,"[",4))

    # calculate position
    xp0 <- x0 + theta.r.c*cos(theta.c)
    yp0 <- y0 + theta.r.c*sin(theta.c)

    label.hjust <- 0.5
  }else{
    lapply(seq_along(x), function(ii){
      label.theta <- as.theta(theta[ii])

      # label rot calculation
      if(nice.facing == TRUE){
        if(label.theta >= 0 & label.theta <= 90){
          new.rot <- label.theta - 90
        }else if(label.theta >= 90 & label.theta < 180){
          new.rot <- label.theta - 90
        }else if(label.theta >= 180 & label.theta < 270){
          new.rot <- label.theta - 270
        }else if(label.theta >= 270 & label.theta <= 360){
          new.rot <- label.theta - 270
        }else{
          new.rot <- label.theta - 270
        }
      }else{
        if(label.theta >= 0 & label.theta <= 90){
          new.rot <- label.theta
        }else if(label.theta >= 90 & label.theta < 180){
          new.rot <- label.theta - 180
        }else if(label.theta >= 180 & label.theta < 270){
          new.rot <- label.theta - 180
        }else if(label.theta >= 270 & label.theta <= 360){
          new.rot <- label.theta - 360
        }else{
          new.rot <- label.theta - 360
        }
      }

      label.rot <- new.rot
    }) %>% unlist() -> label.rot


    # label hjust calculation
    lapply(seq_along(x), function(ii){
      label.theta <- as.theta(theta[ii])

      if(nice.facing == TRUE){
        hjust <- 0.5
      }else{
        if(label.theta >= 0 & label.theta <= 90){
          hjust <- 0
        }else if(label.theta >= 90 & label.theta < 180){
          hjust <- 1
        }else if(label.theta >= 180 & label.theta < 270){
          hjust <- 1
        }else if(label.theta >= 270 & label.theta <= 360){
          hjust <- 0
        }else{
          hjust <- 0
        }
      }

      return(hjust)
    }) %>% unlist() -> label.hjust

    if(inward == TRUE){
      label.hjust <- abs(label.hjust - 1)
    }

    # calculate position
    xp0 <- x0 + r*cos(theta)
    yp0 <- y0 + r*sin(theta)
  }

  xp0 <- scales::rescale(xp0,to = c(0,1),from = c(-1,1))
  yp0 <- scales::rescale(yp0,to = c(0,1),from = c(-1,1))

  # label grob
  text.grob <- textGrob(label = labels,
                        x = xp0,y = yp0,
                        rot = label.rot,
                        hjust = label.hjust,
                        default.units = "npc",
                        gp = text.gp,
                        name = "labels")

  # ============================================================================
  # combine grobs
  # ============================================================================
  grid::gTree(children = grid::gList(text.grob),
              name = "arcTextGrob")
}
