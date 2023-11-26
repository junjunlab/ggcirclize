#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomArcpoint <- ggproto("GeomArcpoint", Geom,
                        required_aes = c("x","y","start","end","r0","r1","clock.wise"),

                        default_aes = aes(
                          start = 0,end = 180,r0 = 0.5,r1 = 1,
                          clock.wise = FALSE,
                          colour = "black",linewidth = .5,
                          size = 5,linetype = 1,
                          shape = 19,fill = NA,
                          alpha = NA,stroke = 1,
                          sector.bg.fill = "grey95",
                          sector.bg.col = "black",
                          sector.bg.lty = 1,
                          sector.bg.lwd = 0.5),

                        # draw_group = function(data, panel_params, coord, ...) {
                        draw_panel = function(data, panel_params, coord,
                                              extend.xscale = 0.05,
                                              extend.yscale = 0.05,
                                              add.bg = TRUE,
                                              sector.bg.extend = 0.025,
                                              add.xaxis = TRUE,
                                              add.yaxis = TRUE,
                                              xAxis.params = list(),
                                              yAxis.params = list()) {

                          # get xlim and ylim range
                          xlim <- range(data$x)
                          ylim <- range(data$y)
                          # xlim <- panel_params$x.range
                          # ylim <- panel_params$y.range

                          # add tile x labels
                          if(panel_params$x$is_discrete()){
                            tile.label.x <- panel_params$x$limits

                            minor.ticks.n.x = 0
                            if(length(tile.label.x) == 1){
                              breaks.x <- c(1,1)
                            }else{
                              breaks.x = 1:length(tile.label.x)
                            }
                            breaks.label.x = tile.label.x
                          }else{
                            minor.ticks.n.x = 5
                            breaks.x = NULL
                            breaks.label.x = NULL
                          }

                          # add tile y labels
                          if(panel_params$y$is_discrete()){
                            tile.label.y <- panel_params$y$limits

                            minor.ticks.n.y = 0
                            if(length(tile.label.y) == 1){
                              breaks.y <- c(1,1)
                            }else{
                              breaks.y = 1:length(tile.label.y)
                            }
                            breaks.label.y = tile.label.y
                          }else{
                            minor.ticks.n.y = 5
                            breaks.y = NULL
                            breaks.label.y = NULL
                          }

                          # update xaxis params
                          arcxAxisGrob.params <- modifyList(list(xscale = xlim,
                                                                 minor.ticks.n = minor.ticks.n.x,
                                                                 breaks = breaks.x,
                                                                 breaks.label = breaks.label.x),
                                                            xAxis.params)
                          arcyAxisGrob.params <- modifyList(list(yscale = ylim,
                                                                 minor.ticks.n = minor.ticks.n.y,
                                                                 breaks = breaks.y,
                                                                 breaks.label = breaks.label.y),
                                                            yAxis.params)


                          # check point shape
                          if (is.character(data$shape)) {
                            data$shape <- translate_shape_string(data$shape)
                          }

                          # Transform the data
                          # coords <- coord$transform(data, panel_params)
                          coords <- data

                          stroke_size <- coords$stroke
                          stroke_size[is.na(stroke_size)] <- 0

                          # draw grobs
                          if(add.bg == TRUE){
                            sector.grob <- arcSectorGrob(start = unique(coords$start),
                                                         end = unique(coords$end),
                                                         r0 = unique(coords$r0),r1 = unique(coords$r1),
                                                         extend.xscale = extend.xscale - sector.bg.extend,
                                                         extend.yscale = extend.yscale - sector.bg.extend,
                                                         xaxis.extend.xscale = extend.xscale,
                                                         xaxis.extend.yscale = extend.yscale - sector.bg.extend,
                                                         yaxis.extend.xscale = extend.xscale - sector.bg.extend,
                                                         yaxis.extend.yscale = extend.yscale,
                                                         add.xaxis = add.xaxis,add.yaxis = add.yaxis,
                                                         arcxAxisGrob.params = arcxAxisGrob.params,
                                                         arcyAxisGrob.params = arcyAxisGrob.params,
                                                         sector.gp = gpar(col = coords$sector.bg.col,
                                                                          fill = coords$sector.bg.fill,
                                                                          lwd = coords$sector.bg.lwd,
                                                                          lty = coords$sector.bg.lty),
                                                         clock.wise = unique(coords$clock.wise))
                          }else{
                            sector.grob <- nullGrob()
                          }

                          point.grob <- arcPointGrob(x = coords$x,y = coords$y,
                                                     start = unique(coords$start),
                                                     end = unique(coords$end),
                                                     r0 = unique(coords$r0),r1 = unique(coords$r1),
                                                     pch = coords$shape,
                                                     size = coords$size,
                                                     xscale = arcxAxisGrob.params$xscale,
                                                     yscale = arcyAxisGrob.params$yscale,
                                                     point.gp = gpar(
                                                       col = alpha(coords$colour, coords$alpha),
                                                       fill = alpha(coords$fill, coords$alpha),
                                                       # Stroke is added around the outside of the point
                                                       fontsize = coords$size * .pt + stroke_size * .stroke / 2,
                                                       lwd = coords$stroke * .stroke / 2),
                                                     extend.xscale = extend.xscale,
                                                     extend.yscale = extend.yscale,
                                                     clock.wise = unique(coords$clock.wise))


                          ggname("geom_arcpoint",
                                 grid::gTree(children = gList(sector.grob,point.grob)))
                        },

                        draw_key = draw_key_point
)


#' Translating shape strings
#'
#' `translate_shape_string()` is a helper function for translating point shapes
#' given as a character vector into integers that are interpreted by the
#' grid system.
#'
#' @param shape_string A character vector giving point shapes.
#'
#' @return An integer vector with translated shapes.
#' @export
#' @keywords internal
#'
#' @examples
#' translate_shape_string(c("circle", "square", "triangle"))
#'
#' # Strings with 1 or less characters are interpreted as symbols
#' translate_shape_string(c("a", "b", "?"))
translate_shape_string <- function(shape_string) {
  # strings of length 0 or 1 are interpreted as symbols by grid
  if (nchar(shape_string[1]) <= 1) {
    return(shape_string)
  }

  pch_table <- c(
    "square open"           = 0,
    "circle open"           = 1,
    "triangle open"         = 2,
    "plus"                  = 3,
    "cross"                 = 4,
    "diamond open"          = 5,
    "triangle down open"    = 6,
    "square cross"          = 7,
    "asterisk"              = 8,
    "diamond plus"          = 9,
    "circle plus"           = 10,
    "star"                  = 11,
    "square plus"           = 12,
    "circle cross"          = 13,
    "square triangle"       = 14,
    "triangle square"       = 14,
    "square"                = 15,
    "circle small"          = 16,
    "triangle"              = 17,
    "diamond"               = 18,
    "circle"                = 19,
    "bullet"                = 20,
    "circle filled"         = 21,
    "square filled"         = 22,
    "diamond filled"        = 23,
    "triangle filled"       = 24,
    "triangle down filled"  = 25
  )

  shape_match <- charmatch(shape_string, names(pch_table))

  invalid_strings <- is.na(shape_match)
  nonunique_strings <- shape_match == 0

  if (any(invalid_strings)) {
    bad_string <- unique0(shape_string[invalid_strings])
    cli::cli_abort("Shape aesthetic contains invalid value{?s}: {.val {bad_string}}")
  }

  if (any(nonunique_strings)) {
    bad_string <- unique0(shape_string[nonunique_strings])
    cli::cli_abort(c(
      "shape names must be given unambiguously",
      "i" = "Fix {.val {bad_string}}"
    ))
  }

  unname(pch_table[shape_match])
}


# ==============================================================================

#' Create points at the center of arc sectors.
#'
#' \code{geom_arcpoint} is used to create points at the center of arc sectors in a plot.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default: "identity").
#' @param position The position adjustment to be applied (default: "identity").
#' @param na.rm Should missing values be removed? (default: FALSE)
#' @param show.legend Should the legend be shown? (default: NA).
#' @param inherit.aes Should aesthetics be inherited? (default: TRUE)
#' @param extend.xscale Extension factor for x-axis scale (default: 0.05).
#' @param extend.yscale Extension factor for y-axis scale (default: 0.05).
#' @param add.bg Should a background be added? (default: TRUE)
#' @param sector.bg.extend Extension factor for sector background (default: 0.025).
#' @param add.xaxis Should x-axis be added? (default: TRUE)
#' @param add.yaxis Should y-axis be added? (default: TRUE)
#' @param xAxis.params A list of parameters for x-axis (default: list()).
#' @param yAxis.params A list of parameters for y-axis (default: list()).
#' @param ... Additional parameters to be passed to the \code{GeomArcpoint}.
#'
#' @return A layer to be added to a ggplot object.
#'
#' @seealso \code{\link[ggplot2]{geom_point}}
#'
#' @import ggplot2
#' @import rlang vctrs stats
#'
#' @export
geom_arcpoint <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE,
                          extend.xscale = 0.05,
                          extend.yscale = 0.05,
                          add.bg = TRUE,
                          sector.bg.extend = 0.025,
                          add.xaxis = TRUE,
                          add.yaxis = TRUE,
                          xAxis.params = list(),
                          yAxis.params = list(),...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArcpoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  extend.xscale = extend.xscale,
                  extend.yscale = extend.yscale,
                  add.bg = add.bg,
                  sector.bg.extend = sector.bg.extend,
                  add.xaxis = add.xaxis,
                  add.yaxis = add.yaxis,
                  xAxis.params = xAxis.params,
                  yAxis.params = yAxis.params,
                  ...)
  )
}
