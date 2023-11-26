#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @include geom_arcrect.R
#' @export
GeomArcbar <- ggproto("GeomArcbar", GeomArcrect,
                      required_aes = c("x","y",
                                       "start","end","r0","r1","clock.wise"),

                      default_aes = aes(
                        width = 0.9,
                        start = 0,end = 180,r0 = 0.5,r1 = 1,
                        clock.wise = FALSE,
                        colour = "grey30",fill = "grey30",
                        linetype = 1,
                        alpha = NA,linewidth = .5,
                        sector.bg.fill = "grey95",
                        sector.bg.col = "black",
                        sector.bg.lty = 1,
                        sector.bg.lwd = 0.5),

                      # These aes columns are created by setup_data(). They need to be listed here so
                      # that GeomRect$handle_na() properly removes any bars that fall outside the defined
                      # limits, not just those for which x and y are outside the limits
                      non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

                      setup_params = function(data, params) {
                        params$flipped_aes <- has_flipped_aes(data, params)
                        params
                      },

                      extra_params = c("just", "na.rm", "orientation"),

                      setup_data = function(data, params) {
                        data$flipped_aes <- params$flipped_aes
                        data <- flip_data(data, params$flipped_aes)
                        data$width <- data$width %||%
                          params$width %||% (min(vapply(
                            split(data$x, data$PANEL),
                            resolution, numeric(1), zero = FALSE
                          )) * 0.9)
                        data$just <- params$just %||% 0.5

                        data <- transform(data,
                                          ymin = pmin(y, 0), ymax = pmax(y, 0),
                                          xmin = x - width * just, xmax = x + width * (1 - just),
                                          width = NULL, just = NULL
                        )

                        flip_data(data, params$flipped_aes)

                      },

                      draw_panel = function(self, data, panel_params, coord,
                                            extend.xscale = 0.05,
                                            extend.yscale = 0.05,
                                            add.bg = TRUE,
                                            sector.bg.extend = 0.025,
                                            add.xaxis = TRUE,
                                            add.yaxis = TRUE,
                                            xAxis.params = list(),
                                            yAxis.params = list(),
                                            lineend = "butt", linejoin = "round",
                                            flipped_aes = FALSE) {

                        # get xlim and ylim range
                        xlim <- range(data$xmin,data$xmax)
                        ylim <- range(data$ymin,data$ymax)

                        # add tile x labels
                        if(panel_params$x$is_discrete()){
                          tile.label.x <- panel_params$x$limits

                          minor.ticks.n.x = 0
                          breaks.x = 1:length(tile.label.x)
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
                          breaks.y = 1:length(tile.label.y)
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

                        # Hack to ensure that width is detected as a parameter
                        ggproto_parent(GeomArcrect, self)$draw_panel(
                          data,
                          panel_params,
                          coord,
                          linejoin = linejoin,
                          lineend = lineend,
                          extend.xscale = extend.xscale,
                          extend.yscale = extend.yscale,
                          add.bg = add.bg,
                          sector.bg.extend = sector.bg.extend,
                          add.xaxis = add.xaxis,
                          add.yaxis = add.yaxis,
                          xAxis.params = arcxAxisGrob.params,
                          yAxis.params = arcyAxisGrob.params
                        )
                      },

                      rename_size = TRUE
)


#' Arcbar Geom for Circular Plots
#'
#' This function adds arc-shaped bars to a circular plot created with ggplot2,
#' allowing you to represent data with radial bars.
#'
#' @param mapping Aesthetic mappings (default: NULL).
#' @param data The data frame containing the data (default: NULL).
#' @param stat The statistical transformation to use (default: "bar").
#' @param position The position adjustment for overlapping objects (default: "stack").
#' @param na.rm A logical value indicating whether missing values should be removed (default: FALSE).
#' @param show.legend A logical value indicating whether to show the legend (default: NA).
#' @param inherit.aes A logical value indicating whether to inherit aesthetics from the plot (default: TRUE).
#' @param just A numeric value (between 0 and 1) controlling the justification of the arc bars (default: 0.5).
#' @param extend.xscale The extension factor for the x-axis scale (default: 0.05).
#' @param extend.yscale The extension factor for the y-axis scale (default: 0.05).
#' @param add.bg A logical value indicating whether to add a background (default: TRUE).
#' @param sector.bg.extend The extension factor for the sector background (default: 0.025).
#' @param add.xaxis A logical value indicating whether to add the x-axis (default: TRUE).
#' @param add.yaxis A logical value indicating whether to add the y-axis (default: TRUE).
#' @param xAxis.params A list of parameters for customizing the x-axis (default: list()).
#' @param yAxis.params A list of parameters for customizing the y-axis (default: list()).
#' @param ... Additional parameters passed to the geom_arcbar.
#'
#' @return A circular plot with arc-shaped bars.
#'
#' @import ggplot2
#' @export
geom_arcbar <- function(mapping = NULL, data = NULL,
                        stat = "bar", position = "stack",
                        na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE,
                        just = 0.5,
                        extend.xscale = 0.05,
                        extend.yscale = 0.05,
                        add.bg = TRUE,
                        sector.bg.extend = 0.025,
                        add.xaxis = TRUE,
                        add.yaxis = TRUE,
                        xAxis.params = list(),
                        yAxis.params = list(),
                        ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArcbar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  just = just,
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
