#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @include geom_arcribbon.R
#' @export
GeomArcarea <- ggproto("GeomArcarea", GeomArcribbon,
                       required_aes = c("x", "y"),

                       default_aes = aes(start = 0,end = 180,r0 = 0.5,r1 = 1,
                                         clock.wise = FALSE,
                                         colour = NA,fill = "grey80",
                                         linetype = 1,
                                         alpha = NA,linewidth = .5,
                                         sector.bg.fill = "grey95",
                                         sector.bg.col = "black",
                                         sector.bg.lty = 1,
                                         sector.bg.lwd = 0.5),


                       setup_params = function(data, params) {
                         params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
                         params
                       },

                       setup_data = function(data, params) {
                         data$flipped_aes <- params$flipped_aes
                         data <- flip_data(data, params$flipped_aes)
                         data <- transform(data[order(data$PANEL, data$group, data$x), ], ymin = 0, ymax = y)
                         flip_data(data, params$flipped_aes)
                       }
)



#' Create a stacked area plot in a polar coordinate system.
#'
#' \code{geom_arcarea} is used to create a stacked area plot in a polar coordinate system.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied (default: "align").
#' @param position The position adjustment to be applied (default: "stack").
#' @param na.rm Should missing values be removed? (default: FALSE).
#' @param orientation Orientation of the area plot (default: NA).
#' @param show.legend Should the legend be shown? (default: NA).
#' @param inherit.aes Should aesthetics be inherited? (default: TRUE).
#' @param polar.every Should polar coordinate be used for every data point? (default: TRUE).
#' @param outline.type Type of outline (default: "upper").
#' @param ... other params.
#'
#' @return A layer to be added to a ggplot object.
#'
#' @seealso \code{\link[ggplot2]{geom_area}}
#'
#' @import ggplot2
#'
#' @export
geom_arcarea <- function(mapping = NULL, data = NULL, stat = "align",
                         position = "stack", na.rm = FALSE, orientation = NA,
                         show.legend = NA, inherit.aes = TRUE, ...,
                         polar.every = TRUE,
                         outline.type = "upper") {
  outline.type <- rlang::arg_match0(outline.type, c("both", "upper", "lower", "full"))

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArcarea,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      outline.type = outline.type,
      polar.every = polar.every,
      ...
    )
  )
}
