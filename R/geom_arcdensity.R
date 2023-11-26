defaults <- function(x, y) c(x, y[setdiff(names(y), names(x))])

#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @include geom_arcarea.R
#' @export
GeomArcdensity <- ggproto("GeomArcdensity", GeomArcarea,
                          default_aes = defaults(
                            aes(fill = NA, weight = 1, colour = "black", alpha = NA),
                            GeomArcarea$default_aes
                          )
)


#' @rdname geom_arcarea
#' @import ggplot2
#' @export
geom_arcdensity <- function(mapping = NULL, data = NULL,
                            stat = "density", position = "identity",
                            ...,
                            polar.every = FALSE,
                            na.rm = FALSE,
                            orientation = NA,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            outline.type = "upper") {
  outline.type <- rlang::arg_match0(outline.type, c("both", "upper", "lower", "full"))

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArcdensity,
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
