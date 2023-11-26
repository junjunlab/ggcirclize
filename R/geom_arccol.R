#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @include geom_arcbar.R
#' @export
GeomArccol <- ggproto("GeomArccol", GeomArcbar)


#' @export
#' @rdname geom_arcbar
#' @param ... other params
#' @import ggplot2
geom_arccol <- function(mapping = NULL, data = NULL,
                        position = "stack",stat = "identity",
                        ...,
                        just = 0.5,
                        # width = NULL,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArccol,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      just = just,
      # width = width,
      na.rm = na.rm,
      ...
    )
  )
}
