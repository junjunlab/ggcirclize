#' @rdname ggcirclize-ggproto
#' @import ggplot2
#' @export
geom_archistogram <- function(mapping = NULL, data = NULL,
                              stat = "bin", position = "stack",
                              ...,
                              binwidth = NULL,
                              bins = NULL,
                              na.rm = FALSE,
                              orientation = NA,
                              show.legend = NA,
                              inherit.aes = TRUE) {

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomArcbar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      binwidth = binwidth,
      bins = bins,
      na.rm = na.rm,
      orientation = orientation,
      pad = FALSE,
      ...
    )
  )
}
