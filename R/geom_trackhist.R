#' @rdname ggcirclize-ggproto
#' @import ggplot2
#' @export
geom_trackhist <- function(mapping = NULL, data = NULL,
                           stat = "bin", position = "identity",
                           na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTrackbar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  ...)
  )
}
