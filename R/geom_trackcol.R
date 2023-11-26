#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @include geom_trackbar.R
#' @export
GeomTrackcol <- ggproto("GeomTrackcol", GeomTrackbar)


#' @rdname geom_trackbar
#' @import ggplot2
#' @export
geom_trackcol <- function(mapping = NULL, data = NULL,
                          position = "identity",stat = "identity",
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
    geom = GeomTrackcol,
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
