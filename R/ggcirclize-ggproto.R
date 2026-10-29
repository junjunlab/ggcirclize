#' ggproto classes and low level layers of ggcirclize
#'
#' @description
#' `ggcirclize` builds every circular layer on top of a small set of ggproto
#' classes. The `Geom*` objects below are the drawing backends of the exported
#' `geom_*()` functions, and `stat_arcbar()`, `stat_arcboxplot()` and
#' `stat_arcdensity()` are the statistical transformations that feed them.
#'
#' These objects are documented together because they share the same interface
#' and are not meant to be used directly; call the corresponding `geom_*()`
#' function instead.
#'
#' @name ggcirclize-ggproto
#' @keywords internal
NULL
