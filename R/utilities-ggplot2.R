# ==============================================================================
# Internal helpers adapted from ggplot2
#
# Only the helpers that ggcirclize actually uses are kept here. The original
# 0.0.2 file carried a lot of dead code copied from ggplot2 (including a local
# `tolower()`/`toupper()` that aborted and shadowed the base functions) and it
# relied on ggplot2 internals such as `.pt` and the `ffi_standalone_*` native
# symbols, which are not part of ggplot2's public API.
# ==============================================================================

# Radius of the drawing area, in data units.
#
# Every grob maps data coordinates to npc with from = c(-arc_r_limit, arc_r_limit), which puts
# r = 1 exactly on the edge of the panel.  Facet strip labels are drawn
# outside the circle at r = r1 + strip.label.space + axis.space, which reaches
# 1.21 with the defaults, i.e. npc = 1.105 -- 10.5% past the panel and clipped
# by the device edge.  Mapping from a slightly larger window instead keeps
# everything inside: 1.21 / 1.35 = 0.948 npc, with margin left over.
# coord_cartesian(xlim = c(-1.35, 1.35)) in ggcirclize() keeps the two in sync.
arc_r_limit <- 1.35


#' Merge two sets of default values
#'
#' Values in `x` take priority; `y` only fills in the gaps.
#'
#' @param x,y Lists of default values.
#' @return A list containing the merged values.
#' @noRd
defaults <- function(x, y) c(x, y[setdiff(names(y), names(x))])


#' Merge aesthetic mappings while keeping the mapping class
#'
#' `defaults()` returns a plain list, which ggplot2 (>= 4.0.0) rejects because
#' `mapping` must be created by `aes()`. This variant restores the class of the
#' template mapping so the result can safely be handed to `ggplot()`.
#'
#' @param mapping User supplied mapping (takes priority).
#' @param default_mapping Mapping created by `aes()` holding the defaults.
#' @return An aesthetic mapping.
#' @noRd
merge_mapping <- function(mapping, default_mapping) {
  if (is.null(mapping) || length(mapping) == 0) return(default_mapping)

  out <- utils::modifyList(as.list(default_mapping), as.list(mapping))
  class(out) <- class(default_mapping)
  out
}


#' @noRd
data_frame0 <- function(...) {
  # vctrs::data_frame() (unlike tibble::tibble()) understands `.size`, which is
  # used by the stat/geom code adapted from ggplot2
  vctrs::data_frame(..., .name_repair = "minimal")
}

#' @noRd
unique0 <- function(x, ...) {
  if (is.null(x)) x else vctrs::vec_unique(x, ...)
}


#' @noRd
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}


#' @noRd
check_linewidth <- function(data, name) {
  # `size` was renamed to `linewidth` in ggplot2 3.4.0
  if (is.null(data$linewidth) && !is.null(data$size)) {
    data$linewidth <- data$size
  }
  data
}


#' @noRd
vec_rbind0 <- function(...) {
  vctrs::vec_rbind(...)
}


# Value used for the `group` column when no grouping is present
NO_GROUP <- -1L

#' @noRd
has_groups <- function(data) {
  # 0-row data frames give NA here, which is fine for the callers' purposes
  data$group[1L] != NO_GROUP
}


lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

# chartr() is used instead of tolower()/toupper() because the latter fail to
# convert i to I in the Turkish locale.
#' @noRd
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)

#' @noRd
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)

#' @noRd
snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  to_lower_ascii(x)
}

#' @noRd
snake_class <- function(x) {
  snakeize(class(x)[1])
}


# ==============================================================================
# Grouped apply helpers (used by the path / violin geoms)
# ==============================================================================

#' @noRd
split_matrix <- function(x, col_names = colnames(x)) {
  force(col_names)
  x <- lapply(seq_len(ncol(x)), function(i) x[, i])
  if (!is.null(col_names)) names(x) <- col_names
  x
}

# A grouped column holds a single value when every group is a single row. In
# ggplot2 this is an S3 generic; here a plain function is enough and avoids an
# unregistered S3 method (which made `dapply()` unusable in 0.0.2).
#' @noRd
single_value <- function(x, ...) {
  if (is.factor(x)) {
    # panels are encoded as factor numbers and can never be missing (NA)
    identical(levels(x), "1")
  } else {
    # this attribute is set when the grouping id is created
    identical(attr(x, "n"), 1L)
  }
}

#' @noRd
split_with_index <- function(x, f, n = max(f)) {
  if (n == 1) return(list(x))
  f <- as.integer(f)
  attributes(f) <- list(levels = as.character(seq_len(n)), class = "factor")
  unname(split(x, f))
}

#' @noRd
df_rows <- function(x, i) {
  cols <- lapply(x, `[`, i = i)
  data_frame0(!!!cols, .size = length(i))
}

#' @noRd
# More performant modifyList without recursion
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

#' Apply a function to unique subsets of a data frame
#'
#' Used by the `geom_arcpath()`, `geom_arcviolin()` and `geom_trackviolin()`
#' draw methods.
#'
#' @param df A data.frame
#' @param by A character vector of column names to split by
#' @param fun A function to apply to each split
#' @param ... Further arguments to `fun`
#' @param drop Should unused factor levels in the columns given in `by` be
#'   dropped.
#' @return A data.frame.
#' @importFrom stats setNames
#' @noRd
dapply <- function(df, by, fun, ..., drop = TRUE) {
  grouping_cols <- .subset(df, by)
  fallback_order <- unique0(c(by, names(df)))
  apply_fun <- function(x) {
    res <- fun(x, ...)
    if (is.null(res)) return(res)
    if (length(res) == 0) return(data_frame0())
    vars <- lapply(setNames(by, by), function(col) .subset2(x, col)[1])
    if (is.matrix(res)) res <- split_matrix(res)
    if (is.null(names(res))) names(res) <- paste0("V", seq_along(res))
    if (all(by %in% names(res))) return(data_frame0(!!!unclass(res)))
    res <- modify_list(unclass(vars), unclass(res))
    res <- res[intersect(c(fallback_order, names(res)), names(res))]
    data_frame0(!!!res)
  }

  # Shortcut when only one group
  if (all(vapply(grouping_cols, single_value, logical(1)))) {
    return(apply_fun(df))
  }

  ids <- vctrs::vec_group_id(grouping_cols)
  group_rows <- split_with_index(seq_len(nrow(df)), ids)
  result <- lapply(seq_along(group_rows), function(i) {
    cur_data <- df_rows(df, group_rows[[i]])
    apply_fun(cur_data)
  })
  vec_rbind0(!!!result)
}


# ==============================================================================
# Input checks
# ==============================================================================

#' Check that an input is a single number
#'
#' Lightweight replacement for the ggplot2 internal of the same name. The
#' original implementation called a `ffi_standalone_*` native routine that only
#' exists inside ggplot2's own DLL.
#'
#' @param x Object to test.
#' @param ... Ignored, kept for compatibility.
#' @param min,max Optional bounds (inclusive).
#' @param allow_na Allow `NA` values.
#' @param allow_null Allow `NULL`.
#' @param arg Argument name used in the error message.
#' @param call Call used in the error message.
#' @return `NULL`, invisibly. Called for its side effect.
#' @noRd
check_number_decimal <- function(x,
                                 ...,
                                 min = NULL,
                                 max = NULL,
                                 allow_na = FALSE,
                                 allow_null = FALSE,
                                 arg = "x",
                                 call = rlang::caller_env()) {
  if (is.null(x) && allow_null) return(invisible(NULL))

  bad <- !is.numeric(x) || length(x) != 1L || (!allow_na && is.na(x))
  if (!bad) {
    bad <- (!is.null(min) && x < min) || (!is.null(max) && x > max)
  }

  if (bad) {
    msg <- paste0("`", arg, "` must be a single number")
    if (!is.null(min) && !is.null(max)) {
      msg <- paste0(msg, " between ", min, " and ", max)
    } else if (!is.null(min)) {
      msg <- paste0(msg, " larger than or equal to ", min)
    } else if (!is.null(max)) {
      msg <- paste0(msg, " smaller than or equal to ", max)
    }
    if (allow_na) msg <- paste0(msg, " or `NA`")
    rlang::abort(paste0(msg, "."), call = call)
  }

  invisible(NULL)
}


#' Check that an input is a single logical value
#'
#' @param x Object to test.
#' @param ... Ignored, kept for compatibility.
#' @param allow_na Allow `NA` values.
#' @param allow_null Allow `NULL`.
#' @param arg Argument name used in the error message.
#' @param call Call used in the error message.
#' @return `NULL`, invisibly. Called for its side effect.
#' @noRd
check_bool <- function(x,
                       ...,
                       allow_na = FALSE,
                       allow_null = FALSE,
                       arg = "x",
                       call = rlang::caller_env()) {
  if (is.null(x) && allow_null) return(invisible(NULL))

  ok <- is.logical(x) && length(x) == 1L && (allow_na || !is.na(x))
  if (!ok) {
    what <- if (allow_na) "`TRUE`, `FALSE` or `NA`" else "`TRUE` or `FALSE`"
    rlang::abort(sprintf("`%s` must be %s.", arg, what), call = call)
  }

  invisible(NULL)
}


#' Convert a text size unit to points
#'
#' The original version relied on ggplot2's internal `.pt` constant.
#'
#' @param unit One of "mm", "pt", "cm", "in", "pc".
#' @return Numeric conversion factor to points.
#' @noRd
resolve_text_unit <- function(unit) {
  unit <- rlang::arg_match0(unit, c("mm", "pt", "cm", "in", "pc"))

  # number of points per unit; `.pt` in ggplot2 is 72.27 / 25.4
  switch(
    unit,
    "mm" = 72.27 / 25.4,
    "pt" = 1,
    "cm" = (72.27 / 25.4) * 10,
    "in" = 72.27,
    "pc" = 12
  )
}


# ==============================================================================
# Layout helpers
# ==============================================================================

#' Radius used for a facet strip label
#'
#' When a coordinate axis is drawn on the same side of the sector, the strip
#' label has to be pushed outwards, otherwise it lands on top of the axis tick
#' labels.
#'
#' @param r Radius of the label without an axis.
#' @param space Signed offset from `r` (negative for the inner side).
#' @param add.axis Whether an axis is drawn on that side.
#' @param axis.space Extra room reserved for the axis tick labels.
#' @return The radius to draw the strip label at.
#' @noRd
strip_label_r <- function(r, space, add.axis, axis.space = 0.06) {
  if (isTRUE(add.axis)) r + space + sign(space) * axis.space else r + space
}


#' Subset of labels that still fits into the available space
#'
#' Axis tick labels are drawn at every pretty break, which makes them overlap
#' each other as soon as the arc (or the band) is short. This returns the index
#' of the labels that can be drawn without collisions.
#'
#' @param n Number of labels.
#' @param span Available length in npc.
#' @param size Size of one label along `span`, in npc.
#' @param gap Minimal distance between two labels, in label widths.
#' @return Integer index vector.
#' @noRd
thin_label_index <- function(n, span, size, gap = 1.5) {
  if (n < 2) return(seq_len(n))
  if (!is.finite(span) || !is.finite(size) || size <= 0) return(seq_len(n))

  # 这段空间里实际能放下几个标签
  fits <- floor(span / (size * gap))
  if (!is.finite(fits) || fits >= n) return(seq_len(n))
  fits <- max(1, fits)

  # 只放得下一个时取最靠近中间的，避免永远留第一个
  if (fits == 1) return(round((n + 1) / 2))

  # 保留首尾，中间等间隔取，边界信息不会丢
  keep <- unique(round(seq(1, n, length.out = fits)))

  keep
}


#' Make sure an axis range covers the drawn shapes
#'
#' Track layers receive a `xscale`/`yscale` from upstream code that is often
#' built from the *centre* of each shape. Shapes that are wider than half a
#' step (tiles, rects, bars) then get extrapolated past the sector border.
#' This keeps a range the user supplied explicitly, but replaces one that does
#' not cover the geometry.
#'
#' @param params The `xAxis.params`/`yAxis.params` list.
#' @param name Either "xscale" or "yscale".
#' @param lo,hi Extents of the drawn shapes.
#' @param full Range that is known to cover everything.
#' @return The (possibly corrected) parameter list.
#' @noRd
resize_scale_param <- function(params, name, lo, hi, full) {
  rng <- params[[name]]

  covered <- !is.null(rng) && length(rng) == 2 &&
    is.finite(rng[1]) && is.finite(rng[2]) &&
    min(lo, na.rm = TRUE) >= rng[1] && max(hi, na.rm = TRUE) <= rng[2]

  if (!covered) params[[name]] <- full

  params
}


# ==============================================================================
# Bezier curve (replaces DescTools::DrawBezier)
# ==============================================================================

#' Points of a cubic Bezier curve
#'
#' Equivalent to `DescTools::DrawBezier(plot = FALSE, x, y)` for the four
#' control points it is called with, without pulling in the DescTools
#' dependency.
#'
#' @param x,y Numeric vectors of four control point coordinates.
#' @param n Number of points to return.
#' @return A list with components `x` and `y`.
#' @noRd
bezier_points <- function(x, y, n = 100) {
  if (length(x) != 4L || length(y) != 4L) {
    rlang::abort("`x` and `y` must contain exactly four control points.")
  }

  t <- seq(0, 1, length.out = n)
  u <- 1 - t

  wx <- u^3
  wy <- 3 * u^2 * t
  wz <- 3 * u * t^2
  ww <- t^3

  list(
    x = wx * x[1] + wy * x[2] + wz * x[3] + ww * x[4],
    y = wx * y[1] + wy * y[2] + wz * y[3] + ww * y[4]
  )
}
