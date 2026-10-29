# ==============================================================================
# 圆弧坐标与文字排布的公共基础
#
# 这里放的是全包唯一的“数据值 -> 角度 / 半径”换算，以及和 circlize
# (`circos.text()` / `circos.axis()`) 完全一致的文字朝向规则。
#
# 早期版本里点/线/多边形/文字用一种映射方向，而矩形、线段和坐标轴用另一种，
# 同一个 x 值在两类图层里会落到镜像的位置上。现在统一到这里：
#
#   clock.wise = FALSE   x 小 -> 扇区起点，x 大 -> 扇区终点
#   clock.wise = TRUE    整体镜像（角度取 2*pi - angle）
# ==============================================================================

#' How many npc units one radius unit occupies
#'
#' Every grob finally converts its `[-1, 1]` circle into the `[0, 1]` npc box of
#' the current viewport, but the exact ratio depends on that viewport's scale.
#' Measuring text in npc and comparing it with an arc length therefore needs the
#' real ratio; a hard-coded `1/2` silently breaks inside viewports with a
#' custom scale.
#'
#' @return npc units per radius unit.
#' @noRd
arc_npc_per_radius <- function() {
  r <- try(
    as.numeric(convertWidth(unit(1, "native"), "npc", valueOnly = TRUE)),
    silent = TRUE
  )
  if (inherits(r, "try-error") || !is.finite(r) || r <= 0) 0.5 else r
}


#' Map data values onto arc angles
#'
#' @param x Numeric values on the data scale.
#' @param xscale The data range, `c(min, max)`.
#' @param start,end Sector limits in degrees.
#' @param clock.wise Mirror the result (clockwise sectors).
#' @param extend.xscale Fraction of the sector kept free at both ends, so the
#'   outermost marks do not sit exactly on the sector border.
#' @return Angles in radians.
#' @noRd
arc_theta <- function(x, xscale, start, end, clock.wise = FALSE,
                      extend.xscale = 0) {
  if (is.null(xscale) || diff(range(xscale)) == 0) xscale <- range(c(x, xscale))

  span <- end - start
  ext <- span * extend.xscale
  s <- start + ext
  e <- end - ext

  deg <- scales::rescale(x, to = c(s, e), from = xscale)

  if (isTRUE(clock.wise)) {
    2 * pi - deg * pi / 180
  } else {
    deg * pi / 180
  }
}


#' Map data values onto radii
#'
#' @inheritParams arc_theta
#' @param yscale The data range for the radial axis.
#' @param r0,r1 Inner and outer radius of the track.
#' @return Radii in the same units as `r0`/`r1`.
#' @noRd
arc_radius <- function(y, yscale, r0, r1, extend.yscale = 0) {
  if (is.null(yscale) || diff(range(yscale)) == 0) yscale <- range(c(y, yscale))

  ext <- (r1 - r0) * extend.yscale
  scales::rescale(y, to = c(r0 + ext, r1 - ext), from = yscale)
}


#' Text orientation that matches `circlize::circos.text()`
#'
#' `circos.text()` exposes a `facing` argument that decides how a label sits on
#' the circle. The underlying rotation is a fixed offset from the angle of the
#' point:
#'
#' | facing              | rotation    | reading direction          |
#' |---------------------|-------------|----------------------------|
#' | `inside`            | theta - 90  | along the arc, outward     |
#' | `outside`           | theta + 90  | along the arc, inward      |
#' | `clockwise`         | theta       | along the radius, inward   |
#' | `reverse.clockwise` | theta - 180 | along the radius, outward  |
#' | `downward`          | 0           | always horizontal          |
#'
#' With `nice_facing = TRUE` a label is flipped by 180 degrees when it would
#' otherwise be upside down, and its `adj` is mirrored at the same time - this
#' is exactly what `circos.text(niceFacing = TRUE)` does.
#'
#' @param theta Angles in radians.
#' @param facing One of the facings listed above.
#' @param nice_facing Flip upside-down labels.
#' @param adj Length-1 or length-2 justification, in `graphics::text()` terms.
#' @return A data frame with `rot` (degrees), `hjust` and `vjust`.
#' @noRd
arc_text_geometry <- function(theta, facing = "inside", nice_facing = FALSE,
                              adj = c(0.5, 0.5)) {
  facing <- match.arg(facing, c("inside", "outside", "reverse.clockwise",
                                "clockwise", "downward"))
  if (length(adj) == 1) adj <- c(adj, adj)

  n <- length(theta)
  ax <- rep(adj[1], length.out = n)
  ay <- rep(adj[2], length.out = n)

  deg <- (theta * 180 / pi) %% 360

  rot <- switch(facing,
    inside              = deg - 90,
    outside             = deg + 90,
    "reverse.clockwise" = deg - 180,
    clockwise           = deg,
    downward            = rep(0, n)
  )

  if (isTRUE(nice_facing)) {
    if (facing %in% c("clockwise", "reverse.clockwise")) {
      # Fold the rotation into [-90, 90] so no label ends up upside down. Using
      # `ceiling((base - 90) / 180)` instead of a half-open angle test keeps the
      # 270 deg (straight down) case folded to 90 deg; the previous
      # `deg >= 90 & deg < 270` test left it unfolded and drew bottom labels
      # upside down.
      base <- if (facing == "clockwise") deg else deg - 180
      rot <- base - 180 * ceiling((base - 90) / 180)
      # Anchor the text at the OUTER end, so every radial label grows towards the
      # centre, away from its connector. Whether that needs hjust = 0 or 1
      # depends on which side of the circle the label sits on: `rot - deg` is
      # the angle between the reading direction and the outward radius, so a
      # negative cosine means the justification has to be mirrored. A fixed
      # `adj` (or mirroring on the rot-fold boundary) leaves half the labels
      # growing outwards through their connector.
      radially.outward <- cos((rot - deg) * pi / 180) > 0
      ax <- ifelse(radially.outward, ax, 1 - ax)
    } else if (facing %in% c("inside", "outside")) {
      flip <- !(deg > 0 & deg < 180)
      rot <- ifelse(flip, deg + 90, deg - 90)
      # `circos.text()` mirrors the whole adj vector on the flipped half
      ax <- ifelse(flip, 1 - ax, ax)
      ay <- ifelse(flip, 1 - ay, ay)
    }
  }

  data.frame(rot = as.numeric(rot), hjust = as.numeric(ax), vjust = as.numeric(ay))
}


#' Lay a label out character by character along an arc
#'
#' Port of the `bending.inside` / `bending.outside` branch of
#' `circlize::circos.text()`: every character gets its own angle, computed from
#' its measured width, so the text really follows the arc instead of being
#' spaced evenly by character count.
#'
#' @param theta Angle of the anchor point, in radians.
#' @param r Radius of the anchor point.
#' @param label A single label.
#' @param facing Either `"bending.inside"` or `"bending.outside"`.
#' @param adj Justification, `graphics::text()` style.
#' @param cex,font Passed to the text measuring functions.
#' @param dir `1` when reading order follows increasing angle, `-1` otherwise.
#' @param radius_unit Size of one npc unit in the same units as `r`. Radii are
#'   given on a `[-1, 1]` circle that is later squeezed into `[0, 1]`, so one
#'   npc unit equals two radius units.
#' @return A data frame with `theta`, `rot`, `hjust` for each character.
#' @noRd
arc_bending_chars <- function(theta, r, label, facing = "bending.inside",
                              adj = c(0.5, 0.5), cex = 1, font = 1,
                              dir = NULL, radius_unit = NULL) {
  if (length(adj) == 1) adj <- c(adj, adj)

  chars <- strsplit(as.character(label), "")[[1]]
  if (!length(chars)) return(NULL)
  if (facing == "bending.outside") chars <- rev(chars)

  r <- max(abs(r), 1e-6)

  # 整个标签占用的弧长由它的实际宽度决定，字符在区间内均分
  per.r <- arc_npc_per_radius()
  w.r <- as.numeric(convertWidth(stringWidth(as.character(label)), "npc",
                                 valueOnly = TRUE)) / per.r
  span <- 2 * asin(pmin(1, w.r / 2 / r))

  # 上半圈按角度增大方向读，下半圈反过来，保证文字始终正立
  deg <- (theta * 180 / pi) %% 360
  lower <- deg >= 180
  if (is.null(dir)) dir <- if (facing == "bending.outside") -1 else 1
  if (facing == "bending.inside") {
    dir <- if (lower) -dir else dir
  } else {
    dir <- if (lower) dir else -dir
  }
  if (facing == "bending.outside") adj <- 1 - adj

  n <- length(chars)
  offset <- (seq_len(n) - 0.5) / n - adj[1]
  ang <- theta - dir * span * offset

  rot <- if (!lower) ang * 180 / pi - 90 else ang * 180 / pi + 90

  data.frame(theta = ang, rot = rot, hjust = rep(0.5, n))
}


#' Format axis tick labels
#'
#' @param at Numeric break positions.
#' @param digits Significant digits used by [format()].
#' @param format Optional function turning `at` into a character vector.
#' @return A character vector.
#' @noRd
arc_format_labels <- function(at, digits = 3, format = NULL) {
  if (is.function(format)) {
    out <- format(at)
    return(as.character(out))
  }

  out <- base::format(at, trim = TRUE, scientific = FALSE, digits = digits)
  trimws(out)
}


#' Pretty breaks that stay inside the data range
#'
#' @param range Numeric range.
#' @param n Approximate number of breaks.
#' @return Numeric vector of break positions.
#' @noRd
arc_pretty_breaks <- function(range, n = 5) {
  if (!is.finite(diff(range)) || diff(range) == 0) return(range[1])

  at <- grid::grid.pretty(range, n = n)
  at <- at[at >= min(range) & at <= max(range)]
  if (!length(at)) at <- mean(range)

  at
}


#' Major breaks spaced by an angular distance
#'
#' `circlize` does not ask for a fixed number of tick labels: it picks the data
#' interval that corresponds to `circos.par("major.by.degree")` (10 degrees by
#' default) and then rounds it to a readable number. That is why a narrow
#' sector automatically gets fewer labels than a wide one.
#'
#' @param xscale Data range covered by the axis.
#' @param span_deg Angular width of the sector, in degrees.
#' @param major.by.degree Minimal angular distance between two major ticks.
#' @return Numeric vector of break positions inside `xscale`.
#' @noRd
arc_major_breaks <- function(xscale, span_deg, major.by.degree = 10) {
  span <- diff(range(xscale))
  if (!is.finite(span) || span <= 0) return(mean(xscale))

  steps <- max(1, floor(abs(span_deg) / major.by.degree))
  raw <- span / steps

  # 把步长收敛到 1 / 2 / 5 x 10^k，读起来最舒服
  e <- 10^floor(log10(raw))
  cand <- e * c(1, 2, 5, 10)

  # 数据本身是整数（计数、染色体长度之类）时不要退化成 0.5 这种小数刻度
  int_like <- all(abs(xscale - round(xscale)) < 1e-8) && span >= 3
  if (int_like) {
    int_cand <- cand[cand >= 1 & abs(cand - round(cand)) < 1e-8]
    if (length(int_cand)) cand <- int_cand
  }

  # 按比例而不是绝对差取整，避免总是偏向更密的刻度
  by <- cand[which.min(abs(log(cand / raw)))]

  at <- seq(ceiling(min(xscale) / by) * by, max(xscale), by = by)
  if (!length(at)) at <- mean(xscale)

  at
}


#' How many tick labels a sector can carry
#'
#' A narrow sector cannot host five radial tick labels without them running into
#' the neighbouring sector. The number of breaks is therefore reduced with the
#' angular width of the sector.
#'
#' @param span_deg Angular width of the sector, in degrees.
#' @param n Maximum number of breaks requested by the user.
#' @return Integer number of breaks to use.
#' @noRd
arc_breaks_n_for_span <- function(span_deg, n = 5) {
  if (!is.finite(span_deg) || span_deg <= 0) return(n)

  k <- if (span_deg >= 60) n
  else if (span_deg >= 30) min(n, 4)
  else if (span_deg >= 15) min(n, 3)
  else min(n, 2)

  max(2L, as.integer(k))
}


#' Axis parameters for a discrete scale
#'
#' A discrete scale maps each level to an integer position, so the axis has to
#' tick on the levels themselves and label them with the level names instead of
#' the integer positions. Returns \code{NULL} for continuous scales so that
#' \code{modifyList()} keeps whatever the caller specified.
#'
#' @param scale_view A ggplot2 scale view (\code{panel_params$x} or
#' \code{panel_params$y}).
#' @return A named list with \code{breaks}, \code{breaks.label} and
#' \code{minor.ticks.n}, or \code{NULL}.
#' @noRd
arc_discrete_axis_params <- function(scale_view) {
  if (is.null(scale_view) || !isTRUE(try(scale_view$is_discrete(), silent = TRUE))) {
    return(NULL)
  }

  labels <- as.character(scale_view$limits)
  if (length(labels) == 0) return(NULL)

  breaks <- if (length(labels) == 1) c(1, 1) else seq_along(labels)

  list(breaks = breaks,
       breaks.label = labels,
       minor.ticks.n = 0)
}


#' Regroup ribbon data drawn along a discrete axis
#'
#' ggplot2 derives the default \code{group} from every discrete aesthetic,
#' including a discrete \code{x}. A ribbon along a discrete \code{x} therefore
#' ends up with one group per \code{x} level, i.e. a one point polygon, which
#' renders nothing at all. This rebuilds the groups from the aesthetics that
#' legitimately separate ribbons (panel, sector, colour/fill, ...) and keeps the
#' ordering so that the upper/lower edges still form a single polygon.
#'
#' @param data The layer data as handed to \code{draw_panel()}.
#' @return The data with a rebuilt \code{group} column.
#' @noRd
arc_regroup_ribbon <- function(data) {
  if (is.null(data) || nrow(data) == 0 || !"group" %in% names(data)) return(data)

  cols <- intersect(c("PANEL", "sector", "fill", "colour", "alpha",
                      "linetype", "linewidth", "size"),
                    names(data))
  if (length(cols) == 0) {
    data$group <- 1L
    return(data)
  }

  key <- do.call(paste, c(unname(data[, cols, drop = FALSE]), list(sep = "\r")))
  data$group <- match(key, unique(key))

  data
}


# ==============================================================================
# 径向标签的角向排布
#
# circlize 的 `circos.genomicLabels()` 走的是一条统一规则：每个标签按自己的
# 文字高度占一个最小的角向宽度，和邻居撞上了就整体平移开（最小位移、保持先后
# 顺序）。所有标签用同一个规则、同一个间距，因此图上的标签和连接线看起来是
# 一致的，不会出现"某些标签被单独拉开"的现象。
# ==============================================================================

#' Angular footprint of one radial label
#'
#' A radially oriented label occupies `text height` along the arc, not its width,
#' so its footprint depends on the font size and on the radius it is drawn at.
#'
#' @param fontsize Font size in points.
#' @param r Radius (in radius units, the `[-1, 1]` circle) of the labels.
#' @param ratio Line height multiplier used for the text box.
#' @return Angular width in degrees.
#' @noRd
arc_label_gap <- function(fontsize, r, ratio = 1.45) {
  r <- max(abs(r), 1e-6)
  # 1 radius unit = 0.5 npc (the circle spans 2 units over the whole panel)
  h.npc <- (fontsize / 72 * ratio) / max(grDevices::dev.size("in")[2], 1e-6)
  h.r <- h.npc / 0.5
  deg <- 2 * asin(pmin(1, (h.r / 2) / r)) * 180 / pi
  if (!is.finite(deg) || deg <= 0) deg <- 1
  deg
}

#' Push angles apart with a uniform minimal gap
#'
#' Solves "keep every pair at least `gap` apart, and move the labels as little as
#' possible" exactly: the circle is opened at its largest empty gap, the constraint
#' is turned into a monotonicity constraint by subtracting the cumulative gap, and
#' the least-squares monotone fit is obtained with pool-adjacent-violators. A plain
#' forward sweep moves everything downstream of a dense cluster (a few labels end
#' up tens of degrees away, dragging long connectors across their neighbours);
#' the least-squares fit keeps every label as close to its own gene as the
#' spacing allows.
#'
#' @param angle Angles in degrees.
#' @param gap Required gap in degrees.
#' @param period Circle period in degrees.
#' @return Adjusted angles, in the same order as `angle`.
#' @noRd
arc_smart_align <- function(angle, gap, period = 360) {
  n <- length(angle)
  if (n < 2 || !is.finite(gap) || gap <= 0) return(angle)

  a <- angle %% period
  o <- order(a)
  d <- a[o]

  # open the circle at its widest empty gap so the sequence is strictly increasing
  gaps <- diff(c(d, d[1] + period))
  k <- which.max(gaps)
  if (k < n) {
    d <- c(d[(k + 1):n], d[1:k] + period)
    o <- c(o[(k + 1):n], o[1:k])
  }

  # x[i] >= x[i-1] + gap  <=>  y[i] = x[i] - (i-1)*gap is non-decreasing
  off <- (seq_len(n) - 1) * gap
  y <- d - off

  # pool adjacent violators (unit weights)
  val <- numeric(0)
  cnt <- numeric(0)
  for (i in seq_len(n)) {
    v <- y[i]
    w <- 1
    while (length(val) > 0 && val[length(val)] > v) {
      v <- (val[length(val)] * cnt[length(cnt)] + v * w) / (cnt[length(cnt)] + w)
      w <- cnt[length(cnt)] + w
      val <- val[-length(val)]
      cnt <- cnt[-length(cnt)]
    }
    val <- c(val, v)
    cnt <- c(cnt, w)
  }
  x <- rep(val, cnt) + off

  out <- numeric(n)
  out[o] <- x %% period
  # keep the original branch (+/- 360) so a label never hops to the far side
  hop <- out - (angle %% period)
  hop <- hop - period * round(hop / period)
  angle + hop
}

#' Convert an angle back into a data value for a sector
#'
#' Inverse of [arc_theta()]: used to place a label at a chosen angle.
#'
#' @noRd
arc_deg_to_x <- function(deg, xscale, start, end, extend.xscale = 0.05) {
  span <- end - start
  ext <- span * extend.xscale
  scales::rescale(deg, from = c(start + ext, end - ext), to = xscale)
}
