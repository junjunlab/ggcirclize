#' Create a circular axis graphical object
#'
#' `arcAxisGrob()` draws an axis along an arc or along a radius, in the same
#' spirit as `circlize::circos.axis()` / `circlize::circos.yaxis()`. Argument
#' names follow circlize where it makes sense (`labels.cex`, `labels.facing`,
#' `labels.niceFacing`, `labels.col`, `labels.font`, `labels.digits`,
#' `labels.pos.adjust`), so a plot can be tuned with the vocabulary you already
#' know from circlize.
#'
#' @param start,end Sector limits in degrees.
#' @param r0,r1 Inner and outer radius of the track.
#' @param x0,y0 Centre of the unit circle.
#' @param ticks.len Length of the major ticks. Defaults to `0.03` for the
#'   angular axis and `3` for the radial axis.
#' @param n Number of points used to approximate the arc.
#' @param clock.wise Whether the sector is drawn clockwise.
#' @param axis.type Either `"x"` (angular axis) or `"y"` (radial axis).
#' @param nice.facing Deprecated, use `labels.niceFacing`.
#' @param pos Where the axis sits: `"top"`/`"bottom"` for an angular axis,
#'   `"left"`/`"right"` for a radial axis.
#' @param xscale,yscale Data range the axis covers.
#' @param breaks.n Approximate number of major breaks when `breaks` is `NULL`.
#' @param minor.ticks.n Number of minor ticks between two major ticks. When
#'   `NULL` (default) the angular axis uses 5 and the radial axis uses 0, which
#'   mirrors `circlize` (`circos.yaxis()` draws no minor ticks).
#' @param major.by.degree Minimal angular distance, in degrees, between two
#'   major ticks. Break positions are derived from it and then rounded to a
#'   readable interval, exactly like `circos.par("major.by.degree")` does. Set
#'   it to `NULL` to fall back to `breaks.n` pretty breaks.
#' @param breaks Major tick positions. Defaults to pretty breaks of the scale.
#' @param breaks.label Labels for the major ticks. Defaults to the formatted
#'   break values.
#' @param label.space Gap between a tick and its label.
#' @param labels Draw labels at all. Can also be a logical vector, as in
#'   circlize.
#' @param labels.cex Size multiplier for the axis labels.
#' @param labels.facing Text orientation, see `arc_text_geometry()`. Defaults to
#'   `"inside"`, i.e. reading along the arc.
#' @param labels.niceFacing Flip labels that would be upside down.
#' @param labels.col Colour of the axis labels.
#' @param labels.font Font face of the axis labels.
#' @param labels.digits Significant digits used when formatting the break values.
#' @param labels.format Optional function used to format the break values, for
#'   example `function(x) paste0(x / 1e6, "Mb")`.
#' @param labels.pos.adjust Nudge the first and the last label inwards when they
#'   overhang the sector, the same way `circlize::circos.axis()`'s
#'   `labels.pos.adjust` does.
#' @param direction `"outside"` (default) or `"inside"`; overrides the side
#'   implied by `pos`.
#' @param lwd,col Line width and colour of the axis line, ticks and labels.
#' @param corner.trim Drop the outermost radial label, for when an angular axis
#'   is drawn on the same sector and both labels would collide in the corner.
#' @param pos.adjust Deprecated, use `labels.pos.adjust`.
#' @param x.gp,y.gp Graphical parameters for the axis line and ticks.
#' @param x.label.gp,y.label.gp Graphical parameters for the labels.
#' @param extend.xscale,extend.yscale Fraction of the sector kept free at both
#'   ends.
#' @param name,gp,vp Standard grid graphical object arguments.
#'
#' @return A `grob` of class `arcAxisGrob`.
#'
#' @examples
#' \dontrun{
#' grid.draw(arcAxisGrob(start = 0, end = 120, r0 = 0.7, r1 = 1,
#'                       axis.type = "x", pos = "top"))
#' }
#'
#' @importFrom grid convertWidth gTree gpar grid.pretty linesGrob nullGrob
#'   segmentsGrob stringWidth textGrob
#' @export
arcAxisGrob <- function(start = 0, end = 60,
                        r0 = 0.5, r1 = 1,
                        x0 = 0, y0 = 0,
                        ticks.len = NULL,
                        n = 100, clock.wise = FALSE,
                        axis.type = c("x", "y"),
                        nice.facing = NULL,
                        pos = c("left", "right", "top", "bottom"),
                        xscale = NULL, yscale = c(0, 1),
                        breaks.n = 5,
                        minor.ticks.n = NULL,
                        major.by.degree = 20,
                        breaks = NULL,
                        breaks.label = NULL,
                        label.space = NULL,
                        labels = TRUE,
                        labels.cex = 1,
                        labels.facing = NULL,
                        labels.niceFacing = TRUE,
                        labels.col = NULL,
                        labels.font = 1,
                        labels.digits = 3,
                        labels.format = NULL,
                        labels.pos.adjust = NULL,
                        direction = NULL,
                        lwd = NULL,
                        col = NULL,
                        corner.trim = FALSE,
                        pos.adjust = NULL,
                        x.gp = NULL, y.gp = NULL,
                        x.label.gp = gpar(fontsize = 8),
                        y.label.gp = gpar(fontsize = 8),
                        extend.xscale = 0.05,
                        extend.yscale = 0.05,
                        name = NULL,
                        gp = NULL, vp = NULL) {

  axis.type <- match.arg(axis.type, c("x", "y"))
  pos <- match.arg(pos, c("left", "right", "top", "bottom"))

  # `nice.facing` / `pos.adjust` are the pre-0.1 spellings
  if (!is.null(nice.facing)) labels.niceFacing <- nice.facing
  if (is.null(labels.pos.adjust)) labels.pos.adjust <- pos.adjust %||% TRUE

  lst <- list(start = start, end = end,
              r0 = r0, r1 = r1,
              x0 = x0, y0 = y0,
              ticks.len = ticks.len,
              n = n, clock.wise = clock.wise,
              axis.type = axis.type,
              nice.facing = nice.facing,
              pos = pos,
              xscale = xscale, yscale = yscale,
              breaks.n = breaks.n,
              minor.ticks.n = minor.ticks.n,
              major.by.degree = major.by.degree,
              breaks = breaks,
              breaks.label = breaks.label,
              label.space = label.space,
              labels = labels,
              labels.cex = labels.cex,
              labels.facing = labels.facing,
              labels.niceFacing = labels.niceFacing,
              labels.col = labels.col,
              labels.font = labels.font,
              labels.digits = labels.digits,
              labels.format = labels.format,
              labels.pos.adjust = labels.pos.adjust,
              direction = direction,
              lwd = lwd,
              col = col,
              corner.trim = corner.trim,
              pos.adjust = pos.adjust,
              x.gp = x.gp, y.gp = y.gp,
              x.label.gp = x.label.gp,
              y.label.gp = y.label.gp,
              extend.xscale = extend.xscale,
              extend.yscale = extend.yscale,
              name = name, gp = gp, vp = vp,
              cl = "arcAxisGrob")

  do.call(gTree, lst)
}


#' @method makeContent arcAxisGrob
#' @export
makeContent.arcAxisGrob <- function(x) {
  g <- .arcAxisGrob(start = x$start, end = x$end,
                    r0 = x$r0, r1 = x$r1,
                    x0 = x$x0, y0 = x$y0,
                    ticks.len = x$ticks.len,
                    n = x$n, clock.wise = x$clock.wise,
                    axis.type = x$axis.type,
                    nice.facing = x$nice.facing,
                    pos = x$pos,
                    xscale = x$xscale, yscale = x$yscale,
                    breaks.n = x$breaks.n,
                    minor.ticks.n = x$minor.ticks.n,
                    major.by.degree = x$major.by.degree %||% 20,
                    breaks = x$breaks,
                    breaks.label = x$breaks.label,
                    label.space = x$label.space,
                    labels = x$labels %||% TRUE,
                    labels.cex = x$labels.cex %||% 1,
                    labels.facing = x$labels.facing,
                    labels.niceFacing = x$labels.niceFacing %||% TRUE,
                    labels.col = x$labels.col,
                    labels.font = x$labels.font %||% 1,
                    labels.digits = x$labels.digits %||% 3,
                    labels.format = x$labels.format,
                    labels.pos.adjust = x$labels.pos.adjust %||% TRUE,
                    direction = x$direction,
                    lwd = x$lwd, col = x$col,
                    corner.trim = x$corner.trim %||% FALSE,
                    pos.adjust = x$pos.adjust,
                    x.gp = x$x.gp, y.gp = x$y.gp,
                    x.label.gp = x$x.label.gp,
                    y.label.gp = x$y.label.gp,
                    extend.xscale = x$extend.xscale,
                    extend.yscale = x$extend.yscale,
                    name = x$name, gp = x$gp, vp = x$vp)
  grid::setChildren(x, children = g$children)
}


# ==============================================================================
# 内部实现
# ==============================================================================

#' @noRd
#' @importFrom grid segmentsGrob linesGrob textGrob nullGrob gpar
.arcAxisGrob <- function(start = 0, end = 60,
                         r0 = 0.5, r1 = 1,
                         x0 = 0, y0 = 0,
                         ticks.len = NULL,
                         n = 100, clock.wise = FALSE,
                         axis.type = c("x", "y"),
                         nice.facing = NULL,
                         pos = c("left", "right", "top", "bottom"),
                         xscale = NULL, yscale = c(0, 1),
                         breaks.n = 5,
                         minor.ticks.n = NULL,
                         major.by.degree = 20,
                         breaks = NULL,
                         breaks.label = NULL,
                         label.space = NULL,
                         labels = TRUE,
                         labels.cex = 1,
                         labels.facing = NULL,
                         labels.niceFacing = TRUE,
                         labels.col = NULL,
                         labels.font = 1,
                         labels.digits = 3,
                         labels.format = NULL,
                         labels.pos.adjust = TRUE,
                         direction = NULL,
                         lwd = NULL,
                         col = NULL,
                         corner.trim = FALSE,
                         pos.adjust = NULL,
                         x.gp = NULL, y.gp = NULL,
                         x.label.gp = gpar(fontsize = 8),
                         y.label.gp = gpar(fontsize = 8),
                         extend.xscale = 0.05,
                         extend.yscale = 0.05,
                         name = NULL,
                         gp = NULL, vp = NULL) {

  axis.type <- match.arg(axis.type, c("x", "y"))
  pos <- match.arg(pos, c("left", "right", "top", "bottom"))
  if (!is.null(nice.facing)) labels.niceFacing <- nice.facing
  if (!is.null(pos.adjust)) labels.pos.adjust <- pos.adjust

  # ---------------------------------------------------------------- 尺度扩展
  ext.theta <- (end - start) * extend.xscale
  s.ed <- start + ext.theta
  e.ed <- end - ext.theta

  ext.r <- (r1 - r0) * extend.yscale
  r0.ed <- r0 + ext.r
  r1.ed <- r1 - ext.r

  if (axis.type == "x") {
    if (is.null(ticks.len)) ticks.len <- 0.03
    if (is.null(label.space)) label.space <- 0.03
    # circlize 默认每个主刻度之间 4 个次刻度
    if (is.null(minor.ticks.n)) minor.ticks.n <- 5
  } else {
    if (is.null(ticks.len)) ticks.len <- 3
    if (is.null(label.space)) label.space <- 3
    if (is.null(minor.ticks.n)) minor.ticks.n <- 5
  }

  # 轴线方向：pos 决定默认值，direction 可覆盖
  sign.dir <- if (pos %in% c("top", "right")) 1 else -1
  if (!is.null(direction)) {
    direction <- match.arg(direction, c("outside", "inside"))
    sign.dir <- if (direction == "outside") 1 else -1
  }

  facing.auto <- is.null(labels.facing)
  if (is.null(labels.facing)) {
    # 角向轴沿弧读；径向轴在下面按实际角度决定（见 y 分支）
    labels.facing <- if (axis.type == "y") "downward" else "inside"
  }
  labels.facing <- match.arg(labels.facing,
                             c("inside", "outside", "reverse.clockwise",
                               "clockwise", "downward"))

  # 标签图层参数：以传入的 gp 为底，再叠加 circlize 风格的覆盖项
  lab.gp <- if (axis.type == "x") x.label.gp else y.label.gp
  lab.gp <- lab.gp %||% gpar()
  lab.gp <- do.call(gpar, as.list(lab.gp))
  if (!is.null(labels.col)) lab.gp$col <- labels.col
  if (!is.null(col)) lab.gp$col <- labels.col %||% col
  if (!is.null(labels.font)) lab.gp$font <- labels.font
  if (!is.null(lab.gp$fontsize) && !is.null(labels.cex)) {
    lab.gp$fontsize <- lab.gp$fontsize * labels.cex
  }

  line.gp <- if (axis.type == "x") x.gp else y.gp
  line.gp <- line.gp %||% gpar()
  line.gp <- do.call(gpar, as.list(line.gp))
  if (!is.null(col)) line.gp$col <- col
  if (!is.null(lwd)) line.gp$lwd <- lwd

  # ============================================================ 角度 → npc
  npc <- function(v) scales::rescale(v, to = c(0, 1), from = c(-1, 1))

  # 数据值 → 角度（弧度），与所有图元共用同一个映射
  ang <- function(v) arc_theta(v, xscale, s.ed, e.ed, clock.wise = clock.wise)

  # ============================================================ 角向轴 (x)
  if (axis.type == "x") {
    angle.only <- is.null(xscale)
    if (angle.only) xscale <- c(start, end)

    at <- breaks %||% arc_major_breaks(xscale, e.ed - s.ed, major.by.degree)
    at <- at[at >= min(xscale) & at <= max(xscale)]
    if (!length(at)) at <- mean(xscale)

    theta <- ang(at)

    r.line <- if (sign.dir > 0) r1.ed else r0.ed
    r.tick <- r.line + sign.dir * ticks.len
    r.lab <- r.line + sign.dir * (ticks.len + label.space)

    # ---- 轴线 ----
    main.theta <- ang(c(min(xscale), max(xscale)))
    arc.theta.seq <- seq(main.theta[1], main.theta[2], length.out = max(n, 2))
    main.grob <- linesGrob(x = npc(x0 + r.line * cos(arc.theta.seq)),
                           y = npc(y0 + r.line * sin(arc.theta.seq)),
                           default.units = "npc",
                           gp = line.gp,
                           name = "arc.x.main")

    # ---- 主刻度 ----
    tick.grob <- segmentsGrob(x0 = npc(x0 + r.line * cos(theta)),
                              x1 = npc(x0 + r.tick * cos(theta)),
                              y0 = npc(y0 + r.line * sin(theta)),
                              y1 = npc(y0 + r.tick * sin(theta)),
                              default.units = "npc",
                              gp = line.gp,
                              name = "arc.x.ticks")

    # ---- 次刻度 ----
    minor.theta <- NULL
    if (length(at) >= 2 && minor.ticks.n > 0) {
      k <- seq_len(minor.ticks.n) / (minor.ticks.n + 1)
      minor.at <- as.numeric(outer(diff(at), k, "*") + at[-length(at)])
      minor.theta <- ang(minor.at)
      minor.theta <- minor.theta[!is.na(minor.theta)]
    }

    if (length(minor.theta)) {
      r.minor <- r.line + sign.dir * ticks.len / 2
      minor.grob <- segmentsGrob(x0 = npc(x0 + r.line * cos(minor.theta)),
                                 x1 = npc(x0 + r.minor * cos(minor.theta)),
                                 y0 = npc(y0 + r.line * sin(minor.theta)),
                                 y1 = npc(y0 + r.minor * sin(minor.theta)),
                                 default.units = "npc",
                                 gp = line.gp,
                                 name = "arc.x.ticks.minor")
    } else {
      minor.grob <- nullGrob()
    }

    # ---- 标签 ----
    lab <- if (!is.null(breaks.label)) {
      rep_len(as.character(breaks.label), length(at))
    } else if (angle.only) {
      paste0(arc_format_labels(at, labels.digits, labels.format), "\u00b0")
    } else {
      arc_format_labels(at, labels.digits, labels.format)
    }

    if (length(labels) > 1) {
      keep <- rep_len(labels, length(at))
      at <- at[keep]; theta <- theta[keep]; lab <- lab[keep]
    }

    # 只有一个刻度时标签不携带任何信息（常见于 sector 与 x 同为一个离散变量，
    # 分面标签已经写明类别），直接不画，避免和分面标签重复
    if (length(at) < 2) {
      label.grob <- nullGrob()
      return(grid::gTree(children = grid::gList(main.grob, minor.grob,
                                                tick.grob, label.grob),
                         name = "arcAxisGrob"))
    }

    if (isTRUE(labels[1]) && length(theta)) {
      # 标签过密时按实测文字宽度抽稀（放不下就少画几个）
      if (length(theta) >= 2) {
        half.w <- convertWidth(stringWidth(lab), "npc", valueOnly = TRUE) / 2
        arc.span <- abs(diff(range(arc.theta.seq))) * abs(r.line) * arc_npc_per_radius()
        lab.size <- max(convertWidth(stringWidth(lab), "npc", valueOnly = TRUE))
        keep <- thin_label_index(length(theta), arc.span, lab.size)

        if (length(keep) < length(theta)) {
          theta <- theta[keep]; at <- at[keep]; lab <- lab[keep]
        }
      }

      # 首尾标签越界 → 沿弧往扇区内挪（circlize 的 labels.pos.adjust）
      if (isTRUE(labels.pos.adjust) && length(theta) >= 2) {
        half.w <- convertWidth(stringWidth(lab), "npc", valueOnly = TRUE) / 2
        half.ang <- half.w / (max(abs(r.lab), 1e-6) * arc_npc_per_radius())

        edge <- main.theta
        inward <- sign(theta[2] - theta[1])

        for (k in c(1, length(theta))) {
          if (!is.finite(theta[k])) next
          over <- half.ang[k] - abs(theta[k] - edge[if (k == 1) 1 else 2])

          nb <- if (k == 1) 2 else length(theta) - 1
          room <- abs(theta[nb] - theta[k]) - half.ang[k] - half.ang[nb]
          over <- min(over, max(0, room) / 2)

          if (is.finite(over) && over > 0) theta[k] <- theta[k] + inward * over
        }
      }

      # 与径向轴同处扇区拐角时，省掉最外侧的标签
      if (isTRUE(corner.trim) && length(lab) > 2) {
        lab <- lab[-length(lab)]
        theta <- theta[-length(theta)]
      }

      geo <- arc_text_geometry(theta, labels.facing, labels.niceFacing,
                               adj = if (sign.dir > 0) c(0.5, 0) else c(0.5, 1))

      label.grob <- textGrob(label = lab,
                             x = npc(x0 + r.lab * cos(theta)),
                             y = npc(y0 + r.lab * sin(theta)),
                             rot = geo$rot,
                             hjust = geo$hjust,
                             vjust = geo$vjust,
                             default.units = "npc",
                             gp = lab.gp,
                             name = "arc.x.labels")
    } else {
      label.grob <- nullGrob()
    }

    return(grid::gTree(children = grid::gList(main.grob, minor.grob,
                                              tick.grob, label.grob),
                       name = "arcAxisGrob"))
  }

  # ============================================================ 径向轴 (y)
  # 窄扇区放不下 5 个径向标签，按角度跨度自动减少刻度数量
  n.breaks <- arc_breaks_n_for_span(abs(e.ed - s.ed), breaks.n)
  at <- breaks %||% arc_pretty_breaks(yscale, n.breaks)
  at <- at[at >= min(yscale) & at <= max(yscale)]
  if (!length(at)) at <- mean(yscale)

  r.at <- arc_radius(at, yscale, r0.ed, r1.ed)

  d.tick <- if (pos == "left") e.ed else s.ed
  d.tick.end <- d.tick + sign.dir * ticks.len
  d.lab <- d.tick + sign.dir * (ticks.len + label.space)

  rad <- function(d) if (isTRUE(clock.wise)) 2 * pi - d * pi / 180 else d * pi / 180
  th.tick <- rad(d.tick)
  th.tick.end <- rad(d.tick.end)
  th.lab <- rad(d.lab)

  # ---- 轴线：沿半径方向 ----
  main.grob <- segmentsGrob(x0 = npc(x0 + min(r.at) * cos(th.tick)),
                            x1 = npc(x0 + max(r.at) * cos(th.tick)),
                            y0 = npc(y0 + min(r.at) * sin(th.tick)),
                            y1 = npc(y0 + max(r.at) * sin(th.tick)),
                            default.units = "npc",
                            gp = line.gp,
                            name = "arc.y.main")

  # ---- 主刻度 ----
  tick.grob <- segmentsGrob(x0 = npc(x0 + r.at * cos(th.tick)),
                            x1 = npc(x0 + r.at * cos(th.tick.end)),
                            y0 = npc(y0 + r.at * sin(th.tick)),
                            y1 = npc(y0 + r.at * sin(th.tick.end)),
                            default.units = "npc",
                            gp = line.gp,
                            name = "arc.y.ticks")

  # ---- 次刻度 ----
  # 径向次刻度间距太小时自动关掉，否则短轴上会糊成一排梳齿
  if (minor.ticks.n > 0 && length(at) >= 2) {
    span.npc <- abs(diff(range(r.at))) 
    gaps <- (length(at) - 1) * (minor.ticks.n + 1)
    if (!is.finite(span.npc) || span.npc / max(gaps, 1) < 0.01) minor.ticks.n <- 0
  }

  minor.r <- NULL
  if (length(at) >= 2 && minor.ticks.n > 0) {
    k <- seq_len(minor.ticks.n) / (minor.ticks.n + 1)
    minor.at <- as.numeric(outer(diff(at), k, "*") + at[-length(at)])
    minor.r <- arc_radius(minor.at, yscale, r0.ed, r1.ed)
    minor.r <- minor.r[!is.na(minor.r)]
  }

  if (length(minor.r)) {
    th.minor.end <- rad(d.tick + sign.dir * ticks.len / 2)
    minor.grob <- segmentsGrob(x0 = npc(x0 + minor.r * cos(th.tick)),
                               x1 = npc(x0 + minor.r * cos(th.minor.end)),
                               y0 = npc(y0 + minor.r * sin(th.tick)),
                               y1 = npc(y0 + minor.r * sin(th.minor.end)),
                               default.units = "npc",
                               gp = line.gp,
                               name = "arc.y.ticks.minor")
  } else {
    minor.grob <- nullGrob()
  }

  # ---- 标签 ----
  lab <- if (!is.null(breaks.label)) {
    rep_len(as.character(breaks.label), length(at))
  } else {
    arc_format_labels(at, labels.digits, labels.format)
  }

  if (isTRUE(labels[1]) && length(r.at)) {
    keep <- rep_len(TRUE, length(r.at))
    if (length(labels) > 1) keep <- rep_len(labels, length(r.at))

    r.at <- r.at[keep]; lab <- lab[keep]

    # 径向标签过密时抽稀
    if (length(r.at) >= 2) {
      # 径向标签是旋转的，占位取宽高里较大的那个
      lab.size <- max(pmax(
        convertWidth(stringWidth(lab), "npc", valueOnly = TRUE),
        convertHeight(stringHeight(lab), "npc", valueOnly = TRUE)
      ))
      span <- abs(max(r.at) - min(r.at)) * arc_npc_per_radius()
      idx <- thin_label_index(length(r.at), span, lab.size)
      if (length(idx) < length(r.at)) {
        r.at <- r.at[idx]; lab <- lab[idx]
      }
    }

    if (isTRUE(corner.trim) && length(lab) > 2) {
      # 最靠内/最靠外的那一个会和角向轴的标签撞在一起
      drop <- if (sign.dir > 0) length(lab) else 1
      lab <- lab[-drop]; r.at <- r.at[-drop]
    }

    # 首尾标签越过扇区径向边界时按半个文字宽内收（对应 labels.pos.adjust）
    if (isTRUE(labels.pos.adjust) && length(r.at) >= 2) {
      halfw <- convertWidth(stringWidth(lab), "npc", valueOnly = TRUE) / 2 /
        max(arc_npc_per_radius(), 1e-6)

      r.lo <- min(r0.ed, r1.ed)
      r.hi <- max(r0.ed, r1.ed)
      ord <- order(r.at)

      lo <- ord[1]
      hi <- ord[length(ord)]

      # 不要把自己推到相邻标签身上
      room.lo <- (r.at[ord[2]] - r.at[lo]) - halfw[lo] - halfw[ord[2]]
      room.hi <- (r.at[hi] - r.at[ord[length(ord) - 1]]) - halfw[hi] -
        halfw[ord[length(ord) - 1]]

      r.at[lo] <- r.at[lo] + max(0, min(r.lo + halfw[lo] - r.at[lo], room.lo / 2))
      r.at[hi] <- r.at[hi] - max(0, min(r.at[hi] - (r.hi - halfw[hi]), room.hi / 2))
    }

    # 自动选择朝向：切向倾角在 ±60 度以内就跟轴走，否则转成水平，
    # 免得扇区端部（切向接近竖直）出现竖排数字
    if (facing.auto) {
      srt <- (th.lab * 180 / pi - 90) %% 360
      if (srt > 90) srt <- srt - 180
      labels.facing <- if (abs(srt) > 60) "downward" else "inside"
    }

    geo <- arc_text_geometry(rep(th.lab, length(r.at)), labels.facing,
                             labels.niceFacing,
                             adj = if (pos == "left") c(1, 0.5) else c(0, 0.5))

    label.grob <- textGrob(label = lab,
                           x = npc(x0 + r.at * cos(th.lab)),
                           y = npc(y0 + r.at * sin(th.lab)),
                           rot = geo$rot,
                           hjust = geo$hjust,
                           vjust = geo$vjust,
                           default.units = "npc",
                           gp = lab.gp,
                           name = "arc.y.labels")
  } else {
    label.grob <- nullGrob()
  }

  grid::gTree(children = grid::gList(main.grob, minor.grob, tick.grob, label.grob),
              name = "arcAxisGrob")
}
