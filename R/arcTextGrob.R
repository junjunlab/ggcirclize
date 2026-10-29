#' Create custom arc text labels graphical object.
#'
#' `arcTextGrob()` places labels on an arc. Text orientation follows the same
#' vocabulary as `circlize::circos.text()`: `facing` can be `"inside"`,
#' `"outside"`, `"clockwise"`, `"reverse.clockwise"`, `"downward"`,
#' `"bending.inside"` or `"bending.outside"`, and `nice.facing` flips labels
#' that would otherwise be upside down.
#'
#' @param x The x-coordinates of the text labels. Default is NULL.
#' @param y The y-coordinates of the text labels. Default is NULL.
#' @param labels The text labels to be displayed. Default is NULL.
#' @param shift The factor by which the labels are shifted along the arc. Default is 0.5.
#' @param start The starting angle of the arc (in degrees). Default is 0.
#' @param end The ending angle of the arc (in degrees). Default is 60.
#' @param r0 The inner radius of the arc. Default is 0.5.
#' @param r1 The outer radius of the arc. Default is 1.
#' @param x0 The x-coordinate of the center of the unit circle. Default is 0.
#' @param y0 The y-coordinate of the center of the unit circle. Default is 0.
#' @param text.gp A graphical parameter object for the text labels' appearance.
#'   Default is NULL.
#' @param facing Text orientation, see `arc_text_geometry()`. When `NULL` it is
#'   derived from `curved.label` and `inward`.
#' @param nice.facing Flip labels that would be upside down. Default is TRUE.
#' @param inward Put the labels on the inner side of the arc. Default is FALSE.
#' @param clock.wise A logical value indicating whether the arc is drawn in a
#'   clockwise direction. Default is FALSE.
#' @param auto.fit Rotate a label into the radial direction when it is wider
#'   than the arc it sits on, instead of letting it run into its neighbours.
#'   Default is TRUE.
#' @param adj Justification of the labels, as in [grid::textGrob()]. Useful with
#'   radial labels: `c(0, 0.5)` makes the text grow outwards from its anchor and
#'   `c(1, 0.5)` inwards. Default is `c(0.5, 0.5)`.
#' @param curved.label Lay the label out character by character along the arc
#'   (`bending.inside`). Default is FALSE.
#' @param extend.xscale The extension factor for the scaling of the arc. Default is 0.05.
#' @param extend.yscale The extension factor for the scaling of the arc. Default is 0.05.
#' @param xscale The scaling range for the x-axis. Default is NULL.
#' @param yscale The scaling range for the y-axis. Default is NULL.
#' @param name The name of the arc text labels graphical object. Default is NULL.
#' @param gp A graphical parameter object for the arc text labels. Default is NULL.
#' @param vp A viewport object for the arc text labels. Default is NULL.
#'
#' @return A graphical object of class 'grob'.
#'
#' @examples
#' \dontrun{
#' arc_text_labels <- arcTextGrob(x = c(0, 10, 20), y = c(0, 0, 0),
#'                                labels = c("A", "B", "C"))
#' grid.draw(arc_text_labels)
#' }
#'
#' @export
arcTextGrob <- function(x = NULL, y = NULL,
                        labels = NULL, shift = 0.5,
                        start = 0, end = 60,
                        r0 = 0.5, r1 = 1,
                        x0 = 0, y0 = 0,
                        text.gp = NULL,
                        facing = NULL,
                        nice.facing = TRUE,
                        inward = FALSE,
                        clock.wise = FALSE,
                        curved.label = FALSE,
                        auto.fit = TRUE,
                        adj = c(0.5, 0.5),
                        extend.xscale = 0.05,
                        extend.yscale = 0.05,
                        xscale = NULL,
                        yscale = NULL,
                        name = NULL,
                        gp = NULL, vp = NULL) {

  lst <- list(x = x, y = y,
              labels = labels, shift = shift,
              start = start, end = end,
              r0 = r0, r1 = r1,
              x0 = x0, y0 = y0,
              text.gp = text.gp,
              facing = facing,
              nice.facing = nice.facing,
              inward = inward,
              clock.wise = clock.wise,
              curved.label = curved.label,
              auto.fit = auto.fit,
              adj = adj,
              extend.xscale = extend.xscale,
              extend.yscale = extend.yscale,
              xscale = xscale,
              yscale = yscale,
              name = name, gp = gp, vp = vp,
              cl = "arcTextGrob")

  do.call(gTree, lst)
}


#' @method makeContent arcTextGrob
#' @export
makeContent.arcTextGrob <- function(x) {
  g <- .arcTextGrob(x = x$x, y = x$y,
                    labels = x$labels, shift = x$shift,
                    start = x$start, end = x$end,
                    r0 = x$r0, r1 = x$r1,
                    x0 = x$x0, y0 = x$y0,
                    text.gp = x$text.gp,
                    facing = x$facing,
                    nice.facing = x$nice.facing %||% TRUE,
                    inward = x$inward %||% FALSE,
                    clock.wise = x$clock.wise,
                    curved.label = x$curved.label %||% FALSE,
                    auto.fit = x$auto.fit %||% TRUE,
                    adj = x$adj %||% c(0.5, 0.5),
                    extend.xscale = x$extend.xscale,
                    extend.yscale = x$extend.yscale,
                    xscale = x$xscale,
                    yscale = x$yscale,
                    name = x$name, gp = x$gp, vp = x$vp)
  grid::setChildren(x, children = g$children)
}


#' Resolve the facing of an arc text label
#'
#' @param facing Explicit `facing`, may be `NULL`.
#' @param curved.label Lay the text out along the arc.
#' @param inward Put the label on the inner side.
#' @return A single facing string.
#' @noRd
arc_resolve_facing <- function(facing = NULL, curved.label = FALSE,
                               inward = FALSE) {
  if (!is.null(facing)) {
    facing <- match.arg(facing,
                        c("inside", "outside", "reverse.clockwise", "clockwise",
                          "downward", "bending", "bending.inside",
                          "bending.outside"))
    if (facing == "bending") facing <- "bending.inside"
    return(facing)
  }

  if (isTRUE(curved.label)) return("bending.inside")

  if (isTRUE(inward)) "outside" else "inside"
}


#' @noRd
.arcTextGrob <- function(x = NULL, y = NULL,
                         labels = NULL, shift = 0.5,
                         start = 0, end = 60,
                         r0 = 0.5, r1 = 1,
                         x0 = 0, y0 = 0,
                         text.gp = NULL,
                         facing = NULL,
                         nice.facing = TRUE,
                         inward = FALSE,
                         clock.wise = FALSE,
                         curved.label = FALSE,
                         auto.fit = TRUE,
                         adj = c(0.5, 0.5),
                         extend.xscale = 0.05,
                         extend.yscale = 0.05,
                         xscale = NULL,
                         yscale = NULL,
                         name = NULL,
                         gp = NULL, vp = NULL) {

  facing <- arc_resolve_facing(facing, curved.label, inward)

  npc <- function(v) scales::rescale(v, to = c(0, 1), from = c(-arc_r_limit, arc_r_limit))

  # ---------------------------------------------------------------- 尺度扩展
  ext.theta <- (end - start) * extend.xscale
  s.ed <- start + ext.theta
  e.ed <- end - ext.theta

  ext.r <- (r1 - r0) * extend.yscale
  r0.ed <- r0 + ext.r
  r1.ed <- r1 - ext.r

  # 分面标签往往只给一个 x/y，range() 会退化，这里对称展开
  thata.scale <- xscale %||% range(x)
  if (diff(thata.scale) == 0) thata.scale <- thata.scale + c(-1, 1)

  r.scale <- yscale %||% range(y)
  if (diff(r.scale) == 0) r.scale <- r.scale + c(-1, 1)

  theta <- arc_theta(x, thata.scale, s.ed, e.ed, clock.wise = clock.wise)
  r <- arc_radius(y, r.scale, r0.ed, r1.ed)

  n <- length(theta)
  labels <- rep_len(labels, n)
  r <- rep_len(r, n)

  text.gp <- text.gp %||% gpar()
  base.fontsize <- text.gp$fontsize %||% 12

  # ---------------------------------------------------------------- 逐字排布
  if (grepl("bending", facing)) {
    parts <- lapply(seq_len(n), function(i) {
      arc_bending_chars(theta[i], r[i], labels[i],
                        facing = facing,
                        adj = c(shift, 0.5),
                        cex = base.fontsize / 12)
    })

    keep <- !vapply(parts, is.null, logical(1))
    if (!any(keep)) return(grid::gTree(children = grid::gList(), name = "arcTextGrob"))

    theta.c <- unlist(lapply(parts[keep], `[[`, "theta"))
    rot <- unlist(lapply(parts[keep], `[[`, "rot"))
    hjust <- unlist(lapply(parts[keep], `[[`, "hjust"))
    rr <- rep(r[keep], vapply(parts[keep], function(p) nrow(p), integer(1)))

    chars <- strsplit(labels[keep], "")
    if (facing == "bending.outside") chars <- lapply(chars, rev)
    lab <- unlist(chars)

    text.grob <- textGrob(label = lab,
                          x = npc(x0 + rr * cos(theta.c)),
                          y = npc(y0 + rr * sin(theta.c)),
                          rot = rot,
                          hjust = hjust,
                          default.units = "npc",
                          gp = text.gp,
                          name = "labels")

    return(grid::gTree(children = grid::gList(text.grob), name = "arcTextGrob"))
  }

  # ---------------------------------------------------------------- 整段排布
  geo <- arc_text_geometry(theta, facing, nice.facing, adj = adj)
  rot <- geo$rot
  hjust <- geo$hjust
  vjust <- geo$vjust

  # 放不下就改成径向排布，还不够再缩字号
  if (isTRUE(auto.fit) && n > 0) {
    lab.w <- convertWidth(stringWidth(labels), "npc", valueOnly = TRUE)
    arc.room <- abs(as.radian(e.ed) - as.radian(s.ed)) * r * arc_npc_per_radius()
    too.wide <- is.finite(lab.w) & arc.room > 0 & lab.w > arc.room * 0.85

    if (any(too.wide)) {
      # Only re-orient labels that still run along the arc. A caller that already
      # asked for radial text (`facing = "clockwise"`) also supplied its own `adj`
      # to push the text outwards; recomputing it here reset the alignment to the
      # centred default and made those labels bend back over their connectors.
      if (!grepl("clockwise", facing)) {
        radial <- arc_text_geometry(theta[too.wide], "clockwise", TRUE,
                                    adj = adj)
        rot[too.wide] <- radial$rot
        hjust[too.wide] <- radial$hjust
        vjust[too.wide] <- radial$vjust
      }

      # Once rotated into the radial direction the label runs along a radius, so
      # its width is bounded by the distance to the nearer circle (inner or
      # outer). Only shrink when that room is genuinely too small, and keep a
      # generous floor so labels stay readable on full circles.
      radial.room <- pmax(r[too.wide], 1 - r[too.wide]) * arc_npc_per_radius()
      shrink <- pmax(0.7, pmin(1, (radial.room * 0.95) / lab.w[too.wide]))

      if (any(shrink < 0.999)) {
        fs <- rep(base.fontsize, n)
        fs[too.wide] <- fs[too.wide] * shrink
        text.gp <- do.call(gpar, modifyList(as.list(text.gp), list(fontsize = fs)))
      }
    }
  }

  text.grob <- textGrob(label = labels,
                        x = npc(x0 + r * cos(theta)),
                        y = npc(y0 + r * sin(theta)),
                        rot = rot,
                        hjust = hjust,
                        vjust = vjust,
                        default.units = "npc",
                        gp = text.gp,
                        name = "labels")

  grid::gTree(children = grid::gList(text.grob), name = "arcTextGrob")
}
