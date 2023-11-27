#' Add Custom Points to a ggplot2 Plot
#'
#' This function allows you to add custom points to a ggplot2 plot. It is a
#' geom layer for creating scatterplots
#'
#' @param mapping Aesthetic mappings (default: NULL).
#' @param data The data frame containing the data (default: NULL).
#' @param stat The statistical transformation to use (default: "identity").
#' @param position The position adjustment for overlapping points (default: "identity").
#' @param ... Additional parameters to customize the appearance of the points.
#' @param na.rm A logical value indicating whether to remove missing values (default: FALSE).
#' @param show.legend A logical value indicating whether to show the legend (default: NA).
#' @param inherit.aes A logical value indicating whether to inherit aesthetics from the plot (default: TRUE).
#'
#' @return A layer to be added to a ggplot object.
#'
#' @import ggplot2
#'
#' @export
geom_cpoint <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCpoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggcirclize-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomCpoint <- ggproto("GeomCpoint", Geom,
                      required_aes = c("x", "y"),
                      non_missing_aes = c("size", "shape", "colour"),
                      default_aes = aes(
                        shape = 19, colour = "black", size = 1.5, fill = NA,
                        alpha = NA, stroke = 0.5,cluster = NULL,
                      ),

                      draw_panel = function(self, data, panel_params, coord, na.rm = FALSE,
                                            psize = 1,
                                            label.gp = gpar()) {
                        if (is.character(data$shape)) {
                          data$shape <- translate_shape_string(data$shape)
                        }

                        # coords <- coord$transform(data, panel_params)
                        coords <- data

                        coords$x <- rescale(coords$x,to = c(0,1))
                        coords$y <- rescale(coords$y,to = c(0,1))

                        # calculate cluster centers
                        if("cluster" %in% colnames(coords)){
                          centers <- coords %>%
                            dplyr::group_by(cluster) %>%
                            dplyr::summarise(x = median(x = x), y = median(x = y))

                          label_grob <- textGrob(x = centers$x,y = centers$y,
                                                 label = centers$cluster,
                                                 gp = label.gp,
                                                 vp = viewport(width = psize,height = psize))
                        }else{
                          label_grob <- zeroGrob()
                        }

                        # =======================================================
                        # point grob
                        stroke_size <- coords$stroke
                        stroke_size[is.na(stroke_size)] <- 0

                        point_grob <- pointsGrob(
                          coords$x, coords$y,
                          pch = coords$shape,
                          default.units = "npc",
                          gp = gpar(
                            col = alpha(coords$colour, coords$alpha),
                            fill = alpha(coords$fill, coords$alpha),
                            # Stroke is added around the outside of the point
                            fontsize = coords$size * .pt + stroke_size * .stroke / 2,
                            lwd = coords$stroke * .stroke / 2
                          ),
                          vp = viewport(width = psize,height = psize))

                        ggname("geom_cpoint",grid::gTree(children = gList(point_grob,label_grob)))
                      },

                      draw_key = draw_key_point
)
