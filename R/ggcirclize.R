#' Create Circular Plot with ggplot2
#'
#' @author Jun Zhang
#'
#' @description
#' This function creates a circular plot using ggplot2 with specified data and aesthetics.
#'
#' @param data The data frame containing the data (default: NULL).
#' @param mapping Aesthetic mapping (default: aes(start = 0, end = 180, r0 = 0.5, r1 = 1, clock.wise = FALSE)).
#' @param mg.t Top margin for the plot (default: 2).
#' @param mg.r Right margin for the plot (default: 1).
#' @param mg.b Bottom margin for the plot (default: 2).
#' @param mg.l Left margin for the plot (default: 1).
#' @param environment The environment to use for evaluating the function (default: parent.frame()).
#' @param ... Additional arguments to be passed to ggplot function.
#'
#' @return A circular plot created with ggplot2.
#'
#' @examples
#' \dontrun{
#' # Example usage of ggcirclize
#' ggcirclize(data = my_data, mapping = aes(x = value, y = category))
#' }
#'
#' @import ggplot2
#'
#' @export
ggcirclize <- function(data = NULL,
                       mapping = aes(),
                       mg.t = 2,
                       mg.r = 1,
                       mg.b = 2,
                       mg.l = 1,
                       environment = parent.frame(),
                       ...) {

  mapping <- modifyList(aes(start = 0,end = 180,r0 = 0.5,r1 = 1,
                            clock.wise = FALSE),
                        mapping)

  p <- ggplot(data = data,
              mapping = mapping,
              mg.t = mg.t,
              mg.r = mg.r,
              mg.b = mg.b,
              mg.l = mg.l,
              ...)


  p <- p +
    theme_void() +
    theme(aspect.ratio = 1,
          legend.position = c(1.2,0.5),
          plot.margin = margin(t = mg.t,r = mg.r,b = mg.b,l = mg.l,unit = "cm")) +
    coord_cartesian(expand = 0,clip = "off",xlim = c(-1,1),ylim = c(-1,1))


  class(p) <- c("ggcirclize", class(p))

  return(p)
}
