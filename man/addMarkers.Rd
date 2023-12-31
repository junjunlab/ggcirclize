% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-funcs.R
\name{addMarkers}
\alias{addMarkers}
\title{Add Markers to a pheatmap Plot}
\usage{
addMarkers(
  object = NULL,
  mark.label = NULL,
  marker.space = 0.5,
  use.smartAlign2 = FALSE,
  link.line.length = 0.075,
  link.label.space = 0.025,
  mark.scale = c(0.01, 0.99),
  link.label.gp = gpar(fontsize = 10),
  link.line.gp = gpar(fill = "black", col = "black"),
  link.circle.start.gp = gpar(fill = "black", col = "black"),
  link.circle.end.gp = gpar(fill = "black", col = "black"),
  link.start.type = c("line", "circle", "arrow"),
  link.end.type = c("line", "circle", "arrow"),
  circle.arrow.size = c(0.01, 0.01),
  pos = c("right", "bottom")
)
}
\arguments{
\item{object}{A pheatmap object generated by the pheatmap package.}

\item{mark.label}{The label to be added as a marker.}

\item{marker.space}{The spacing between markers.}

\item{use.smartAlign2}{Logical, whether to use smart alignment for markers.}

\item{link.line.length}{Length of the connecting lines between markers and labels.}

\item{link.label.space}{Spacing between marker labels and connecting lines.}

\item{mark.scale}{A numeric vector specifying the scale for marker positioning.}

\item{link.label.gp}{A graphical parameter for marker label appearance.}

\item{link.line.gp}{A graphical parameter for the connecting lines.}

\item{link.circle.start.gp}{A graphical parameter for the start circle (if used).}

\item{link.circle.end.gp}{A graphical parameter for the end circle (if used).}

\item{link.start.type}{Type of shape at the start of the connecting lines ("line", "circle", or "arrow").}

\item{link.end.type}{Type of shape at the end of the connecting lines ("line", "circle", or "arrow").}

\item{circle.arrow.size}{A numeric vector specifying the size of circles or arrows.}

\item{pos}{Position for marker placement ("right" or "bottom").}
}
\value{
A modified pheatmap plot with marker labels.
}
\description{
This function adds marker labels to a pheatmap plot.
}
\details{
This function adds marker labels to a pheatmap plot generated by the pheatmap package.
Markers can be positioned either on the right or at the bottom of the heatmap.
The function supports various options for marker appearance and alignment.
}
