% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arcAxisGrob.R
\name{arcAxisGrob}
\alias{arcAxisGrob}
\title{Create a custom arc axis graphical object.}
\usage{
arcAxisGrob(
  start = 0,
  end = 60,
  r0 = 0.5,
  r1 = 1,
  x0 = 0,
  y0 = 0,
  ticks.len = NULL,
  n = 100,
  clock.wise = FALSE,
  axis.type = c("x", "y"),
  nice.facing = FALSE,
  pos = c("left", "right", "top", "bottom"),
  xscale = NULL,
  yscale = c(0, 1),
  breaks.n = 5,
  minor.ticks.n = 5,
  breaks = NULL,
  breaks.label = NULL,
  label.space = NULL,
  x.gp = NULL,
  y.gp = NULL,
  x.label.gp = gpar(fontsize = 8),
  y.label.gp = gpar(fontsize = 8),
  extend.xscale = 0.05,
  extend.yscale = 0.05,
  name = NULL,
  gp = NULL,
  vp = NULL
)
}
\arguments{
\item{start}{The starting angle of the arc axis (in degrees). Default is 0.}

\item{end}{The ending angle of the arc axis (in degrees). Default is 60.}

\item{r0}{The inner radius of the arc axis. Default is 0.5.}

\item{r1}{The outer radius of the arc axis. Default is 1.}

\item{x0}{The x-coordinate of the center of the unit circle. Default is 0.}

\item{y0}{The y-coordinate of the center of the unit circle. Default is 0.}

\item{ticks.len}{The length of the tick marks along the arc axis. Default is NULL.}

\item{n}{The number of ticks to be drawn along the arc axis. Default is 100.}

\item{clock.wise}{A logical value indicating whether the arc axis should be
drawn clockwise. Default is FALSE.}

\item{axis.type}{A character string specifying the type of the axis ('x' or 'y').
Default is 'x'.}

\item{nice.facing}{A logical value indicating whether tick labels should be
nicely aligned. Default is FALSE.}

\item{pos}{The position of the arc axis ('left', 'right', 'top', or 'bottom').
Default is 'left'.}

\item{xscale}{The scaling factor for the x-axis. Default is NULL.}

\item{yscale}{The scaling range for the y-axis. Default is c(0, 1).}

\item{breaks.n}{The number of major tick marks to be drawn. Default is 5.}

\item{minor.ticks.n}{The number of minor tick marks to be drawn between major
ticks. Default is 5.}

\item{breaks}{A vector specifying the positions of major tick marks. Default is NULL.}

\item{breaks.label}{A vector of labels for major tick marks. Default is NULL.}

\item{label.space}{The space between tick labels and the axis. Default is NULL.}

\item{x.gp}{A graphical parameter object for the x-axis. Default is NULL.}

\item{y.gp}{A graphical parameter object for the y-axis. Default is NULL.}

\item{x.label.gp}{A graphical parameter object for x-axis labels. Default is gpar(fontsize = 8).}

\item{y.label.gp}{A graphical parameter object for y-axis labels. Default is gpar(fontsize = 8).}

\item{extend.xscale}{The extension factor for the x-axis. Default is 0.05.}

\item{extend.yscale}{The extension factor for the y-axis. Default is 0.05.}

\item{name}{The name of the arc axis. Default is NULL.}

\item{gp}{A graphical parameter object for the arc axis. Default is NULL.}

\item{vp}{A viewport object for the arc axis. Default is NULL.}
}
\value{
A graphical object of class 'grob'.
}
\description{
This function creates a custom arc axis graphical object that can be added to
a plot in R. The arc axis can be used to display data along a curved axis,
such as a circular or radial axis.
}
\examples{
\dontrun{
# Create an arc axis
arc_axis <- arcAxisGrob(start = 0, end = 180, r0 = 0.5, r1 = 1, axis.type = "x",pos = "top")
# Add the arc axis to a plot
grid.draw(arc_axis)
}

}
