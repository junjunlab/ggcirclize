% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_trackgenomicrect.R
\name{geom_trackgenomicrect}
\alias{geom_trackgenomicrect}
\title{geom_trackgenomicrect Function}
\usage{
geom_trackgenomicrect(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  chrom_data = NULL,
  keep.all.chrom = FALSE,
  strip.label = TRUE,
  strip.label.pos = c("top", "bottom"),
  strip.label.space = 0.15,
  strip.label.fontface = "bold",
  strip.label.col = "black",
  strip.label.size = 10,
  scales = c("fixed", "free", "free_x", "free_y"),
  space = c("free_x", "fixed"),
  sector.gap = 3,
  extend.xscale = 0.05,
  extend.yscale = 0.05,
  add.bg = TRUE,
  sector.bg.extend = 0.025,
  add.xaxis = TRUE,
  add.yaxis = FALSE,
  xAxis.params = list(),
  yAxis.params = list(),
  ...
)
}
\arguments{
\item{mapping}{Mapping rules for mapping data to visual aesthetics.}

\item{data}{A data frame containing the plotting data.}

\item{stat}{The name of the statistical transformation.}

\item{position}{The name of positioning adjustment.}

\item{na.rm}{Whether to remove missing values (default = FALSE).}

\item{show.legend}{Whether to display the legend (default = NA).}

\item{inherit.aes}{Whether to inherit aesthetics from the parent layer (default = TRUE).}

\item{chrom_data}{Data frame with 2 columns containing chromosome size information (default = NULL).}

\item{keep.all.chrom}{Whether to keep all chromosomes in the plot (default = FALSE).}

\item{strip.label}{Whether to display track labels (default = TRUE).}

\item{strip.label.pos}{The position of track labels ("top" or "bottom") (default = "top").}

\item{strip.label.space}{The spacing between track labels and track genomic area plots (default = 0.15).}

\item{strip.label.fontface}{The font style of track labels (default = "bold").}

\item{strip.label.col}{The color of track labels (default = "black").}

\item{strip.label.size}{The size of track labels (default = 10).}

\item{scales}{Specifies the scaling type for x and y axes (default = "fixed": unused arg now).}

\item{space}{Specifies the spatial type for track genomic area plots (default = "free_x").}

\item{sector.gap}{The gap between track genomic area plots (default = 3).}

\item{extend.xscale}{The scaling factor for the x-axis (default = 0.05).}

\item{extend.yscale}{The scaling factor for the y-axis (default = 0.05).}

\item{add.bg}{Whether to add a background (default = TRUE).}

\item{sector.bg.extend}{The background extension factor (default = 0.025).}

\item{add.xaxis}{Whether to add the x-axis (default = TRUE).}

\item{add.yaxis}{Whether to add the y-axis (default = FALSE).}

\item{xAxis.params}{Parameters for the x-axis (default = list()).}

\item{yAxis.params}{Parameters for the y-axis (default = list()).}

\item{...}{Other parameters to be passed to the function.}
}
\value{
A ggplot2 layer.
}
\description{
This is a custom geom function used to draw track genomic rectangle plots.
}
