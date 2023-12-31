% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities-funcs.R
\name{download_chromInfo}
\alias{download_chromInfo}
\title{Download Chromosome Information Data}
\usage{
download_chromInfo(genome = "hg19")
}
\arguments{
\item{genome}{The name of the genome (default: "hg19").}
}
\value{
Downloads chromosome size and cytoband data for the specified genome.
}
\description{
This function downloads chromosome size and cytoband data for a specified
genome from the UCSC Genome Browser.
}
\examples{
\dontrun{
# Example usage of download_chromInfo
download_chromInfo(genome = "hg38")
}

}
