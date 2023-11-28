globalVariables(c("."))

#' This function converts degrees to radians.
#' @param degree Numeric vector or scalar containing angles in degrees.
#' @return Numeric vector or scalar with corresponding angles in radians.
#' @export
as.radian <- function(degree) {
  radians <- (degree / 180) * pi
  return(radians)
}


#' This function converts radians to degrees.
#' @param radian Numeric vector or scalar containing angles in radians.
#' @return Numeric vector or scalar with corresponding angles in degrees.
#' @export
as.theta <- function(radian) {
  theta <- (radian * 180) / pi
  return(theta)
}


#' Download Chromosome Information Data
#'
#' This function downloads chromosome size and cytoband data for a specified
#' genome from the UCSC Genome Browser.
#'
#' @param genome The name of the genome (default: "hg19").
#'
#' @return Downloads chromosome size and cytoband data for the specified genome.
#'
#' @examples
#' \dontrun{
#' # Example usage of download_chromInfo
#' download_chromInfo(genome = "hg38")
#' }
#'
#' @export
download_chromInfo <- function(genome = "hg19"){
  url_chromsize <- paste0("https://hgdownload.soe.ucsc.edu/goldenPath/",
                          genome,
                          "/bigZips/",genome,".chrom.sizes")

  download.file(url = url_chromsize,destfile = "./")

  url_cytoband <- paste0("https://hgdownload.cse.ucsc.edu/goldenPath/",
                         genome,"/database/cytoBandIdeo.txt.gz")

  download.file(url = url_cytoband,destfile = "./")
}



#' @export
#' @import grid
newpage <- function(){
  grid.newpage()
  pushViewport(viewport(width = 0.9,height = 0.9,
                        xscale = c(-1,1),yscale = c(-1,1),
                        default.units = "snpc"))
  grid.circle(r = 0.5,gp = gpar(fill = "grey95",col = NA))
}
