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



#' @description
#' A simple viewport to be created to plot arc graphics.
#'
#' @export
#' @import grid
newpage <- function(){
  grid.newpage()
  pushViewport(viewport(width = 0.9,height = 0.9,
                        xscale = c(-1,1),yscale = c(-1,1),
                        default.units = "snpc"))
  grid.circle(r = 0.5,gp = gpar(fill = "grey95",col = NA))
}



#' Calculate Height Between Arc Angles
#'
#' This function calculates the height between two arc angles based on the specified
#' starting angle, ending angle, inner radius, and outer radius.
#'
#' @param start Starting angle in degrees.
#' @param end Ending angle in degrees.
#' @param r0 Inner radius.
#' @param r1 Outer radius.
#'
#' @return The height between the specified arc angles.
#'
#' @export
get_height <- function(start,end,r0,r1){
  th <- abs(end - start)
  if(th <= 180){
    h <- rescale(th,to = c(r0,r1),from = c(0,180))
  }else{
    th <- th - 180
    h <- rescale(th,to = c(r0,r1),from = c(0,180))
    h <- abs(r1 - r0) + h

  }
  return(h)
}


#' Calculate Height for Fixed Arc Angle Span
#'
#' This function calculates the height for a fixed arc angle span based on the specified
#' starting angle, ending angle, inner radius, outer radius, and height.
#'
#' @param start Starting angle in degrees.
#' @param end Ending angle in degrees.
#' @param r0 Inner radius.
#' @param r1 Outer radius.
#' @param height Fixed height (default: NULL).
#'
#' @return The calculated height for the specified arc angle span.
#'
#' @export
get_fixedHeight <- function(start,end,r0,r1,height){
  th <- abs(end - start)
  if(th <= 180){
    h <- height
  }else{
    if(height >= r0 & height < r1){
      h <- abs(r1 - r0) + r1 - height
    }else{
      h <- height
    }
  }
  return(h)
}


# get index for vector
get_order_name <- function(from,all_name,all_from,shift = 0){
  idx <- match(from,all_name) + shift
  all_from_name <- rep(all_name,length(all_from))
  new_order <- all_from_name[idx:(idx + length(all_name) - 1)]

  return(new_order)
}


#' Get Symmetric Values from a Matrix
#'
#' This function extracts the symmetric values from a matrix and returns them in a long format.
#'
#' @param mat A matrix from which symmetric values will be extracted.
#' @param keep.diag Logical value indicating whether to keep the diagonal elements (default is TRUE).
#'
#' @return A data frame with three columns: 'from', 'to', and 'value', representing the row index, column index, and
#'         corresponding value of the symmetric elements.
#'
#' @export
get_symmetric_val <- function(mat = NULL,keep.diag = TRUE){
  diag <- ifelse(keep.diag == TRUE,FALSE,TRUE)

  logi <- upper.tri(mat,diag = diag)
  mat[logi] <- "heiheihei"
  mat_long <- reshape2::melt(mat)
  colnames(mat_long) <- c("from","to","value")
  mat_long <- subset(mat_long,value != "heiheihei")
  mat_long$value <- as.numeric(mat_long$value)

  return(mat_long)
}
