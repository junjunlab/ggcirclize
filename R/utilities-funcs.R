globalVariables(c(".","value","hg38_chrom_info","hg19_chrom_info","mm39_chrom_info",
                  "mm10_chrom_info","mm9_chrom_info"))

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


# add suitble suffix for a value
addsuffix <- function(value = NULL){
  fun <- function(val){
    if(val <= 10^3){
      labels <- paste0(val,"bp")
    }else if(val >= 10^3 & val <= 10^6){
      labels <- paste0(val/10^3,"Kb")
    }else{
      labels <- paste0(val/10^6,"Mb")
    }
  }

  sapply(value,FUN = fun)
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



#' Convert a BED matrix to a long format
#'
#' This function converts a BED matrix into a long format data frame for further analysis.
#'
#' @param bed.mat A data frame containing BED format data with columns 'chr',
#' 'start', and 'end'.
#' @param retain.var A character vector of additional variables to retain in the
#' long format data frame (optional).
#'
#' @return A long format data frame.
#'
#' @details This function orders the input BED matrix by chromosome
#' and start position, and then creates a long format data frame with columns
#' for chromosome, start, end, x-coordinate, variable, and y-coordinate.
#'
#' @export
#'
#' @seealso \code{\link{melt}}, \code{\link{order}}
#'
#' @importFrom reshape2 melt
#'
#' @rdname bedMatTolong
bedMatTolong <- function(bed.mat = NULL,retain.var = NULL){
  bed.mat <- bed.mat[order(bed.mat$chr,bed.mat$start),]
  x <- as.numeric(unlist(lapply(table(bed.mat$chr), function(x) 1:x)))
  bed.mat$x <- x

  if(is.null(retain.var)){
    id.vars <- c("chr","start","end","x")
  }else{
    id.vars <- c("chr","start","end","x",retain.var)
  }

  bed_long <- reshape2::melt(bed.mat,id.vars = id.vars)
  bed_long$y <- as.numeric(bed_long$variable)

  return(bed_long)
}



#' Add Markers to a pheatmap Plot
#'
#' This function adds marker labels to a pheatmap plot.
#'
#' @param object A pheatmap object generated by the pheatmap package.
#' @param mark.label The label to be added as a marker.
#' @param marker.space The spacing between markers.
#' @param use.smartAlign2 Logical, whether to use smart alignment for markers.
#' @param link.line.length Length of the connecting lines between markers and labels.
#' @param link.label.space Spacing between marker labels and connecting lines.
#' @param mark.scale A numeric vector specifying the scale for marker positioning.
#' @param link.label.gp A graphical parameter for marker label appearance.
#' @param link.line.gp A graphical parameter for the connecting lines.
#' @param link.circle.start.gp A graphical parameter for the start circle (if used).
#' @param link.circle.end.gp A graphical parameter for the end circle (if used).
#' @param link.start.type Type of shape at the start of the connecting lines ("line", "circle", or "arrow").
#' @param link.end.type Type of shape at the end of the connecting lines ("line", "circle", or "arrow").
#' @param circle.arrow.size A numeric vector specifying the size of circles or arrows.
#' @param pos Position for marker placement ("right" or "bottom").
#'
#' @return A modified pheatmap plot with marker labels.
#'
#' @details
#' This function adds marker labels to a pheatmap plot generated by the pheatmap package.
#' Markers can be positioned either on the right or at the bottom of the heatmap.
#' The function supports various options for marker appearance and alignment.
#'
#' @import gtable
#' @importFrom grid nullGrob
#' @export
addMarkers <- function(object = NULL,
                       mark.label = NULL,
                       marker.space = 0.5,
                       use.smartAlign2 = FALSE,
                       link.line.length = 0.075,
                       link.label.space = 0.025,
                       mark.scale = c(0.01,0.99),
                       link.label.gp = gpar(fontsize = 10),
                       link.line.gp = gpar(fill = "black",col = "black"),
                       link.circle.start.gp = gpar(fill = "black",col = "black"),
                       link.circle.end.gp = gpar(fill = "black",col = "black"),
                       link.start.type = c("line","circle","arrow"),
                       link.end.type = c("line","circle","arrow"),
                       circle.arrow.size = c(0.01,0.01),
                       pos = c("right","bottom")
){
  pos <- match.arg(pos,c("right","bottom"))

  # check class
  if(!inherits(object,"pheatmap")){
    message("Please supply pheatmap output.")
    stop()
  }

  # get gene names
  gt <- object$gtable

  if(pos == "right"){
    all.label <- object$tree_row$labels
    orders <- object$tree_row$order
    all.label <- all.label[orders]

    x = 0.05;y = 0.5
  }else{
    all.label <- object$tree_col$labels
    orders <- object$tree_col$order
    all.label <- rev(all.label[orders])

    x = 0.5;y = 0.95
  }


  # generate label grob
  label_grob <- smartLabelAlignGrob(all.label = rev(all.label),
                                    mark.label = mark.label,
                                    x = x,y = y,
                                    use.smartAlign2 = use.smartAlign2,
                                    link.line.length = link.line.length,
                                    link.label.space = link.label.space,
                                    mark.scale = mark.scale,
                                    link.label.gp = link.label.gp,
                                    link.line.gp = link.line.gp,
                                    link.circle.start.gp = link.circle.start.gp,
                                    link.circle.end.gp = link.circle.end.gp,
                                    link.start.type = link.start.type,
                                    link.end.type = link.end.type,
                                    circle.arrow.size = circle.arrow.size,
                                    pos = pos)

  # re-plot
  if(pos == "right"){
    gt$grobs[[5]] <- nullGrob()
    gt <- gtable::gtable_add_grob(gt,label_grob,t = 4,l = 4)
    gt <- gtable_add_cols(gt,unit(marker.space, "cm"),4)
    gt <- gtable_add_cols(gt,unit(0.5, "cm"),0)
  }else{
    gt$grobs[[4]] <- nullGrob()
    gt <- gtable::gtable_add_grob(gt,label_grob,t = 5,l = 3)
  }

  grid.newpage()
  pushViewport(viewport(width = 0.9,height = 0.9))
  grid.draw(gt)
}




#' Fetch chromosome size from ggcirclize package
#'
#' @param genome Genome version includes hg38,hg19,mm39,mm10 or mm9.
#'
#' @return A 2-column data.frame includes chromosome length.
#' @export
get_chrom_data <- function(genome = "hg19"){
  if(genome == "hg38"){
    hg38_chrom_info <- ggcirclize::hg38_chrom_info
    chr_df <- hg38_chrom_info$chromsize
  }else if(genome == "hg19"){
    hg19_chrom_info <- ggcirclize::hg19_chrom_info
    chr_df <- hg19_chrom_info$chromsize
  }else if(genome == "mm39"){
    mm39_chrom_info <- ggcirclize::mm39_chrom_info
    chr_df <- mm39_chrom_info$chromsize
  }else if(genome == "mm10"){
    mm10_chrom_info <- ggcirclize::mm10_chrom_info
    chr_df <- mm10_chrom_info$chromsize
  }else if(genome == "mm9"){
    mm9_chrom_info <- ggcirclize::mm9_chrom_info
    chr_df <- mm9_chrom_info$chromsize
  }else{
    message("Please supply genome version which in hg38,hg19,mm39,mm10 or mm9.")
  }
}
