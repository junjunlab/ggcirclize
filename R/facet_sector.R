
#' Calculate Sector Information for Faceting
#'
#' This function calculates sector information for faceting in circular plots.
#'
#' @param data The data frame containing the data (default: NULL).
#' @param space Type of space scales for faceting ("fixed" or "free_x") (default: "fixed").
#' @param data.type Type of data to use for calculating sectors ("other" or "genomic") (default: "other").
#' @param sector.gap Gap between sectors in degrees (default: 3).
#'
#' @return A data frame containing sector information for faceting.
#'
#' @examples
#' \dontrun{
#' # Example usage of facet_sector
#' facet_sector(data = my_data, space = "free_x", data.type = "genomic", sector.gap = 5)
#' }
#'
#' @export
facet_sector <- function(data = NULL,
                         space = c("fixed","free_x"),
                         data.type = c("other","genomic"),
                         sector.gap = 3){
  space <- match.arg(space,c("fixed","free_x"))
  data.type <- match.arg(data.type,c("other","genomic"))

  # calculate theta pos for each sectors
  if(data.type == "genomic"){
    secs <- data[,2]
    names(secs) <- data[,1]
  }else{
    if(!("sector" %in% colnames(data))){
      data$sector <- "sector"
    }

    # calculate sectors theta
    secs <- table(data[,c("sector")])
  }

  track.theta <- abs(unique(data$end) - unique(data$start))

  x <- track.theta - length(secs)*sector.gap

  if(space == "fixed"){
    x.sec <- x/length(secs)
  }else{
    x.sec <- (x/sum(secs))*secs
  }

  # sector gaps
  if(length(sector.gap) == 1){
    sector.gap <- rep(sector.gap,length(secs))
    tmp <- cumsum(x.sec + sector.gap)
  }else{
    tmp <- cumsum(x.sec + sector.gap)
  }

  # get start/end r0/r1
  x.start <- c(0,tmp[1:(length(secs) - 1)])
  x.end <- x.start + x.sec
  names(x.start) <- names(x.end)

  sector_name = names(secs)

  track_info <- data.frame(sector_name = names(secs),
                           sector_start = as.numeric(x.start) + unique(data$start),
                           sector_end = as.numeric(x.end) + unique(data$start))

  if(data.type == "genomic"){
    track_info$length <- secs
  }

  return(track_info)
}
