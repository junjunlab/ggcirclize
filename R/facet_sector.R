
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

  if(is.null(data) || nrow(data) == 0){
    stop("`data` must be a non-empty data.frame.",call. = FALSE)
  }

  # calculate theta pos for each sectors
  if(data.type == "genomic"){
    if(ncol(data) < 2){
      stop("`data` must have chromosome names in the first column and ",
           "their lengths in the second one.",call. = FALSE)
    }

    secs <- unlist(data[,2])
    names(secs) <- unlist(data[,1])
  }else{
    if(!("sector" %in% colnames(data))){
      data$sector <- "sector"
    }

    # calculate sectors theta
    secs <- table(data[,c("sector")])
  }

  if(length(secs) == 0){
    stop("No sector could be found in `data`.",call. = FALSE)
  }

  # total angle spanned by the data
  track.theta <- diff(range(c(data$start,data$end),na.rm = TRUE))

  if(length(sector.gap) == 1){
    sector.gap <- rep(sector.gap,length(secs))
  }else if(length(sector.gap) != length(secs)){
    stop("`sector.gap` must be a single value or have one value per sector.",
         call. = FALSE)
  }

  free.theta <- track.theta - sum(sector.gap)
  if(!is.finite(free.theta) || free.theta <= 0){
    stop("`sector.gap` is too large for the angle span of `data`.",call. = FALSE)
  }

  # angle allocated to each sector
  if(space == "fixed"){
    x.sec <- rep(free.theta/length(secs),length(secs))
  }else{
    x.sec <- free.theta * as.numeric(secs)/sum(as.numeric(secs))
  }

  # sector gaps
  tmp <- cumsum(x.sec + sector.gap)

  # get start/end r0/r1
  x.start <- c(0,tmp[-length(tmp)])
  x.end <- x.start + x.sec

  base.theta <- min(data$start,na.rm = TRUE)

  track_info <- data.frame(sector_name = names(secs),
                           sector_start = as.numeric(x.start) + base.theta,
                           sector_end = as.numeric(x.end) + base.theta,
                           stringsAsFactors = FALSE)

  if(data.type == "genomic"){
    track_info$length <- as.numeric(secs)
  }

  return(track_info)
}
