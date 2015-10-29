#' @title Search the ZEIT archive
#' @description Get images close to a certain point defined my longitude, latitude, 
#' max angle, min angle and a radius in meters.
#'
#' @param lat Latitude to search in circle from.
#' @param lon Longitude to search in circle from.
#' @param distance Search radius in meters.
#' @param start_time Start time in EPOCH ms.
#' @param end_time End time in EPOCH ms.
#' @param min_ca Minimum angle of image in degrees.
#' @param max_ca Maximum angle of image in degrees.
#' @param user Just objects for specific user.
#' @param limit Results per page in pagination.
#' @param page Page number in pagination.
#' @param print if \code{TRUE} (default) the search results are printed.
#' @return A list of matching images with corresponding metadata.
#' @source \url{https://a.mapillary.com/#get-searchimclose}
#' @export
#' @examples
#' \dontrun{
#'
#' }
m_search_im_close <- function(lat, lon, distance, 
  start_time, end_time, min_ca, max_ca, 
  user, limit, page, print=TRUE) {
	
	# check parameters
	if(missing(lat) || missing(lon)) stop("'lat' and 'lon' are mandatory parameters.") 
	if(!is.numeric(lat) || !is.numeric(lon)) stop("'lat' and 'lon' must be specified in decimal degrees.")
	if(lat < 0 || lat > 90 || lon < -180 || lon > 180) stop("'lat' and 'lon' must be specified in decimal degrees.")
	if(!missing(min_ca)) {
		if(!is.numeric(min_ca)) stop("'min_ca' must be numeric.")
		if(min_ca < 0 || min_ca > 360) stop("'min_ca' must be a value between 0 and 360.")
	}
	if(!missing(max_ca)) {
		if(!is.numeric(max_ca)) stop("'max_ca' must be numeric.")
		if(max_ca < 0 || max_ca > 360) stop("'max_ca' must be a value between 0 and 360.")
	}
	
	# drop empty parameters
	if(missing(distance)) distance <- NULL
	if(missing(start_time)) start_time <- NULL
	if(missing(end_time)) end_time <- NULL
	if(missing(min_ca)) min_ca <- NULL
	if(missing(max_ca)) max_ca <- NULL
	if(missing(user)) user <- NULL
	if(missing(limit)) limit <- NULL
	if(missing(page)) page <- NULL
	
	# make request
  res <- m_get_url(path="search/im/close", lat=lat, lon=lon, distance=distance, 
    start_time=start_time, end_time=end_time, min_ca=min_ca, max_ca=max_ca, 
		user=user, limit=limit, page=page)
  raw <- m_parse(res)
  df <- to_df(raw)
  
  # return
  if(print) print(df)
  invisible(df)
}

