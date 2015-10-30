#' @title Search for images
#' @description Search for images by different constrains.
#'
#' @param min_lat Minimum latitude.
#' @param max_lat Maximum latitude.
#' @param min_lon Minimum longitude.
#' @param max_lon Maximum longitude.
#' @param start_time Start time in EPOCH ms.
#' @param end_time End time in EPOCH ms.
#' @param user Just objects for specific user.
#' @param limit Results per page in pagination.
#' @param page Page number in pagination.
#' @param print if \code{TRUE} (default) the search results are printed.
#' @return A \code{data.frame} of matching images.
#' @source \url{https://a.mapillary.com/#get-searchim}
#' @export
#' @examples
#' \dontrun{
#'   m_search_im(lat=46.804159, lon=7.166325)
#' }
m_search_im <- function(min_lat, max_lat, min_lon, max_lon, 
  start_time, end_time, user, limit, page, print=TRUE) {
	
	# check parameters
	if(missing(min_lat) && missing(max_lat)) stop("At least one of 'min_lat' and 'max_lat' is required.")
	if(missing(min_lon) && missing(max_lon)) stop("At least one of 'min_lon' and 'max_lon' is required.") 
	if(!missing(min_lat)) {
		if(!is.numeric(min_lat)) stop("'min_lat' must be specified in decimal degrees.")
		if(min_lat < 0 || min_lat > 90) stop("'min_lat' must have a value between 0 and 90.")
	}
	if(!missing(max_lat)) {
		if(!is.numeric(max_lat)) stop("'max_lat' must be specified in decimal degrees.")
		if(max_lat < 0 || max_lat > 90) stop("'max_lat' must have a value between 0 and 90.")
	}
	if(!missing(min_lon)) {
		if(!is.numeric(min_lon)) stop("'min_lon' must be specified in decimal degrees.")
		if(min_lon < -180 || min_lon > 180) stop("'min_lon' must have a value between -180 and 180.")
	}
	if(!missing(max_lon)) {
		if(!is.numeric(max_lon)) stop("'max_lon' must be specified in decimal degrees.")
		if(max_lon < -180 || max_lon > 180) stop("'max_lon' must have a value between -180 and 180.")
	}
	if(!missing(min_lat) && !missing(max_lat)) {
		min_lat <- min(min_lat, max_lat)
		max_lat <- max(min_lat, max_lat)
	}
	if(!missing(min_lon) && !missing(max_lon)) {
		min_lon <- min(min_lon, max_lon)
		max_lon <- max(min_lon, max_lon)
	}
	
	# drop empty parameters
	if(missing(min_lat)) min_lat <- NULL
	if(missing(max_lat)) max_lat <- NULL
	if(missing(min_lon)) min_lon <- NULL
	if(missing(max_lon)) max_lon <- NULL
	if(missing(start_time)) start_time <- NULL
	if(missing(end_time)) end_time <- NULL
	if(missing(user)) user <- NULL
	if(missing(limit)) limit <- NULL
	if(missing(page)) page <- NULL
	
	# make request
  res <- m_get_url(path="search/im", min_lat=min_lat, max_lat=max_lat, min_lon=min_lon, max_lon=max_lon, 
    start_time=start_time, end_time=end_time, user=user, limit=limit, page=page)
  raw <- m_parse(res)
  df <- to_df(raw)
  
  # return
  if(print) print(df)
  invisible(df)
}


#' @title Search for images close to location
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
#' @return A \code{data.frame} of matching images.
#' @source \url{https://a.mapillary.com/#get-searchimclose}
#' @export
#' @examples
#' \dontrun{
#'   m_search_im_close(lat=46.804159, lon=7.166325, distance=50)
#' }
m_search_im_close <- function(lat, lon, distance, 
  start_time, end_time, min_ca, max_ca, 
  user, limit, page, print=TRUE) {
	
	# check parameters
	if(missing(lat) || missing(lon)) stop("'lat' and 'lon' are mandatory parameters.") 
	if(!is.numeric(lat) || !is.numeric(lon)) stop("'lat' and 'lon' must be specified in decimal degrees.")
	if(lat < 0 || lat > 90 || lon < -180 || lon > 180) stop("'lat' and 'lon' must be specified in decimal degrees.")
	if(!missing(distance)) {
		if(!is.numeric(distance)) stop("'distance' must be numeric.")
		if(distance < 0) stop("'distance' must have a positive value.")
	}
	if(!missing(start_time)) {
		if(!is.numeric(start_time)) stop("'start_time' must be numeric.")
		if(start_time <= 0) stop("'start_time' must have a positive value.")
	}
	if(!missing(end_time)) {
		if(!is.numeric(end_time)) stop("'end_time' must be numeric.")
		if(end_time <= 0) stop("'end_time' must have a positive value.")
	}
	if(!missing(start_time) && !missing(end_time)) {
		start_time <- min(start_time, end_time)
		end_time <- max(min_lon, end_time)
	}
	if(!missing(min_ca)) {
		if(!is.numeric(min_ca)) stop("'min_ca' must be numeric.")
		if(min_ca < 0 || min_ca > 360) stop("'min_ca' must have a value between 0 and 360.")
	}
	if(!missing(max_ca)) {
		if(!is.numeric(max_ca)) stop("'max_ca' must be numeric.")
		if(max_ca < 0 || max_ca > 360) stop("'max_ca' must have a value between 0 and 360.")
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

