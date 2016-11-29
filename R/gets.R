#' @title Search for images
#' @description Search for images.
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
#' search_im(min_lat=49.317328, max_lat=49.325832, 
#'   min_lon=19.963211, max_lon=20.004066)
#' }
search_im <- function(min_lat, max_lat, min_lon, max_lon, 
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
  df <- to_df(raw, "search_im")
  
  # return
  if(print) print(df)
  invisible(df)
}



#' @title Search for images close to location
#' @description Get images close to a certain point defined by longitude, latitude, 
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
#' search_im_close(lat=46.804159, lon=7.166325, distance=50)
#' }
search_im_close <- function(lat, lon, distance, 
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
		end_time <- max(start_time, end_time)
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
  df <- to_df(raw, "search_im_close")
  
  # return
  if(print) print(df)
  invisible(df)
}



#' @title Search for commented images
#' @description Search for images that have at least one comment in an area.
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
#' @source \url{https://a.mapillary.com/#get-searchimcm}
#' @export
#' @examples
#' \dontrun{
#' search_im_cm(min_lat=41.31995, max_lat=41.32001, 
#'   min_lon=19.79985, max_lon=19.79995)
#' }
search_im_cm <- function(min_lat, max_lat, min_lon, max_lon, 
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
  res <- m_get_url(path="search/im/cm", min_lat=min_lat, max_lat=max_lat, min_lon=min_lon, max_lon=max_lon, 
    start_time=start_time, end_time=end_time, user=user, limit=limit, page=page)
  raw <- m_parse(res)
  df <- to_df(raw, "search_im_cm")
  
  # return
  if(print) print(df)
  invisible(df)
}



#' @title Search for sequences
#' @description Search for sequences.
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
#' @return A \code{data.frame} of matching sequences.
#' @source \url{https://a.mapillary.com/#get-searchsul}
#' @export
#' @examples
#' \dontrun{
#' search_seq(min_lat=49.019063, max_lat=49.328211,
#'   min_lon=-123.233919, max_lon=-122.298288)
#' }
search_seq <- function(min_lat, max_lat, min_lon, max_lon, 
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
  res <- m_get_url(path="search/s/ul", min_lat=min_lat, max_lat=max_lat, min_lon=min_lon, max_lon=max_lon, 
    start_time=start_time, end_time=end_time, user=user, limit=limit, page=page)
  raw <- m_parse(res)
  df <- to_df(raw, "search_seq")
  
  # return
  if(print) print(df)
  invisible(df)
}



#' @title Get random image
#' @description Get a random image from the Mapillary image archive, 
#' the images is randomized from a set of curated images.
#'
#' @param print if \code{TRUE} (default) the search results are printed.
#' @return A \code{data.frame} of the random image.
#' @source \url{https://a.mapillary.com/#get-searchimrandomselected}
#' @note This request might fail irregularly (HTTP error 404).
#' @export
#' @examples
#' \dontrun{
#' search_im_random()
#' }
search_im_random <- function(print=TRUE) {
	
	# make request
  res <- m_get_url(path="search/im/randomselected")
  raw <- m_parse(res)
  df <- to_df(raw, "search_im_random")
  
  # return
  if(print) print(df)
  invisible(df)
}



#' @title Search for usernames
#' @description Search for usernames.
#'
#' @param user String to try to match for a username.
#' @param print if \code{TRUE} (default) the search results are printed.
#' @return A \code{data.frame} of the matching usernames.
#' @source \url{https://a.mapillary.com/#get-searchu}
#' @export
#' @examples
#' \dontrun{
#' search_user("mynameis")
#' }
search_user <- function(user, print=TRUE) {
	
	# check parameter
	if(missing(user)) stop("'user' is mandatory.")
	if(!is.character(user)) stop("Please specify 'user' as string.")
	
	# make request
  res <- m_get_url(path="search/u", match=user)
  raw <- m_parse(res)
  df <- to_df(raw, "search_user")
  
  # return
  if(print) print(df)
  invisible(df)
}



#' @title Statistics about images in the Mapillary system
#' @description Retrieve statistics about images in the Mapillary system.
#'
#' @param print if \code{TRUE} (default) the statistics are printed.
#' @return A \code{data.frame} with basic statistics.
#' @source \url{https://a.mapillary.com/#get-statsim}
#' @export
#' @examples
#' \dontrun{
#' stats_im()
#' }
stats_im <- function(print=TRUE) {
	
	# make request
  res <- m_get_url(path="stats/im")
  raw <- m_parse(res)
  df <- to_df(raw, "stats_im")
  
  # return
  if(print) print(df)
  invisible(df)
}



#' @title Top lists statistics for Mapillary
#' @description Retrieve weekly and total top lists for Mapillary.
#'
#' @param cname Name of country for stats (optional - if not added whole world is assumed).
#' @param limit Number of users in every list (default 10, optional).
#' @param print if \code{TRUE} (default) the statistics are printed.
#' @return A \code{list} of two \code{data.frame}s, one for each toplist.
#' @source \url{https://a.mapillary.com/#get-statsimtoplist}
#' @export
#' @examples
#' \dontrun{
#' stats_top("namibia")
#' }
stats_top <- function(cname, limit, print=TRUE) {
	
	# check parameter
	if(!missing(cname)) if(!is.character(cname)) stop("Please specify 'cname' as string.")
	if(!missing(limit)) if(!is.numeric(limit)) stop("Please specify 'limit' as integer.")
	
	# drop empty parameters
	if(missing(cname)) cname <- NULL
	if(missing(limit)) limit <- NULL
	
	# make request
  res <- m_get_url(path="stats/toplist", cname=cname, limit=limit)
  raw <- m_parse(res)
  df <- to_df(raw, "stats_top")
  
  # return
  if(print) print(df)
  invisible(df)
}



#' @title Get images
#' @description Save images and display images in R.
#'
#' @param key Image key.
#' @param size Image size. One of \code{s[mall]} (320px), 
#' \code{m[edium]} (640px, the default), \code{l[arge]} (1024px) 
#' or \code{h[uge]} (2048px).
#' @param save Directory where to save the image file. Optional --
#' if missing, the image is saved as temporary file and diplayed.
#' @return An image.
#' @source \url{https://a.mapillary.com/#images}
#' @export
#' @examples
#' \dontrun{
#' img <- search_im_close(lat=46.804159, lon=7.166325, 
#'   distance=10000, limit=1, print=FALSE)$key
#' get_im(key=img, size="m")
#' }
get_im <- function(key, size="m", save) {
  
  # check parameters
  if(!missing(key)) if(!is.character(key)) stop("Please specify 'key' as string.")
  if(!missing(size)) if(!is.character(size)) stop("Please specify 'size' as string.")
  if(!missing(save)) if(!is.character(save)) stop("Please specify 'save' as string.")
  
  # prepare url
  avail_sizes <- c("small", "medium", "large", "huge")
  sizes <- c(320, 640, 1024, 2048)
  size <- sizes[pmatch(size, avail_sizes)]
  img_url <- paste0("https://d1cuyjsrcm0gby.cloudfront.net/", key, "/thumb-", size, ".jpg")
  
  # download image
  rtrn <- FALSE
  if(missing(save)) {
    save <- tempdir()
    rtrn <- TRUE
  }
  img_path <- file.path(save, paste(key, "jpg", sep="."))
  download.file(img_url, img_path, quiet=TRUE, mode="wb")
  
  # display image
  img <- readJPEG(img_path, native=TRUE)
  plot(0:1, 0:1, type="n", ann=FALSE, axes=FALSE)
  rasterImage(img, 0, 0, 1, 1)
  
  # return image path
  if(rtrn) invisible(img_path)
}



#' @title View images
#' @description View images online.
#'
#' @param key Image key.
#' @param mode Image view mode. Use \code{m[apillary]} (the default), 
#' to open the image in the mapillary website image view. 
#' Use \code{[d]irect}, to open the direct image link. 
#' @param size Image size. Used only for \code{direct} image view mode.
#' One of \code{s[mall]} (320px), \code{m[edium]} (640px, the default), 
#' \code{l[arge]} (1024px) or \code{h[uge]} (2048px).
#' @return An image.
#' @source \url{https://a.mapillary.com/#images}
#' @export
#' @examples
#' \dontrun{
#' img <- search_im_close(lat=46.804159, lon=7.166325, 
#'   distance=10000, limit=1, print=FALSE)$key
#' view_im(key=img)
#' }
view_im <- function(key, mode="m", size="m") {
  
  # check parameters
  if(!missing(key)) if(!is.character(key)) stop("Please specify 'key' as string.")
  if(!missing(mode)) if(!is.character(size)) stop("Please specify 'mode' as string.")
  if(!missing(size)) if(!is.character(save)) stop("Please specify 'size' as string.")
  
  # prepare url
  avail_modes <- c("mapillary", "direct")
  mode <- avail_modes[pmatch(mode, avail_modes)]
  if(mode=="direct") {
    img_url <- paste0("https://d1cuyjsrcm0gby.cloudfront.net/", key, "/thumb-", size, ".jpg")
  } else {
    img_url <- paste0("http://www.mapillary.com/map/im/", key)
  }
    
  # browse image
  browseURL(img_url)
  
  # return image path
  invisible(img_url)
}
