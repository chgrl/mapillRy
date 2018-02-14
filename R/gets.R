#' @title Search for images
#' @description Search for images.
#'
#' @param bbox Bounding box, given as vector of minx, miny, maxx, maxy.
#' @param closeto Location that images are close to, given as vector of longitude and latitude.
#' @param radius Radius around the \code{closeto} location, given in meters (default 100).
#' @param lookat Location in which direction images are taken (and therefore that location 
#' is likely to be visible in the images), given as vector of longitude and latitude.
#' @param start_time Start time images are captured (following ISO 8601 rules).
#' @param end_time End time images are captured before (following ISO 8601 rules).
#' @param user_name Just objects for specific users, given as vector of usernames.
#' @param user_key Just objects for specific users, given as vector of user keys.
#' @param project_key Just objects for specific projects, given as vector of project keys.
#' @param page Page number in pagination.
#' @param per_page Results per page in pagination.
#' @param fields Partially selected output fields, given as string or vector of strings. 
#' Fields are sorted in given order. Available fields: \code{camera_angle}, 
#' \code{camera_make}, \code{camera_model}, \code{captured_at}, \code{img_key}, 
#' \code{panorama}, \code{user_key}, \code{user_name}, \code{project_key}, \code{longitude}, \code{latitude}. 
#' If \code{fields} is missing (default), all available fields are returned.
#' @param print If \code{TRUE} (default) the search results are printed.
#' @return A \code{data.frame} of matching images.
#' @source \url{https://a.mapillary.com/#images}
#' @export
#' @examples
#' \dontrun{
#' images(bbox=c(19.963211,49.317328,20.004066,49.325832), page=1, per_page=10)
#' }
images <- function(bbox, closeto, radius, lookat, 
  start_time, end_time, user_name, user_key, project_key, 
  page, per_page, fields, print=TRUE) {
	
  available_fields <- getOption("mapillRy.available.fields")
  
	# drop empty parameters
	if(missing(bbox)) bbox <- NULL
	else bbox <- paste(bbox, collapse=",")
	if(missing(closeto)) closeto <- NULL
	else closeto <- paste(closeto, collapse=",")
	if(missing(radius)) radius <- NULL
	if(missing(lookat)) lookat <- NULL
	else lookat <- paste(lookat, collapse=",")
	if(missing(start_time)) start_time <- NULL
	if(missing(end_time)) end_time <- NULL
	if(missing(user_name)) user_name <- NULL
	else user_name <- paste(user_name, collapse=",")
	if(missing(user_key)) user_key <- NULL
	else user_key <- paste(user_key, collapse=",")
	if(missing(project_key)) project_key <- NULL
	else project_key <- paste(project_key, collapse=",")
	if(missing(page)) page <- NULL
	if(missing(per_page)) per_page <- NULL
	if(missing(fields)) fields <- available_fields
	
	# make request
	res <- m_get_url(path="images", bbox=bbox, closeto=closeto, radius=radius, lookat=lookat, 
	                 start_time=start_time, end_time=end_time, 
	                 usernames=user_name, userkeys=user_key, project_keys=project_key, 
	                 page=page, per_page=per_page)
	raw <- m_parse(res)
	fields <- available_fields[grep(paste(fields, collapse="|"), available_fields)]
	df <- img_to_df(raw, fields)

	# return
  if(print) print(df)
  invisible(df)
}


#' @title Search for sequences
#' @description Search for sequences.
#'
#' @param bbox Bounding box, given as vector of minx, miny, maxx, maxy.
#' @param start_time Start time images are captured (following ISO 8601 rules).
#' @param end_time End time images are captured before (following ISO 8601 rules).
#' @param user_name Just objects for specific users, given as vector of usernames.
#' @param user_key Just objects for specific users, given as vector of user keys.
#' @param starred If \code{TRUE}, only starred sequences are requested. Default is \code{FALSE}.
#' @param page Page number in pagination.
#' @param per_page Results per page in pagination.
#' @param fields Partially selected output fields, given as string or vector of strings. 
#' Available fields: \code{camera_angle}, \code{camera_make}, \code{camera_model}, 
#' \code{captured_at}, \code{img_key}, \code{panorama}, \code{user_key}, \code{user_name}, 
#' \code{longitude}, \code{latitude}. Default is all available fields.
#' @param print if \code{TRUE} (default) the search results are printed.
#' @return A \code{data.frame} of matching sequences.
#' @source \url{https://a.mapillary.com/#sequences}
#' @export
#' @examples
#' \dontrun{
#' sequences(bbox=c(19.963211,49.317328,20.004066,49.325832), page=1, per_page=10)
#' }
sequences <- function(bbox, start_time, end_time, 
                      user_name, user_key, starred=FALSE, 
                      page, per_page, fields, print=TRUE) {
	
  available_fields <- getOption("mapillRy_available_fields")
  
  # drop empty parameters
  if(missing(bbox)) bbox <- NULL
  else bbox <- paste(bbox, collapse=",")
  if(missing(start_time)) start_time <- NULL
  if(missing(end_time)) end_time <- NULL
  if(missing(user_name)) user_name <- NULL
  else user_name <- paste(user_name, collapse=",")
  if(missing(user_key)) user_key <- NULL
  else user_key <- paste(user_key, collapse=",")
  if(missing(page)) page <- NULL
  if(missing(per_page)) per_page <- NULL
  if(missing(fields)) fields <- available_fields
	
	# make request
  res <- m_get_url(path="sequences", bbox=bbox,  
    start_time=start_time, end_time=end_time, 
    usernames=user_name, userkeys=user_key, starred=tolower(starred), 
    page=page, per_page=per_page)
  raw <- m_parse(res)
  df <- seq_to_df(raw, fields)
  
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
#' img.path <- get_im(key=img, size="h", save=getwd())
#' img.path
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
