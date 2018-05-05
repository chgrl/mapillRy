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
#' @param json If \code{FALSE} (default) the results are returned as simplified 
#' \code{data.frame}. \code{TRUE} (invisibly) returns the original JSON object (\code{fields} is
#' ignored).
#' @param print If \code{TRUE} (default) the search results are printed. If \code{json=TRUE},
#' nothing is printed.
#' @return A \code{data.frame} or \code{list} (if \code{json=TRUE}) of matching images.
#' @source \url{https://a.mapillary.com/#images}
#' @export
#' @examples
#' \dontrun{
#' images(bbox=c(19.963211,49.317328,20.004066,49.325832), page=1, per_page=10)
#' }
images <- function(bbox, closeto, radius, lookat, 
                  start_time, end_time, 
                  user_name, user_key, project_key, 
                  page, per_page, fields, 
                  json=FALSE, print=TRUE) {
	
  available_fields <- getOption("mapillRy.available.img.fields")
  
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
	if(json) print <- FALSE
	
	# make request
	res <- m_get_url(path="images", bbox=bbox, closeto=closeto, radius=radius, lookat=lookat, 
	                 start_time=start_time, end_time=end_time, 
	                 usernames=user_name, userkeys=user_key, project_keys=project_key, 
	                 page=page, per_page=per_page)
	raw <- m_parse(res)
	
	# return
	if(json) {
	  invisible(raw)
	} else {
	  fields <- sapply(fields, function(x) available_fields[grep(x, available_fields)])
	  df <- img_to_df(raw, fields)
    if(print) print(df)
    invisible(df)
	}
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
#' Fields are sorted in given order. Available fields: \code{camera_make}, 
#' \code{captured_at}, \code{created_at}, \code{seq_key}, \code{panorama}, 
#' \code{user_key}, \code{user_name}, \code{num_img}. 
#' If \code{fields} is missing (default), all available fields are returned.
#' @param json If \code{FALSE} (default) the results are returned as simplified
#' \code{data.frame}. \code{TRUE} (invisibly) returns the original JSON object (\code{fields} is
#' ignored.
#' @param print If \code{TRUE} (default) the search results are printed. If \code{json=TRUE},
#' nothing is printed.
#' @return A \code{data.frame} or \code{list} (if \code{json=TRUE}) of matching sequences.
#' @source \url{https://a.mapillary.com/#sequences}
#' @export
#' @examples
#' \dontrun{
#' sequences(bbox=c(19.963211,49.317328,20.004066,49.325832), page=1, per_page=10)
#' }
sequences <- function(bbox, start_time, end_time, 
                      user_name, user_key, starred=FALSE, 
                      page, per_page, fields, 
                      json=FALSE, print=TRUE) {
	
  available_fields <- getOption("mapillRy.available.seq.fields")
  
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
  if(json) print <- FALSE
	
	# make request
  res <- m_get_url(path="sequences", bbox=bbox,  
    start_time=start_time, end_time=end_time, 
    usernames=user_name, userkeys=user_key, starred=tolower(starred), 
    page=page, per_page=per_page)
  raw <- m_parse(res)
  
  # return
  if(json) {
    invisible(raw)
  } else {
    fields <- sapply(fields, function(x) available_fields[grep(x, available_fields)])
    df <- seq_to_df(raw, fields)
    if(print) print(df)
    invisible(df)
  }
}


#' @title Search for users
#' @description Search for users who contributed in a certain region.
#'
#' @param bbox Bounding box, given as vector of minx, miny, maxx, maxy.
#' @param user_name String (or vector of strings) to filter for usernames.
#' @param user_key String (or vector of strings) to filter for user keys.
#' @param page Page number in pagination.
#' @param per_page Results per page in pagination.
#' @param fields Partially selected output fields, given as string or vector of strings. 
#' Fields are sorted in given order. Available fields: \code{about}, 
#' \code{avatar}, \code{created_at}, \code{key}, \code{username}. 
#' If \code{fields} is missing (default), all available fields are returned.
#' @param json If \code{FALSE} (default) the results are returned as simplified
#' \code{data.frame}. \code{TRUE} (invisibly) returns the original JSON object (\code{fields} is
#' ignored.
#' @param print if \code{TRUE} (default) the search results are printed.
#' @return A \code{data.frame} of users.
#' @details Returned users are ordered by \code{created_at}. If \code{bbox} is provided, 
#' users are ordered by their last captured times.
#' @source \url{https://a.mapillary.com/#users}
#' @export
#' @examples
#' \dontrun{
#' users(user_name="billy_bob")
#' }
users <- function(bbox, user_name, user_key, 
                  page, per_page, fields, 
                  json=FALSE, print=TRUE) {
	
  available_fields <- getOption("mapillRy.available.usr.fields")
  
	# drop empty parameters
  if(missing(bbox)) bbox <- NULL
  else bbox <- paste(bbox, collapse=",")
  if(missing(user_name)) user_name <- NULL
  else user_name <- paste(user_name, collapse=",")
  if(missing(user_key)) user_key <- NULL
  else user_key <- paste(user_key, collapse=",")
  if(missing(page)) page <- NULL
  if(missing(per_page)) per_page <- NULL
  if(missing(fields)) fields <- available_fields

  	# make request
  res <- m_get_url(path="users", bbox=bbox,
                   usernames=user_name, userkeys=user_key, 
                   page=page, per_page=per_page)
  raw <- m_parse(res)
  
  # return
  if(json) {
    invisible(raw)
  } else {
    fields <- sapply(fields, function(x) available_fields[grep(x, available_fields)])
    df <- usr_to_df(raw, fields)
    if(print) print(df)
    invisible(df)
  }
}


#' @title Get user stats
#' @description Get statistics about a user.
#'
#' @param user_name Username as string (or vector of strings). Optional, if \code{user_key} is given.
#' @param user_key User key as string (or vector of strings). Optional, if \code{user_name} is given.
#' @param fields Partially selected output fields, given as string or vector of strings. 
#' Fields are sorted in given order. Available fields: \code{user_name}, \code{user_key},
#' \code{images}, \code{sequences}, \code{edits}, \code{blurs}. 
#' If \code{fields} is missing (default), all available fields are returned.
#' @param json If \code{FALSE} (default) the results are returned as simplified
#' \code{data.frame}. \code{TRUE} (invisibly) returns the original JSON object (\code{fields} is
#' ignored.
#' @param print if \code{TRUE} (default) the search results are printed.
#' @return A \code{data.frame} of user statistics.
#' @source \url{https://a.mapillary.com/#the-user-statistics}
#' @export
#' @examples
#' \dontrun{
#' user_stats(user_name="billy_bob")
#' user_stats(user_name=c("billy_bob", "mthagaard"), 
#'            user_key=c("5gXh9Bb43yNhWOCoVC-FjQ", "vRyJQKolUExxn6HQiTZMRg"), 
#'            fields=c("user_name","blurs","images", "distance"))
#' }
user_stats <- function(user_name, user_key, 
                       fields, json=FALSE, print=TRUE) {
  
  available_fields <- getOption("mapillRy.available.usr.stats.fields")
  
  # prepare parameters
  if(missing(user_name)) {
    if(missing(user_key)) {
      stop("Please give user_name or user_key")
    } else {
      user_name <- users(user_key=user_key, print=FALSE)$user_name
    }
  } else {
    if(missing(user_key)) {
      user_key <- users(user_name=user_name, print=FALSE)$user_key
    } else {
      users_from_key <- users(user_key=user_key, print=FALSE)
      users_from_name <- users(user_name=user_name, print=FALSE)
      users <- rbind(users_from_key, users_from_name)
      user_key <- users$user_key
      user_name <- users$user_name
    }
  }
  names(user_name) <- NULL
  attributes(user_key)$names <- user_name
  if(missing(fields)) fields <- available_fields
  
  # make request(s)
  raw <- lapply(user_key, function(x) m_parse(m_get_url(path=paste("users", x, "stats", sep="/"))))
  
  # return
  if(json) {
    invisible(raw)
  } else {
    fields <- sapply(fields, function(x) available_fields[grep(x, available_fields)])
    df <- usr_stats_to_df(raw, fields)
    if(print) print(df)
    invisible(df)
  }
}


#' @title Leaderboard for image uploads
#' @description Show the leaderboard for a certain region and/or time of image capture.
#'
#' @param bbox Bounding box, given as vector of minx, miny, maxx, maxy.
#' @param country Countries, given as vector of ISO 3166 country codes.
#' @param start_time Start time images are captured (following ISO 8601 rules).
#' @param end_time End time images are captured before (following ISO 8601 rules).
#' @param user_name String (or vector of strings) to filter for usernames.
#' @param user_key String (or vector of strings) to filter for user keys.
#' @param page Page number in pagination.
#' @param per_page Results per page in pagination.
#' @param fields Partially selected output fields, given as string or vector of strings. 
#' Fields are sorted in given order. Available fields: \code{user_name}, \code{user_key}, 
#' \code{images}.
#' If \code{fields} is missing (default), all available fields are returned.
#' @param json If \code{FALSE} (default) the results are returned as simplified
#' \code{data.frame}. \code{TRUE} (invisibly) returns the original JSON object (\code{fields} is
#' ignored.
#' @param print if \code{TRUE} (default) the search results are printed.
#' @return A \code{data.frame} of users.
#' @source \url{https://a.mapillary.com/#leaderboard}
#' @references \url{https://en.wikipedia.org/wiki/ISO_3166}
#' @export
#' @examples
#' \dontrun{
#' leaderboard(countries=c("de", "at", "ch"), page=1, per_page=10))
#' }
leaderboard <- function(bbox, country,
                        start_time, end_time,
                        user_name, user_key, 
                        page, per_page, fields, 
                        json=FALSE, print=TRUE) {
  
  available_fields <- getOption("mapillRy.available.ldrbrd.fields")
  
  # prepare parameters
  if(missing(bbox)) bbox <- NULL
  else bbox <- paste(bbox, collapse=",")
  if(missing(country)) country <- NULL
  else country <- paste(country, collapse=",")
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
  res <- m_get_url(path="leaderboard/images", bbox=bbox, iso_countries=country, 
                   usernames=user_name, userkeys=user_key, 
                   page=page, per_page=per_page)
  raw <- m_parse(res)
  
  # return
  if(json) {
    invisible(raw)
  } else {
    fields <- sapply(fields, function(x) available_fields[grep(x, available_fields)])
    df <- lead_to_df(raw, fields)
    if(print) print(df)
    invisible(df)
  }
}


#' @title Get images
#' @description Save images and display images in R.
#'
#' @param img_key Image key.
#' @param size Image size. One of \code{s[mall]} (320px), 
#' \code{m[edium]} (640px, the default), \code{l[arge]} (1024px) 
#' or \code{h[uge]} (2048px).
#' @param save Directory where to save the image file. Optional --
#' if missing, the image is saved as temporary file and diplayed.
#' @return If \code{save=FALSE}, an image is displayed only. 
#' Otherwise, the path of the saved image is returned.
#' @source \url{https://a.mapillary.com/#images}
#' @export
#' @examples
#' \dontrun{
#' img <- images(closeto=c(9.436385,46.336591), radius=1000, 
#'   page=1, per_page=1, print=FALSE)$img_key
#' get_img(img_key=img)
#' img.path <- get_img(img_key=img, size="h", save=tempdir())
#' img.path
#' }
get_img <- function(img_key, size="m", save) {
  
  # prepare url
  avail_sizes <- c("small", "medium", "large", "huge")
  sizes <- c(320, 640, 1024, 2048)
  size <- sizes[pmatch(size, avail_sizes)]
  img_url <- paste0("https://d1cuyjsrcm0gby.cloudfront.net/", img_key, "/thumb-", size, ".jpg")
  
  # download image
  rtrn <- TRUE
  if(missing(save)) {
    save <- tempdir()
    rtrn <- FALSE
  }
  img_path <- file.path(save, paste(img_key, "jpg", sep="."))
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
