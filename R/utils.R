# make request
m_get_url <- function(path, ...) {
	res <- GET("https://a.mapillary.com", path=paste0("v3/", path), query=list(client_id="QmpJMFpwR09HVG9NdV9lZHo2ZlFGQTpiZDUzNzRiYTc5NWRiYzc3", ...))
	#message("Request: ", res$url) # for debugging
	m_check(res)
	return(res)
}


# check response
m_check <- function(res) {
	if(res$status_code < 400) return(invisible())
	message <- m_parse(res)$message
	stop("HTTP failure: ", res$status_code, "\n", message, call.=FALSE)
}


# json parser
m_parse <- function(res) {
	json <- content(res, as="text")
	if(identical(json, "")) stop("Not output to parse", call.=FALSE)
	if(length(grep("application/json", res$headers$'content-type', fixed=TRUE)) == 0) stop("No JSON to parse", call.=FALSE)
	fromJSON(json, simplifyVector=FALSE)
}


# convert image list to data frame
img_to_df <- function(lst, fields) {
	num_ims <- length(lst[["features"]])
	if(num_ims==0) df <- NULL
	else {
	  # set missing (optional) properties
	  for(i in 1:num_ims) {
	    if(is.null(lst[["features"]][[i]][["properties"]][["ca"]])) {
	      lst[["features"]][[i]][["properties"]][["ca"]] <- NA
	    }
	    if(is.null(lst[["features"]][[i]][["properties"]][["project_key"]])) {
	      lst[["features"]][[i]][["properties"]][["project_key"]] <- NA
	    }
	  }
	  # get properties
		ca <- unlist(sapply(lst[["features"]], function(x) x[["properties"]][["ca"]]))
		camera_make <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["camera_make"]]))
		camera_model <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["camera_model"]]))
		captured_at <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["captured_at"]]))
		key <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["key"]]))
		pano <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["pano"]]))
		project_key <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["project_key"]]))
		user_key <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["user_key"]]))
		username <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["username"]]))
		lon <- unlist(lapply(lst[["features"]], function(x) x[["geometry"]][["coordinates"]][[1]]))
	  lat <- unlist(lapply(lst[["features"]], function(x) x[["geometry"]][["coordinates"]][[2]]))
	  df <- data.frame(camera_angle=ca, camera_make=camera_make, camera_model=camera_model, 
		                 captured_at=captured_at, img_key=key, panorama=pano, 
		                 user_key=user_key, user_name=username, project_key=project_key, 
		                 longitude=lon, latitude=lat, 
		                 stringsAsFactors=FALSE)
		# select output
	  df <- df[fields]
	}
	
	return(df)
}


# convert sequence list to data frame
seq_to_df <- function(lst, fields) {
  num_seq <- length(lst[["features"]])
  if(num_seq==0) df <- NULL
  else {
    # set missing (optional) properties
    for(i in 1:num_seq) {
      if(is.null(lst[["features"]][[i]][["properties"]][["ca"]])) {
        lst[["features"]][[i]][["properties"]][["ca"]] <- NA
      }
      if(is.null(lst[["features"]][[i]][["properties"]][["project_key"]])) {
        lst[["features"]][[i]][["properties"]][["project_key"]] <- NA
      }
    }
    # get properties
    camera_make <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["camera_make"]]))
    captured_at <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["captured_at"]]))
    created_at <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["created_at"]]))
    key <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["key"]]))
    pano <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["pano"]]))
    user_key <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["user_key"]]))
    username <- unlist(lapply(lst[["features"]], function(x) x[["properties"]][["username"]]))
    num_img <- unlist(lapply(lst[["features"]], function(x) length(x[["properties"]][["coordinateProperties"]][["image_keys"]])))
    
    df <- data.frame(camera_make=camera_make, captured_at=captured_at, created_at=created_at, 
                     seq_key=key, panorama=pano, user_key=user_key, user_name=username, 
                     num_img=num_img, 
                     stringsAsFactors=FALSE)
    # select output
    df <- df[fields]
  }
  
  return(df)
}


# convert user list to data frame
usr_to_df <- function(lst, fields) {
  num_usr <- length(lst)
  if(num_usr==0) df <- NULL
  else {
    # set missing (optional) properties
    for(i in 1:num_usr) {
      if(is.null(lst[[i]][["about"]])) {
        lst[[i]][["about"]] <- NA
      }
    }
    # get properties
    about <- unlist(lapply(lst, function(x) x[["about"]]))
    #avatar <- grepl("profile.png", unlist(lapply(lst, function(x) x[["avatar"]])), fixed=TRUE)
    avatar <- unlist(lapply(lst, function(x) x[["avatar"]]))
    #avatar <- gsub("https://d4vkkeqw582u.cloudfront.net/|/profile.png|https://www.mapillary.com/external/fake-avatar.png", "", avatar)
    created_at <- unlist(lapply(lst, function(x) x[["created_at"]]))
    key <- unlist(lapply(lst, function(x) x[["key"]]))
    username <- unlist(lapply(lst, function(x) x[["username"]]))
    df <- data.frame(user_name=username, user_key=key, about=about, 
                     avatar=avatar, created_at=created_at, 
                     stringsAsFactors=FALSE)
    # select output
    df <- df[fields]
  }
  
  return(df)
}


# convert user list to data frame
usr_stats_to_df <- function(lst, fields) {
  num_usr <- length(lst)
  if(num_usr==0) df <- NULL
  else {
    # get properties
    username <- names(lst)
    key <- unlist(lapply(lst, function(x) x[["user_key"]]))
    images <- unlist(lapply(lst, function(x) x[["images"]][["total_count"]]))
    sequences <- unlist(lapply(lst, function(x) x[["sequences"]][["total_count"]]))
    distance <- unlist(lapply(lst, function(x) x[["sequences"]][["total_distance"]]))
    edits <- unlist(lapply(lst, function(x) x[["edits"]][["total_count"]]))
    blurs <- unlist(lapply(lst, function(x) x[["blurs"]][["total_count"]]))
    df <- data.frame(user_name=username, user_key=key, 
                     images=images, sequences=sequences, distance=distance, 
                     edits=edits, blurs=blurs, 
                     stringsAsFactors=FALSE)
    row.names(df) <- NULL
    # select output
    df <- df[fields]
  }
  
  return(df)
}