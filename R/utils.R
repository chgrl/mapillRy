# make request
m_get_url <- function(path, ...) {
	res <- GET("https://a.mapillary.com", path=paste0("v2/", path), query=list(client_id="QmpJMFpwR09HVG9NdV9lZHo2ZlFGQTo1YjkyNzcwMjkxYjhiZGY4", ...))
	message("Request: ", res$url) # for debugging
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


# format timestamp
epoch_to_date <- function(epoch) {
	date <- as.POSIXct(epoch, origin="1970-01-01")
  date_form <- strftime(date, "YYYY-MM-DD %H:%M:%S")
  return(date_form)
}


# convert list to data frame
to_df <- function(lst, from) {
	if(from=="search_im_random") {
		lst_cln <- lapply(lst, function(x) ifelse(is.null(x), "", x))
		df <- as.data.frame(lst_cln, stringsAsFactors=FALSE)
	} else if(from=="search_user") {
		num_user <- length(lst$matches)
		if(num_user==0) df <- NULL
		else {
			df <- as.data.frame(lst$matches[[1]], stringsAsFactors=FALSE)
			if(is.null(df$avatar)) df$avatar <- FALSE
			else df$avatar <- TRUE
			if(num_user > 1) {
				for(i in 2: num_user) {
					usr <- as.data.frame(lst$matches[[i]], stringsAsFactors=FALSE)
					if(is.null(usr$avatar)) usr$avatar <- FALSE
					else usr$avatar <- TRUE
					df <- rbind(df, usr)
				}
			}
		}
	} else {	
		num_ims <- length(lst$ims)
		if(num_ims==0) df <- NULL
		else {
			lst_cln <- lapply(lst$ims, function(x) lapply(x, function(x) ifelse(is.null(x), "", x)))
			df <- as.data.frame(lst_cln[[1]], stringsAsFactors=FALSE)
			if(num_ims > 1) {
				for(i in 2:num_ims) {
					df <- rbind(df, as.data.frame(lst_cln[[i]], stringsAsFactors=FALSE))
				}
			}
		}
	}
	
	return(df)
}

