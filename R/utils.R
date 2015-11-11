# make request
m_get_url <- function(path, ...) {
	res <- GET("https://a.mapillary.com", path=paste0("v2/", path), query=list(client_id="QmpJMFpwR09HVG9NdV9lZHo2ZlFGQTpiZDUzNzRiYTc5NWRiYzc3", ...))
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


# convert timestamp to date
epoch_to_date <- function(epoch) {
	date <- as.POSIXct(epoch/1000, origin="1970-01-01")
  date_form <- strftime(date, "%Y-%m-%d %H:%M:%S")
  return(date_form)
}


# convert timestamp to epoch
date_to_epoch <- function(date) {
	if(nchar(date)==19) {
		if(length(grep("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}", date))!=1) stop("Cannot read 'start_time'/'end_time'")
	} else if(nchar(date)==16) {
		if(length(grep("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}", date))!=1) stop("Cannot read 'start_time'/'end_time'")
		date <- paste0(date, ":00")
	} else if(nchar(date)==13) {
		if(length(grep("[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}", date))!=1) stop("Cannot read 'start_time'/'end_time'")
		date <- paste0(date, ":00:00")
	} else if(nchar(date)==10) {
		if(length(grep("[0-9]{4}-[0-9]{2}-[0-9]{2}", date))!=1) stop("Cannot read 'start_time'/'end_time'")
		date <- paste0(date, " 00:00:00")
	} else stop("'start_time' and 'end_time' must be specified as ISO date")
	epoch <- as.character(as.numeric(strptime(date, "%Y-%m-%d %H:%M:%S"))*1000)
	return(epoch)
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
			if(!is.null(df$avatar)) df$avatar <- NULL
			if(num_user > 1) {
				for(i in 2: num_user) {
					usr <- as.data.frame(lst$matches[[i]], stringsAsFactors=FALSE)
					if(!is.null(usr$avatar)) usr$avatar <- NULL
					df <- rbind(df, usr)
				}
			}
		}
	} else if(from=="stats_im") {
		df <- as.data.frame(lst, stringsAsFactors=FALSE)
	} else if(from=="stats_top") {
		#toplist <- lst$toplist
		total <- lst$total
		total_count <- lst$total_count
		df_total <- as.data.frame(total[[1]], stringsAsFactors=FALSE)
		for(i in 2:length(total)) {
			df_total <- rbind(df_total, as.data.frame(total[[i]], stringsAsFactors=FALSE))
		}
		df <- list(total=df_total, total_count=total_count)
	} else if(from=="search_im_cm") {
		df <- lst
	} else {	
		num_ims <- length(lst[[2]])
		if(num_ims==0) df <- NULL
		else {
			lst_cln <- lapply(lst[[2]], function(x) lapply(x, function(x) ifelse(is.null(x), "", x)))
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

