% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gets.R
\name{leaderboard}
\alias{leaderboard}
\title{Leaderboard for image uploads}
\source{
\url{https://a.mapillary.com/#leaderboard}
}
\usage{
leaderboard(bbox, country, start_time, end_time, user_name, user_key, page,
  per_page, fields, json = FALSE, print = TRUE)
}
\arguments{
\item{bbox}{Bounding box, given as vector of minx, miny, maxx, maxy.}

\item{country}{Countries, given as vector of ISO 3166 country codes.}

\item{start_time}{Start time images are captured (following ISO 8601 rules).}

\item{end_time}{End time images are captured before (following ISO 8601 rules).}

\item{user_name}{String (or vector of strings) to filter for usernames.}

\item{user_key}{String (or vector of strings) to filter for user keys.}

\item{page}{Page number in pagination.}

\item{per_page}{Results per page in pagination.}

\item{fields}{Partially selected output fields, given as string or vector of strings. 
Fields are sorted in given order. Available fields: \code{user_name}, \code{user_key}, 
\code{images}.
If \code{fields} is missing (default), all available fields are returned.}

\item{json}{If \code{FALSE} (default) the results are returned as simplified
\code{data.frame}. \code{TRUE} (invisibly) returns the original JSON object (\code{fields} is
ignored.}

\item{print}{if \code{TRUE} (default) the search results are printed.}
}
\value{
A \code{data.frame} of users.
}
\description{
Show the leaderboard for a certain region and/or time of image capture.
}
\examples{
\dontrun{
leaderboard(countries=c("de", "at", "ch"), page=1, per_page=10))
}
}
\references{
\url{https://en.wikipedia.org/wiki/ISO_3166}
}
