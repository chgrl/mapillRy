.onAttach <- function(libname, pkgname) {
  ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
  packageStartupMessage(" ")
  packageStartupMessage(paste("This is", pkgname, ver))
  packageStartupMessage(" ")
  packageStartupMessage("Type changes(\"mapillRy\") to see changes/bug fixes, help(mapillRy) for documentation")
  packageStartupMessage("or citation(\"mapillRy\") for how to cite mapillRy.")
  packageStartupMessage(" ")
}



.onLoad <- function(libname, pkgname) {
  options(mapillRy.available.img.fields = c("camera_angle", "camera_make", "camera_model", 
                                        "captured_at", "img_key", "panorama", 
                                        "user_key", "user_name", "project_key", 
                                        "longitude", "latitude"))
  options(mapillRy.available.seq.fields = c("camera_make", "captured_at", "created_at",
                                            "seq_key", "panorama", "user_key", "user_name", 
                                            "num_img"))
  options(mapillRy.available.usr.fields = c("user_name", "user_key", "about",
                                            "created_at", "avatar"))
  options(mapillRy.available.usr.stats.fields = c("user_name", "user_key", "images", "sequences", "distance", 
                                                  "edits", "blurs"))
  options(mapillRy.available.ldrbrd.fields = c("user_name", "user_key", "images"))
}
          



#' @title View changes notes.
#' @description \code{changes} brings up the NEWS file of the package. 
#'
#' @param pkg Set to the default "mapillRy". Other packages make no sense.
#' @export
#' @examples
#' \dontrun{
#' changes()
#' }
changes <- 
function(pkg="mapillRy") {
    if(pkg=="mapillRy") file.show(file.path(system.file(package="mapillRy"), "NEWS"))
}
