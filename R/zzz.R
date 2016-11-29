.onAttach <- 
function(libname, pkgname) {
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
    packageStartupMessage(" ")
    packageStartupMessage(paste("This is", pkgname, ver))
    packageStartupMessage(" ")
    packageStartupMessage("Type changes(\"mapillRy\") to see changes/bug fixes, help(mapillRy) for documentation")
    packageStartupMessage("or citation(\"mapillRy\") for how to cite mapillRy.")
    packageStartupMessage(" ")
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
