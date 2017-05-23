# This code is run whenever the package is loaded
# Put any default configurations/settings in here
.onLoad <- function(libname, pkgname) {
  #never ever ever convert strings to factors
  options(stringsAsFactors = FALSE)
}