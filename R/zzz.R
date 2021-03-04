

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n",
    " *********************************************************** \n",
    "          Loading standardize package version 0.2.2          \n",
    "     Call standardize.news() to see new features/changes     \n",
    " *********************************************************** \n"
  )
}


#' Print the version history of the \code{standardize} package.
#'
#' @return The function prints the changes and new features for each version
#'   of the package (starting with the newest version).
#'
#' @examples
#' standardize.news()
#'
#' @author Christopher D. Eager <eager.stats@gmail.com>
#'
#' @export
standardize.news <- function() {
  file.show(system.file("NEWS.html", package = "standardize"))
}

