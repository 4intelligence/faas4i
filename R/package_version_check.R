#' @title Checking package version for faas4i
#'
#' @description This function checks if the package is running with the latest vesion available of faas4i.
#'
#' @param repo repository url to obtain latest version
#' @return OUTPUT_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname package_version_check
#' @seealso
#'  \code{\link[remotes]{install_github}}
#' @importFrom remotes install_github
package_version_check <- function(repo = "https://github.com/4intelligence/faas4i") {

  ## Getting latest tag and making it into a package version object
  latest_tag <- system(paste0("git ls-remote --refs --tags ", repo, " | cut --delimiter='/' --fields=3 | tr '-' '~'| sort --version-sort | tail --lines=1"), intern = T)
  latest_tag <- gsub("^v", "", latest_tag)
  latest_tag <- as.package_version(latest_tag)
  ## If can't find latest tag, we return nothing
  if(length(latest_tag) == 0) {return(invisible())}

  ## Obtaining installed version of faas4i
  package_version <- packageVersion("faas4i")

  if (latest_tag > package_version){
    var1 <- readline(prompt =cat("'faas4i' has a more recent version, would you like to update it now? (y/n)
    y: YES (recommended)
    n: NO "))

    if (var1 == "y"){
      remotes::install_github("4intelligence/faas4i", force = TRUE)
      warning("Please restart your R session.")
      return(TRUE)
    }
  }
  return(FALSE)
}
