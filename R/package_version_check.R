#' @title Checking package version for faas4i
#'
#' @description This function checks if the package is running with the latest vesion available of faas4i.
#'
#' @param repo repository url to obtain latest version
#' @param proxy_url The url to be used as proxy
#' @param proxy_port Proxy port number
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
#' @importFrom httr2 request req_proxy req_error req_perform resp_body_json
#' @importFrom remotes install_github
package_version_check <- function(repo = "https://github.com/4intelligence/faas4i",
                                  proxy_url, proxy_port) {

  ## Getting latest tag and making it into a package version object
  uri <- 'https://api.github.com/repos/4intelligence/faas4i/releases/latest'
  req <- httr2::request(uri)
  req <- httr2::req_proxy(req = req,
                          url = proxy_url,
                          port = proxy_port)

  ## Adding req_error so any error in req_perform is not converted into http
  ## error and it is saved in the appropriate object
  req <- httr2::req_error(req = req, is_error = \(req) FALSE)

  git_response <- httr2::req_perform(req)

  try({response_content <- httr2::resp_body_json(git_response)}, silent = TRUE)
  latest_tag <- response_content[["tag_name"]]

  if (is.null(latest_tag)) {
    return(FALSE)
  }

  latest_tag <- gsub("^v", "", latest_tag)
  latest_tag <- as.package_version(latest_tag)


  ## Obtaining installed version of faas4i
  package_version <- packageVersion("faas4i")

  if (latest_tag != package_version) {
    var1 <- readline(prompt = cat("'faas4i' has a more recent version, would you like to update it now? (y/n)
    y: YES (recommended)
    n: NO "))

    if (tolower(var1) == "y") {
      remotes::install_github("4intelligence/faas4i", force = TRUE)
      warning("Please restart your R session.")
      return(TRUE)
    } else {
      if (tolower(var1) != "n") {
        stop('Invalid input, please enter Y/y for yes or N/n for No.')
      }
    }
  }

  return(FALSE)
}
