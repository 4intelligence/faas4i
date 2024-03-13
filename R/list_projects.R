#' @title Listing user's projects
#'
#' @description Returns a list of projects that belong to the user.
#' @return List with information for all projects sent by the user.
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#'  ## EXAMPLE - Getting list of projects
#'  # Note that you will need to have a previously ran project
#'
#'  my_projects <- faas4i::list_projects()
#'  }
#' }
#' @rdname list_projects
#' @export
#' @seealso
#'  \code{\link[faas4i]{character(0)}}
#'  \code{\link[utils]{str}}
#' @param ... advanced parameters.
#' @importFrom httr2 request req_proxy req_headers req_timeout req_error req_perform resp_status resp_body_json
#' @importFrom utils str
list_projects <- function(...){

    extra_arguments <- list(...)

    if (any(!names(extra_arguments) %in% c("version_check", "proxy_url", "proxy_port"))) {
        invalid_args <- names(extra_arguments)[!names(extra_arguments) %in% c("version_check", "proxy_url", "proxy_port")]
        stop(paste0("Unexpected extra argument(s): ", paste0(invalid_args, collapse = ", "),"."))
    }

    if (is.null(extra_arguments$version_check)) extra_arguments$version_check <- TRUE

    if (extra_arguments$version_check) {
        update_package <- package_version_check(proxy_url = extra_arguments$proxy_url,
                                                proxy_port = extra_arguments$proxy_port)
        if (update_package) return(invisible())
    }

    # Gera o token de autenticação no auth0 auth0
    access_token <- get_access_token()

    url <- get_url("models")

    req <- httr2::request(url)
    req <- httr2::req_proxy(req = req,
                            url = extra_arguments$proxy_url,
                            port = extra_arguments$proxy_port)
    req <- httr2::req_headers(.req = req,
                              "authorization" = paste0("Bearer ", access_token))
    req <- httr2::req_timeout(req = req, seconds = 1200)

    ## Adding req_error so any error in req_perform is not converted into http
    ## error and it is saved in the appropriate object
    req <- httr2::req_error(req = req, is_error = \(req) FALSE)

    response <- httr2::req_perform(req)

    response_status <- httr2::resp_status(response)
    try({response_content <- httr2::resp_body_json(response)}, silent = TRUE)

    if (response_status >= 400) {
        if (response_status == 503) {
            message("API Status code: ", response_status,
                    ".\nContent: Service Unavailable.",
                    ".\nPlease try again later.")
        } else if (response_status == 401) {
            message("API Status code: ", response_status,
                    ".\nContent: Expired Authentication.",
                    ".\nPlease run 'login()' again.")
        } else{
            message("Status Code: ", response_status,
                    "\nAPI Error: An error occurred when trying to retrieve the requested information.",
                    "\nPlease try again later.")
        }
    } else{
        message(utils::str(response_content$records))

        invisible(response_content$records)
    }

}
