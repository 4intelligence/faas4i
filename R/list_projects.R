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
#'  \code{\link[httr]{insensitive}},\code{\link[httr]{GET}},\code{\link[httr]{add_headers}},\code{\link[httr]{timeout}},\code{\link[httr]{content}}
#'  \code{\link[utils]{str}}
#' @importFrom httr insensitive GET add_headers timeout content status_code
#' @importFrom utils str
list_projects <- function(...){

    extra_arguments <- list(...)

    if (any(! names(extra_arguments) %in% c("version_check"))){
        invalid_args <- names(extra_arguments)[! names(extra_arguments) %in% c("version_check")]
        stop(paste0("Unexpected extra argument(s): ", paste0(invalid_args, collapse = ", "),"."))
    }

    if (is.null(extra_arguments$version_check)) extra_arguments$version_check <- TRUE

    if(extra_arguments$version_check){
        update_package <- package_version_check()
        if(update_package) return(invisible())
    }

    # Gera o token de autenticação no auth0 auth0
    access_token <- get_access_token()

    headers <- c("authorization"= paste0("Bearer ", access_token))
    headers <- httr::insensitive(headers)

    url <- get_url("models")

    response <- httr::GET(
        url,
        httr::add_headers(.headers = headers),
        config = httr::timeout(1200))

    response_content <- httr::content(response)

    if (httr::status_code(response) >= 400){
        if(httr::status_code(response) == 503){
            message("API Status code: ", httr::status_code(response),
                    ".\nContent: Service Unavailable.",
                    ".\nPlease try again later.")
        } else if(httr::status_code(response) == 401){
            message("API Status code: ", httr::status_code(response),
                    ".\nContent: Expired Authentication.",
                    ".\nPlease run 'login()' again.")
        } else{
            message("Status Code: ", httr::status_code(response),
                    "\nAPI Error: An error occurred when trying to retrieve the requested information.",
                    "\nPlease try again later.")
        }
    } else{
        message(utils::str(response_content$records))

        invisible(response_content$records)
    }

}
