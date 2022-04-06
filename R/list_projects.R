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
list_projects <- function(){

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
