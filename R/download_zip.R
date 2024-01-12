#' @title Downloads zipped file of modeling results
#'
#' @description This function allows the user to download the \code{forecast_pack}'s for any model associated
#' to the \code{logged user}.
#'
#' @param project_id id of the project to be downloaded which can be founded by using the function \code{list_files}..
#' @param ... advanced parameters.
#' @param path folder in which the downloaded files should be saved.
#' @param filename name of the zipped file.
#' @return API response with information about saved file.
#' @examples
#' \dontrun{
#' if(interactive()){
#'
#'  ## EXAMPLE - Downloading project files from an example
#'  # Note that you will need to have a previously ran project
#'  project_id <- 'User project ID'
#'  folder_path <- 'Folder in which the file will be saved'
#'  filename <- 'Name of the file to be saved'
#'
#'  faas4i::download_zip(project_id = project_id, path=folder_path, filename = filename)
#'  }
#' }
#' @rdname download_zip
#' @export
#' @seealso
#'  \code{\link[faas4i]{character(0)}}
#' @importFrom httr2 request req_proxy req_headers req_timeout req_error req_perform resp_status resp_body_json
download_zip <- function(project_id, path, filename,...){

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

    if (grepl("[^[:alnum:]\\_\\-]",filename)) {
        return(message("Parameter 'filename' must not contain special characters."))
    }

    # HTTR2: Headers are case insensitive
    #  resp |> resp_header("SERVER")
    base_url <- get_url("models")
    url <- paste0(base_url,"/", project_id)

    ## Let's check if project is ready for download
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
    response_content <- httr2::resp_body_json(response)

    download_allowed <- FALSE
    if (response_status >= 400) {
        if (response_status == 503) {
            message("API Status code: ", response_status,
                    ".\nContent: Service Unavailable.",
                    ".\nPlease try again later.")
        } else if (response_status == 401) {
            message("API Status code: ", response_status,
                    ".\nContent: Expired Authentication.",
                    ".\nPlease run 'login()' again.")
        } else if (response_status == 403) {
            message("API Status code: ", response_status,
                    ".\nContent: You don't have access rights to this content.")
        } else {
            message("Status Code: ", response_status,
                    "\nAPI Error: An error occurred when trying to retrieve the requested information.",
                    "\nPlease try again later.")
        }
    } else {
        if (!response_content$status %in% c("success", "partial_success", "error", "excluded")) {
            message("Your request is still being processed, with the following status: ", response_content$status)
        } else {
            if (response_content$status == "error") {
                message("Error: There was an error while running your job.")
            }
            if (response_content$status == "excluded") {
                message("Error: The project with this project_id has been excluded.")
            }
            if (response_content$status == "partial_success") {
                message("At least one of the outputs from this request is not yet ready, downloading available files.")
                download_allowed <- TRUE
            }
            if (response_content$status == "success") {
                download_allowed <- TRUE
            }
        }
    }


    if (download_allowed) {
        if (!dir.exists(path)) {dir.create(path)}

        save_file <- paste0(path, '/forecast-', filename, '.zip')

        download_url <- paste0(url, "/download")
        req <- httr2::request(download_url)
        req <- httr2::req_proxy(req = req,
                                url = extra_arguments$proxy_url,
                                port = extra_arguments$proxy_port)
        req <- httr2::req_headers(.req = req,
                                  "authorization" = paste0("Bearer ", access_token))
        req <- httr2::req_timeout(req = req, seconds = 1200)

        ## Adding req_error so any error in req_perform is not converted into http
        ## error and it is saved in the appropriate object
        req <- httr2::req_error(req = req, is_error = \(req) FALSE)

        response_zip <- httr2::req_perform(req,
                                           path = save_file)

        if (httr2::resp_status(response_zip) >= 400) {
            if (httr2::resp_status(response_zip) == 503) {
                message("API Status code: ", httr2::resp_status(response_zip),
                        ".\nContent: Service Unavailable.",
                        ".\nPlease try again later.")
            } else{
                message("Status Code: ", httr2::resp_status(response_zip),
                        "\nError: An empty file was saved to ", save_file,
                        "\nPlease check your internet connection and try again later.")
            }
        } else{
            message(paste0('File saved to ', save_file))
        }
    }
}
