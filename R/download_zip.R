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
#'  \code{\link[httr]{insensitive}},\code{\link[httr]{GET}},\code{\link[httr]{add_headers}},\code{\link[httr]{timeout}},\code{\link[httr]{write_disk}}
#' @importFrom httr insensitive GET use_proxy add_headers timeout content status_code write_disk
download_zip <- function(project_id, path, filename,...){

    extra_arguments <- list(...)

    if (any(! names(extra_arguments) %in% c("version_check", "proxy_url", "proxy_port"))){
        invalid_args <- names(extra_arguments)[! names(extra_arguments) %in% c("version_check", "proxy_url", "proxy_port")]
        stop(paste0("Unexpected extra argument(s): ", paste0(invalid_args, collapse = ", "),"."))
    }

    if (is.null(extra_arguments$version_check)) extra_arguments$version_check <- TRUE

    if(extra_arguments$version_check){
        update_package <- package_version_check(proxy_url = extra_arguments$proxy_url,
                                                proxy_port = extra_arguments$proxy_port)
        if(update_package) return(invisible())
    }

    # Gera o token de autenticação no auth0 auth0
    access_token <- get_access_token()

    if (grepl("[^[:alnum:]\\_\\-]",filename)){
        return(message("Parameter 'filename' must not contain special characters."))
    }

    headers <- c("authorization"= paste0("Bearer ", access_token))
    headers <- httr::insensitive(headers)

    base_url <- get_url("models")
    url <- paste0(base_url,"/", project_id)

    ## Let's check if project is ready for download
    response <- httr::GET(
        url,
        httr::use_proxy(url = extra_arguments$proxy_url,
                        port = extra_arguments$proxy_port),
        httr::add_headers(.headers = headers),
        config = httr::timeout(1200))

    response_content <- httr::content(response)

    download_allowed <- FALSE
    if (httr::status_code(response) >= 400){
        if(httr::status_code(response) == 503){
            message("API Status code: ", httr::status_code(response),
                    ".\nContent: Service Unavailable.",
                    ".\nPlease try again later.")
        } else if(httr::status_code(response) == 401){
            message("API Status code: ", httr::status_code(response),
                    ".\nContent: Expired Authentication.",
                    ".\nPlease run 'login()' again.")
        } else if(httr::status_code(response) == 403){
            message("API Status code: ", httr::status_code(response),
                    ".\nContent: You don't have access rights to this content.")
        } else{
            message("Status Code: ", httr::status_code(response),
                    "\nAPI Error: An error occurred when trying to retrieve the requested information.",
                    "\nPlease try again later.")
        }
    } else{
        if (! response_content$status %in% c("success", "partial_success", "error", "excluded")){
            message("Your request is still being processed, with the following status: ", response_content$status)
        } else{
            if (response_content$status == "error"){
                message("Error: There was an error while running your job.")
            }
            if (response_content$status == "excluded"){
                message("Error: The project with this project_id has been excluded.")
            }
            if (response_content$status == "partial_success"){
                message("At least one of the outputs from this request is not yet ready, downloading available files.")
                download_allowed <- TRUE
            }
            if (response_content$status == "success"){
                download_allowed <- TRUE
            }
        }
    }


    if (download_allowed){
        if (!dir.exists(path)){dir.create(path)}

        save_file <- paste0(path, '/forecast-', filename, '.zip')

        response_zip <- httr::GET(
            paste0(url, "/download"),
            httr::use_proxy(url = extra_arguments$proxy_url,
                            port = extra_arguments$proxy_port),
            httr::add_headers(.headers = headers),
            config = httr::timeout(1200),
            httr::write_disk(save_file, overwrite=TRUE))

        if (httr::status_code(response_zip) >= 400){
            if(httr::status_code(response_zip) == 503){
                message("API Status code: ", httr::status_code(response_zip),
                        ".\nContent: Service Unavailable.",
                        ".\nPlease try again later.")
            } else{
                message("Status Code: ", httr::status_code(response_zip),
                        "\nError: An empty file was saved to ", save_file,
                        "\nPlease check your internet connection and try again later.")
            }
        } else{
            message(paste0('File saved to ', save_file))
        }
    }
}
