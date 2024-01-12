#' @title Get Auth0 acess token
#'
#' @description Gets access token from the api that will be used for modeling and update.
#'
#' @param config_file set the user email.
#' @return auth0 token
#' @rdname get_access_token
#' @examples
#' \dontrun{
#' if(interactive()){
#'  ## EXAMPLE - Downloading project files from an example
#'
#'  my_token <- faas4i::get_access_token()
#'  }
#' }
#' @seealso
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @importFrom jsonlite fromJSON
get_access_token <- function(config_file = paste0(system.file(package = "faas4i"),"/config.json")){

  DOMAIN <- "4intelligence.auth0.com"



  if(!file.exists(config_file)){

    stop("Login to 4CastHub has not been set up yet.\nPlease use function 'faas4i::login()' to authenticate.")

  } else {

    config_json <- jsonlite::fromJSON(config_file)
    access_token <- config_json[["auths"]][[DOMAIN]][["access_token"]]

    return(access_token)
  }
}
