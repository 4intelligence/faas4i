#' @title Login to 4CastHub
#' @description This function is used to validate the two factor authentication of your 4CastHub user. If you select the checkbox 'Remember this device por 30 days', the login only needs to be done once every 30 days.
#'
#' @return A file with your authentication will be saved at your package location.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  faas4i::login()
#'  ## Once the url is printed, copy and paste it to your browser and follow with authentication
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{POST}},\code{\link[httr]{add_headers}},\code{\link[httr]{timeout}},\code{\link[httr]{content}},\code{\link[httr]{status_code}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname login
#' @export
#' @importFrom httr POST add_headers timeout status_code content
#' @importFrom jsonlite toJSON
login <- function(){
  ### Setting some default configurations
  DOMAIN <- "4intelligence.auth0.com"
  CLIENT_ID <- "BBpqcGVYyoFKsjwgf8GcEDhxIBQRQM1H"
  AUDIENCE <- "4casthub"

  SCHEME <- "https://"

  AUTH0_DEVICE_CODE_URL <- "/oauth/device/code"
  AUTH0_TOKEN_REQUEST_URL <- "/oauth/token"
  FOURI_USER_AGENT <- "4i - PyFaas Script 1.2.0"

  scope <- "offline_access openid profile email"
  payload <- paste0("client_id=",CLIENT_ID, "&scope=offline_access+openid+profile+email&audience=4casthub")

  TIMEOUT <- 10
  TOKEN_ETA <- 90
  HTTP_403 <- 403

  ### Starting  authentication
  url <- paste0(SCHEME, DOMAIN, AUTH0_DEVICE_CODE_URL)

  headers <- c("content-type" = "application/x-www-form-urlencoded",
               "User-Agent" = FOURI_USER_AGENT)

  response <- httr::POST(url,
                         body = payload,
                         httr::add_headers(.headers = headers),
                         config = httr::timeout(TIMEOUT))


  if(httr::status_code(response) >= 400){

    message("\nSomething went wrong!\n",
            httr::content(token_response)[["error_description"]],
            "\nPlease restart the login flow running `faas4i::login()`.")

  }else{
    response_content <- httr::content(response)
    verification_url <- response_content$verification_uri_complete

    message("Please copy and paste the URL below in your browser to authorize your device.",
            "\nDevice verification URI: ", verification_url)
    # try(browseURL(verification_url), silent = TRUE)

    config_dict <- list("auths" = list("4intelligence.auth0.com" = response_content))

    url_token <- paste0(SCHEME, DOMAIN, AUTH0_TOKEN_REQUEST_URL)
    device_code <- response_content$device_code
    payload_token <- paste0("grant_type=urn:ietf:params:oauth:grant-type:device_code&device_code=", device_code,
                            "&client_id=", CLIENT_ID)

    message("Waiting for URI Authentication...")
    pb   <- txtProgressBar(1, TOKEN_ETA, style=3)
    for (i in seq(from = 1, to = TOKEN_ETA)){
      Sys.sleep(1)
      setTxtProgressBar(pb,i)
    }
    ### Getting token
    token_response <- httr::POST(url_token,
                                body = payload_token,
                                httr::add_headers(.headers = headers),
                                config = httr::timeout(TIMEOUT))

    if(httr::status_code(token_response) < 400){
      ## Saving config.json
      config_dict[["auths"]][[DOMAIN]][['expires_in']] = NULL
      config_dict[["auths"]][[DOMAIN]] =c(config_dict[["auths"]][[DOMAIN]], httr::content(token_response))
      write(jsonlite::toJSON(config_dict, auto_unbox = TRUE), paste0(system.file(package = "faas4i"),"/config.json"))

      message("\nLogin successful!")
    } else{
      message("\nSomething went wrong!\n",
              httr::content(token_response)[["error_description"]],
              "\nPlease restart the login flow running `faas4i::login()`.")
    }

  }
}
