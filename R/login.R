#' @title Login to 4CastHub
#' @description This function is used to validate the two factor authentication of your 4CastHub user. If you select the checkbox 'Remember this device por 30 days', the login only needs to be done once every 30 days.
#' @param sleep_time Maximum waiting for URI authentication
#' @param ... PARAM_DESCRIPTION
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
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname login
#' @export
#' @importFrom httr2 request req_proxy req_headers req_timeout req_body_raw req_error req_perform resp_status resp_body_json
#' @importFrom jsonlite toJSON
login <- function(sleep_time = 90, ...){

  extra_arguments <- list(...)

  if (any(!names(extra_arguments) %in% c("proxy_url", "proxy_port"))) {
    invalid_args <- names(extra_arguments)[!names(extra_arguments) %in% c("proxy_url", "proxy_port")]
    stop(paste0("Unexpected extra argument(s): ", paste0(invalid_args, collapse = ", "),"."))
  }

  ## If sleep_time is less than or equal to 1, make it 2
  if (sleep_time <= 1) {
    sleep_time <- 2
  }

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
  TOKEN_ETA <- sleep_time
  HTTP_403 <- 403

  ### Starting  authentication
  url <- paste0(SCHEME, DOMAIN, AUTH0_DEVICE_CODE_URL)

  req <- httr2::request(url)
  req <- httr2::req_proxy(req = req,
                          url = extra_arguments$proxy_url,
                          port = extra_arguments$proxy_port)
  req <- httr2::req_headers(.req = req,
                            "content-type" = "application/x-www-form-urlencoded",
                            "User-Agent" = FOURI_USER_AGENT)
  req <- httr2::req_timeout(req = req, seconds = TIMEOUT)
  req <- httr2::req_body_raw(req = req, body = payload)

  ## Adding req_error so any error in req_perform is not converted into http
  ## error and it is saved in the appropriate object
  req <- httr2::req_error(req = req, is_error = \(req) FALSE)

  response <- httr2::req_perform(req)

  response_status <- httr2::resp_status(response)
  try({response_content <- httr2::resp_body_json(response)}, silent = TRUE)

  if (response_status >= 400) {

    message("\nSomething went wrong!\n",
            response_content[["error_description"]],
            "\nPlease restart the login flow running `faas4i::login()`.")

  }else{
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
    pb   <- txtProgressBar(1, TOKEN_ETA, style = 3)
    for (i in seq(from = 1, to = TOKEN_ETA)) {
      Sys.sleep(1)
      setTxtProgressBar(pb,i)
    }
    ### Getting token
    req <- httr2::request(url_token)
    req <- httr2::req_proxy(req = req,
                            url = extra_arguments$proxy_url,
                            port = extra_arguments$proxy_port)
    req <- httr2::req_headers(.req = req,
                              "content-type" = "application/x-www-form-urlencoded",
                              "User-Agent" = FOURI_USER_AGENT)
    req <- httr2::req_timeout(req = req, seconds = TIMEOUT)
    req <- httr2::req_body_raw(req = req, body = payload_token)

    ## Adding req_error so any error in req_perform is not converted into http
    ## error and it is saved in the appropriate object
    req <- httr2::req_error(req = req, is_error = \(req) FALSE)

    token_response <- httr2::req_perform(req)

    response_status <- httr2::resp_status(token_response)
    try({response_content <- httr2::resp_body_json(token_response)}, silent = TRUE)

    if (response_status < 400) {
      ## Saving config.json
      config_dict[["auths"]][[DOMAIN]][['expires_in']] = NULL
      config_dict[["auths"]][[DOMAIN]] = c(config_dict[["auths"]][[DOMAIN]], response_content)
      write(jsonlite::toJSON(config_dict, auto_unbox = TRUE), paste0(system.file(package = "faas4i"),"/config.json"))

      message("\nLogin successful!")
    } else{
      message("\nSomething went wrong!\n",
              response_content[["error_description"]],
              "\nPlease restart the login flow running `faas4i::login()`.")
    }

  }
}
