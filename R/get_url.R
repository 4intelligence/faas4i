#' @title Getting url for modeling
#'
#' @description Getting url for FaaS modeling.
#'
#' @param run_local if TRUE, it will be sent to Docker to run locally. Default: FALSE.
#' @return Api url.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_url
#' @param type if 'models' or 'update'
get_url <- function(type) {

  base_url <- "https://4i-4casthub-faas-prod-api.azurewebsites.net/api/v1/"

  if (type == "models"){
    return(paste0(base_url, "projects"))
  }

  if (type == "update"){
    return(paste0(base_url, "projects/model-update"))
  }
  if (type == "validate"){
    return("https://4i-4casthub-validation-prod-api.azurewebsites.net/api/v1/validate")
  }
}
