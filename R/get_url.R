#' @title Getting url for modeling
#'
#' @description Getting url for FaaS modeling.
#'
#' @param type Modeling type, if 'models' or 'update'
#' @param body_size Size of compressed body, in megabits
#' @return Api url.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_url
get_url <- function(type, body_size = NULL) {

  base_url <- "https://run-prod-4casthub-faas-modelling-api-zdfk3g7cpq-ue.a.run.app/api/v1/"

  if (type == "models"){
    return(paste0(base_url, "projects"))
  }

  if (type == "validate"){
    return("https://run-prod-4casthub-api-faas-validation-zdfk3g7cpq-ue.a.run.app/api/v1/validate")
  }
}