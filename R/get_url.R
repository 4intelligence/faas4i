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

  if (type == "update"){
    ## If body_size is not NULL and it is greater than 30mb, we set base_url
    ## to an alternative api, with larger payload
    if(!is.null(body_size)){
      if (body_size > 30){
        base_url <- "https://modelling.4casthub.ai/api/v1/"
      }
    }

    return(paste0(base_url, "projects/model-update"))
  }
  if (type == "validate"){
    return("https://run-prod-4casthub-api-faas-validation-zdfk3g7cpq-ue.a.run.app/api/v1/validate")
  }
}
