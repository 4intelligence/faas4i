#' @title Prepare body for FaaS modeling
#'
#' @description This function will prepare the encoded body to be sent via api.
#' @param data_list list with named datasets to be modeled.
#' @param date_variable name of variable with date information in all datasets in \code{data_list}.
#' @param date_format format of \code{date_variable} in all datasets in \code{data_list}.
#' @param model_spec list with modeling and cross validation setup.
#' @param project_name project name. It accepts character and numeric inputs. Special characters will be removed.
#' @param user_email email to receive the outputs.
#' @return Encoded and compressed body to send the request.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[caTools]{base64encode & base64decode}}
#'  \code{\link[base]{memCompress}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#' @rdname prepare_body
#' @importFrom utils packageVersion
#' @importFrom caTools base64encode
#' @importFrom jsonlite toJSON
prepare_body <- function(data_list, date_variable, date_format, model_spec, project_name, user_email) {

  ## Treating special characters
  names(data_list) <- make.names(iconv(names(data_list), to = 'ASCII//TRANSLIT'))
  names(data_list) <- gsub("[^[:alnum:]]","_", names(data_list))
  date_variable <- make.names(iconv(date_variable, to = 'ASCII//TRANSLIT'), unique = TRUE)
  date_variable <- gsub("[^[:alnum:]]","_", date_variable)

  if(length(model_spec[["exclusions"]]) > 0) {
    tidy_exc_names <- function(x) {
      x <- make.names(iconv(x,  to = 'ASCII//TRANSLIT'), unique = TRUE)
      x <- gsub("[^[:alnum:]]","_", x)
      return(x)
      }
    model_spec[["exclusions"]] <- lapply(model_spec[["exclusions"]],
                                         function(x) {
                                           if(is.list(x)){
                                             out <- lapply(x,function(y) tidy_exc_names(y))
                                           }else{
                                             out <- tidy_exc_names(x)
                                           }
                                           out
                                         })
  }

  if(length(model_spec[["golden_variables"]]) > 0) {
    model_spec[["golden_variables"]] <- make.names(iconv(model_spec[["golden_variables"]], to = 'ASCII//TRANSLIT'), unique = TRUE)
    model_spec[["golden_variables"]] <- gsub("[^[:alnum:]]","_", model_spec[["golden_variables"]])
  }

  data_list <- lapply(data_list, function(x) {
    names(x) <- make.names(iconv(names(x), to = 'ASCII//TRANSLIT'), unique = TRUE)
    names(x) <- gsub("[^[:alnum:]]","_", names(x))
    return(x)})


  # # Force date variable to be called 'data_tidy'  =====================
  # # Select and format date variable
  # data_list = lapply(data_list, function(x) {
  #   names(x)[names(x) == date_variable] <- "data_tidy"
  #   x$data_tidy <- as.Date(x$data_tidy, format = date_format)
  #   x })
  #

  # Add prefix at Y variables =====================
  y_names <- sapply(seq_along(data_list), function(x) {
    a <- paste0("forecast_", x, "_", names(data_list)[x])
    a})
  names(data_list) <- y_names


  ## Creating a list with all info needed for the api request.
  body <- list(data_list = data_list,
               model_spec = model_spec,
               user_email = user_email,
               project_id = project_name,
               date_variable = date_variable,
               date_format = date_format,
               version = as.character(utils::packageVersion("faas4i"))
  )

  ## Compressing and enconding the body
  body <- caTools::base64encode(memCompress(jsonlite::toJSON(body), type = "gzip"))

  return(body)
}
