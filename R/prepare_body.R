#' @title Prepare body for FaaS modeling
#'
#' @description This function will prepare the encoded body to be sent via api.
#' @param data_list list with named datasets to be modeled.
#' @param date_variable name of variable with date information in all datasets in \code{data_list}.
#' @param date_format format of \code{date_variable} in all datasets in \code{data_list}.
#' @param model_spec list with modeling and cross validation setup.
#' @param project_name project name. A string with character and/or numeric inputs that should be at most 50 characters long. Special characters will be removed.
#' @param user_email email to receive the outputs.
#' @param user_model list containing the models constraints to create a model customized by the user.
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
prepare_body <- function(data_list, date_variable, date_format, model_spec, project_name, user_email, user_model) {

  ## Treating special characters
  names(data_list) <- tolower(make.names(iconv(names(data_list), to = 'ASCII//TRANSLIT')))
  names(data_list) <- gsub("[^[:alnum:]]","_", names(data_list))
  names(user_model) <- tolower(make.names(iconv(names(user_model), to = 'ASCII//TRANSLIT')))
  names(user_model) <- gsub("[^[:alnum:]]","_", names(user_model))
  date_variable <- tolower(make.names(iconv(date_variable, to = 'ASCII//TRANSLIT'), unique = TRUE))
  date_variable <- gsub("[^[:alnum:]]","_", date_variable)

  tidy_exc_names <- function(x) {
    x <- tolower(make.names(iconv(x,  to = 'ASCII//TRANSLIT'), unique = TRUE))
    x <- gsub("[^[:alnum:]]","_", x)
    return(x)
  }

  if(length(model_spec[["exclusions"]]) > 0) {
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

  if(length(model_spec[["user_model"]]) > 0) {
    model_spec[["user_model"]] <- lapply(model_spec[["user_model"]], tidy_exc_names)
  }

  if(length(model_spec[["lags"]]) > 0){
    names(model_spec[["lags"]]) <- tidy_exc_names(names(model_spec[["lags"]]))
  }

  if(length(model_spec[["golden_variables"]]) > 0) {
    model_spec[["golden_variables"]] <- tolower(make.names(iconv(model_spec[["golden_variables"]], to = 'ASCII//TRANSLIT'), unique = TRUE))
    model_spec[["golden_variables"]] <- gsub("[^[:alnum:]]","_", model_spec[["golden_variables"]])
  }

  data_list <- lapply(data_list, function(x) {
    names(x) <- tolower(make.names(iconv(names(x), to = 'ASCII//TRANSLIT'), unique = TRUE))
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
  data_list_names_map <- names(data_list)
  y_names <- sapply(seq_along(data_list), function(x) {
    a <- paste0("forecast_", x, "_", names(data_list)[x])
    a})
  names(data_list) <- y_names

  ## Applying prefix on user model
  names(data_list_names_map) <- y_names
  names(user_model) <- names(data_list_names_map)[match(names(user_model),
                                                        data_list_names_map)]

  ## Treating special characters on user_model
  for(name in names(user_model)){

    user_model[[name]] <- lapply(user_model[[name]], function(x){
      ## Treating special characters on "vars"
      
      x[["vars"]] <- tolower(make.names(iconv(x[["vars"]], to = 'ASCII//TRANSLIT')))
      x[["vars"]] <- gsub("[^[:alnum:]]","_", x[["vars"]])

      ## If "constraints" exists, treat special character on "constraints" names
      ## and convert "constraints" values into character -> make compatible with pyfaas
      if("constraints" %in% names(x)){
        
        names(x[["constraints"]]) <- tolower(make.names(iconv(names(x[["constraints"]]), to = 'ASCII//TRANSLIT')))
        names(x[["constraints"]]) <- gsub("[^[:alnum:]]","_", names(x[["constraints"]]))

        x[["constraints"]] <- lapply(x[["constraints"]], as.character)
      }
      
      return(x)
    })
  }

  ## Creating a list with all info needed for the api request.
  body <- list(data_list = data_list,
               model_spec = model_spec,
               user_email = user_email,
               project_id = project_name,
               date_variable = date_variable,
               date_format = date_format,
               user_model = user_model,
               version = as.character(utils::packageVersion("faas4i"))
  )

  ## Compressing and enconding the body
  body <- caTools::base64encode(memCompress(jsonlite::toJSON(body, digits = 6),
                                            type = "gzip"))

  return(body)
}
