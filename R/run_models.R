#' @title FaaS - Sending scale modeling request
#'
#' @description Scale modeling performs an exhaustive search for best models in
#' time series data, providing information about the fit of the best models,
#' their cross-validation accuracy measures and many other outputs that are usually
#' of interest. Using the api to send the request allows for multiple requests at once.
#'
#' @param data_list list with datasets to be modeled, where the list elements must be named after the dependent variable.
#' @param ... advanced parameters.
#' @param date_variable name of variable with date information in all datasets in \code{data_list}.
#' @param date_format format of \code{date_variable} in all datasets in \code{data_list}.
#' @param model_spec list containing: \code{n_steps} (required), \code{n_windows} (required), \code{log}, \code{seas.d}, \code{n_best}, \code{accuracy_crit}, \code{info_crit}, \code{exclusion}, \code{golden_variables}, \code{fill_forecast}, \code{cv_summary} and \code{selection_methods}. See details for more information.
#' @param project_name project name. It accepts character and numeric inputs. Special characters will be removed.
#' @param save_local [DEV ONLY] directory to save base64 with body that would be sent to the API. With this parameter the function will not send your modeling request to the API. Default: NULL.
#' @return Message indicating that the request has been successfully sent, or an error message indicating what went wrong.
#' @details The \code{model_spec} is a list with all modeling and cross-validation setup. Regardless of whether you are modeling one or multiple dependent variables, you will only specify one \code{model_spec}. The arguments are:
#' \itemize{
#'   \item \code{n_steps}: forecast horizon that will be used in the cross-validation (if 3, 3 months ahead; if 12, 12 months ahead, etc.). \code{n_steps} should be an integer greater than or equal to 1. It is recommended that \code{n_steps}+\code{n_windows}-1 does not exceed 30\% of the length of your data.
#'   \item \code{n_windows}: how many windows the size of ‘Forecast Horizon’ will be evaluated during cross-validation (CV). \code{n_windows} should be an integer greater than or equal to 1. It is recommended that \code{n_steps}+\code{n_windows}-1 does not exceed 30\% of the length of your data.
#'   \item \code{log}: if TRUE apply log transformation to the data (only variables with all values greater than 0 will be log transformed). Can be set to TRUE or FALSE. Default: TRUE.
#'   \item \code{seas.d}: if TRUE, it includes seasonal dummies in every estimation. Can be set to TRUE or FALSE. Default: TRUE.
#'   \item \code{n_best}: number of best arima models to be chosen for each feature selection method. Default: 20.
#'   \item \code{accuracy_crit}: which criterion to measure the accuracy of the forecast during the CV. Can be set to 'MPE', 'MAPE', 'WMAPE' or 'RMSE'. Default: 'MAPE'.
#'   \item \code{info_crit}: which information criterion to use while modeling.Can be set to 'AIC' and 'BIC'. Default: 'AIC'.
#'   \item \code{exclusions}: restrictions on features in the same model (which variables should not be included in the same model). If none, \code{exclusions = list()}, otherwise it should receive a list containing vectors  or lists of variables (see examples 3 and 4). Default: \code{list()}.
#'   \item \code{golden_variables}: features that must be included in, at least, one model (separate or together). If none, \code{golden_variables = c()}, otherwise it should receive a vector with the golden variables (see advanced options below for examples). Default: \code{c()}.
#'   \item \code{fill_forecast}: if TRUE, it enables forecasting explanatory variables in order to avoid NAs in future values. Can be set to TRUE or FALSE. Default: FALSE.
#'   \item \code{cv_summary}: determines whether mean or median will be used to calculate the summary statistic of the accuracy measure over the CV windows. Can be set to 'mean' or 'median'. Default:'mean'.
#'   \item \code{selection_methods}: specifies which selection methods should be used for feature selection and whether explanatory variables should be chosen in order to avoid collinearity.
#'   \itemize{
#'   \item \code{lasso}: TRUE if our method of feature selection using Lasso should be applied. Default: TRUE.
#'   \item \code{rf}: TRUE if our method of feature selection using Random Forest should be applied. Default: TRUE.
#'   \item \code{corr}: TRUE if our method of feature selection using correlation should be applied. Default: TRUE.
#'   \item \code{apply.collinear}: TRUE if you wish that our feature selection avoids collinearity within the explanatory variables in the models - this is equivalent to setting \code{c("corr","rf","lasso","no_reduction")}. FALSE or \code{""} otherwise. Default: TRUE.
#'   }}
#'
#' @examples
#' \dontrun{
#'  ## EXAMPLE 1 - One dependent variable, basic model_spec
#'  # 1) Load the dataset
#'  # Since this dataset is stored in the package, we can load it directly
#'  dataset_1 <- dataset_1
#'
#'  # But you will need to do something similar to the line below when loading your own dataset
#'  # dataset_1 <- readxl::read_excel("./inputs/dataset_1.xlsx")
#'
#'  # Put it inside a list (therefore, a 'data list')
#'  data_list_ex1 <-list(dataset_1)
#'  names(data_list_ex1) <- c("fs_pim")
#'
#'  # Also, specify the date variable and its format
#'  date_variable <- "DATE_VARIABLE"
#'  date_format <- '%Y-%m-%d' # or'%m/%d/%Y'
#'
#'  # 2) Basic Modeling Setup
#'  model_spec_ex1 <- list(n_steps = 1,
#'                         n_windows = 12)
#'  # 3) Set Project Name
#'  project_name <- "example_project"
#'
#'
#'  # Send request
#'  faas4i::run_models(data_list = data_list_ex1, date_variable = date_variable,
#'                     date_format = date_format, model_spec = model_spec_ex1,
#'                     project_name = project_name)
#'
#'  ## EXAMPLE 2 - suppose you want to use same setup and model_spec as example 1,
#'  ## but sending the request for multiple dependent variables at once.
#'  ## Notice the way the data_list is defined.
#'
#'  # 1) Load the dataset
#'  # Since this dataset is stored in the package, we can load it directly,
#'  # when loading your own dataset you will need to read it into your R session
#'  dataset_1 <- dataset_1
#'  dataset_2 <- dataset_2
#'  dataset_3 <- dataset_3
#'
#'  # Put it inside a list and name the list element with the name of the dependent variable
#'  data_list_ex2 <-  list(dataset_1, dataset_2, dataset_3)
#'  names(data_list_ex2) <- c("fs_pim", "fs_pmc", "fs_pib")
#'
#'  # Also, specify the date variable and its format
#'  # (must have the same name in all datasets)
#'  date_variable <- "DATE_VARIABLE"
#'  date_format <- '%Y-%m-%d' # or'%m/%d/%Y'
#'
#'  # Send request
#'
#'  faas4i::run_models(data_list = data_list_ex2, date_variable = date_variable,
#'                     date_format = date_format, model_spec = model_spec_ex1,
#'                     project_name = project_name)
#'
#'  ## Advanced options
#'  ## EXAMPLE 3 - suppose you want to use same setup as example 1, but
#'  ## with a different model_spec.
#'  ## Notice the way the exclusions and golden_variables are defined.
#'  ## For this example we are changing the default for some parameters,
#'  ## some of them are advanced options, so make sure you understand their
#'  ## implications before using.
#'
#'  model_spec_ex3 <- list(n_steps = 1,
#'                        n_windows = 12,
#'                        log = FALSE,
#'                        seas.d = TRUE,
#'                        n_best = 20,
#'                        accuracy_crit = "RMSE",
#'                        info_crit = "AIC",
#'                        exclusions = list(c("fs_massa_real", "fs_rend_medio"),
#'                                          c("fs_pop_ea", "fs_pop_des", "fs_pop_ocu")),
#'                        golden_variables = c("fs_pmc", "fs_ici"),
#'                        fill_forecast = TRUE,
#'                        cv_summary = 'median',
#'                        selection_methods = list(
#'                          lasso = TRUE,
#'                          rf = TRUE,
#'                          corr = TRUE,
#'                          apply.collinear = TRUE))
#'
#'  # Send request
#'  faas4i::run_models(data_list = data_list_ex1, date_variable = date_variable,
#'                     date_format = date_format, model_spec = model_spec_ex3,
#'                     project_name = project_name)
#'
#'  ## EXAMPLE 4 - suppose you want to use same setup as example 1, but
#'  ## with a different setup for the exclusions in model_spec.
#'
#'  ## Suppose you would like to avoid “fs_massa_real” and “fs_rend_medio”
#'  ## to enter in the same model and also to run with any variable within
#'  ## “fs_pop_ea”, “fs_pop_des” and “fs_pop_ocu” group, but would not be a
#'  ## problem to see models with any of “fs_pop_ea”, “fs_pop_des” and
#'  ## “fs_pop_ocu” together. In other words, you would like to avoid a
#'  ## variable (or more than one variable) to run with a group of other
#'  ## variables, but there would be no restriction within this group.
#'
#'  model_spec_ex4 <- list(n_steps = 1,
#'                         n_windows = 12,
#'                         exclusions = list(list("fs_massa_real",
#'                                                "fs_rend_medio",
#'                                                c("fs_pop_ea", "fs_pop_des", "fs_pop_ocu")) )
#'
#'  # Send request
#'  faas4i::run_models(data_list = data_list_ex1, date_variable = date_variable,
#'                     date_format = date_format, model_spec = model_spec_ex4,
#'                     project_name = project_name)
#' }
#' @seealso
#'  \code{\link[httr]{POST}},\code{\link[httr]{add_headers}},\code{\link[httr]{timeout}}
#' @rdname run_models
#' @export
#' @importFrom httr insensitive POST add_headers timeout content status_code
run_models <- function(data_list, date_variable, date_format, model_spec, project_name,
                       save_local = NULL, ...) {

  extra_arguments <- list(...)

  # Setting dummy user_email
  user_email <- 'user@legitmail.com'
  ## If skip_validation was not defined, we set it to FALSE
  if (is.null(extra_arguments$skip_validation)) extra_arguments$skip_validation <- FALSE

  validate_user_input(data_list, date_variable, date_format, model_spec, project_name, user_email,
                      skip_validation = extra_arguments$skip_validation)
  ## Filling empty parameters in model_spec
  model_spec <- fill_model_spec(model_spec)
  ## Preparing body
  body <- prepare_body(data_list, date_variable, date_format, model_spec, project_name, user_email)

  ## If the argument 'save_local' was passed, we save 'body' in path provided
  if(!is.null(save_local)){
    if( dir.exists(save_local) == FALSE){
      stop(paste0(save_local, " does not exist."))
    } else{
      ## Getting time stamp to save file and creating name of file
      time_stamp <- gsub("\\.", "", as.numeric(Sys.time()))
      file_name <- paste0(save_local,"/b64rm_", project_name,"_", time_stamp)
      ## Saving body to file location defined
      write(body, file = file_name)
      warning("This project was only saved locally, it was not sent to the modeling API.")
      return(message(paste0("Body saved as ", file_name)))
    }
  }

  # Gera o token de autenticação no auth0 auth0
  access_token <- get_access_token()

  # Define a chave de acesso para poder fazer requisições via API ============
  headers <- c("authorization"= paste0("Bearer ", access_token))
  headers <- httr::insensitive(headers)
  ### Envia requisição POST ==================================================

  ## 1) Validation ===========================================================

  # Start the boolean indicating error on validation
  error_val <- FALSE

  # if skip_validation is set to TRUE, we should skip this block
  if(!extra_arguments$skip_validation){

    ## Envia requisição para validar
    validate_url <- get_url("validate")

    response_val <- httr::POST(validate_url,
                               body = list(body = body, check_model_spec = TRUE),
                               httr::add_headers(.headers = headers),
                               #                       encode = "json",
                               config = httr::timeout(1200))

    res_status_val <- NULL
    try(res_status_val <- httr::content(response_val)$status, silent = TRUE)

    if(! httr::status_code(response_val) %in% c(200,201,202)) {
      error_val <- TRUE
      if(httr::status_code(response_val) %in% c(408,504)){
        message("API Status code: ", httr::status_code(response_val),
                ".\nContent: Timeout",
                ".\nPlease try sending a smaller data_list.")

      } else if(httr::status_code(response_val) == 503){
        message("API Status code: ", httr::status_code(response_val),
                ".\nContent: Validation - Service Unavailable",
                ".\nPlease try again later.")

      } else if(httr::status_code(response_val) == 401){
        message("API Status code: ", httr::status_code(response_val),
                ".\nContent: Expired Authentication",
                ".\nPlease run 'login()' again.")

      } else{
        message("API Status code: ", httr::status_code(response_val),
                ".\nContent: ", httr::content(response_val, "text"),
                ".\nSomething went wrong in the api, please check if you have the latest version of this package and/or try again later.")
      }
    } else{
      if(any(c(200,201,202) %in% res_status_val) ){
        message("Status code: 200 - Request successfully validated!")
      } else if(! is.null(res_status_val)){
        message("Something went wrong!\nStatus code: ", res_status_val)
      } else {
        error_val <- TRUE
        message("API Status Code: ", httr::status_code(response_val), ".\nContent: ", httr::content(response_val),
                ".\nUnmapped internal error.")
      }
    }
    response_info <- NULL
    try(response_info <- httr::content(response_val)$info, silent = TRUE)
    # message(utils::str(response_info$info_list))
    if (length(response_info$error_list) > 0){
      message(paste0("\nError User Input: \n",pretty_R(response_info$error_list)))
      error_val <- TRUE
    }

    if (length(response_info$warning_list) > 0){
      message(paste0("Warning User Input: \n",pretty_R(response_info$warning_list)))
    }
  }

  ## 2) Modeling =============================================================

  if(!error_val){
    base_url <- get_url("models")

    response <- httr::POST(
      base_url,
      body = list("body" = body, "skip_validation" = TRUE),
      httr::add_headers(.headers = headers),
      encode = "json",
      config = httr::timeout(1200))

    # message(response)
    res_content <- NULL
    try(res_content <- httr::content(response), silent = TRUE)

    if(! httr::status_code(response) %in% c(200,201,202)) {

      if(httr::status_code(response) %in% c(408,504)){
        message("API Status code: ", httr::status_code(response),
                ".\nContent: Timeout.",
                ".\nPlease try sending a smaller data_list.")
      } else if(httr::status_code(response) == 503){
        message("API Status code: ", httr::status_code(response),
                ".\nContent: Modeling - Service Unavailable.",
                ".\nPlease try again later.")
      } else if(httr::status_code(response) == 401){
        message("API Status code: ", httr::status_code(response),
                ".\nContent: Expired Authentication.",
                ".\nPlease run 'login()' again.")
      } else{
        if (extra_arguments$skip_validation == TRUE){
          message("Status code: ", httr::status_code(response),
                  ".\nContent: ", httr::content(response, "text"),
                  ".\nSomething went wrong! Please try again setting 'skip_validation = FALSE'.")
        } else {
          message("API Status code: ", httr::status_code(response),
                  ".\nContent: ", httr::content(response, "text"),
                  ".\nSomething went wrong in the api, please check if you have the latest version of this package and/or try again later.")
        }
      }


    } else{
      if( "created" %in% res_content[["status"]] ) {
        message("\nStatus code: 200 - Request successfully received for modeling!\n",
                "Results will soon be available in your Projects module.\n")
      } else {
        message("Something went wrong when sending to modeling!\nContent: ", res_content)
      }
    }
  }

}