#' @title FaaS - Validating information for request of scale modeling
#'
#' @description This is an optional function that allows users to check if the specifications defined
#' for the scale modeling request are according to the requirements.
#'
#' @param data_list list with datasets to be modeled, where the list elements must be named after the dependent variable. You cannot have more than one dependent variable with same name in a \code{data_list}.
#' @param ... advanced parameters.
#' @param date_variable name of variable with date information in all datasets in \code{data_list}.
#' @param date_format format of \code{date_variable} in all datasets in \code{data_list}.
#' @param model_spec list containing: \code{n_steps} (required), \code{n_windows} (required), \code{log}, \code{seas.d}, \code{n_best}, \code{accuracy_crit}, \code{exclusion}, \code{golden_variables}, \code{fill_forecast}, \code{cv_summary}, \code{selection_methods} and \code{lags}. See details for more information.
#' @param project_name project name. A string with character and/or numeric inputs that should be at most 50 characters long. Special characters will be removed.
#' @return Message indicating that everything looks good with the request, or an error message indicating what went wrong.
#' @details The \code{model_spec} is a list with all modeling and cross-validation setup. Regardless of whether you are modeling one or multiple dependent variables, you will only specify one \code{model_spec}. The arguments are:
#' \itemize{
#'   \item \code{n_steps}: forecast horizon that will be used in the cross-validation (if 3, 3 months ahead; if 12, 12 months ahead, etc.). \code{n_steps} should be an integer greater than or equal to 1. It is recommended that \code{n_steps}+\code{n_windows}-1 does not exceed 30\% of the length of your data.
#'   \item \code{n_windows}: how many windows the size of ‘Forecast Horizon’ will be evaluated during cross-validation (CV). \code{n_windows} should be an integer greater than or equal to 1. It is recommended that \code{n_steps}+\code{n_windows}-1 does not exceed 30\% of the length of your data.
#'   \item \code{log}: if TRUE apply log transformation to the data (only variables with all values greater than 0 will be log transformed). Can be set to TRUE or FALSE. Default: TRUE.
#'   \item \code{seas.d}: if TRUE, it includes seasonal dummies in every estimation. Can be set to TRUE or FALSE. Default: TRUE.
#'   \item \code{n_best}: number of best arima models to be chosen for each feature selection method. Default: 20.
#'   \item \code{accuracy_crit}: which criterion to measure the accuracy of the forecast during the CV. Can be set to 'MPE', 'MAPE', 'WMAPE' or 'RMSE'. Default: 'MAPE'.
#'   \item \code{exclusions}: restrictions on features in the same model (which variables should not be included in the same model). If none, \code{exclusions = list()}, otherwise it should receive a list containing vectors  or lists of variables (see examples 3, 4 and 5). Default: \code{list()}.
#'   \item \code{golden_variables}: features that must be included in, at least, one model (separate or together). If none, \code{golden_variables = c()}, otherwise it should receive a vector with the golden variables (see advanced options below for examples). Default: \code{c()}.
#'   \item \code{fill_forecast}: if TRUE, it enables forecasting explanatory variables in order to avoid NAs in future values. Can be set to TRUE or FALSE. Default: FALSE.
#'   \item \code{cv_summary}: determines whether mean or median will be used to calculate the summary statistic of the accuracy measure over the CV windows. Can be set to 'mean' or 'median'. Default:'mean'.
#'   \item \code{selection_methods}: specifies which selection methods should be used for feature selection and whether explanatory variables should be chosen in order to avoid collinearity.
#'   \itemize{
#'   \item \code{lasso}: TRUE if our method of feature selection using Lasso should be applied. Default: TRUE.
#'   \item \code{rf}: TRUE if our method of feature selection using Random Forest should be applied. Default: TRUE.
#'   \item \code{corr}: TRUE if our method of feature selection using correlation should be applied. Default: TRUE.
#'   \item \code{apply.collinear}: TRUE if you wish that our feature selection avoids collinearity within the explanatory variables in the models - this is equivalent to setting \code{c("corr","rf","lasso","no_reduction")}. FALSE or \code{""} otherwise. Default: TRUE.
#'   }
#'   \item \code{lags}: list of lags of explanatory variables to be tested in dataset (see example 5 below). Additionally, if you wish to include lags 1 and 2 for all explanatory variables, 'lags' can be set to \code{list(all=c(1,2))}, for example.
#'   \item \code{allowdrift}: if TRUE, drift terms are considered in arima models.
#'   \item \code{user_model}: definition of a model (or more than one) that user wants to see among the ones available in output. If none, \code{exclusions = list()}, otherwise it should receive a list containing vectors of variables to be included in user models (see examples 3 and 5). Default: \code{list()}.
#'   }
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
#'  faas4i::validate_models(data_list = data_list_ex1, date_variable = date_variable,
#'                          date_format = date_format, model_spec = model_spec_ex1,
#'                          project_name = project_name)
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
#'  faas4i::validate_models(data_list = data_list_ex2, date_variable = date_variable,
#'                          date_format = date_format, model_spec = model_spec_ex1,
#'                          project_name = project_name)
#'
#'  ## Advanced options
#'  ## EXAMPLE 3 - suppose you want to use same setup as example 1, but
#'  ## with a different model_spec.
#'  ## Notice the way the exclusions and golden_variables are defined.
#'  ## For this example we are changing the default for some parameters,
#'  ## some of them are advanced options, so make sure you understand their
#'  ## implications before using.
#'  ## Additionally, two models will be included, one with 'fs_rend_medio' and
#'  ## another with 'fs_massa_real' and 'fs_pmc'.
#'
#'  model_spec_ex3 <- list(n_steps = 1,
#'                        n_windows = 12,
#'                        log = FALSE,
#'                        seas.d = TRUE,
#'                        n_best = 20,
#'                        accuracy_crit = "RMSE",
#'                        exclusions = list(c("fs_massa_real", "fs_rend_medio"),
#'                                          c("fs_pop_ea", "fs_pop_des", "fs_pop_ocu")),
#'                        golden_variables = c("fs_pmc", "fs_ici"),
#'                        fill_forecast = TRUE,
#'                        cv_summary = 'median',
#'                        selection_methods = list(
#'                          lasso = TRUE,
#'                          rf = TRUE,
#'                          corr = TRUE,
#'                          apply.collinear = TRUE),
#'                        user_model = list(c("fs_massa_real", "fs_pmc"),
#'                                          c("fs_rend_medio")))
#'
#'  # Send request
#'  faas4i::validate_models(data_list = data_list_ex1, date_variable = date_variable,
#'                          date_format = date_format, model_spec = model_spec_ex3,
#'                          project_name = project_name)
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
#'  faas4i::validate_models(data_list = data_list_ex1, date_variable = date_variable,
#'                          date_format = date_format, model_spec = model_spec_ex4,
#'                          project_name = project_name)
#'
#'  ## EXAMPLE 5 - suppose you want to use same setup as example 1, but
#'  ## testing some lags of explanatory variables, while including them as
#'  ## exclusions, golden_variables and also using in user_model.
#'
#'  ## In the example below lags 1, 2 and 3 will be tested for the variable 'fs_rend_medio',
#'  ## and 1 and 3 for the variable 'fs_pop_ocu'. If you wish to include a lag as a golden_variable
#'  ## or in a list of exclusions, you can do so by using the following structure:
#'  ## 'l<lag number>_<variable name>', as shown in the model_spec below.
#'
#'  model_spec_ex5 <- list(n_steps = 1,
#'                         n_windows = 12,
#'                         exclusions = list(c("fs_pop_ea", "fs_pop_des", "fs_pop_ocu",
#'                                             "l1_fs_rend_medio","l2_fs_rend_medio",
#'                                             "l3_fs_rend_medio")),
#'                         golden_variables = c("l1_fs_pop_ocu"),
#'                         lags = list("fs_rend_medio" = c(1,2,3),
#'                                     "fs_pop_ocu" = c(1,3)),
#'                         user_model = list(c("fs_massa_real","l1_fs_pop_ocu"),
#'                                           c("fs_rend_medio")))
#'
#'  # Send request
#'  faas4i::validate_models(data_list = data_list_ex1, date_variable = date_variable,
#'                          date_format = date_format, model_spec = model_spec_ex5,
#'                          project_name = project_name)
#' }
#' @rdname validate_models
#' @export
#' @seealso
#'  \code{\link[httr2]{request}}, \code{\link[httr2]{req_proxy}}, \code{\link[httr2]{req_headers}}, \code{\link[httr2]{req_timeout}}, \code{\link[httr2]{req_body}}, \code{\link[httr2]{req_perform}}, \code{\link[httr2]{resp_status}}, \code{\link[httr2]{resp_body_raw}}
#' @importFrom httr2 request req_proxy req_headers req_timeout req_body_multipart req_error req_perform resp_status resp_body_json resp_body_string
validate_models <- function(data_list, date_variable, date_format, model_spec,
                            project_name,...) {

  extra_arguments <- list(...)

  if (any(!names(extra_arguments) %in% c("version_check", "proxy_url", "proxy_port"))) {
    invalid_args <- names(extra_arguments)[!names(extra_arguments) %in% c("version_check", "proxy_url", "proxy_port")]
    stop(paste0("Unexpected extra argument(s): ", paste0(invalid_args, collapse = ", "),"."))
  }

  if (is.null(extra_arguments$version_check)) extra_arguments$version_check <- TRUE

  if (extra_arguments$version_check) {
    update_package <- package_version_check(proxy_url = extra_arguments$proxy_url,
                                            proxy_port = extra_arguments$proxy_port)
    if (update_package) return(invisible())
  }

  # Gera o token de autenticação no auth0 auth0
  access_token <- get_access_token()

  # Setting dummy user_email
  user_email <- 'user@legitmail.com'
  validate_user_input(data_list, date_variable, date_format, model_spec, project_name, user_email,
                      skip_validation = FALSE)
  ## Filling empty parameters in model_spec
  model_spec <- fill_model_spec(model_spec, data_list, date_variable)
  ## Preparing body
  body <- prepare_body(data_list, date_variable, date_format, model_spec, project_name, user_email)

  ### Envia requisição POST ==================================================
  url <- get_url("validate")

  req <- httr2::request(url)
  req <- httr2::req_proxy(req = req,
                          url = extra_arguments$proxy_url,
                          port = extra_arguments$proxy_port)
  req <- httr2::req_headers(.req = req,
                            "authorization" = paste0("Bearer ", access_token))
  req <- httr2::req_timeout(req = req, seconds = 1200)
  req <- httr2::req_body_multipart(.req = req, body = body, check_model_spec = "TRUE")

  ## Adding req_error so any error in req_perform is not converted into http
  ## error and it is saved in the appropriate object
  req <- httr2::req_error(req = req, is_error = \(req) FALSE)

  response <- httr2::req_perform(req)

  response_status_model <- httr2::resp_status(response)
  try({response_content_model <- httr2::resp_body_json(response)}, silent = TRUE)
  try({response_content_model_text <- httr2::resp_body_string(response)},
      silent = TRUE)

  res_status <- NULL
  try({res_status <- response_content_model$status}, silent = TRUE)

  if (!response_status_model %in% c(200,201,202)) {
    if (response_status_model %in% c(408,504)) {
      message("API Status code: ", response_status_model,
              ".\nContent: Timeout",
              ".\nPlease try sending a smaller data_list.")

    } else if (response_status_model == 503) {
      message("API Status code: ", response_status_model,
              ".\nContent: Validation - Service Unavailable.",
              ".\nPlease try again later.")
    } else if (response_status_model == 401) {
      message("API Status code: ", response_status_model,
              ".\nContent: Expired Authentication.",
              ".\nPlease run 'login()' again.")
    } else{
      message("API Status code: ", response_status_model,
              ".\nContent: ", response_content_model_text,
              ".\nSomething went wrong in the api, please check if you have the latest version of this package and/or try again later.")
    }
  } else{
    if (any(c(200,201,202) %in% res_status)) {
      message("Status code: 200 - Request successfully validated!\n",
              "Now you can call the run_models function to run your model.")
    } else if (!is.null(res_status)) {
      message("Something went wrong!\nStatus code: ", res_status)
    } else {
      message("API Status Code: ", response_status_model, ".\nContent: ", response_content_model,
              ".\nUnmapped internal error.")
    }
  }

  response_info <- NULL
  try({response_info <- response_content_model$info}, silent = TRUE)
  if (length(response_info$error_list) > 0) {
    message(paste0("\nError User Input: \n",pretty_R(response_info$error_list, extra_list = TRUE)))
  }

  if (length(response_info$warning_list) > 0) {
    message(paste0("\nWarning User Input: \n",pretty_R(response_info$warning_list, extra_list = FALSE)))
  }


}


