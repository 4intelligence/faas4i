#' @title Validate User Input for FaaS modeling
#'
#' @description This function will perform an initial check if all necessary
#' arguments to run the \code{run_models} were provided.
#'
#' @param data_list list with datasets to be modeled, where the list elements must be named after the dependent variable. You cannot have more than one dependent variable with same name in a \code{data_list}.
#' @param date_variable name of variable with date information in all datasets in \code{data_list}.
#' @param date_format format of \code{date_variable} in all datasets in \code{data_list}.
#' @param model_spec list with modeling and cross validation setup.
#' @param project_name project name. A string with character and/or numeric inputs that should be at most 50 characters long. Special characters will be removed.
#' @param user_email email to receive the outputs.
#' @param user_model list containing the models constraints to create a model customized by the user.
#' @param skip_validation TRUE or FALSE, indicating if validation should be skipped.
#' @return None. Will break if any argument is not properly defined.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname validate_user_input
validate_user_input <- function(data_list, date_variable, date_format, model_spec, project_name, user_email, user_model, skip_validation) {

  # Checking if user defined minimum necessary arguments ===============================
  if(any(missing(data_list), missing(model_spec), missing(date_variable), missing(date_format),
         missing(project_name), missing(user_email))) {

    stop("You must declare every argument: 'data_list', 'model_spec', 'project_name', 'user_email', 'date_format', 'date_variable'")

  }

  message_list <- "\n"

  if (project_name == "") {
    message_list <- paste0(message_list,"Your request did not include a required/valid parameter: project_name.","\n")
  } else if (nchar(project_name) > 50) {
    message_list <- paste0(message_list,"The project_name should be at most 50 characters long.","\n")
  }

  if (user_email == "") {
    message_list <- paste0(message_list,"Your request did not include a required parameter: user_email.","\n")
  }else if(!grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
                  as.character(user_email),
                  ignore.case=TRUE)){ # Check for valid email (return TRUE if valid)
    message_list <- paste0(message_list,"Your request did not include a valid user_email.","\n")
  }

  if (!"n_steps" %in% names(model_spec)){
    message_list <- paste0(message_list,"Your request did not include a required parameter in model_spec: n_steps","\n")
  } else if(model_spec[["n_steps"]] <= 0 | !is.numeric(model_spec[["n_steps"]])){
    message_list <- paste0(message_list,"Your request did not include a valid parameter in model_spec: n_steps","\n")
  }

  if (!"n_windows" %in% names(model_spec)){
    message_list <- paste0(message_list,"Your request did not include a required parameter in model_spec: n_windows","\n")
  } else if(model_spec[["n_windows"]] <= 0 | !is.numeric(model_spec[["n_windows"]])){
    message_list <- paste0(message_list,"Your request did not include a valid parameter in model_spec: n_windows","\n")
  }

  if (length(data_list) == 0) {
    message_list <- paste0(message_list,"Your request did not include a required parameter: data_list.","\n")
  } else{
    for (i in 1:length(data_list)){
      if(!is.data.frame(data_list[[i]])){
        message_list <- paste0(message_list,"Your request did not include a valid data.frame for dataset ",i,".","\n")
      }
    }
  }

  if (is.null(names(data_list)) ) {
    message_list <- paste0(message_list,"Your request did not include a required parameter: a named data_list.","\n")
  } else {
    ## Checking if there is at least one duplicated y name in data_list
    duplicated_names <- unique(names(data_list)[duplicated(names(data_list))])
    if (length(duplicated_names) > 0) {
      message_list <- paste0(message_list,"Your request included more than one dependent variable with the following name(s): ",
                             paste0(duplicated_names, collapse = ", "), ".","\n")
    }
  }

  for (i in 1:length(data_list)){
    if(!date_variable %in% names(data_list[[i]])){
      message_list <- paste0(message_list,"Dataset ",i," does not include 'date_variable'.","\n")
    }

    if (any(nchar(names(data_list[[i]])) > 50)) {
      message_list <- paste0(message_list,"Dataset ",i," includes at least one variable with name longer than 50 characters.","\n")
    }

  }

  listFormat = c(
    # ano / mês / dia
    "%Y/%m/%d", "%y/%m/%d",
    # ano / dia / mês
    "%Y/%d/%m", "%y/%d/%m",
    # dia / mês / ano
    "%d/%m/%Y", "%d/%m/%y",
    # mês / dia / ano
    "%m/%d/%Y", "%m/%d/%y",
    # ano - mês - dia
    "%Y-%m-%d", "%y-%m-%d",
    # ano - dia - mês
    "%Y-%d-%m", "%y-%d-%m",
    # dia - mês - ano
    "%d-%m-%Y", "%d-%m-%y",
    # mês - dia - ano
    "%m-%d-%Y", "%m-%d-%y"
  )

  if(!date_format %in% listFormat){
    message_list <- paste0(message_list,"Your request did not include a valid parameter: date_format.","\n")
  }

  lags <- model_spec[["lags"]]
  if(length(lags) > 0){
    if (! is.list(lags) | is.null(names(lags))) {
      message_list <- paste0(message_list,"Parameter 'lags' in 'model_spec' must be a named list.","\n")
    }
  }
  
  ## Checking user model param
  if (length(user_model) > 0){
    data_list_names <- names(data_list)
    user_model_names <- names(user_model)
    ys_not_in_data_list <- setdiff(user_model_names, data_list_names)
    
    ## If a variable is set in user model but it is not in the data list
    if (length(ys_not_in_data_list) != 0){
      message_list <- paste0(message_list, "Some variables in user model are not in the original data: ",
                             paste0(ys_not_in_data_list, collapse = ", "), ".","\n")
    }
  }

  ## If message list has more than 1 character, it means that at least one error was found
  ## so we need to stop.
  if(nchar(message_list) > 1){
    if (skip_validation == TRUE) message_list <- paste0("Setting 'skip_validation' to TRUE is not recommended.","\n",message_list)
    stop(message_list)
  }else if (skip_validation == TRUE){
    warning("Setting 'skip_validation' to TRUE is not recommended.")
  }

}
