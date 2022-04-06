#' @title Validate User Input for Model Update
#'
#' @description This function will perform an initial check if all necessary
#' arguments to run the \code{run_update} were provided.
#'
#' @param pack_list list with information about all packs to be updated. For each pack a list with a \code{forecast_pack} and a \code{new_data} should be provided.
#' @param date_variable name of variable with date information in all \code{new_data} in \code{pack_list}.
#' @param date_format format of \code{date_variable} in all \code{new_data} in \code{pack_list}.
#' @param project_name project name. It accepts character and numeric inputs. Special characters will be removed.
#' @param user_email email to receive the outputs.
#' @param cv_update TRUE or FALSE, indicating whether cross validation should be ran again.
#' @param model_spec list containing: \code{fill_forecast}, \code{n_steps}, \code{n_windows} and \code{cv_summary}, see details for its description. All arguments are optional, however \code{n_steps}, \code{n_windows} and \code{cv_summary} should only be used when \code{cv_update = TRUE}.
#' @param base_dates TRUE or FALSE, indicating whether initial date in modeling should be kept for update, if possible.
#' @param force_request TRUE or FALSE, indicating whether estimation should continue when not all packs can be updated.
#' @param outlier_update TRUE or FALSE, indicating whether a new search for outliers should be conducted. Search for outliers will be done only when \code{cv_update} = TRUE.
#' @param breakdown TRUE, FALSE or vector with 'row_id' of model to run breakdown (max of 3 models will be updated). If \code{breakdown} is TRUE, will run breakdown for the first 3 arima models, if a vector with 'row_id' is provided, the first 3 arima models in this list will be updated.
#' @return None. Will break if any argument is not properly defined.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname validate_user_input_update
validate_user_input_update <- function(pack_list, date_variable, date_format, project_name, user_email,
                                       cv_update, model_spec, base_dates, force_request,
                                       outlier_update, breakdown) {

  # Checking if user defined minimum necessary arguments ===============================
  if(any(missing(pack_list), missing(date_variable), missing(date_format),
         missing(project_name), missing(user_email))) {

    stop("You must declare every argument: 'pack_list', 'date_variable', 'date_format', 'project_name', 'user_email'.")

  }

  for (i in 1:length(pack_list)){
    ## For each set in the pack list we check if it has the necessary arguments
    if (all(c("forecast_pack", "new_data") %in% names(pack_list[[i]])) == FALSE){
      stop(paste0("Pack list ", i, " is either missing an argument or it was not named correctly. \nEach pack should have a 'forecast_pack' and a 'new_data'."))
    }
  }

  ### We start by making a few general checks
  if ("fill_forecast" %in% names(model_spec)){
    if( ! model_spec[["fill_forecast"]] %in% c(TRUE, FALSE))
      stop("Set 'fill_forecast = TRUE' or 'fill_forecast = FALSE'.")
  }

  if( ! base_dates %in% c(TRUE, FALSE)) stop("Set 'base_dates = TRUE' or 'base_dates = FALSE'.")
  if( ! force_request %in% c(TRUE, FALSE)) stop("Set 'force_request = TRUE' or 'force_request = FALSE'.")
  if( ! outlier_update %in% c(TRUE, FALSE)) stop("Set 'outlier_update = TRUE' or 'outlier_update = FALSE'.")
  if( ! cv_update %in% c(TRUE, FALSE)) stop("Set 'cv_update = TRUE' or 'cv_update = FALSE'.")

  if ( cv_update == FALSE & any(c("n_steps", "n_windows", "cv_summary") %in% names(model_spec))){
    warning("The parameters defined in model spec ('n_steps', 'n_windows' and/or 'cv_summary') will be ignored since 'cv_update = FALSE'.")
  }

  if ( cv_update == FALSE & outlier_update == TRUE){
    warning("Parameter 'outlier_update' will be set to FALSE since search for new outliers is only done when 'cv_update = TRUE'.")
  }

  if ("n_steps" %in% names(model_spec)){
    if (! is.numeric(model_spec[["n_steps"]]) || model_spec[["n_steps"]] <= 0 ){
      stop("n_steps should be an integer at least 1 if you want to reestimate the cross validation.")
    }
  }

  if ("n_windows" %in% names(model_spec)){
    if (! is.numeric(model_spec[["n_windows"]]) ||model_spec[["n_windows"]] <= 0 ){
      stop("n_windows should be an integer at least 1 if you want to reestimate the cross validation")
    }
  }

  if ("cv_summary" %in% names(model_spec)){
    if (! model_spec[["cv_summary"]] %in% c("mean", "median")) {
      stop("cv_summary should be set to 'mean' or 'median' in the model_spec.")
    }
  }

  if ("cv_summary" %in% names(model_spec)){
    if (! model_spec[["cv_summary"]] %in% c("mean", "median")) {
      stop("cv_summary should be set to 'mean' or 'median' in the model_spec.")
    }
  }

  if (any(!breakdown %in% c(TRUE, FALSE))){
    is_not_arima <- !grepl("-ARIMA", breakdown)

    if( any(is_not_arima)){
      warning(paste0("Model breakdown is done only for ARIMA models, ", paste0(breakdown[is_not_arima], collapse = ", "),
                     " will be removed from breakdown list."))
      # Updating breakdown
      breakdown <- breakdown[!is_not_arima]
    }

    if (length(breakdown) > 3){
      warning("There are more than 3 valid row_id's in 'breakdown'. Only first 3 will be updated in each 'forecast_pack'.")
    }
  }

  if( breakdown == FALSE && cv_update == TRUE){
    warning("Model breakdown will not be done since 'breakdown' is set to FALSE.")
  }
}
