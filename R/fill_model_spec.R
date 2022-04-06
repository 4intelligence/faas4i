#' @title Fill list with modeling specifications
#'
#' @description This function fill empty parameters for \code{model_spec}.
#' @param model_spec list with modeling and cross validation setup.
#' @return \code{model_spec} complete with default settings for parameters not defined
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname fill_model_spec
fill_model_spec <- function(model_spec){

  ## If model_spec_default$selection_methods$apply.collinear was set to TRUE or FALSE, we need to
  ## change these values to the format accepted by modeling
  collinear <- model_spec$selection_methods$apply.collinear
  if(is.logical(collinear) & length(collinear) == 1){
    if (collinear == TRUE) {
      model_spec$selection_methods$apply.collinear <- c("corr","rf","lasso","no_reduction")
    } else if (collinear == FALSE){
      model_spec$selection_methods$apply.collinear <- ""
    }
  }

  ## We will fill the model_spec starting from the default model_spec
  model_spec_default <- list(log = TRUE,
                             seas.d = TRUE,
                             n_steps = NA,
                             n_windows = NA,
                             n_best = as.integer(20),
                             accuracy_crit = "MAPE",
                             info_crit = "AIC",
                             cv_summary = "mean",
                             fill_forecast = FALSE,
                             exclusions = list(),
                             golden_variables = c(),
                             selection_methods = list(
                               lasso = TRUE,
                               rf = TRUE,
                               corr = TRUE,
                               apply.collinear = c("corr","rf","lasso","no_reduction")
                             )
  )

  ## If selection_methods, was defined by user, we update the values defined in model_spec_default
  if("selection_methods" %in% names(model_spec)) {
    for(arg in names(model_spec_default$selection_methods)) {
      ## For each argument in selection_methods we check if the user defined it, and update if necessary
      if(arg %in% names(model_spec$selection_methods)) {
        model_spec_default$selection_methods[[arg]] = model_spec$selection_methods[[arg]]
      }
    }
  }

  ## For all other parameters (but selection_methods) we check if it was defined, and if so we update its value
  for(arg in names(model_spec_default)) {
    if(arg %in% names(model_spec) & arg != "selection_methods") {
      model_spec_default[[arg]] = model_spec[[arg]]
    }
  }

  return(model_spec_default)
}
