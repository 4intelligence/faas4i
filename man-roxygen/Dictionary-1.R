#' @param ... advanced parameters.
#' @param base_dates TRUE or FALSE, indicating whether initial date in modeling should be kept for update, if possible.
#' @param base_dates TRUE or FALSE, indicating whether initial date in modeling should be kept for update, if possible. Default: TRUE.
#' @param breakdown TRUE, FALSE or vector with 'row_id' of model to run breakdown (max of 3 models will be updated). If \code{breakdown} is TRUE, will run breakdown for the first 3 arima models, if a vector with 'row_id' is provided, the first 3 arima models in this list will be updated.
#' @param breakdown TRUE, FALSE or vector with 'row_id' of models to run breakdown (max of 3 models). If \code{breakdown} is TRUE, model breakdown will be calculated for the first 3 arima models. If \code{breakdown} is FALSE, model breakdown from previous modeling will be removed from \code{forecast_pack}.
#' @param breakdown TRUE, FALSE or vector with 'row_id' of models to run breakdown (max of 3 models). If \code{breakdown} is TRUE, model breakdown will be calculated for the first 3 arima models. If \code{breakdown} is FALSE, model breakdown from previous modeling will be removed from \code{forecast_pack}. Default is TRUE when \code{cv_update} is TRUE, and FALSE otherwise.
#' @param config_file set the user email.
#' @param cv_update TRUE or FALSE, indicating whether cross validation should be ran again.
#' @param cv_update TRUE or FALSE, indicating whether cross validation should be ran again. Default: FALSE.
#' @param data_list list with datasets to be modeled, where the list elements must be named after the dependent variable.
#' @param data_list list with named datasets to be modeled.
#' @param dataset dataset with names to be matched with \code{names_fp}.
#' @param date_format format of \code{date_variable} in all \code{new_data} in \code{pack_list}.
#' @param date_format format of \code{date_variable} in all datasets in \code{data_list}.
#' @param date_variable name of variable with date information in all \code{new_data} in \code{pack_list}.
#' @param date_variable name of variable with date information in all datasets in \code{data_list}.
#' @param filename name of the zipped file.
#' @param force_request TRUE or FALSE, indicating whether estimation should continue when not all packs can be updated.
#' @param forecast_pack pack outputted by FaaS with modeling information to be updated.
#' @param index index of \code{forecast_pack} and \code{new_data} in \code{pack_list}.
#' @param message_list The list with messages, either error list or warning list
#' @param model_spec list containing: \code{fill_forecast}, \code{n_steps}, \code{n_windows} and \code{cv_summary}, see details for its description. All arguments are optional, however \code{n_steps}, \code{n_windows} and \code{cv_summary} should only be used when \code{cv_update = TRUE}.
#' @param model_spec list containing: \code{fill_forecast}, \code{n_steps}, \code{n_windows} and \code{cv_summary}, see details for its description. All arguments are optional, however \code{n_steps}, \code{n_windows} and \code{cv_summary} should only be used when \code{cv_update = TRUE}. Default: list().
#' @param model_spec list containing: \code{n_steps} (required), \code{n_windows} (required), \code{log}, \code{seas.d}, \code{n_best}, \code{accuracy_crit}, \code{exclusion}, \code{golden_variables}, \code{fill_forecast}, \code{cv_summary} and \code{selection_methods}. See details for more information.
#' @param model_spec list with modeling and cross validation setup.
#' @param names_fp names of variables to match in the \code{dataset}.
#' @param new_data dataset to be used to update models in \code{forecast_pack}. Variable names should be the same as inital modeling.
#' @param outlier_update TRUE or FALSE, indicating whether a new search for outliers should be conducted. Search for outliers will be done only when \code{cv_update} = TRUE.
#' @param outlier_update TRUE or FALSE, indicating whether a new search for outliers should be conducted. Search for outliers will be done only when \code{cv_update} = TRUE. Default is TRUE when \code{cv_update} is TRUE, and FALSE otherwise.
#' @param pack_list list with information about all packs to be updated. For each pack a list with a \code{forecast_pack} and a \code{new_data} should be provided.
#' @param pack_list list with information about all packs to be updated. For each pack a list with a \code{forecast_pack} and a \code{new_data} should be provided. See examples below.
#' @param path folder in which the downloaded files should be saved.
#' @param project_id id of the project to be downloaded which can be founded by using the function \code{list_files}..
#' @param project_name project name. It accepts character and numeric inputs. Special characters will be removed.
#' @param repo repository url to obtain latest version
#' @param run_local if TRUE, it will be sent to Docker to run locally. Default: FALSE.
#' @param save_local [DEV ONLY] directory to save base64 with body that would be sent to the API. With this parameter the function will not send your modeling request to the API. Default: NULL.
#' @param skip_validation TRUE or FALSE, indicating if validation should be skipped.
#' @param string name to be cleaned.
#' @param type if 'models' or 'update'
#' @param user_email email to receive the outputs.
#' @param user_email set the user email. We are going to use it to let you know when the modeling is over.
#' @param var_names vector of variable names to be made into clean names.
