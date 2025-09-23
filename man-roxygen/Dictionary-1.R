#' @param ... advanced parameters.
#' @param ... PARAM_DESCRIPTION
#' @param body_size Size of compressed body, in megabits
#' @param config_file set the user email.
#' @param data_list list with datasets to be modeled, where the list elements must be named after the dependent variable. You cannot have more than one dependent variable with same name in a \code{data_list}.
#' @param data_list list with named datasets to be modeled.
#' @param dataset dataset with names to be matched with \code{names_fp}.
#' @param date_format format of \code{date_variable} in all datasets in \code{data_list}.
#' @param date_variable name of variable with date information in all datasets in \code{data_list}.
#' @param extra_list A boolean indicating if there is an extra layer of list (currently TRUE for error lists)
#' @param filename name of the zipped file.
#' @param get_project_id if TRUE, returns the project ID when the request is successful
#' @param message_list The list with messages, either error list or warning list
#' @param model_spec list containing: \code{n_steps} (required), \code{n_windows} (required), \code{log}, \code{seas.d}, \code{n_best}, \code{accuracy_crit}, \code{exclusion}, \code{golden_variables}, \code{fill_forecast}, \code{cv_summary}, \code{selection_methods}, \code{lags}, \code{allowdrift} and \code{allowoutliers}. See details for more information.
#' @param model_spec list with modeling and cross validation setup.
#' @param names_fp names of variables to match in the \code{dataset}.
#' @param path folder in which the downloaded files should be saved.
#' @param project_id id of the project to be downloaded which can be founded by using the function \code{list_files}..
#' @param project_id if provided, retrieves information for a specific project
#' @param project_name project name. A string with character and/or numeric inputs that should be at most 50 characters long. Special characters will be removed.
#' @param proxy_port Proxy port number
#' @param proxy_url The url to be used as proxy
#' @param repo repository url to obtain latest version
#' @param save_local [DEV ONLY] directory to save base64 with body that would be sent to the API. With this parameter the function will not send your modeling request to the API. Default: NULL.
#' @param skip_validation TRUE or FALSE, indicating if validation should be skipped.
#' @param sleep_time Maximum waiting for URI authentication
#' @param string name to be cleaned.
#' @param type Modeling type, if 'models' or 'update'
#' @param user_email email to receive the outputs.
#' @param user_model list containing the models constraints to create a model customized by the user.
#' @param user_model list containing the models constraints to create a model customized by the user. See details for more information.
#' @param var_names vector of variable names to be made into clean names.
