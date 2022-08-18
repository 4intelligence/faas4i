#' @title Validates info in forecast_pack and new_data for model_update
#'
#' @description This function will perform consistency checks for a single new_data
#' and forecast_pack, to guarantee that the user has provided appropriated parameters.
#'
#' @param index index of \code{forecast_pack} and \code{new_data} in \code{pack_list}.
#' @param forecast_pack pack outputted by FaaS with modeling information to be updated.
#' @param new_data dataset to be used to update models in \code{forecast_pack}. Variable names should be the same as inital modeling.
#' @param date_variable name of variable with date information in all \code{new_data} in \code{pack_list}.
#' @param date_format format of \code{date_variable} in all \code{new_data} in \code{pack_list}.
#' @param base_dates TRUE or FALSE, indicating whether initial date in modeling should be kept for update, if possible.
#' @return A list with the following components:
#' \itemize{
#'    \item update_i - TRUE if \code{forecast_pack} and \code{new_data} passed all checks and can be updated, FALSE otherwise.
#'    \item new_data - same \code{new_data} inputted, but with clean names, as used in \code{forecast_pack}.
#'    \item txt - string with error messages, or empty string when no error is found.
#' }
#' @details This function will perform the following checks:
#' \itemize{
#'    \item If \code{forecast_pack} has at least one non combination model, as we need to update models used to compose combination to be able to update combination.
#'    \item If \code{date_variable} and \code{date_format} were properly defined.
#'    \item If all variables used in original modeling in \code{forecast_pack} are available in \code{new_data}.
#'    \item If transformations applied in original modeling are still appropriate. Eg., if the variable was originally log transformed but assumes negative values in \code{new_data}, we are not able to update this pack.
#' }
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[base]{as.Date}},\code{\link[base]{funprog}},\code{\link[base]{merge}},\code{\link[base]{any}},\code{\link[base]{log}}
#' @rdname validate_info_update
validate_info_update <- function(index, forecast_pack, new_data, date_variable, date_format, base_dates){
  ## This function will check if the index^th pack list can be updated, and returns true or false

  ## We start by setting it to true
  update_i <- TRUE

  ## Getting info about version of sminfra4i which pack was created
  if (!is.null(forecast_pack$infos[[1]]$version)){
    fp_version <- as.package_version(forecast_pack$infos[[1]]$version$sminfra4i)
  } else{
    fp_version <- NULL
  }

  ### If there are only combinations in the pack, we cant update them
  if(nrow(forecast_pack[!grepl("^comb_", forecast_pack[["type"]]),]) == 0 ){
    txt <- paste0("There is no appropriate model to update in forecast_pack of pack: ", index,".")

    update_i <- FALSE
    return(list(update_i = update_i, new_data = new_data, txt = txt))
  }

  # Force date variable to be called 'data_tidy'  =====================
  names(new_data)[names(new_data) == date_variable] <- "data_tidy"

  ## Checking if date_variable was passed incorrectly
  if (! "data_tidy" %in% names(new_data)){
    txt <- paste0( "Incorrect 'date_variable' name in pack_list: ", index, ".")

    update_i <- FALSE
    return(list(update_i = update_i, new_data = new_data, txt = txt))
  }

  # Select and format date variable
  date_column <- try(as.Date(new_data$data_tidy, format = date_format), silent=T)
  if(class(date_column) == "try-error"){
    txt <- paste0( "Incorrect 'date_format' for pack_list: ", index, ".")

    update_i <- FALSE
    return(list(update_i = update_i, new_data = new_data, txt = txt))
  } else{
    new_data$data_tidy <- as.Date(new_data$data_tidy, format = date_format)
  }

  ## Checking if lag variables were added in modeling
  lag_names <- lapply(forecast_pack$infos, function(x) x[["lags"]])
  lag_names <- unique(unlist(lag_names))
  ## Forming one long character
  lag_names <- paste0(lag_names, collapse = ",")
  ## For forecast packs in older versions, 'lags' won't be in infos, so we need to check it
  if(is.null(lag_names)) {
    lag_names <- ""
  }
  ## Separating character into a vector
  lag_names <- trimws(strsplit(lag_names, split = ',')[[1]])
  ## Making sure we get only the unique values
  lag_names <- unique(lag_names)

  ## Then we need to know maximum number of lags, as we need to keep the rows for
  ## the cases when there is information for lag in those cases.
  if(length(lag_names) > 0){
    ## Removing prefix'l', then we split by '_' and get the first argument
    lag_max <- sub("^l", "", lag_names)
    lag_max <- lapply(lag_max, function(x) strsplit(x, split = '_')[[1]][1])
    ## And get max number of lags
    lag_max <- max(as.numeric(unlist(lag_max)))
  } else{
    lag_max <- 0
  }

  # If user defined that we should keep initial date, we filter the new_data
  if( base_dates ){
    date_init <- lapply(forecast_pack$data, function(x) min(x$data_tidy))
    date_init <- as.Date(min(do.call("c", date_init)), format = date_format)
    ## We will filter dataset to start at the same position as the initial date
    ## in forecast_pack. But if there are lags in dataset, we try to keet as many rows
    ## as lag_max
    pos_init <- min(which(new_data$data_tidy >= date_init))
    pos_init <- max(pos_init-lag_max, 1)

    ## Then we filter dates that are at least 'date_init'
    new_data <- new_data[pos_init:nrow(new_data),]
  }

  # Checking if there is more than 1 observation per frequency period in dataset
  if (any(duplicated(new_data$data_tidy))){
    txt <- paste0( "There are repeated values in your date column in pack_list: ", index, ".")

    update_i <- FALSE
    return(list(update_i = update_i, new_data = new_data, txt = txt))
  }

  ## Joining all variables used in forecast pack
  ### Start by creating a column binding data and data_proj
  forecast_pack$data_all <- mapply(function(x,y){
    rbind(x,y)
  }, x = forecast_pack$data, y = forecast_pack$data_proj, SIMPLIFY = FALSE)
  ### Then we check the number of rows in each data_all
  forecast_pack$n_row <- lapply(forecast_pack$data_all, nrow)
  ### And sort the rows such that models with larger number of rows come first
  forecast_pack <- forecast_pack[order(unlist(forecast_pack$n_row),decreasing=TRUE),]
  ### Getting the row from the forecast pack just with random forest, since new versions
  ### treat the data differently
  forecast_pack_rf <- forecast_pack[forecast_pack$type=="RandomForest",]
  ### Then we remove the random forest row from the forecast_pack
  if (any(forecast_pack$type == "RandomForest") & nrow(forecast_pack) > 1){
    forecast_pack <- forecast_pack[-(forecast_pack$type=="RandomForest"),]
  }

  ## Now we can reduce all data_all (in a list format) from the models to form one unique dataset
  all_var_fp <- Reduce(function(dat1,dat2){
    ## We will merge dat1 with dat2, but for dat2 we remove the columns which names are in dat1.
    ## Notice that we allow for data_tidy (the first column) to be in both datasets, as we want to join
    ## by this variable
    names_not_common <- names(dat2)[!names(dat2) %in% names(dat1)]
    if (length(names_not_common) > 0){
      dat2 <- as.data.frame(dat2[,c("data_tidy", names_not_common)])
      merge(dat1, dat2, all.x = TRUE, by = "data_tidy")
    } else{
      dat1
    } }, forecast_pack$data_all)

  ### Since we want to know which variables are only in the random forest model (newer packs wont be
  ### log transformed, so we cant exp to bring it back to original scale), we perform the same calculations
  ### as before, but separately
  vars_rf_only <- character(0)
  if (nrow(forecast_pack_rf) > 0){
    dat_rf <- forecast_pack_rf$data_all[[1]]
    vars_rf_only <- names(dat_rf)[!names(dat_rf) %in% names(all_var_fp)]
    if(length(vars_rf_only) > 0){
      all_var_fp <- merge(all_var_fp,
                          as.data.frame(dat_rf[,c("data_tidy", vars_rf_only)]),
                          all.x = TRUE, by = "data_tidy")
    }
  }

  names_all_var_fp <- names(all_var_fp)

  ### Bringing random forest model back to forecast_pack
  forecast_pack <- rbind(forecast_pack, forecast_pack_rf)

  ## Then we remove the ones that are added in modeling pipeline

  ## Removing y_fit and outlier dummies
  if (length(grep("^y_fit.|^do_",names_all_var_fp)) > 0) {
    names_all_var_fp <- names_all_var_fp[-grep("^y_fit.|^do_",names_all_var_fp)]
  }

  ## Removing seasonal dummies - no matter the format or frequency
  monthly_dummies <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  daily_dummies <- c("S1","S2","S3","S4","S5","S6","S7","Mon","Tue","Wed","Thu","Fri","Sat","Sun",
                     "seg","ter","qua","qui","sex","sab","sáb","dom")
  quarterly_dummies <- c("Q1","Q2","Q3","Q4")

  ## Removing prefix of seasonal dummies
  names_all_var_fp <- gsub("d4i_", "", names_all_var_fp)

  if (any(names_all_var_fp %in% c(monthly_dummies, daily_dummies, quarterly_dummies))){
    names_all_var_fp <- names_all_var_fp[!names_all_var_fp %in% c(monthly_dummies, daily_dummies, quarterly_dummies)]
  }
  ## Setting names in new_data to match names used in forecast_pack
  new_data <- match_names(dataset = new_data, names_fp = names_all_var_fp)

  ### At this point we will check if the classes of variables match the ones needed for update
  ## Checking if variables are all numeric or integer
  non_numeric_var <- names(new_data)[sapply(new_data, function(x) ! any(c("numeric","integer") %in% class(x)))]
  ## Removing the 'data_tidy' from this list
  non_numeric_var <- non_numeric_var[non_numeric_var != "data_tidy"]

  ## Checking if these variables are used in fp
  non_numeric_var <- non_numeric_var[non_numeric_var %in% names_all_var_fp]
  if (length(non_numeric_var) > 0){
    txt <- paste0( "Incorrect class of variable(s) ", paste0(non_numeric_var, collapse = ", "), " in pack_list: ", index, ".")

    update_i <- FALSE
    return(list(update_i = update_i, new_data = new_data, txt = txt))
  }


  ### If there are some variables in fp that are not in new_data, we perform some extra checks
  if (all(names_all_var_fp %in% names(new_data)) == FALSE){
    ## Names of variables (in forecast pack) not found in new data
    var_names <- names_all_var_fp[!names_all_var_fp %in% names(new_data)]

    ## Removing variable 'z_gap' that can be added in pipeline for daily data
    if ("z_gap" %in% var_names){
      var_names <- var_names[var_names != "z_gap"]
    }

    ## Here we check if the variables were not found due to 'z_' added during modeling
    ## If so, we remove them from the list of variables not found
    if ( any(gsub("^z_", "",var_names) %in% names(new_data))){
      ## These will only be the cases when log=F and both forms are in pack
      var_names <- var_names[!gsub("^z_", "",var_names) %in% names(new_data)]
    }

    ## Here we check if the variables missing were lag variables that we added inside modeling
    ## Verifying if any of the variables in var_names is in lag_names
    lag_names <- c(lag_names, paste0("z_", lag_names))
    if (any(lag_names %in% var_names)){
      var_names <- var_names[! var_names %in% lag_names]
    }

    ## Check to see if all variables needed for modeling were found
    if (length(var_names) > 0){
      txt <- paste0("\nVariable(s) ",paste0(var_names, collapse = ", "),
                    " was (were) not found in new_data of pack_list: ", index,".",
                    "\nConsider checking for renamed variables and apply them in new_data.")

      update_i <- FALSE
      return(list(update_i = update_i, new_data = new_data, txt = txt))
    }
  }

  ### We check if it is possible the keep the transformations in the forecast_pack (fp)
  ## This is only needed if log is true in fp
  if(any(grepl("[Ll]og",forecast_pack$transformation))){

    ## The idea here is that we need to make sure we can keep passing log to variables that
    ## were originally log-transformed.
    ## If the variable has any of the prefixes 'data_tidy', 'z_' or 'do_', we never apply log,
    ## so we don't need to worry about them.
    ## We also don't need to worry about variables that are all greater than 0, since we can
    ## always apply log to them. Also, variables with only 2 unique values are always identified
    ## as dummy, and we don't apply log.
    ## Since we don't need to worry about these cases, we filter only the ones that are not in any
    ## of these conditions (all we have left are variables that assume any value <= 0, have at least
    ## 3 unique values and don't have any of the prefixes defined - this is done considering the entire
    ## new_data).
    ## At this point we know which variables we should worry and get these variables from the dataset
    ## that comes originally from fp, and are only variables that we know cannot be log-transformed,
    ## so if we find that the variables were originally transformed, we can't keep with update.
    ## To find if the variables were log transformed, we will compare the new_data with dataset from fp,
    ## and the dataset from fp when applying exp (back transforming). If new_data is more similar
    ## to the dataset from fp, it means that the variable was not log tranformed, however, if the new_data
    ## is more similar to the dataset from fp with exp, it means that the variable was log transformed.


    ## Selecting columns that we dont need to worry about back transforming
    new_data_temp <- new_data
    vars_with_prefix <- grepl("data_tidy|^z_|^do_", names(new_data_temp))
    new_data_temp <- new_data_temp[,!vars_with_prefix, drop = FALSE]

    vars_all_positive <- as.logical(apply(new_data_temp, 2, function(x){ all(x>0, na.rm = TRUE)}))
    new_data_temp <- new_data_temp[,!vars_all_positive, drop = FALSE]

    vars_2_unique <- as.logical(apply(new_data_temp, 2, function(x){ length(unique(x[!is.na(x)])) <= 2}))
    new_data_temp <- new_data_temp[,!vars_2_unique, drop = FALSE]

    vars_check_log <- names(new_data_temp)

    ## Making sure we get just the variables used in forecast_pack
    vars_check_log <- vars_check_log[vars_check_log %in% names_all_var_fp]

    if (length(vars_check_log) > 0){
      ## Obtaining dataset from fp that was inputted by user
      dat_fp <- all_var_fp[,names_all_var_fp]
      ## Filtering observed data
      # We first need to filter the modeling period
      ## Start by defining the y variable
      y_var <- names(forecast_pack$data[[1]])[2]
      ## Getting the position of the y_var in new_data
      y_pos <- which(names(new_data) == y_var)[1]
      ## Getting index of max obs in y_var
      y_max <- max(which(!is.na(new_data[,y_pos])))
      ## Filtering modeling period
      dat_fp <- dat_fp[1:y_max,]
      ## Getting only data_tidy and the variables that we need to perform this test
      index_dat_fp <- match(c("data_tidy",vars_check_log), names(dat_fp))
      dat_fp <- dat_fp[, index_dat_fp[!is.na(index_dat_fp)], drop = FALSE]

      ## Getting only data_tidy and the variables that we need to perform this test from new_data
      ## then we make sure we are comparing only the period that was in forecast_pack
      index_new_data <- match(c("data_tidy",vars_check_log), names(new_data))
      new_data_orig <- new_data[, index_new_data[!is.na(index_new_data)], drop = FALSE]

      intersect_dates <- as.Date(intersect(as.character(dat_fp$data_tidy), as.character(new_data_orig$data_tidy)))
      new_data_orig <- new_data_orig[new_data_orig$data_tidy %in% intersect_dates,, drop = FALSE]

      ## One last check to make sure we are comparing the exact same dates, since user can pass new_data
      ## smaller than the original
      dat_fp <- dat_fp[dat_fp$data_tidy %in% intersect_dates,, drop = FALSE]

      ## Removing column data_tidy
      dat_fp <- dat_fp[,-1, drop = FALSE]
      new_data_orig <- new_data_orig[,-1, drop = FALSE]

      ## We start by comparing the variables in new_data and in the forecast_pack at level
      diff_level <- abs(new_data_orig - dat_fp)
      diff_level <- apply(diff_level,2,sum, na.rm = TRUE)

      ## Then we compare the variable in new_data with backtransformed data in forecast_pack
      diff_exp <- abs(new_data_orig - apply(dat_fp,2,exp))
      diff_exp <- apply(diff_exp,2,sum, na.rm = TRUE)

      ## Checking if variables were log transformed
      var_became_neg <- c()
      for (k in seq_len(length(vars_check_log))){

        ## If the difference to the exponentially variable is less than to the original one,
        ## it means that it originally was log-transformed
        log_k <- ifelse(diff_exp[k] < diff_level[k], TRUE, FALSE)
        ## Then, if log_k is true, it means that we won't be able to keep the transformation
        if (as.logical(log_k)){ var_became_neg <- c(var_became_neg, names(log_k)) }

      }
      ## var_became_neg is more problematic and we can't follow with pipe
      if (length(var_became_neg) > 0){
        txt <- paste("\nVariable(s) ",paste0(var_became_neg, collapse = ", "),
                     " was (were) originally log-transformed in pack_list:", index,".",
                     "This is no longer possible due to negative or zero values. Please run the model using 'run_models' again.")

        update_i <- FALSE
        return(list(update_i = update_i, new_data = new_data, txt = txt))
      }
    }
  }

  ## Here we check if any variable in new_data during modeling period has only 1 unique value,
  ## and inform the user

  # We first need to filter the modeling period
  ## Start by defining the y variable
  y_var <- names(forecast_pack$data[[1]])[2]
  ## Getting the position of the y_var in new_data
  y_pos <- which(names(new_data) == y_var)[1]
  ## Getting index of max obs in y_var
  y_max <- max(which(!is.na(new_data[,y_pos])))
  ## Filtering modeling period
  data_modeling <- new_data[1:y_max,]

  ## Then we check which variables have only 1 unique value (if any)
  ## And add the warning if any of them have only 1 unique value.
  var_unique_value <- which(apply(data_modeling, 2, function(x) length(unique(x[!is.na(x)])) == 1))
  if ( length(var_unique_value) > 0){
    txt <- paste("\nVariable(s) ",paste0(names(new_data)[var_unique_value], collapse = ", "),
                 " has(have) only 1 unique value in modeling period in pack_list:", index,".",
                 "Models with this(these) variable(s) might not converge.")

    update_i <- TRUE ## we can still update remaining models
    return(list(update_i = update_i, new_data = new_data, txt = txt))
  }

  return(list(update_i = update_i, new_data = new_data, txt = ""))
}
