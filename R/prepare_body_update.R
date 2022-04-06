#' @title Prepares body for model update
#'
#' @description This function will prepare the encoded body to be sent via api after a series of consistency checks have been performed.
#'
#' @param pack_list list with information about all packs to be updated. For each pack a list with a \code{forecast_pack} and a \code{new_data} should be provided.
#' @param date_variable name of variable with date information in all \code{new_data} in \code{pack_list}.
#' @param date_format format of \code{date_variable} in all \code{new_data} in \code{pack_list}.
#' @param project_name project name. It accepts character and numeric inputs. Special characters will be removed.
#' @param user_email set the user email. We are going to use it to let you know when the modeling is over.
#' @param cv_update TRUE or FALSE, indicating whether cross validation should be ran again.
#' @param model_spec list containing: \code{fill_forecast}, \code{n_steps}, \code{n_windows} and \code{cv_summary}, see details for its description. All arguments are optional, however \code{n_steps}, \code{n_windows} and \code{cv_summary} should only be used when \code{cv_update = TRUE}.
#' @param base_dates TRUE or FALSE, indicating whether initial date in modeling should be kept for update, if possible.
#' @param force_request TRUE or FALSE, indicating whether estimation should continue when not all packs can be updated.
#' @param outlier_update TRUE or FALSE, indicating whether a new search for outliers should be conducted. Search for outliers will be done only when \code{cv_update} = TRUE.
#' @param breakdown TRUE, FALSE or vector with 'row_id' of models to run breakdown (max of 3 models). If \code{breakdown} is TRUE, model breakdown will be calculated for the first 3 arima models. If \code{breakdown} is FALSE, model breakdown from previous modeling will be removed from \code{forecast_pack}.
#' @return List with encoded \code{forecast_pack} and \code{new_data}.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[jsonlite]{base64}},\code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[base]{serialize}},\code{\link[base]{memCompress}}
#'  \code{\link[caTools]{base64encode & base64decode}}
#' @rdname prepare_body_update
#' @importFrom jsonlite base64_enc toJSON
#' @importFrom utils packageVersion
#' @importFrom caTools base64encode
prepare_body_update <- function(pack_list, date_variable, date_format,  project_name, user_email,
                                cv_update, model_spec, base_dates, force_request, outlier_update, breakdown) {

  ## The following vector will be updated for each item in the pack_list if it
  ## passes the consistency checks.
  pack_update <- rep(FALSE, times = length(pack_list))

  ## We will append any error/warning message in message_list
  message_list <- "\n"

  ## For each item in the pack list we check if we can update it and organize the
  ## names of the variables in new_data
  for (i in 1:length(pack_list)){

    ## Checking if the i^th forecast pack can be updated
    validate_i <- validate_info_update(index = i,
                                       forecast_pack = pack_list[[i]][["forecast_pack"]],
                                       new_data =  pack_list[[i]]$new_data,
                                       date_variable = date_variable,
                                       date_format = date_format,
                                       base_dates = base_dates)

    pack_update[i] <- validate_i$update_i

    ## If there is any message for this pack, we append the message outputted by validate_info_update
    if ( validate_i$txt != "" ){
      message_list <- paste0(message_list,validate_i$txt,"\n")
    }

    ## Serializing and encoding the forecast pack for the i^th pack list
    y_name_i <- names(pack_list[[i]][["forecast_pack"]]$data[[1]])[2]
    pack_list[[i]][["forecast_pack"]] <- jsonlite::base64_enc(serialize(pack_list[[i]][["forecast_pack"]], NULL))
    pack_list[[i]][["new_data"]] <- validate_i$new_data
    pack_list[[i]][["y_name"]] <- y_name_i

    rm(validate_i); rm(y_name_i)
  }

  ## If pack_update are all FALSE, we can't update any model and stop
  if (sum(pack_update) == 0){

    stop(paste0(message_list, "\nNo pack can be updated, see messages above."))
  }

  ## But if some of them can be updated (meaning that at least one of pack_update
  ## was FALSE), but not all, we give a warning or stop if force_request = FALSE
  if (sum(!pack_update) > 0) {
    if (force_request == TRUE) {
      warning(paste0(message_list,"\nPack(s) that can't be updated: ", paste(which(!pack_update), collapse = ", "),"."))
    } else{
      stop(paste0(message_list, "\nPack(s) that can't be updated: ", paste(which(!pack_update), collapse = ", "),". \n",
                  "To send successful ones, pass 'force_request = TRUE'"))
    }
  }

  ## If all models can be updated, but there are warnings to be printed
  if( (sum(pack_update) == length(pack_update)) & nchar(message_list) > 1){
    message(paste0(message_list))
  }

  ## Removing packs that won't be updated from pack_list
  pack_list <- pack_list[pack_update]

  ## Due to the size of the forecast_pack, we will send them one at a time
  # body <- list()
  # for (i in 1:length(pack_list)){
  #   ## Creating a list with all info needed for the api request.
  #   body[[i]] <- list(pack_list = list(pack_list[[i]]),
  #                     cv_update = cv_update,
  #                     model_spec = model_spec,
  #                     user_email = user_email,
  #                     project_id = project_name,
  #                     date_variable = date_variable,
  #                     date_format = date_format)

  #   ## Compressing and enconding the body
  #   body[[i]] <- caTools::base64encode(memCompress(jsonlite::toJSON(body[[i]]),
  #                                                  type = "gzip"))

    # Creating JSON that will be sent
    #body[[i]] <- jsonlite::toJSON(list("body" = body[[i]], "skip_validation" = FALSE), auto_unbox = TRUE)
    # body[[i]] <- list("body" = body[[i]], "skip_validation" = TRUE

  body <- list(pack_list = pack_list,
               cv_update = cv_update,
               model_spec = model_spec,
               user_email = user_email,
               project_id = project_name,
               date_variable = date_variable,
               date_format = date_format,
               base_dates = base_dates,
               outlier_update = outlier_update,
               breakdown = breakdown,
               version = as.character(utils::packageVersion("faas4i")))

    ## Compressing and enconding the body
    body <- caTools::base64encode(memCompress(jsonlite::toJSON(body),
                                                   type = "gzip"))

    # Creating JSON that will be sent
    # body <- jsonlite::toJSON(list("body" = body, "skip_validation" = TRUE), auto_unbox = TRUE)
    body <- list("body" = body, "skip_validation" = TRUE)

  # }

  return(body)
}
