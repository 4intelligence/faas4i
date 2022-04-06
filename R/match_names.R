#' @title Matching original names with forecast_pack formulations
#'
#' @description This function will receive a dataset and a vector of all names
#' of variables from dataset that is used in the forecast_pack, and matches them
#' using any parameterization that we currently use at 4i (either front or api).
#'
#' @param dataset dataset with names to be matched with \code{names_fp}.
#' @param names_fp names of variables to match in the \code{dataset}.
#' @return The dataset with matched names.
#' @details For variables not found in \code{names_fp}, we apply the api parameterization, to make sure we have clean names.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[base]{chartr}}
#' @rdname match_names
match_names <- function(dataset, names_fp){
  ## This function will match the dataset names with its equivalent in the
  ## forecast_pack, and return the dataset with the matched names

  ## Obtaining variable names
  Names <- names(dataset)

  ## The approach with be the following:
  ## We create a dataframe with all possible adjustments for the variable names,
  ## from which we will try to match with the names used in the forecast_pack.
  names_matrix <- data.frame(id = 1:length(Names),original = Names, front = NA,
                             z_front = NA,
                             api = NA, z_api = NA, fp = NA)

  ## Adding format names could be if rum via front, and its version with 'z_'
  names_matrix$front <- vec_string_treat(tolower(Names))
  names_matrix$z_front <- paste0("z_", names_matrix$front)

  ## Adding format names could be if rum via api, and its version with 'z_'
  names_matrix$api <- make.names(iconv(Names, to = 'ASCII//TRANSLIT'), unique = TRUE)
  names_matrix$api <- vec_string_treat(tolower(names_matrix$api))
  names_matrix$z_api <- paste0("z_", names_matrix$api)

  ## Matching the names in dataset with the ones in the pack - names_fp
  ## For each row we check if any of the format in the names_matrix are in
  ## names_fp, if any format is in there, we save the name used in names_fp
  ## The bind excludes the id and fp columns
  names_matrix$fp <- apply(names_matrix[,-c(1,7)], 1, function(x){
    names_fp[grepl(paste0(paste0("^",x,"$"), collapse="|"), names_fp)]})

  ## If the forecast_pack has more than one form of the variable name,
  ## we will make sure that we keep the name originally inputted by user
  index_dup <- which(unlist(lapply(names_matrix$fp,length)) > 1)
  if ( length(index_dup) > 0){
    ## We name it without the 'z_' as this was the original input, it is
    ## the shorter version
    for (j in 1:length(index_dup)){
      dup_names <- names_matrix$fp[index_dup[j]][[1]]
      names_matrix$fp[index_dup[j]][[1]] <- dup_names[which.min(nchar(dup_names))]
    }
  }

  ## Adding column that checks if length of the strings is 0 in 'fp' column
  ## This will be used to check which column names were not matched
  names_matrix$fp_length <- unlist(lapply(names_matrix$fp,function(x) identical(x, character(0))))

  ## If fp_length is TRUE, it means that there is no exact match, so we set fp
  ## to the form that would be outputted via api
  names_matrix$fp[names_matrix$fp_length == TRUE] <- names_matrix$api[names_matrix$fp_length == TRUE]

  ## Making sure the order of the rows in names_matrix is the same as it started
  names_matrix <- names_matrix[names_matrix$id,]
  ## Renaming the columns in the dataset
  names(dataset) <- unlist(names_matrix$fp)

  return(dataset)
}
