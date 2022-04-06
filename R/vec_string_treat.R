#' @title Treats vector of string to have a clean name
#'
#' @description This function will receive a vector of variable names, make them
#' into clean names. Later it will check for duplicates and append number to make
#' sure all names are different.
#'
#' @param var_names vector of variable names to be made into clean names.
#' @return A vector of clean names.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname vec_string_treat
vec_string_treat <- function(var_names){
  ## This function will receive a vector o column var_names and will turn it
  ## into clean var_names, the same way the front does.

  ## We start by treating each of the var_names separately
  treated_names <- unlist(lapply(var_names, function(x) string_treat(x)))

  ## Checking if there is a duplicated name
  ## Appends the column number to repeated instances of duplicate variable names.
  while (any(duplicated(treated_names))) {
    dupe_count <-
      vapply(
        seq_along(treated_names), function(i) {
          sum(treated_names[i] == treated_names[1:i])
        },
        1L
      )

    treated_names[dupe_count > 1] <-
      paste(
        treated_names[dupe_count > 1],
        dupe_count[dupe_count > 1],
        sep = "_"
      )
  }
  return(treated_names)
}
