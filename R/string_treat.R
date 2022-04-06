#' @title Treats string so it is a clean names
#'
#' @description This function will receive a single string and apply treatments
#' to make it into a clean name. This replicates the process applied in our
#' front end.
#'
#' @param string name to be cleaned.
#' @return A clean name.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname string_treat
string_treat <- function(string) {

  ## Checking if only one variable name was given as argument
  if (length(string) > 1){
    stop("'string' should be of length 1.")
  }

  ## Replacing characters that are treated differently
  replaced_names <- gsub("'","",string)
  replaced_names <- gsub("\"","",replaced_names)
  replaced_names <- gsub("%","_percent_",replaced_names)
  replaced_names <- gsub("#","_number_",replaced_names)
  replaced_names <- gsub("µ","m",replaced_names)
  replaced_names <- gsub("²|³|£|¢","_",replaced_names)

  ## We replace "ª" and "º" with "a" and "o" and save their position
  replace_a <- FALSE
  if (grepl("ª", replaced_names)) {
    replace_a <- TRUE
    pos_a <- gregexpr("ª", replaced_names)
    replaced_names <- gsub("ª", "a", replaced_names)
  }

  replace_o <- FALSE
  if (grepl("º", replaced_names)) {
    replace_o <- TRUE
    pos_o <- gregexpr("º", replaced_names)
    replaced_names <- gsub("º", "o", replaced_names)
  }

  ## Taking care of transliteration
  transliterated_names <- iconv(replaced_names,to = 'ASCII//TRANSLIT')

  ## Bringing "ª" and "º" back
  if (replace_a){
    string_splitted_a <- strsplit(transliterated_names,"")[[1]]
    string_splitted_a[unlist(pos_a)] <- "ª"
    transliterated_names <- paste0(string_splitted_a, collapse = "")
  }
  if (replace_o){
    string_splitted_o <- strsplit(transliterated_names,"")[[1]]
    string_splitted_o[unlist(pos_o)] <- "º"
    transliterated_names <- paste0(string_splitted_o, collapse = "")
  }

  ## Removing starting whitespace
  transliterated_names <- trimws(transliterated_names, which = "left")

  ## Removing starting punctuation
  good_start <-
    sub(pattern="\\A[\\h\\s\\p{Punctuation}\\p{Symbol}\\p{Separator}\\p{Other}]*(.*)$",
        replacement="\\1",
        x=transliterated_names )

  # Converting all interior spaces and punctuation to single dots
  good_start <- gsub("\\s", ".", good_start)
  cleaned_within <- gsub("[[:punct:]]", ".", good_start)
  cleaned_within <- gsub('([[:punct:]])\\1+', '\\1', cleaned_within)

  ## Removing starting dot
  cleaned_within <- sub("^[.]","",cleaned_within)

  ## Making clean names
  made_names <- make.names(cleaned_within)

  ## It is possible that it added "X" in the beggining, so we make it
  ## lower case
  made_names <- tolower(made_names)

  ## Replacing dots by "_" and then removing duplicated "_"
  made_names <- gsub('.', '_', made_names, fixed = TRUE)
  made_names <- gsub('^_|_$', '', made_names)

  return(made_names)
}
