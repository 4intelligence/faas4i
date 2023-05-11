#' @title Print messages from validation in a pretty format
#' @name pretty_R
#'
#' @description Ths function uses messages derived from valdation and organize the
#' input to be pretty printed
#'
#' @param message_list The list with messages, either error list or warning list
#' @param extra_list A boolean indicating if there is an extra layer of list (currently TRUE for error lists)
#' @return A formmated string to be nicely printed
#' @rdname pretty_R
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }

pretty_R <- function(message_list, extra_list = FALSE)
{

  organize_message <- function(x){
    out <- paste0(x[["status"]], " ",
                  x[["error_type"]], ". ",
                  "Original Value: ", x[["original_value"]],
                  " in dataset: ", x[["dataset_error"]])
    return(out)
  }


  if(extra_list){

    msgs <- lapply(message_list,
                   function(x)
                   {
                       tmp <- lapply(x,function(y){organize_message(y)})
                       out <- unlist(tmp)
                       paste0(names(tmp),"\n - ",out)
                   })
  } else{
    msgs <- mapply(function(x,y){

      out <- organize_message(x)
      paste0(names(y),"\n - ",out)

    }, x = message_list, y = names(message_list), SIMPLIFY = FALSE)
  }

  names_msgs <- names(msgs)

  msgs_print <- mapply(function(names_msgs,message_to_print)
  {
    tmp_msg <- paste0(unlist(message_to_print),collapse ="\n")
    out <- paste0("*",names_msgs,"*\n",tmp_msg)
  },
  names_msgs = names_msgs,
  message_to_print = msgs,SIMPLIFY = F
  )

  msgs_print <- paste0(unlist(msgs_print),collapse = "\n\n")

  return(msgs_print)

}
