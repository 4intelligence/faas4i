#' @title Print messages from validation in a pretty format
#' @name pretty_R
#'
#' @description Ths function uses messages derived from valdation and organize the
#' input to be pretty printed
#'
#' @param message_list The list with messages, either error list or warning list
#' @return A formmated string to be nicely printed
#' @rdname pretty_R
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }

pretty_R <- function(message_list)
{

  msgs <- lapply(message_list,
                 function(x)
                 {

                   tmp <- lapply(x,
                                 function(y)
                                 {
                                   l1 <- paste0(unlist(y[1], recursive = TRUE), collapse = ",")
                                   l2 <- paste0(unlist(y[2], recursive = TRUE), collapse = ",")
                                   l3 <- paste0(unlist(y[3], recursive = TRUE), collapse = ",")
                                   l4 <- paste0(unlist(y[4], recursive = TRUE), collapse = ",")

                                   mes <- paste0(l1,
                                                 " ",
                                                 l3,
                                                 "; Original Value: ",
                                                 l2,
                                                 " in dataset: ",
                                                 l4)
                                 }
                   )
                   out <- unlist(tmp)
                   paste0(names(tmp),"\n - ",out)
                 })

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
