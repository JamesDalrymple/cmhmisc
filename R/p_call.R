#' @title Call Holder for Function-Creation
#'
#' @description Making data.table and call-lists, their names, and their outputs work
#' hand in hand. This function is for testing only and not for use in real work (yet)
#'
#' @param ... Accepts any R object.
#' @param envir The environment you want to see the final output land inside.
#' Default is global environment.
#'
#' @details This may or may not be a wild goose chase.
#'
#' @return A data.table object. Not for use, more for demonstration purposes.
#'
#' @examples
#' \dontrun{
#' DT <- data.table(1:10, letters[1:10])
#' letter_l <- as.list(LETTERS[1:10])
#' # i cant seem to save this print statement, but w/e not really needing to either
#' p_call("Hey", "Look!", DT, "dont try this at home", letter_l, "but", "have fun")
#' dt_call[call_char=="p_call", call_in]
#' dt_call[call_char=="DT", output]
# p_call("Hey", "Look!", "dont try this at home", letter_l, "but", "have fun")

#' vec_string <- c("this", "is", "a", "string")
#' vec_numeric <- c(1, 1, 2, 3, 5, 8, 13)
#' vec_bool <- c(TRUE, FALSE, FALSE, TRUE)
#'
#'
#' p_call(DT, vec, "phrase")
#' p_call("long string with \n characters to for the return line")
#' p_call("a mixture of every conceivable usage case")
#'
#' }
#'

#'
#' @importFrom data.table data.table :=
#' @section TODO:
#' \itemize{
#'    \item Needs review, documentation, and rewrite.
#' }
#' @export
#' @rdname p_call
p_call <- function(..., envir = .GlobalEnv) {
  call_in <- as.list(sys.call())
  # call_in <- as.list(sys.call())
  call_type <- lapply(call_in, function(x)
    typeof(eval(x)))
  call_class <- lapply(call_in, function(x)
    class(eval(x)))

  dt_call <- data.table(call_in = call_in,
                        call_char = as.character(call_in),
                        call_type, call_class)
  call_args <- call_in[-1]
  new_names <- sapply(call_args, function(x) {
    as.character(as.name(x))
  })
  new_names[names(new_names) != ""] <-
    names(new_names[names(new_names) != ""])
  names(call_args) <- new_names
  output <- lapply(call_args, function(x) {
    eval(x)
  })
  dt_call[, output := c(NA, output)]
  # assign(x = "dt_call", value = dt_call, pos = 1)
  assign(x = "dt_call", value = dt_call, envir = envir)
  return(dt_call)
}

#' @export
#' @rdname p_call
fn <- function(...) { # NOT ACTUALLY DOING ANYTHING YET....
  # callingFun = as.list(sys.call(-1))[[1]]
  calledFun <- as.list(sys.call())[[1]]
  # message(paste(callingFun, " is calling ", calledFun, sep = ""))
  return(paste0('(', calledFun, ')'))
}
