#' @title Pretty print messages convenience
#'
#' @description This function uses is a convenience function for writing error,
#'   warning, and message statements. p_note is an alias for p_statement.
#'
#' @param ... Accepts any mix of character strings and objects containing
#'   character vectors.
#' @param collapse Defaults to " " but you can override it if you desire.
#' @param parent_call Defaults to NULL, but may be override to a character
#' vector of length one which will show up as the name of the parent function
#' from which the error originates.
#'
#' @return A character vector (1) collapsed, (2) comma separated, and
#' (3) having each item in single quotes. Additionally text will be
#' concatenated.
#'
#' @details Any list provided to input x will be unlisted and collapsed in the
#'   same way that any character vector would be collapsed. This function relies
#'   on sprintf and paste. Removes all consecutive (extra) spaces and removes
#'   \code{/r} returns and \code{/n} newlines as well. To keep newlines,
#'   you need to input then as /newline or /new, as /n by itself will be
#'   deleted.
#'
#' @section Note:
#' The regular expression work could be much better, this is a to-do at some
#' point. to-do: p_paste (p_note or p_statement could be renamed instead?)
#' would be a character vector output a person could use, possibly, but would
#' not accept non-character class objects.

#' @examples

#' \dontrun{
#' DT <- data.table(1:10, LETTERS[1:10])
#' DF <- data.frame(v1 = 1:10, v2 = LETTERS[1:10])
#' l <- list(one = "1", two = "2")
#' char <- "this is a message with a return:\newline yay"
#' fac <- factor("this is a message too")
#' mat <- matrix(c(1:10, 10:1), nrow = 10)
#' TBmisc:::capture(DT)
#' TBmisc:::capture(DF)
#' TBmisc:::capture(l)
#' TBmisc:::capture(char)
#' TBmisc:::capture(fac)
#' TBmisc:::capture(mat)
#' p_msg("DT:", DT, char)
#' p_msg("this is a note", list("hi", "bye", NULL))
#' p_warn("DT:", DT, char)
#' testit <- function() p_warn("DT:", DT, char)
#' testit()
#' p_statement("This is just a plain note", DT, char)
#' test_stop <- function() p_stop("DT:", DT, char)
#' test_stop()
#' p_stop("DT:", DT, char)
#' p_stop("This is really bad!", DT, char)
#' p_stop("This is really bad!", DT, char, parent_call = "bad_fn")
#'
#' require(TBmisc)
#' packages <- c("xtable", "knitr", "data.table", "ggplot2", "zoo", "xlsx",
#' "RODBC")
#' pkg_loader(packages, verbose = TRUE)
#' capture(packages)
#' }
#'
#' \dontshow{ # problem
#' fake_msg <- function(x) {
#'   x_list <- list(input_x = x,
#'                   other = c("this", "is", "silly"))
#'   p_msg("I have to tell you:", x_list$input_x)
#' }
#' x <- c("ggplot2", "knitr", "xlsx", "xtable")
#' fake_msg(x = x)
#' }
#'
#' @section TODO:
#' \itemize{
#'    \item Needs cleaning.
#' }
#'
#' @importFrom utils flush.console capture.output str
#'
#' @name messages
NULL

#' @rdname messages
#' @keywords internal
capture <- function(x) {
  x_classes <- class(eval(x))
  if (any(x_classes == "character")) {
    # messy regex work to be cleaned once I learn how to regex better - JDD
    result <- gsub(pattern = "[\r]", replacement = " ", x = x)
    result <- gsub(pattern = "\\n(?!ew)", replacement = " ",
                   x = result, perl = TRUE)
    result <-
      gsub(pattern = "\\n(ewline)|\\n(ew)", replacement = "\n",
           x = result, perl = TRUE)
    result <- gsub(pattern = "( ", fixed = TRUE,
                   replacement = "(", x = result, perl = FALSE)
    result <- gsub(pattern = " )", fixed = TRUE,
                   replacement = ")", x = result, perl = FALSE)
    result <- gsub(pattern = " [", fixed = TRUE,
                   replacement = "[", x = result, perl = FALSE)
    result <- gsub(pattern = " ]", fixed = TRUE,
                   replacement = "]", x = result, perl = FALSE)
    result <- paste0("'", paste(result, collapse = "', '"), "'")
    result <- utils::capture.output(cat(eval(result)))
    result <- gsub(pattern = "^ *|(?<= ) | *$",
                   replacement = "", x = result, perl = TRUE)
    result <- paste0(result, collapse = "\n")
  } else if (any(x_classes %in% c("data.frame", "matrix")) &
             !any(x_classes == "data.table")) {
    x_copy <- data.table(x)
    result <- capture(x_copy)
  } else if (any(x_classes == "data.table")) {
    result <- paste0(utils::capture.output(print(x)), collapse = "\n")
  } else {
    result <- paste0(utils::capture.output(utils::str(x)),collapse = "\n")
  }
  return(result)
}

#' @rdname messages
#' @export
p_msg <- function(..., envir = parent.frame()) {
  full_call <- match.call()[-1]
  p_classes <- lapply(full_call, function(x) class(eval(x, envir)))
  args_l <- lapply(full_call, function(x) capture(eval(x, envir)))
  message(paste0(args_l, sep = "\n"))
}

#' @rdname messages
#' @export
p_warn <- function(..., envir = parent.frame()) {
  full_call <- match.call()[-1]
  p_classes <- lapply(full_call, function(x) class(eval(x, envir)))
  args_l <- lapply(full_call, function(x) capture(eval(x, envir)))
  if (sys.nframe() > 1) {
    parent_call <- sys.call(1)
    warning(paste("in", parent_call), ":\n",
            paste0(args_l, sep = "\n"), call. = FALSE)
  } else {
    warning(paste0(args_l, sep = "\n"), call. = TRUE)
  }
}

#' @rdname messages
#' @export
p_stop <- function(..., parent_call = NULL, envir = parent.frame()) {
  p_call <- match.call()[-1]
  p_classes <- lapply(p_call, function(x) class(eval(x, envir)))
  args_l <- lapply(p_call, function(x) capture(eval(x, envir)))
  if (!is.null(parent_call)) {
    stopifnot(class(parent_call) == "character", length(parent_call) == 1)
    args_l$parent_call <- NULL
  } else {
    parent_call <- sys.call(1)
    parent_call <- as.character(list(parent_call[[1]]))
  }
  stop(parent_call, "\n",
       paste0(args_l, sep = "\n"), call. = FALSE)
}

#' @rdname messages
#' @export
p_statement <- function(..., envir = parent.frame()) {
  CALL <- match.call()[-1]
  args_l <- lapply(CALL, function(x) capture(eval(x, envir)))
  cat(paste0(args_l, sep = "\n"))
}

#' @rdname messages
#' @export
p_note <- function(..., envir = parent.frame()) { # alias to p_statement
  CALL <- match.call()[-1]
  args_l <- lapply(CALL, function(x) capture(eval(x, envir)))
  cat(paste0(args_l, sep = "\n"))
}

#' @rdname messages
#' @description \code{p0ff} is a convenience printer for function and for
#'   internals, used to avoid using \code{flush.console()}.
#' @inheritParams base::paste
#' @examples
#' for (i in seq(5)) p0ff(i)
#' @export
p0ff <- function(..., sep = "", collapse = NULL){
  print(paste(...,  sep = sep, collapse = collapse))
  flush.console()
}

#' @rdname messages
#' @description \code{pf} is used for force the printing of passed objects
#'   within nested functions such as loops, applys, and data.tables. No changes
#'   are made to the passed object.
#' @examples
#' sapply(seq(10), pf)
#' @export
pf <- function(...) {print(...) ; flush.console() ; return(invisible(...))}

#' @rdname messages
#' @description \code{\%P\%{}} Is a convenient pipe function used for interactive programming. When it is desired to print all the rows of a table-like object, one can merely add \code{\%P\%{}} to the end of the expression, which avoids the necessity of wrapping the expression with \code{print(..., nrows = Inf)}.
#' @examples
#' DT <- data.table(x = seq(500), y = seq(500))
#' print(DT) # data.table prints the convenient top-5 bottom-5 by default.
#' DT %P%{} # simple print pipe for infinite nrows.
#' @export
`%P%` <- function(x, P = NULL) print(x, nrows = Inf)
