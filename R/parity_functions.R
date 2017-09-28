#' Even/Odd (Parity) Functions

#' @description Extract even/odd characters via odd_char/even_char. Extract
#' even/odd elements of an R objects (vector or list) via even_obj/odd_obj
#'
#' @param x A character vector for even_char/odd_char or a R list/vector for
#' even_obj/odd_obj.
#'
#' @return the even or odd elements of the input, in vector format for odd_char/even_char,
#' and the format remains consistent with the input for even_obj/odd_obj.
#'
#' @note convenience functions only. even_char/odd_char perhaps could be more
#' efficient.
#'
#' @examples
#' # ExAMPLE 1
#' odd_char(paste(letters[1:26], collapse = ""))
#'
#' # EXAMPLE 2
#' even_char("a1b2c3d4")
#'
#' # EXAMPLE 3
#' ex_list <-
#'  list(a1 = "warthog", b2 = "bomber",
#'  c3 = "PO", c4 = "explosives are dangerous")
#' odd_obj(ex_list)
#' even_obj(ex_list)
#' @name parity_functions

#' @export
#' @rdname parity_functions
odd_char <- function(x) {
  if (length(x) != 1) stop("x must be of length 1")
  pairs <- substrings(text = x, cuts = seq(0, nchar(x), by = 2))[-1]
  vapply(pairs, function(x) substr(x, 1, 1), USE.NAMES = FALSE, FUN.VALUE = "i")
}
#' @export
#' @rdname parity_functions
even_char <- function(x) {
  if (length(x) != 1) stop("x must be of length 1")
  pairs <- substrings(text = x, cuts = seq(0, nchar(x), by = 2))[-1]
  vapply(pairs, function(x) substr(x, 2, 2), USE.NAMES = FALSE, FUN.VALUE = "i")
}
#' @export
#' @rdname parity_functions
even_obj <- function(x) {
  x[seq(x) %% 2 == 0]
}
#' @export
#' @rdname parity_functions
odd_obj <- function(x) {
  x[seq(x) %% 2 != 0]
}