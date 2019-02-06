#' @title string convenience functions
#' @description String convenience functions not in commonly used string packages.
#'
#' @usage add_quote(x, quote_char = "'")
#' @param x character vector
#' @param quote_char The character(s) to quote the input vector 'x'.
#'
#' @examples
#' test1 <- c("date1", "date2", "10/1/2015")
#' add_quote(test1)
#' test2 <- Sys.Date()
#' add_quote(test2)
#' @return A quoted character vector equal to length of input x.
#'
#' @note Created for R-SQL automation and convenience purposes.
#'
#' @name convenience_strings
NULL

#' @rdname convenience_strings
#' @export
# function for creating a fiscal year from month (date class) ----
add_quote <- function(x, quote_char = "'") {
  if (class(x) == "Date") x <- as.character(x)
  if (class(x) != "character") stop("input x is not of class 'character'")
  paste0("'", x, "'")
}

