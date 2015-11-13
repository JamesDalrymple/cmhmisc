#' @title WCCMH date functions
#' @description standardize date classes
#'
#' @param x A character vector of dates.
#' @param format The format of the incoming date.
#' @param origin The origin (day 1) for the date.
#' @return A vector of dates of class 'Date'.
#'
#' @note more coming soon...
#'
#' @examples
#' \dontrun{
#' date_convert("10/1/2015")
#' date_convert(x = factor("10/1/2015"))
#' }
#' @importFrom EquaPac new_counter as.chr
#'
#' @name date_functions
NULL

#' @rdname date_functions
date_convert <-
  function(x, format = "%m/%d/%Y", origin = "1970-01-01") {
    fn_count <- new_counter()
    i <- fn_count()
    switch(
      class(x),
      "Date" = {
        NULL
      },
      "character" = {
        y <- as.Date(x, format = format, origin = origin)
      },
      "factor" = {
          x <- as.chr(x)
          y <- date_convert(x)
          if (i > 2) {
            p_stop("x already passed twice through 'factor',
                   report bug to wccmh author with example data!")
          }
          i <- fn_count()
      }
    )
    return(y)
  }
