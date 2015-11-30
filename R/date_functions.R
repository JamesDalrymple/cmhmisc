#' @title WCCMH date functions
#' @description standardize date classes
#'
#' @param x A character vector of dates.
#' @param format The format of the incoming date.
#' @param origin The origin (day 1) for the date.
#' @return A vector of dates of class 'Date'.
#'
#' @note .date_convert could be moved to C++. lapply() loses the class 'Date',
#' perhapse a better method exists.
#'
#' @examples
#' \dontrun{
#' date_convert(x = c("10/1/2015", "10/2/2015"))
#' date_convert(x = factor("10/1/2015"))
#' x <- factor(c("10/1/2014", "10/2/2015", "10/1/2014", NA))
#' x <- c("10/1/2014", "10/2/2015", "10/1/2014", NA)
#' x <- as.Date("10/1/2014", format = "%m/%d/%Y")
#' date_convert(x)
#' }
#' @importFrom EquaPac new_counter as.chr
#' @importFrom data.table data.table :=
#'
#' @name date_functions
NULL

#' @rdname date_functions
.date_convert <- function(x, format, origin) {
  {
    if (is.na(x)) {
      result <- as.Date(NA)
    } else {
      split_x <- unlist(strsplit(x, split = "/|-"))
      if (3L != length(split_x)) {
        p_stop(
          "A date must have 3 parts, with separators '-' or '/'.
          You provided",
          x,
          "which is unacceptable."
        )
      }
      first_sep <- regexpr(text = x, pattern = "/|-")
      sep_symbol <- substr(x, first_sep, first_sep)
      # normalize date to have leading zero
      split_x <-
        ifelse(nchar(split_x) == 1L, paste0("0", split_x), split_x)
      date_parts <- sapply(split_x, nchar, USE.NAMES = FALSE)
      if (identical(date_parts, c(2L, 2L, 4L))) {
        format <- paste0("%m", sep_symbol, "%d", sep_symbol, "%Y")
      } else if (identical(date_parts, c(4L, 2L, 2L))) {
        format <- paste0("%Y", sep_symbol, "%m", sep_symbol, "%d")
      } else if (identical(date_parts, c(2L, 2L, 2L))) {
        format <- paste0("%m", sep_symbol, "%d", sep_symbol, "%y")
      } else {
        # error
        date_format_l <- list(
          ISO = 'Y-m-d',
          USA1 = 'm/d/Y',
          USA2 = 'm/d/y',
          USA3 = 'm-d-Y',
          USA4 = 'm-d-y'
        )
        p_stop("The date provided did not follow a date format:",
               date_format_l)
      }
      result <- as.Date(x, format = format, origin = origin)
    }
    return(result)
  }
  }

date_x <- result_x <- NULL # RMD checker appeasement

#' @rdname date_functions
#' @export
date_convert <-
  function(x, format = NULL, origin = "1970-01-01") {
    fn_count <- new_counter()
    i <- fn_count()
    input_dt <- data.table(date_x = x, class_x = class(x))
    switch(
      class(x),
      "Date" = {
        NULL
      },
      "character" = {
        input_dt[, result_x :=
                   vapply(
                     date_x,
                     FUN = .date_convert,
                     FUN.VALUE = as.Date(NA),
                     format,
                     origin,
                     USE.NAMES = FALSE
                   )]
      },
      "factor" = {
        x <- as.chr(x)
        y <- date_convert(x)
        if (i > 2) {
          p_stop(
            "x already passed twice through 'factor',
            report bug to wccmh author with example data!"
          )
        }
        i <- fn_count()
        }
    )
    return(input_dt[, as.Date(result_x, origin)])
      }
