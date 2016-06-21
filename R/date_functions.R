#' @title WCCMH date functions
#' @description standardize date classes
#'
#' @param x A character vector of dates.
#' @param format The format of the incoming date.
#' @param origin The origin (day 1) for the date.
#' @param start_date The start date for date_expansion, must become a date if
#' not already one via date_convert function.
#' @param end_date The end date for date_expansion, must become a date if
#' not already one via date_convert function.
#' @param types A character vector defining which types to be returned. Choices are
#' any combination of c("fy", "qtr", "mon"). Default is all.
#' @return A vector of dates of class 'Date'.
#'
#' @note .date_convert could be moved to C++. lapply() loses the class 'Date',
#' perhapse a better method exists. Also, today:  use S3 methods instead of a
#' switch statement.
#'
#' @examples
#' date_convert(x = c("10/1/2015", "10/2/2015"))
#' date_convert(x = factor("10/1/2015"))
#' x <- factor(c("10/1/2014", "10/2/2015", "10/1/2014", NA))
#' date_convert(x)
#' x <- c("10/1/2014", "10/2/2015", "10/1/2014", NA)
#' date_convert(x)
#' x <- as.Date("10/1/2014", format = "%m/%d/%Y")
#' date_convert(x)
#' x <- as.POSIXct(c("2014-10-1", "2015-10-2", NA))
#' date_convert(x)
#' date_convert(x = as.Date("2014-10-1"))
#' date_expansion(start_date = "10/1/2014", end_date = "9/30/2015")
#' @importFrom EmiscDev as.chr
#' @importFrom data.table data.table := rbindlist copy
#' @importFrom zoo as.yearqtr as.yearmon
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
date_convert <- function(x, format = NULL, origin = "1970-01-01") {
    input_dt <- data.table(date_x = x, class_x = class(x)[1])
    switch(
      class(x)[1],
      "Date" = {
        input_dt[, result_x := date_x]
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
        input_dt[, date_x := as.chr(date_x)]
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
      "POSIXct" = {
        input_dt[, date_x := as.chr(date_x)]
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
      "POSIXt" = {
        input_dt[, date_x := as.chr(date_x)]
        input_dt[, result_x :=
                   vapply(
                     date_x,
                     FUN = .date_convert,
                     FUN.VALUE = as.Date(NA),
                     format,
                     origin,
                     USE.NAMES = FALSE
                   )]
      }
    )
    return(input_dt[, as.Date(x = result_x, origin = origin)])
}

# R checker CMD appeasement
span_start <- span_end <- span_label <- span_type <- NULL

#' @rdname date_functions
#' @export
date_expansion <- function(start_date, end_date,
                           types = c("qtr", "mon", "fy")) {
  # months ---
  mons <- seq(from = as.yearmon(date_convert(start_date)),
    to = as.yearmon(date_convert(end_date)), by = 1/12)
  dt_mons <- data.table(span_label = mons, span_type = "mon")
  dt_mons[, span_start := as.Date(mons, frac = 0)]
  dt_mons[, span_end := as.Date(mons, frac = 1)]
  dt_mons[, span_label := as.chr(span_label)]
  # fiscal quarters ---
  qtrs <- seq(from = as.yearqtr(date_convert(start_date))+.25,
              to = as.yearqtr(date_convert(end_date))+.25,
              by = 0.25)
  dt_qtrs <- data.table(span_label = qtrs, span_type = "qtr")
  dt_qtrs[, span_start := as.Date(qtrs-.25, frac = 0)]
  dt_qtrs[, span_end := as.Date(qtrs-.25, frac = 1)]
  # fiscal year ---
  fys <- unique(my_fy(as.Date(qtrs-.25, frac = 0)))
  dt_fys <- copy(dt_qtrs)
  dt_fys[, span_label := my_fy(span_start)]
  dt_fys[, span_start := min(span_start), by = span_label]
  dt_fys[, span_end := max(span_end), by = span_label]
  dt_fys[, span_type := "fy"]
  dt_fys <- unique(dt_fys)
  # fix qtrs - had to be this far down, dont move upward!
  dt_qtrs[, span_label := as.chr(span_label)]
  dt_combn <- rbindlist(list(dt_mons, dt_qtrs, dt_fys), use.names = TRUE)
  return(dt_combn[span_type %in% c(types)])
}
