#' @title Money Functions
#' @name money
NULL
#' @description Functions for dealing with dollar formats imported from
#' outside of R. Removes '$' symbol and ',' from dollar strings and
#' converts to numeric. Changes numbers inside of paretheses to negative.
#'
#' @usage money_rm(x)
#' @usage money_add(x)
#' @param x an input vector: character for money_rm, numeric for money_add
#'
#' @examples
#' money_string <- c("$1.04", "$2.24")
#' (no_money_string <- money_rm(money_string))
#' money_add(no_money_string)
#' @return A numeric vector.
#'
#' @note Other functions for money will go in this file.
#'
#' @export
#' @rdname money
# remove '$' symbol and ',' from dollar strings, and convert to numeric ----
money_rm <- function(x) {
  result = gsub(x = x,
                pattern = "[$,)]",
                replacement = "")
  result = gsub(x = result,
                pattern = "[(]",
                replacement = "-")
  result = as.numeric(result)
  return(result)
}

#' @usage NULL
#' @rdname money
# add '$' symbol and K, M as appropriate for ggplot graphing ----
money_add_one <- function(x) {
  # if missing values exist, replace result with missing values
  if (is.na(x)) {
    money_lab = NA_character_} else
    # millions
    if (x > 1e6) {
      money_lab = paste("$", round(x / 1e6, 2), "M", sep = "")} else
      # thousands
      if (x < 1e6) {
        money_lab = paste("$", round(x / 1e3, 0), "K", sep = "")} else
        # less than 1000
        if (x < 1e3) {
          money_lab =  paste("$", x, sep = "")
        }
  return(money_lab)
}

#' @export
#' @rdname money
money_add <- function(x) sapply(x, money_add_one)
