#' @title Money Functions
#' @name money
NULL
#' @description Functions for dealing with dollar formats imported from
#' outside of R. Removes '$' symbol and ',' from dollar strings and
#' converts to numeric. Changes numbers inside of paretheses to negative.
#'
#' @usage money_rm(x)
#' @usage money_add(x, sig_fig)
#' @param x an input vector: character for money_rm, numeric for money_add
#' @param sig_fig Number of significant figures; must be coerceable to
#' class integer and be greater than or equal to 0.
#'
#' @examples
#' money_string <- c("$1.04", "$2.24")
#' (no_money_string <- money_rm(money_string))
#' money_add(no_money_string)
#' @return A numeric vector.
#'
#' @note Other functions for money will go in this file.
#'
#' @importFrom data.table data.table setkey
#' @importFrom EquaPac as.num as.int
#' @importFrom stringi stri_c
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
  result = as.num(result)
  return(result)
}


f <- type <- NULL # RMD checker appeasement
#' @usage NULL
#' @rdname money
money_add_one <- function(x, sig_fig = sig_fig) {
  sig_fig <- as.int(sig_fig)
  if (class(sig_fig) != "integer") {
    p_stop("sig_fig is not of class integer, but is class", class(sig_fig))
  }
  # check input x
  switch(class(x),
         "numeric" = NULL,
         "integer" = {x <- as.num(x)},
         "factor" = {
           x <- as.num(x)
           money_add(x)},
         p_stop("money_add does not allow class", class(x), "!"))
  x_type <- cut(x,
              breaks = c(0, 1e3, 1e6, 1e9, 1e12, 1e15, Inf),
              labels = c("none", "K", "M", "B", "T", "scientific"),
              include.lowest = TRUE, right = FALSE)
  dt_call <- data.table(
    type = c("none", "K", "M", "B", "T", "scientic"),
    f = c(
      quote(stringi::stri_c("$", signif(x, digits = sig_fig))),
      quote(stringi::stri_c("$", signif(x / 1e3, digits = sig_fig), "K")),
      quote(stringi::stri_c("$", signif(x / 1e6, digits = sig_fig), "M")),
      quote(stringi::stri_c("$", signif(x / 1e9, digits = sig_fig), "B")),
      quote(stringi::stri_c("$", signif(x / 1e12, digits = sig_fig), "T")),
      quote(format(x, scientific = TRUE, digits = sig_fig))))
  setkey(dt_call, type)
  eval(dt_call[as.character(x_type), f][[1]])

}

# money_add_one <- function(x, signif) {
#   # if missing values exist, replace result with missing values
#   if (is.na(x)) {
#     money_lab = NA_character_} else
#     # millions
#     if (x > 1e6) {
#       money_lab = paste("$", round(x / 1e6, 2), "M", sep = "")} else
#       # thousands
#       if (x < 1e6) {
#         money_lab = paste("$", round(x / 1e3, 0), "K", sep = "")} else
#         # less than 1000
#         if (x < 1e3) {
#           money_lab =  paste("$", x, sep = "")
#         }
#   return(money_lab)
# }


#' @export
#' @rdname money
money_add <- function(x, sig_fig = 3) sapply(x, money_add_one, sig_fig)
