#' @title Money Functions

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
#' money_string <- c("$1.04", "$2.24", "$2,000", "$235,658")
#' (no_money_string <- money_rm(money_string))
#' money_add(no_money_string)
#' money_add(x = c(1.04, 2.56e6, 3.545e9, -1.678e14))
#' @return A numeric vector.
#'
#' @note Other functions for money will go in this file.
#'
#' @importFrom data.table data.table :=
#' @importFrom TBmisc as.num as.int
#' @name money
NULL
#' @export
#' @rdname money
# remove '$' symbol and ',' from dollar strings, and convert to numeric
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

input_x <- eval_x <- sign_x <- NULL # RMD checker appeasement
#' @rdname money
#' @export
money_add <- function(x, sig_fig = 3) {
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
  dt_call <- data.table(input_x = abs(x), eval_x = NA_character_, sign_x = sign(x))
  dt_call[input_x >= 0 & input_x < 1e3,
    eval_x := paste0("$", signif(input_x, digits = sig_fig ))]
  dt_call[input_x >= 1e3 & input_x < 1e6,
          eval_x := paste0("$", signif(input_x/1e3, digits = sig_fig ), "K")]
  dt_call[input_x >= 1e6 & input_x < 1e9,
          eval_x := paste0("$", signif(input_x/1e6, digits = sig_fig ), "M")]
  dt_call[input_x >= 1e9 & input_x < 1e12,
          eval_x := paste0("$", signif(input_x/1e9, digits = sig_fig ), "B")]
  dt_call[input_x >= 1e12 & input_x < 1e15,
          eval_x := paste0("$", signif(input_x/1e12, digits = sig_fig ), "T")]
  dt_call[sign_x == -1,
          eval_x := gsub(x = eval_x, pattern = "$",
                         replacement = "-$", fixed = TRUE)]
  return(dt_call[, eval_x])
}
