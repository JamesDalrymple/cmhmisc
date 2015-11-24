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
#' money_string <- c("$1.04", "$2.24", "$2,000", "$235,658")
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
      bquote(stringi::stri_c("$", signif(x, digits = .(sig_fig) ))),
      bquote(stringi::stri_c("$", signif(x / 1e3, digits = .(sig_fig)), "K")),
      bquote(stringi::stri_c("$", signif(x / 1e6, digits = .(sig_fig)), "M")),
      bquote(stringi::stri_c("$", signif(x / 1e9, digits = .(sig_fig)), "B")),
      bquote(stringi::stri_c("$", signif(x / 1e12, digits = .(sig_fig)), "T")),
      bquote(format(x, scientific = TRUE, digits = sig_fig))))
  eval(dt_call[type==as.character(x_type), f][[1]])
  # setkey(dt_call, type)
  # x <- (1.04,   1000.00, 123456.00)
  # dt_call[as.character(x_type), f][[1]](x[1])
  # dt_call[as.character(x_type), f][[2]](x[2])
  # dt_call2 <- dt_call[J(x_type)]
  # dt_call2[, x := x]
  # lapply(Map(list, dt_call2$f, x), FUN = function(x) as.call(x))
  # lapply(Map(list, dt_call2$f, x), FUN = function(x) eval(as.call(x)))
  # lapply(Map(list, dt_call2$f, x), FUN = function(x) as.call(x))[[1]]
  # lapply(Map(list, dt_call2$f, x), FUN = function(x) eval(as.call(x)))
  # Reduce(f = function(x) c, dt_call[as.character(x_type), f], 2)
}
#' @export
#' @rdname money
money_add <- function(x, sig_fig = 3) sapply(x, money_add_one, sig_fig)
