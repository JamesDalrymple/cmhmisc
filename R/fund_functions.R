#' @title WCCMH fund functions
#' @description recode_fund recodes fund character vectors.
#'
#' @param x A character vector of fund names. Will be coerced to character if
#' class(x) is factor.
#'
#' @return A vector of recoded fund names.
#'
#' @note more coming soon...
#'
#' @examples
#' \dontrun{
#' recode_fund(c("MIChild", "Medicaid"))
#' }
#'
#' @name fund_functions
NULL

#' @rdname fund_functions
recode_fund_key <- list(
  Medicaid = c("Medicaid - State Plan",
               "Medicaid - b3",
               "Medicaid - Hab Sup Waiver",
               "Medicaid - Acute Services",
               "Medicaid - Partial Services",
               "MIChild",
               "Medicaid"),
  "General Fund" = c("General Fund - Partial Services", "GF",
                     "General Fund"),
  "Healthy Michigan Plan" = c("HMP", "HMP-Acute Services",
                              "Healthy Michigan Plan"),
  ABW = c("Adult Benefit Waiver", "ABW", "ABW-Acute Services"),
  "Child Waiver" = "Child Waiver",
  "SED Waiver" = "SED Waiver",
  "HAB Waiver" = "HAB Waiver")

#' @rdname fund_functions
#' @export
recode_fund <- function(x) {
  unknown <- setdiff(x, unlist(recode_fund_key, use.names = FALSE))
  recode_fund_key$unknown <- unknown
  recode_string(x, recode_key = recode_fund_key)
}
