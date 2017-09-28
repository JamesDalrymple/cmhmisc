#' @title "Not in" for inverse value matching
#'
#' @description Like \%in\%, this function compares two vectors to see if any of
#'   the items in the left vector are not present in the right vectors,
#'   essentially isolating what is missing using a logicical vector.
#'
#' @inheritParams Hmisc::`%nin%`
#' @return A logical Vector of values from x that do not have an associated
#'   match in y.
#' @examples
#' which(c(1:5,1:5,11) %nin% 1:10)
#' @section TODO:
#' \itemize{
#'    \item Check to see if direct imports need to be documented like this.
#' }
#' @export
"%nin%" <- Hmisc::`%nin%`

