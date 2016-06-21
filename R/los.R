#' @title Length of Stay
#' @name los
NULL
#' @description los_clinic is for determining clinical, or actual, length of
#' stay for psychiatric hospitalizations.
#' paid_los is for determining the number of paid days a consumer had during a
#' psychiatric hospitalization.
#'
#' @param start 'Date' vector or a character vector that can be coerced to
#' 'Date' class. The start date of the psychiatric hospital admission.
#' Cannot be missing.
#' @param expiration 'Date' vector or a character vector that can be coerced
#' to 'Date' class. The expiration date of the psychiatric hospital admission.
#' Cannot be missing.
#' @param discharge 'Date' vector or a character vector that can be coerced
#' to 'Date' class. The discharge date of the psychiatric hospital admission.
#' If discharge is missing, use expiration date plus one day.
#'
#' @examples
#' \dontrun{
#' 1+1
#' }
#' @return A character vector.
#'
#' @note Consider making a new fiscal_year class and/or fiscal_quarter class.
#' Also consider making some methods (this needs further consideration).
#'
#' @importFrom data.table data.table :=
#' @importFrom EMiscDev as.num

los <- NULL # RMD checker appeasement
#' @export
#' @rdname los
los_clinic <- function(start, expiration, discharge) {
  dt_clinic <- data.table(start = start,
                          expiration = expiration,
                          discharge = discharge)
  # case when discharge is missing
  dt_clinic[is.na(discharge), discharge := expiration + 1]
  # calculaying clinical los
  dt_clinic[, los := discharge - start]
  return(dt_clinic[, as.num(los)])
}
