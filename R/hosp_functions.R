#' @title WCCMH hospital functions
#' @description
#' recode_state_hosp recodes state of Michigan psychiatric hospitals where
#' consumers are staying long-term to a standardized format.
#'
#' @param x A character vector of state hospital names. Will be coerced to
#' character if class(x) is factor.
#'
#' @return recode_state_hosp: Returns a vector of recoded state hospital names.
#'
#' @examples
#' recode_hosp(c("Hawthorn Center -State Facility", "Kalamazoo Psych"))
#' @importFrom EmiscDev recode_string
#'
#' @name hosp_functions
NULL

#' @rdname hosp_functions
hosp_recode_l <- list(
  Hawthorn = c("Hawthorn", "Hawthorn Center",
               "Hawthorn Center -State Facility"),
  `Kalamazoo Psych` =
    c("Kalamazoo Psychiatric Hospital & Pheasant Ridge",
      "Kalamazoo Psych/Pheasant R", "Kalamazoo Psych",
      "Kalamazoo Psychiatric"),
  `Caro` = c("Caro", "Caro Center-State Facility", "Caro Center"),
  `Walter Reuther` = c("Walter Reuther", "Walter Reuther Hospital"),
  `Forensic` = c("Forensic", "Forensic Center"))

#' @rdname hosp_functions
#' @export
recode_hosp <- function(x) {
  recode_string(x, recode_key = hosp_recode_l)
}
