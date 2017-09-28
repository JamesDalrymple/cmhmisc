#' @title WCCMH team functions
#' @description
#' cmh_recode recodes Washtenaw CMH team names to a standardized
#' team format.
#' recode_team_prog recodes Washtenaw CMH team/program names to a standardized
#' program format.
#' cmh_teams_f factors (ordered is an option) teams.
#' cmh_priority_dt assigns a priority to all of the main teams.
#'
#' @param x A character vector of team names. Will be coerced to character if
#' class(x) is factor.
#' @param missing_key What will happen if a recode_ function is supplied a value not
#' found in recode key. Default is 'non-CMH'. If missing_key is assigned to NULL,
#' an error will occur if any values are in x and not in recode_key.
#' @param levels The levels that will be assigned. Unspecified inputs result
#' in NA.
#' @param level_order The order of the levels. Defaults to NULL.
#'
#' @return recode_x functions: A vector of recoded team/program names.
#' cmh_teams_f A factored vector.
#' cmh_priority_dt A data.table object.
#'
#' @note need testing, consider adding an automatic "missing" assignment
#' with a warning message.
#'
#' @examples
#' cmh_recode("WSH - ACT")
#' cmh_recode(c("WSH - ACT", "DD Adult"))
#' @importFrom TBmisc as.chr
#' @importFrom data.table data.table :=
#'
#' @name team_functions
NULL

#' @rdname team_functions
cmh_team_key <- list(
  ACT = c("ACT", "WSH - ACT"),
  DD = c("DD", "WSH - DD Adult", "DD Adult"),
  MI = c("WSH - MI - Adult", "WSH - ATO", "MI", "MI Adult"),
  Child = c("Child", "WSH - Children's Services", "Children's Services"),
  "Child HB" = c("WSH - Children's Services - Home Based", "Child HB",
                 "Home Based", "Child Home Based"),
  PORT = c("WSH - PATH/PORT"),
  Access = c("Community Support and Treatment Services - CSTS",
             "CSTS", "WSH - Access/Engagement", "Access", "Access/Engagement",
             "Washtenaw County Community Mental Health"),
  OBRA = c("WSH - OBRA", "OBRA"),
  UM = c("WSH - Utilization Management", "UM"),
  `non-CMH` = c("non-CMH", "Non-CMH", "WSH - MH Court", "WSH - ICSS team",
             "WSH - Sobriety Court", "Crisis Residential Services")
)

#' @rdname team_functions
cmh_program_key <- list(
  DD = c("DD", "WSH - DD Adult", "DD Adult"),
  MI = c("ACT", "WSH - ACT", "WSH - MI - Adult", "WSH - ATO", "MI", "MI Adult"),
  `Y&F` = c("Child", "WSH - Children's Services", "Children's Services",
            "WSH - Children's Services - Home Based", "Child HB", "Home Based"),
  PORT = c("WSH - PATH/PORT"),
  Access = c("Community Support and Treatment Services - CSTS",
             "CSTS", "WSH - Access/Engagement", "Access", "Access/Engagement",
             "Washtenaw County Community Mental Health"),
  OBRA = c("WSH - OBRA", "OBRA"),
  UM = c("WSH - Utilization Management", "UM"),
  `non-CMH` = c("non-CMH", "Non-CMH", "WSH - MH Court",
             "WSH - Sobriety Court", "Crisis Residential Services")
)

#' @rdname team_functions
#' @export
cmh_recode <- function(x, missing_key = "non-CMH") {
  if (class(x) == "factor") x <- as.chr(x)
  if (any(is.na(x))) x[is.na(x)] <- missing_key
  recode_key <- cmh_team_key
  unknown <- setdiff(x, unlist(recode_key, use.names = FALSE))
  if (length(unknown) > 0) {
    recode_key$unknown <- unknown
  }
  recode_string(x = x, recode_key = recode_key)
}

#' @rdname team_functions
#' @export
recode_team_prog <- function(x, missing_key = "non-CMH") {
  if (class(x) == "factor") x <- as.chr(x)
  if (any(is.na(x))) x[is.na(x)] <- missing_key
  recode_key <- cmh_program_key
  unknown <- setdiff(x, unlist(recode_key, use.names = FALSE))
  recode_key$unknown <- unknown
  recode_string(x, recode_key = recode_key)
}

#' @rdname team_functions
#' @export
cmh_teams_f <- function(x,
                        levels = c("ACT", "DD", "MI", "Child HB", "Child"),
                        level_order = NULL) {
  if (missing(level_order) || is.null(level_order)) {
    level_order <- FALSE
  } else {
    level_order <- is.ordered(x)
  }
  result <- factor(
    x, levels, labels = levels,
    exclude = setdiff(x = x, levels), ordered = level_order
  )
  return(result)
}

#' @rdname team_functions
#' @export
cmh_priority_dt <-
  data.table(team = c("OBRA", "DD", "ACT", "MI", "Child HB", "Child",
                      "PORT", "UM", "Access", "non-CMH"),
             priority = 1:10)
