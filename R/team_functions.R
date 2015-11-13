#' @title WCCMH team functions
#' @description cmh_recode recodes Washtenaw CMH team names to a standardized
#' format.
#'
#' @param x A character vector of team names. Will be coerced to character if
#' class(x) is factor.
#' @param levels The levels that will be assigned. Unspecified inputs result
#' in NA.
#' @param level_order The order of the levels. Defaults to NULL.
#'
#' @return A vector of recoded team names.
#'
#' @note more coming soon...
#'
#' @examples
#' \dontrun{
#' cmh_recode("WSH - ACT")
#' cmh_recode(c("WSH - ACT", "DD Adult"))
#' }
#' @importFrom EquaPac recode_string as.chr
#' @importFrom data.table data.table :=
#'
#' @name team_functions
NULL

#' @usage NULL
#' @rdname team_functions
cmh_team_key <- list(
  ACT = c("ACT", "WSH - ACT"),
  DD = c("DD", "WSH - DD Adult", "DD Adult"),
  MI = c("WSH - MI - Adult", "WSH - ATO", "MI", "MI Adult"),
  Child = c("Child", "WSH - Children's Services", "Children's Services"),
  "Child HB" = c("WSH - Children's Services - Home Based", "Child HB",
                 "Home Based"),
  PORT = c("WSH - PATH/PORT"),
  Access = c("Community Support and Treatment Services - CSTS",
             "CSTS", "WSH - Access/Engagement", "Access",
             "Washtenaw County Community Mental Health"),
  OBRA = c("WSH - OBRA", "OBRA"),
  UM = c("WSH - Utilization Management", "UM"),
  `non-CMH` = c("non-CMH", "Non-CMH", "WSH - MH Court",
             "WSH - Sobriety Court", "Crisis Residential Services")
)

#' @usage NULL
#' @rdname team_functions
cmh_program_key <- list(
  DD = c("DD", "WSH - DD Adult", "DD Adult"),
  MI = c("ACT", "WSH - ACT", "WSH - MI - Adult", "WSH - ATO", "MI", "MI Adult"),
  `Y&F` = c("Child", "WSH - Children's Services", "Children's Services",
            "WSH - Children's Services - Home Based", "Child HB", "Home Based"),
  PORT = c("WSH - PATH/PORT"),
  Access = c("Community Support and Treatment Services - CSTS",
             "CSTS", "WSH - Access/Engagement", "Access",
             "Washtenaw County Community Mental Health"),
  OBRA = c("WSH - OBRA", "OBRA"),
  UM = c("WSH - Utilization Management", "UM"),
  `non-CMH` = c("non-CMH", "Non-CMH", "WSH - MH Court",
             "WSH - Sobriety Court", "Crisis Residential Services")
)

#' @rdname team_functions
#' @export
cmh_recode <- function(x) {
  if (class(x) == "factor") x <- as.chr(x)
  recode_string(x = x,
                recode_key = cmh_team_key
                )
}

#' @rdname team_functions
#' @export
recode_team_prog <- function(x) {
  vapply(
    X = x, FUN = recode_string,
    recode_key = cmh_program_key,
    FUN.VALUE = as.character("one"),
    USE.NAMES = FALSE
  )
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
  data.table(team = c("OBRA", "ACT", "DD", "MI", "Child HB", "Child",
                      "PATH/PORT", "Access", "non-CMH"),
             priority = 1:9)
# cmh_priority_dt[, priority := .I]
