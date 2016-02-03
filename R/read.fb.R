#' @title FB file reader
#' @description Read in funding bucket files via data.table::fread and convert
#' them to the "fb" class. The header is stored as attributes. Applies
#' attrubutes based off of the file header information.
#'
#' @param file_path Path to fb .csv file.
#' @return A data.table object that is also class 'fb.'
#'
#' @note Could format the columns coming in as factors if that were desirable.
#'
#' @examples
#' \dontrun{
#' require(data.table)
#' folder_path  <- 'C:/Users/dalrymplej/Dropbox/EToPac/data-sets'
#' file_name <- 'fb contract 10_01_14_to_07_31_15 run 10_19_15.csv'
#' file_name <- 'fb direct 10_01_14_to_07_31_15 run 10_19_15.csv'
#' file_path <- file.path(folder_path, file_name)
#' my_fb_dt <- read.fb(file_path)
#' lapply(my_fb_dt, class)
#' attributes(my_fb_dt)
#' }
#' @importFrom EquaPac p_stop %nin%
#' @importFrom data.table data.table last := .SD fread

#' @name read.fb
NULL

#' @rdname read.fb
char_cols <-
  list(
    `PROVIDER NAME` = "character",
    `PROVIDER ID` = "integer",
    `CASE #` = "integer",
    `PRI PROCEDURE CODE` = "character",
    `SEC PROCECURE CODE` = c("logical", "character"),
    MOD = "character",
    `SERVICE DESC` = "character",
    `CATEGORY DESC` = "character",
    `UNIT TYPE` = "character",
    `FROM DATE` = "character",
    `THRU DATE` = "character",
    UNITS = "integer",
    DURATION = "integer",
    `ALLOWED AMOUNT` = "numeric",
    `CHARGED AMOUNT` = "integer",
    POPULATION = "character",
    `ADULT OR CHILD` = "character",
    AGE = "integer",
    `COV: MEDICAID` = "character",
    `COV: HAB WAIVER` = "character",
    `COV: CHILD WAIVER` = "character",
    `COV: SED WAIVER` = "character",
    `COV: ABW` = "character",
    `COV: HEALTHY MI PLAN` = "character",
    `COV: MI CHILD` = "character",
    `COV: AUTISM` = "character",
    `COV: COFR` = "character",
    `SRV: STATE PLAN` = "character",
    `SRV: ALT/B3` = "character",
    `SRV: HAB WAIVER` = "character",
    `SRV: EPSDT` = "character",
    `SRV: CHILD WAIVER` = "character",
    `SRV: GF ONLY` = "character",
    `BKT: GF` = "character",
    `BKT: STATE PLAN` = "character",
    `BKT: ALT/B3` = "character",
    `BKT: EPSDT` = "character",
    `BKT: HAB WAIVER` = "character",
    `BKT: ABW` = "character",
    `BKT: HEALTHY MI PLAN` = "character",
    `BKT: MICHILD` = "character",
    `BKT: CHILD WAIVER` = "character",
    `BKT: SED WAIVER` = "character",
    `BKT: AUTISM/MICHILD` = "character",
    `BKT: AUTISM/MEDICAID` = "character",
    `BKT: COFR` = "character",
    `STAFF NAME` = "character",
    `PLACE OF SERVICE` = "integer",
    `BATCH ID` = "integer",
    `CLAIM ID` = "integer",
    `CLAIM DETAIL ID` = "integer",
    SOURCE = "character")

#' @rdname read.fb
read_header <- function(file_path) {
  s <- 0
  DT_header <-
    read.table(file = file_path, header = FALSE,
               sep = ",", skip = s, nrows = 1)
  repeat {
    s <- s + 1
    add_line <- read.table(file = file_path, header = FALSE,
                           sep = ",", skip = s, nrows = 1)
    if (sum(is.na(add_line)) == length(add_line)) break
    DT_header <- rbind(DT_header, add_line)
  }
  if (s >= 8) p_stop("More than 7 rows in the header, something has changed in
                     fb header. Investigate now, alter read_header in
                     read.fb.R in 'package:wccmh' if necessary.")
  return(data.table(DT_header))
}

#' @rdname read.fb
#' @export
read.fb <- function(file_path) {
  V1 <- V2 <- col_values <- incoming_dt <- NULL # checker appeasment
  attr_DT <- read_header(file_path)
  DT <- fread(input = file_path, showProgress = FALSE)
  # check classes and column names
  check_col_dt <- data.table(col_names = names(char_cols),
             col_values = char_cols)
  check_col_dt[, incoming_dt := lapply(DT, class)]
  if (nrow(check_col_dt[incoming_dt %nin% col_values]) > 0) {
    p_stop("The file does not have FB structure, please examine classes of
            fread(file), file = ", file_path)
  }
  attr_DT[, c(names(attr_DT)) := lapply(.SD, function(x) gsub(":", "", x))]
  for (i in seq.int(nrow(attr_DT))) { # i=1
    attr(DT, attr_DT[i, V1]) <- as.character(attr_DT[i, V2])
  }
  class(DT) <- c(class(DT), 'fb')
  return(DT)
}
