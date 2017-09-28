#' @title String recoding
#'
#' @description This function is an optimized version of recode. It is designed
#'   to take a list, or a named vector as the recode object.
#'
#' @param x The input takes a vector of one to many strings, and is optimized
#' to run without recursion using for loops or apply loops.
#'
#' @param recode_key A list of named character vectors or named character
#'   vectors. Adding 'else' into the named list will recode all unspecified
#'   values to the else value.
#'
#' @details This function uses data.table and an optimized version of
#'   \code{melt} from reshape2 (via data.table) in order to provide speedy
#'   string replacement. Primarily designed for use in data.table, but works on
#'   stand-alone vectors as well.
#'
#' @return A recoded character vector. If  the input was class factor,
#' the output will be converted to class character.
#'
#' @note May actually be slower for small datasets, but speed does not
#' matter in such cases. Refactoring could be an option if users report
#' a desire for such an option.
#' @section TODO:
#' \itemize{
#'    \item This needs a redo. I am having a lot of issues with it.
#'    \item Added a dropout for car::recode syntax
#' }
#' @examples
#' x <- c("A", "E", 7, "I", "11", "fifteen", 16)
#' recode_key = c(
#'   "'1'  = c('A','B','C')",
#'   "'DD' = c('D','E', 7)",
#'   "'2'  = c('H','I','J')",
#'   "'last_num' = 11:13",
#'   "'else' = NA"
#' )
#' recode_string(x, recode_key)
#' recode_key = c(
#'   "'1'  = c('A','B','C')"
#' )
#' recode_string(x, recode_key)
#' recode_key = c(
#'   "'1'  = c('A','B','C');DD = c('D','E', 7);'2' = c('H','I','J');
#'   last_num = 11:13;'else' = NA"
#' )
#' recode_string(x, recode_key)
#' recode_key = list(
#'   '1' = c("A", "B", "C"),
#'   DD  = c("D", "E", 7),
#'   '2' = c("H", "I", "J"),
#'   last_num = c(11, 12, 13),
#'   'else' = c("somethingelse")
#' )
#' recode_string(x, recode_key)
#' recode_key = list(
#'   '1' = c("A", "B", "C"),
#'   DD  = c("D", "E",  7),
#'   '2' = c("H", "I", "J"),
#'   last_num = c(11, 12, 13),
#'   'else' = c(NA)
#' )
#' recode_string(x, recode_key)
#' \dontrun{
### Doesn't work. Number as names must be quoted ###
#' recode_key = c(
#'   "1  = c('A','B','C)",
#'   "DD = c('D','E', 7)",
#'   "2 = c('H','I','J')",
#'   "last_num = 11:13",
#'   "else = NA")
#' }
#' @name recode_string
NULL

#' @importFrom data.table data.table melt
#' @rdname recode_string
recode_s <- function(x, recode_key) {
  L1<-value<-NULL # R CMD Checker Appeasement
  melt_key <- data.table(melt(recode_key), key = "value")
  melt_x <- melt_key[x]
  if ('else' %in% names(recode_key)) {
    to_cls <- class(melt_x[['L1']])
    fm_cls <- class(recode_key[['else']])
    rk_else <-
      if (to_cls != fm_cls) {
        if (to_cls == "character") {
          as.character(recode_key[['else']])
        } else {
          as.numeric(recode_key[['else']])
        }
      } else {
        recode_key[['else']]
      }
    melt_x[is.na(L1), L1 := rk_else]
  } else {
    melt_x[is.na(L1), L1 := value]
  }
  return(melt_x[, L1])
}

#' @rdname recode_string
#' @export
recode_string <- function(x, recode_key) {
  L1<-value<-NULL # R CMD Checker Appeasement
  if (typeof(recode_key) == 'list') {
    recode_key <- lapply(recode_key, as.character)
  } else if (typeof(recode_key) == 'character') {
    if (is.null(names(recode_key))) {
        rc_split <- unlist(strsplit(recode_key, split = ";"))
      text <- paste0("recode_key<-list(", paste0(rc_split, collapse = ","),")")
      eval(parse(text = text))
    }
    recode_key <- as.list(recode_key)
  }

  return(recode_s(x, recode_key))
}

#' @rdname recode_string
#' @export
str_rec <- recode_string

