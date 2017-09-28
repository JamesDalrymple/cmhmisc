#' @title WCCMH word functions
#' @description cap_word capitalizes the first letter of every
#' space-separated word.
#'
#' @param x A character vector which will be capitalized via sapply.
#' @return A vector of recoded fund names.
#'
#' @note more coming soon...
#'
#' @examples
#' cap_word(c("level", "one"))
#' cap_word(c("level one", "one", "this is a sentence."))
#' locus_word2num("Level One")
#' @importFrom data.table data.table := setkey .I
# #' @importClassesFrom english english
# #' @importFrom english
#' @importFrom TBmisc as.chr
#' @name word
NULL

#' @rdname word
single_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  s <- tolower(s)
  s <- paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
  return(s)
}

#' @rdname word
#' @export
cap_word <- function(x) {sapply(x, single_cap, USE.NAMES = FALSE)}

#' @rdname word
#' @export
locus_word2num <- function(x) {
  word <- num <- NULL # R checker appeasement
  word2num_dt <-
    data.table(word = cap_word(paste("level", as.chr(english::english(0:6)))))
  word2num_dt[, num := .I - 1]
  word2num_dt[word == "Level Six", num := 3] # per Kelly B. 5/7/2015
  result <- setkey(word2num_dt, word)[x, num]
  return(result)
}
