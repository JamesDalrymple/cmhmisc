#' Multiple gsub Replacement
#'
#' @description Multiple gsub replacement: Replaces the need for multiple,
#' consecutive gsub functions. If you have more than once replacement rule 
#' then this convenience function is appropriate to use.
#'
#' @param x a character vector where matches are sought, or an object which can
#' be coerced by as.character to a character vector. Long vectors are
#' supported.
#' @param replace_list A list of at least length one where each element is of
#' length 2. Each two element vector has the 'old' pattern and the 'new'
#' replacement which are passed to gsub via Reduce.
#'
#' @return a character vector of the same length and with the same attributes
#' as x (after possible coercion to character). Elements of character vectors x
#' which are not substituted will be returned unchanged. See ?gsub for further
#' details.
#'
#' @note Code idea taken from:
#' http://stackoverflow.com/questions/15253954/replace-multiple-arguments-with-gsub
#'
#' @examples
#' ex_string = 'Here are the two dates: @startdate, @enddate'
#' ex_list = list(c('@startdate', '10/1/2010'), c('@enddate', '9/30/2012'))
#' mgsub(x = ex_string, replace_list = ex_list)
#'
#' @name mgsub
NULL

# R CMD checker appeasement

#' @rdname mgsub
#' @export
mgsub <- function(x, replace_list){
  if (class(x) != "character") {stop("'x' must be class character.")}
  if (!is.list(replace_list)) {
    stop("'replace_list' is not a list.")
  }
  l_len <- lapply(replace_list, length)
  if (length(l_len) < 1) {
    stop("length of 'replace_list' must be at least one")
  }
  if (any(l_len != 2)) {
    stop("replace_list' is not a list of two vectors.")
  }
  gsub2 <- function(l, x){
    do.call(what = 'gsub', args = list(x = x,
      pattern = l[1], replacement = l[2]))
  }
  Reduce(f = gsub2, x = replace_list, init = x, right = TRUE)
}