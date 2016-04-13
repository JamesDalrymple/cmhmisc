#' @title cut with point-by-point closure
#' @description a function to categorize data points based on distinct
#' non-overlapping intervals. This function is analagous to cut, except it is
#' for situations where all of the points do not share closure on the
#' right/left side of a given interval. If cut can be used, it should; cut is
#' faster in such situations.
#'
#' @param vec the vector to be categorized
#' @param guide_dt a data.table object with three required columns:
#'   see interval names (2 cols) and category (1 col). Other columns will not
#' be used
#' @param closure is the argument closed in Intervals_full, and needs to match
#' a vector or matrix of TRUE/FALSE options, see interval::Intervals_full for
#' more information.
#' @param category is the unique category in guide_dt per combination of
#' interval_names. Duplicates are not permitted.
#' @param interval_names are the start/end columns of the interval which is used to
#' categorize points.
#' @param type Can be 'R" or "Z" for real numbers and integers, respectively. Feeds
#' type in interval::Intervals_full.
#'
#' @return a vector of the same length as vec and of the categories found in
#' guide_dt (attributes will be preserved).
#'
#' @note Reino says there is a faster way. data.table::foverlaps is a
#' potential consideration.
#'
#' @examples
#' chol_guide <- data.table(init = c(0, 200, 240), end = c(200, 240, Inf),
#'                          cat = c("best", "borderline", "poor"))
#' test_chol_dt <- data.table(chol = sample(150:400, size = 20, replace = TRUE))
#' interval_cut(vec = test_chol_dt[, chol],
#'    guide_dt = chol_guide,
#'    closure = c(TRUE, FALSE,
#'                TRUE, TRUE,
#'                FALSE, FALSE),
#'    category = "cat",
#'    interval_names = c("init", "end"),
#'    type = "R")
#'
#' test_chol_dt[, chol_cat := interval_cut(vec = chol,
#'                                         guide_dt = chol_guide,
#'                                         closure = c(TRUE, FALSE,
#'                                                     TRUE, TRUE,
#'                                                     FALSE, FALSE),
#'                                         category = "cat",
#'                                         interval_names = c("init", "end"),
#'                                         type = "R")]
#' print(test_chol_dt)
#' vec = c(189, 300, 240, 201, 241, 89, 175, 199, 200)
#' guide_dt = chol_guide
#' closure = c(TRUE, FALSE,
#'             TRUE, TRUE,
#'             FALSE, FALSE)
#' category = "cat"
#' interval_names = c("init", "end")
#' type = "R"
#'
#'  interval_cut(vec, guide_dt, closure, category,
#'    interval_names, type = "R")
#'
#' @import data.table
#' @importFrom Hmisc Cs
#' @importFrom EquaPac p_stop setf
#' @importFrom intervals Intervals_full interval_overlap
#' @name interval_cut
NULL

# R CMD checker appeasement
value <- NULL

#' @export
#' @rdname interval_cut
interval_cut <- function(vec, guide_dt, closure, category,
                         interval_names, type = "R") {
  if (nrow(guide_dt[get(interval_names[2]) < get(interval_names[1])]) > 0) {
    p_stop("interval_names are out of order or some ends proceed starts")
  }
  setf(guide_dt, j = interval_names, value = as.numeric)
  stopifnot(guide_dt[, length(unique(get(category)))] == nrow(guide_dt),
            is.data.table(guide_dt))
  guide_int <-
    guide_dt[, Intervals_full(as.matrix(.SD),
                                closed = matrix(closure, byrow = TRUE, ncol = 2),
                                type = type), .SDcols = interval_names]
  idx_ovrlp <- interval_overlap(from = guide_int, to = vec)
  names(idx_ovrlp) <- guide_dt[, get(category)]
  attr(idx_ovrlp, "class") <- "list"
  m_df <- melt(idx_ovrlp, level = "cat") # melt.list from reshape2
  setorder(m_df, value)[["Lcat"]]
}

# testing purposes only -------------------------------------------------------
test <- if (FALSE) {
intervals_output <-
  structure(list(
    c(12L, 15L, 18L, 19L, 20L), c(11L, 13L),
    c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 14L, 16L, 17L)
  ), class = "AsIs")
names(intervals_output) <- letters[1:3]
# tsub(intervals_output, width = 1, FUN = as.character)
# attr(intervals_output, "class") <- "list"
# melt(intervals_output, level = "cat")[["Lcat"]]
}
