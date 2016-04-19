#' Interval Closure Classification
#'
#' @description interval categorization capable of handling each interval's
#' boundary closures independently. This is designed to be used when base::cut
#' does not fully meet your needs, and is a wrapper for cut when breaks are
#' not named (see breaks).
#'
#' @param x A numeric or integer vector to be categorized. Factors are
#' coerced to integers.
#' @param breaks Closure is determined by 'i' and 'e' for include and exclude,
#' respectively. If an interval has breaks c(i, i), the start of the next
#' interval is going to be (e, ?) since any other choice would create a hole
#' at that point in the range. If breaks are not named, the function defaults
#' to base::cut. Breaks that leave gaps will result in NA values. If you name
#' breaks with 'i' and 'e', do so consistently or you will be redirected to
#' base::cut with a warning.
#' @param labels The labels for the breaks. Order and length of labels must be
#' consistent with breaks. Default null causes labels to be based on break
#' intervals.
#' @param dig_lab the desired number of digits after the decimal point
#' (format = "f") or significant digits (format = "g", = "e" or = "fg").
#' Default: 2 for integer, 4 for real numbers. If less than 0, the C default of
#' 6 digits is used. If specified as more than 50, 50 will be used with a
#' warning unless format = "f" where it is limited to typically 324.
#' (Not more than 15-21 digits need be accurate, depending on the OS and
#' compiler used. This limit is just a precaution against segfaults in the
#' underlying C runtime.)
#' @param ordered_result single logical value; should result be order?
#' default FALSE.
#' @param env the environment of ???
#' @param fac single logical value. Should returned vector be of class
#' factor? Default TRUE.
#' @param allow_gap single logical value. Should a local gap be
#' allowed (break name 'ee')? Default FALSE. Not yet implemented.
#'
#' @return a vector of the same length as vec and of the categories found in
#' labels, or breaks if labels is null.
#'
#' @note (1) It is not possible to define a gap where a particular interval is
#' not categorized, unless the gap is at the start or end of the breaks.
#' (2) http://stackoverflow.com/questions/2769510/numeric-comparison-difficulty-in-r
#' Please be aware that this function is not designed for categorizing
#' numeric intervals that require sensitivity past what base R handles
#' (around 1e-10).
#'
#' @examples
#' \dontrun{ # maybe this will work later, not now
#' chol_guide <- data.table(
#'   init = c(0, 200, 240),
#'   end = c(200, 240, Inf),
#'   cat = c("best", "borderline", "poor"))
#' }
#' d <- data.table(
#'   chol = sample(150:400, size = 1e7, replace = TRUE))
#' breaks  <-  c(i = 0, e = 200, i = 240, e = Inf)
#' d[, cat := closure_cut(chol, breaks)]
#' d
#'
#' # gaps will generate NA values, consistent with cut
#' closure_cut(1, breaks = c(10, 20)) # base::cut applied
#' closure_cut(x=1, breaks = c(i = 10, i = 20))
#'
#' \dontrun{
#' # BAD - will error
#' # error: too many labels
#' closure_cut(1, breaks = c(i=0, i=0, i = 1), labels = c("zero", "one", "two"))
#' two errors: break 4 misnamed, break 5 not named
#' closure_cut(x = 1, breaks = c(i=1,e = 2, i = 3, eerie = 4, 5))
#' }
#'
#' ### create an intentional gap ####'
#' \dontrun{
#' # BAD - if repeat x input (i.e. chol), will break
#' d[, chol, by = .(closure_cut(chol, breaks))]
#' #' # BAD - this wil give wrong answers... dont force a gap!
#' closure_cut(10, breaks = c(e=1, e=10-1e-10, i=10+1e-10, e=20),
#'    allow_gap = TRUE)
#' # GOOD - if you really want a gap, fix it like this:
#' test_gap <- closure_cut(c(1, 10, 15, 20),
#'  breaks = c(i=1, e=10-1e-10, i=10+1e-10, e=20),
#'  labels = c("a", "gap", "b"), allow_gap = TRUE)
#' print(test_gap)
#' test_gap[test_gap == "gap"] <- NA
#' print(test_gap)
#' }
#'
#' @importFrom EquaPac is.l1 is.l0
#' @importFrom data.table data.table foverlaps shift setkey
#' @importFrom Hmisc Cs
#'
#' @name closure_cut
NULL

# R CMD checker appeasement
x1 <- x2 <- NULL

# closure_cut(x, breaks, labels, dig_lab, ordered_result = TRUE, fac = TRUE, allow_gap = TRUE)

#' @rdname closure_cut
#' @export
closure_cut <- function(x, breaks, labels = NULL, dig_lab = 3L,
  ordered_result = FALSE, # ..., verbose = getOption("verbose"),
  env = parent.frame(), fac = TRUE, allow_gap = FALSE) {
  # need to generate ERROR if user defined interval overlaps

# LATER ...  allow various inputs
#   switch(class(breaks)[1],
#          "character",
#          "list" = {breaks <- list("ii" = c(1, 2, 3, 4, 5), "ei" = c(5, 10, 15, 20))
#               setNames(unlist(breaks, use.names=FALSE),rep(names(breaks), lengths(breaks))},
#          "data.table" = {  break_dt <- data.table(left_pt = c(1, 10, 20, 30),
#   right_pt = c(10, 20, 30, 100),
#   l_type = c("i", "e", "i", "i"),
#   r_type = c("i", "e", "e", "e"))})

  # Example while building cut_closure()
  # x = c(1, 10, 20, 30, 40, 49, 50)
  # breaks <- c(inc = 1, ei = 10, ie = 20, ee = 30, ie = 40, excl = 50)
  # labels <- c(letters[1:5])
  # allow_gap = TRUE
  # or  [1, 10), [10, 20], (20, 30), (30, 40], (40, 50)

  # prepare base::cut in case it needs to be used
  cut_call <- match.call()
  cut_call[1] <- call('cut')
  if (is.integer(x)) x <- as.numeric(x)
  if (!is.numeric(x)) stop("'x' must be numeric")
  # if breaks not named, sending to cut
  if (!is.named(breaks)) {
    return(eval(cut_call, envir = env))
  }
  breaks <- sort(breaks)
  # overlapping boundary points prohibited
  if (any(breaks <= shift(breaks), na.rm = TRUE)) {
    stop("breaks found with overlap, categories must be uniquely defined!")
  }

  b_names <- names(breaks)
  b_names <- gsub(pattern = "[^?ie]", x = names(breaks),
                  replacement = "", perl = TRUE)
  b_names <- substr(b_names, 1, 2)
  if (!is.logical(ordered_result) | !is.l1(ordered_result)) {
    stop("ordered_result must be a logical vector of length 1")
  }
  if (!is.logical(allow_gap) | !is.l1(allow_gap)) {
    stop("allow_gap must be a logical vector of length 1")
  }
  if (!is.logical(fac) | !is.l1(fac)) {
    stop("fac must be a logical vector of length 1")
  }
  # error checking global boundary points---
  if (!identical(nchar(b_names)[1], 1L)) {
    stop("starting point must be named either 'i' or 'e'")
  }
  if (!identical(nchar(b_names[length(b_names)]), 1L)) {
    stop("ending point must be named either 'i' or 'e'")
  }
  b_names[-c(1, length(b_names))] <-
    gsub(pattern = "^i$", x = b_names[-c(1, length(b_names))], replacement = "ie")
  b_names[-c(1, length(b_names))] <-
    gsub(pattern = "^e$", x = b_names[-c(1, length(b_names))], replacement = "ei")

  if (any(names(breaks) == "ii")) {
    warning("breaks labeled 'ii' are relabeled 'ie' to prevent category overlap")
    breaks[breaks == "ii"] <- "ie"
  }
  if (!allow_gap && any(names(breaks) == "ee")) {
    stop("a break is labeled 'ee', causing a gap when allow_gap = FALSE")
  }

    # we dont want any names except i or e
    if (!is.l0(grep("i|e", b_names, ignore.case = FALSE, invert = TRUE))) {
      warning("breaks were named, but every point must be labeled 'i' or 'e'.
             x sent to base::cut since names(breaks) were ambiguous.")
      return(eval(cut_call, envir = env))
    }
  b_nums <- c(breaks[1], rep(breaks[-c(1, length(breaks))], each = 2),
              last(breaks))
  b_num_names <- paste(b_names, collapse = "")
  b_even_names <- even_char(b_num_names)
  b_odd_names <- odd_char(b_num_names)
  b_even_names <- gsub(x = b_even_names, pattern = "i", replacement = "]")
  b_odd_names <- gsub(x = b_odd_names, pattern = "i", replacement = "[")
  b_even_names <- gsub(x = b_even_names, pattern = "e", replacement = ")")
  b_odd_names <- gsub(x = b_odd_names, pattern = "e", replacement = "(")
  b_odd_nums <- odd_obj(b_nums)
  b_even_nums <- even_obj(b_nums)
  interval_label <- paste0(b_odd_names, b_odd_nums, ",", b_even_nums, b_even_names)

  # TODO: need to add pretty print breaks
  # breaks <- sapply(breaks, prettyNum, digits = dig.lab)

  # .bincode needs well defined breaks
  interval_breaks <- strsplit(interval_label, ",")
  interval_breaks <-
    rapply(interval_breaks, f = function(x) gsub(pattern = "\\[|\\]", x = x, replacement = ""))
  epsilon <- 1e-9 # 4e-15
  add_eps <- grep(x = interval_breaks, pattern = "[(]")
  sub_eps <- grep(x = interval_breaks, pattern = "[)]")

  interval_breaks[add_eps] <- paste(gsub(x = interval_breaks[add_eps],
    pattern = "[(]", replacement = ""), "+", epsilon)
  interval_breaks[sub_eps] <- paste("-", epsilon, "+",
      gsub(x = interval_breaks[sub_eps], pattern = "[)]", replacement = ""))
  # parse breaks
  interval_breaks <-
    vapply(
      interval_breaks, FUN = function(x) {
        eval(parse(text = x))
      }, FUN.VALUE = 1.1, USE.NAMES = FALSE)

  interval_breaks[is.infinite(interval_breaks)] <- 1e+308
  # heart of code ... Rcpp would give a boost, dont know how much
  int_bk_dt <-
    data.table(matrix(
      interval_breaks, ncol = 2, byrow = TRUE,
      dimnames = list(NULL, Cs(start, end))
    ))
  if (is.null(labels) && is.character(labels)) {
    int_bk_dt[, labels := interval_label]
  } else {
    int_bk_dt[, labels := interval_label]
  }

  setkey(int_bk_dt, start, end)
  int_bk_dt <- foverlaps(data.table(x1 = x, x2 = x),
            int_bk_dt,
            by.x = Cs(x1, x2),
            by.y = Cs(start, end))
  int_bk_dt[, labels]
}
