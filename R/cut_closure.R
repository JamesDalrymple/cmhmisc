#' Interval Closure Classification
#'
#' @description interval categorization capable of handling each interval's
#' boundary closures independently. This is designed to be used when base::cut
#' does not fully meet your needs, and is a wrapper for cut when breaks are
#' not named (see breaks). Unlike cut, gaps are permissible in consecutive
#' intervals, but will generate NAs.
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
#' @param label_vec The labels for the breaks. Order and length of labels must be
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
#' @param env the environment of base::cut, if that is triggered.
#' Default parent.frame().
#'
#' @return a factored vector; x is classified based on user inputs.
#'
#' @note Very large and very small numbers (less than 1e-12, greater than
#' 1e16) may not work, use at your own risk, or, transform your data with a
#' shift parameter to a safe input range.
#'
#' @examples
#' \dontshow{ # maybe this will work later, not now
#' chol_guide <- data.table(
#'   init = c(0, 200, 240),
#'   end = c(200, 240, Inf),
#'   cat = c("best", "borderline", "poor"))
#' }
#' # Example boundary vector
#' boundary_test_vec <- c(-1, 0, 199, 200, 200.1, 239, 240, 240.1, 255, 500)
#'
#' #' # Example 1 - ordered vector with custom label
#' closure_cut(x = boundary_test_vec,
#'  breaks = c(i=0, ei = 200, ie = 240, e = Inf),
#'  ordered_result = TRUE, label_vec = Cs(best, borderline, poor))
#'
#' # Example 2 - vector with no order nor custom label
#' closure_cut(x = boundary_test_vec,
#'  breaks = c(i=0, ei = 200, ie = 240, e = Inf),
#'  ordered_result = FALSE, label_vec = NULL)
#'
#' # Example boundary data.table (data.frame should be similar)
#' d <- data.table(
#'   chol = sample(150:400, size = 1e3, replace = TRUE))
#' breaks  <-  c(i = 0, e = 200, i = 240, e = Inf)
#'
#' # Example 3 - data.table
#' d[, cat := closure_cut(chol, breaks)]
#' d[between(chol, 0, 200-1e-3), unique(cat)]
#' d[between(chol, 200, 240), unique(cat)]
#' d[between(chol, 240+1e-3, 9e10), unique(cat)]
#'
#' # Example 4 - single point with ordered, custom labels
#' closure_cut(x = 200, breaks = c(i=0, ei = 200, ie = 240, e = Inf),
#' ordered_result = TRUE, label_vec = Cs(best, borderline, poor))
#'
#' # Example 5 - single point without order and custom labels
#' closure_cut(x = 200, breaks = c(i=0, ei = 200, ie = 240, e = Inf),
#'  ordered_result = FALSE, label_vec = NULL)
#'
#' # Example 6 - single point with ordered, default labels
#' closure_cut(x = 200, breaks = c(i=0, ei = 200, ie = 240, e = Inf),
#'    ordered_result = TRUE, label_vec = NULL)
#'
#' # Example 7: data.table most recommended way to use
#' d <- data.table(
#'   chol = sample(150:400, size = 1e3, replace = TRUE))
#' # breaks  <-  c(i = 0, ie = 200, ie = 240, e = Inf)
#' breaks  <-  c(i = 0, e = 200, i = 240, e = Inf)
#' d[, cat := closure_cut(chol, breaks)]
#' print(d)
#'
#' # gaps will generate NA values, consistent with cut
#' closure_cut(1, breaks = c(10, 20)) # base::cut applied
#' closure_cut(x=1, breaks = c(i = 10, i = 20))
#'
#' # Not recommended, but possible...
#' d[, chol, by = .(closure_cut(chol, breaks))]
#'
#' \dontrun{
#' # BAD - will error
#' # error: too many labels
#' closure_cut(1, breaks = c(i=0, i=0, i = 1), label_vec = c("zero", "one", "two"))
#' two errors: break 4 misnamed, break 5 not named
#' closure_cut(x = 1, breaks = c(i=1,e = 2, i = 3, eerie = 4, 5))
#'
#' # create an intentional gap
#' # You can force a gap, but we do not support/advise. Picking your boundary
#' # points depends on the smallest step between points in your dataset.
#' # works but potentially dangerous - make your own safeguards!
#' closure_cut(10, breaks = c(e=1, e=10-1e-10, i=10+1e-10, e=20))
#'
#' # if you really want a gap, this approach is safer:
#' test_gap <- closure_cut(c(1, 10, 15, 20),
#'  breaks = c(i=1, e=10-1e-10, i=10+1e-10, e=20),
#'  label_vec = c("a", "gap", "b"))
#' print(test_gap)
#' test_gap[test_gap == "gap"] <- NA
#' print(test_gap)
#' }
#'
#' \dontshow{
#' \dontrun{
#' x_data  = readRDS("C:/Users/dalrymplej/Documents/a1c_data.RDS")
#' x = x_data[[1]]
#' x_data[[2]]
#' x1 = c(-1, 0, 199, 200, 200.1, 239, 240, 240.1, 255, 500)
#' x2 = c(199.9, 200, 201)
#' x3 = c(0,1.1,6,5.5,3.8)
#' breaks1 = c(i=-Inf, ei = 200, ie = 240, e = Inf)
#' breaks2 = c(i=0, ei = 6, ei = 6.5, e = Inf)
#' dig_lab = 3L
#' ordered_result = FALSE
#' env = parent.frame()
#' label_vec = NULL
#' closure_cut(x = x, breaks = breaks2, label_vec = NULL, dig_lab, ordered_result, env)
#' x = 200
#' breaks = c(i=0, ei = 200, ie = 240, e = Inf)
#' dig_lab = 3L
#' ordered_result = FALSE
#' env = parent.frame()
#' closure_cut(x, breaks, label_vec = NULL, dig_lab, ordered_result, env)
#' }}
#'
#' @importFrom data.table data.table foverlaps shift setkey
#' @importFrom Hmisc Cs
#' @importFrom TBmisc is.named is.l1 is.l0 is.anyT is.F is.named is.T
#'
#' @name closure_cut
NULL

# R CMD checker appeasement
int_labs <- result_labs <- epsilon <- NULL

#' @rdname closure_cut
#' @export
closure_cut <- function(x, breaks, label_vec = NULL, dig_lab = 3L,
                        ordered_result = FALSE, env = parent.frame()) {
  # prepare base::cut in case it needs to be used
  cut_call <- match.call()
  cut_call[1] <- call('cut')

  # user input checking
  if (is.integer(x)) x <- as.numeric(x)
  if (!is.numeric(x)) stop("'x' must be numeric")
  if (!is.null(label_vec) && !is.character(label_vec)) {
    stop("label_vec must be character class matching interval length, or null")
  }
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

  if (any(names(b_names) == "ii")) {
    warning("breaks labeled 'ii'; relabeled 'ie' to prevent category overlap")
    b_names[b_names == "ii"] <- "ie"
  }

  # we dont want any names except i or e
  if (!is.l0(grep("i|e", b_names, ignore.case = FALSE, invert = TRUE))) {
    warning("breaks were named, but every point must be labeled 'i' or 'e'.
            x sent to base::cut since names(breaks) were ambiguous.")
    return(eval(cut_call, envir = env))
  }
  b_nums <- c(breaks[[1]], rep(breaks[-c(1, length(breaks))], each = 2),
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
  precision_int <- c(b_odd_nums, b_even_nums)
  attr(precision_int, "names") <- c(b_odd_names, b_even_names)
  p_order <-
    c(seq(1, 2*length(b_odd_names), 2), seq(2, 2*length(b_odd_names), 2))
  precision_int <- precision_int[order(p_order)]
  interval_label <- paste0(b_odd_names, b_odd_nums, ",", b_even_nums, b_even_names)
  # for aesthetic reasons only, fixing +/- Inf label
  interval_label <- gsub(x = interval_label,
                         pattern = "Inf]", replacement = "Inf)")
  interval_label <- gsub(x = interval_label,
                         pattern = "\\[-Inf", replacement = "(-Inf")

  # TODO: need to add pretty print breaks
  # breaks <- sapply(breaks, prettyNum, digits = dig_lab)

  # smaller espilons possible via:
  # https://cran.r-project.org/web/packages/Rmpfr/vignettes/Rmpfr-pkg.pdf

  # digit checking for error and for epsilon creation
  dig_dec_m <- vapply(c(x, precision_int), FUN = dig_dec, FUN.VALUE = dig_dec(1))
  max_dec <- max(dig_dec_m["decimals", ], 1, na.rm = TRUE)
  max_dig <- max(dig_dec_m["digits", ], 1, na.rm = TRUE)
  max_dec_dig <- max(max_dec, max_dig, na.rm = TRUE)

  if (max_dec_dig >= 15) {
    p_warn("R precision fails past 16 digits.
           (1) check a few extreme cases of x by hand to see if you are ok.
           (2) shift your dataset above 0.
           (3) consider packages like Rmpfr to handle your data.")
  }
  epsilon <- 10 ^ -(min(max_dec + 1, 10))

  add_eps <- grep(x = names(precision_int), pattern = "[(]")
  sub_eps <- grep(x = names(precision_int), pattern = "[)]")
  precision_int[add_eps] <- precision_int[add_eps] + epsilon
  precision_int[sub_eps] <- precision_int[sub_eps] - epsilon
  add_eps <- grep(x = names(precision_int), pattern = "]")
  precision_int[add_eps] <- precision_int[add_eps] + epsilon/2
  precision_int <- sort(unique((unlist(precision_int, use.names = FALSE))))

  if (any(is.infinite(precision_int))) {
    if (precision_int[1] == -Inf) {
      precision_int[1] <- -10 ^ (max_dig + 1)
    }
    if (last(precision_int) == Inf) {
      precision_int[length(precision_int)] <- 10 ^ (max_dig + 1)
    }
    if (any(is.infinite(precision_int))) {
      "+/- Inf values found in breaks outside of the first and last index"
    }
  }

  inc_low = b_names[1] == "i"
  bin_segments <-
    .bincode(c(odd_obj(precision_int), last(precision_int)),
             breaks = precision_int,
             include.lowest = inc_low, right = FALSE)
  # matching segments up to x_binned categories
  bin_segments <- sort(unique((bin_segments)))

  # heart of this function
  x_binned <- .bincode(x, include.lowest = inc_low,
                       breaks = precision_int, right = FALSE)
  # factor with label_vec as labels
  if (!is.null(label_vec)) {
    x_cat <-
      factor(x_binned, levels = bin_segments,
             labels = label_vec, ordered = ordered_result)
  } else {
    # factor with interval levels as labels
    x_cat <- factor(x_binned, levels = bin_segments,
                    ordered = ordered_result, labels = interval_label)
  }
  return(x_cat)
}