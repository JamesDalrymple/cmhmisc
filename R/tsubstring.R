#' @title tsubstring and transpose the resulting list efficiently
#'
#' @description \code{substrings} & \code{tsubstring} work in similar fashion to
#'   \code{strsplit} & \code{tstrsplit}. They are used to split strings, or
#'   vectors of strings at a specified point. \code{tsubstring} was also
#'   designed for optimized use in data.table's, however, its function is to
#'   split string columns at a specific indices or widths, instead of using
#'   regular expressions.
#'
#' @name tsubstring
NULL

#' @rdname tsubstring
#' @description \code{substrings} is a feature extended version of
#'   \code{substring} that allows for a varity of handling behaviors by way of
#'   argument selection.
#'
#' @param text A character vector.
#' @param cuts The vector of indices, or string widths which will be used to cut
#'   the character vector into sub-strings. By default, \code{cuts = -1}. Any
#'   negative integer will cause this function to return an unaltered
#'   string.
#' @param widths Default is \code{FALSE}. This argument alteres the
#'   interpretiation of the \code{cuts} vector as widths instead of indices.
#' @param recycle FALSE by default, when TRUE, the \code{cuts} sequence -- or
#'   widths -- is repeated until the end of the string.
#' @param extra FALSE by default, when TRUE, any extra characters not contained
#'   in the initial \code{cuts} sequence will be appended onto the end of the
#'   returned list.
#'
#' @return An index split vector of sub-strings.
#' @seealso \code{\link{substring}}
#' @section TODO:
#' \itemize{
#'    \item{Add '.' to tsub like setf}
#' }
#' @examples
#' txt <- "ABCDEFGJIJKLMN"
#' cuts <- c(1, 2, 4)
#' substrings(txt, cuts, widths = FALSE, recycle = FALSE, extra = FALSE)
#' substrings(txt, cuts, widths = TRUE , recycle = FALSE, extra = FALSE)
#' substrings(txt, cuts, widths = FALSE, recycle = TRUE , extra = FALSE)
#' substrings(txt, cuts, widths = TRUE , recycle = TRUE , extra = FALSE)
#' substrings(txt, cuts, widths = FALSE, recycle = TRUE , extra = TRUE )
#' substrings(txt, cuts, widths = TRUE , recycle = TRUE , extra = TRUE )
#' substrings(txt, cuts, widths = FALSE, recycle = FALSE, extra = TRUE )
#' substrings(txt, cuts, widths = TRUE , recycle = FALSE, extra = TRUE )
#'
#' @export
substrings <- function(text, cuts = -1L,
                       widths = FALSE, recycle = FALSE, extra = FALSE) {
  if (is.anyT(cuts < 0)) return(text) else cuts <- as.integer(cuts)
    #cuts <- sapply(cuts, as.integer)
  ntxt <- nchar(text)
  if (widths) {
    wids <- cuts
    cuts <- cumsum(cuts)
  }else {
    cuts <- sort(cuts)
    wids <- cuts - c(0, cuts[-length(cuts)]) # Faster than diff()
  }
  if (recycle) {
    last_c_idx <- cuts[length(cuts)]
    cycl <- ntxt %/% last_c_idx
    exta <- ntxt %%  last_c_idx
    wids <- c(rep(wids, cycl), wids[cuts <= exta])
    cuts <- cumsum(wids)
  }
  if (extra) {
    last_c_idx <- cuts[length(cuts)]
    exta <- ntxt - last_c_idx
    if (exta < 1) exta <- integer()
    wids <- c(wids, exta)
    cuts <- cumsum(wids)
  }
  scuts <- c(1, (cuts + 1)[-length(cuts)])
  substring(text, scuts, cuts) # TODO keep names somehow
}

#' @rdname tsubstring
#'
#' @param x  The vector to split (and transpose), usually a column wrapped in a
#'   data.table.
#' @param fill Default is \code{NA}. It is used to fill shorter list elements so
#'   as to return each element of the transposed result of equal lengths.
#' @param type.convert \code{TRUE} calls \code{\link{type.convert}} with
#'   \code{as.is=TRUE} on the columns.
#' @param give.names This setting is relevent when \code{tsubstrings} is not
#'   being used within data.table, and is by default NULL. Column names can be
#'   passed into this arguement, as well as TRUE, which will set names with V#,
#'   and FALSE, which will force no names to be generated. Additionally, if the
#'   arguement \code{cuts} is named, the \code{cuts} names will be used if
#'   \code{give.names} is neglected. Like \code{tstrsplit}, \code{give.names} is
#'   FALSE by default for \code{tsubstr}.
#'
#' @details It internally calls \code{substrings} first, and then
#'   \code{\link{transpose}} on the result. \code{give.names} argument can be
#'   used to return an auto named list, although this argument does not have any
#'   effect when used with \code{:=}, which requires names to be provided
#'   explicitly. It might be useful in other scenarios.
#'
#' @seealso \code{\link{tstrsplit}}, \code{\link{transpose}}
#'
#' @examples
#' cnames <- c("one", "third", "two")
#' cuts <- setNames(cuts, cnames)
#' widths = FALSE ; recycle = FALSE ; extra = TRUE ; fill = NA
#' give.names = NULL #cnames
#' x <- rep(txt, 3)
#' DT <- data.table(x = x)
#' DT[, tsubstrings(x, cuts, give.names = give.names, extra = extra,
#'                 recycle = recycle, widths = widths)]
#' @return A transposed list after splitting by the indices provided.
#' @importFrom data.table last transpose setattr
#' @importFrom stats setNames
#' @export
tsubstrings <- function(x, cuts, widths = FALSE,
                       recycle = FALSE, extra = FALSE,
                       fill = NA, type.convert = FALSE,
                       give.names = NULL) {
  ans <- transpose(lapply(x, substrings, cuts, widths, recycle, extra),
                   fill = fill, ignore.empty = FALSE)
  if (type.convert) ans <- lapply(ans, type.convert, as.is = TRUE)
  if (is.F(give.names)) return(ans)
  ## Handling decision hierarchy for column naming
  if (is.character(give.names)) {
    names_vec <- if (widths) give.names else
      names(sort(stats::setNames(cuts, give.names)))
  } else if (is.named(cuts)) {
    names_vec <- if (widths) names(cuts) else names(sort(cuts))
  } else if (is.T(give.names)) {
    names_vec <- paste0("V", seq_along(ans))
  } else {
    return(ans)
  }
  ## Corrects name vector lengths
  if (length(names_vec) == length(ans)) {
    setattr(ans, "names", names_vec)
  } else if (length(names_vec) < length(ans)) {
    v_names_vec <- paste0("V", seq(length(ans) - length(names_vec)))
    setattr(ans, "names", c(names_vec, v_names_vec))
  } else {
    setattr(ans, "names", names_vec[seq_along(ans)])
  }

  ans
}

#' @rdname tsubstring
#' @description \code{tsubstr} is the basic version of \code{tsubstrings} which
#'   only calls \code{substr} to do it's subsetting.
#' @param start An integer of the first element to be subsetted.
#' @param stop An integer of the last element to be subsetted.
#'
#' @details It internally calls \code{substr} first, and then
#'   \code{\link{transpose}} on the result.
#'
#' @seealso \code{\link{tstrsplit}}, \code{\link{transpose}}
#'
#' @examples
#' DT[, tsubstr(x, start = 3, stop = 4)]
#' @return A transposed list after subsetting by \code{start} and \code{stop}.
#' @importFrom data.table transpose setattr
#' @export
tsubstr <- function(x, start, stop,
                       fill = NA, type.convert = FALSE,
                       give.names = FALSE) {
  ans <- transpose(lapply(x, substr, start, stop),
                   fill = fill, ignore.empty = FALSE)
  if (type.convert)
    ans = lapply(ans, type.convert, as.is = TRUE)
  if (give.names)
    setattr(ans, "names", paste("V", seq_along(ans), sep = ""))
  ans
}

#' @rdname tsubstring
#' @description \code{tsubstring} is the basic version of \code{tsubstrings}
#'   which only calls \code{substring} to do it's subsetting. It is slightly
#'   faster than tsubstrings, yet has less functionality.
#' @param first An integer of the first element to be subsetted.
#' @param last An integer of the last element to be subsetted.
#'
#' @details It internally calls \code{substring} first, and then
#'   \code{\link{transpose}} on the result.
#'
#' @seealso \code{\link{tstrsplit}}, \code{\link{transpose}}
#'
#' @examples
#' DT[, tsubstring(text = x, first = c(1,3,5,7), last = c(2,4,6,8))]
#' @return A transposed list after subsetting by \code{first} and \code{last}.
#' @importFrom data.table transpose setattr
#' @export
tsubstring <- function(text, first, last = 1000000L,
                    fill = NA, type.convert = FALSE,
                    give.names = FALSE) {
  ans <- transpose(lapply(text, substring, first, last),
                   fill = fill, ignore.empty = FALSE)
  if (type.convert)
    ans = lapply(ans, type.convert, as.is = TRUE)
  if (give.names)
    setattr(ans, "names", paste("V", seq_along(ans), sep = ""))
  ans
}

#' @rdname tsubstring
#' @description \code{tsubs} does the analogously the same thing as the other
#'   transpose wrapper functions, above, however, it allows the user to insert
#'   the  string handling function of your choice.
#' @inheritParams base::lapply
#'
#' @details It internally calls whatever function is loaded into the \code{FUN}
#'   arguement, and than calls \code{\link{transpose}} on the result. A this
#'   functions core, a call to \code{transpose(lapply(X, FUN, ...))} is being
#'   evalutated. This is very simular to just calling \code{apply(X, 1, FUN)} in
#'   order to apply a function on a row by row basis. I suspect using apply is
#'   more efficent in general.
#'
#'
#' @seealso \code{\link{lapply}}, \code{\link{apply}}, \code{\link{transpose}}
#' @section TODO:
#' Adding the "." pacement function like setf
#' @examples
#' DT[, apply(.SD, MARGIN = 1, FUN = substring,
#'            first = c(1,3,5,7), last = c(2,4,6,8)),
#'    .SDc = 'x']
#' DT[, tsub(X = x, FUN = substring, first = c(1,3,5,7), last = c(2,4,6,8))]
#' @return A transposed list after subsetting by the FUN function and it's
#'   arguements.
#' @importFrom data.table transpose setattr
#' @export
tsub <- function(X, FUN, ...,
                 fill = NA, type.convert = FALSE,
                 give.names = FALSE) {
  ans <- transpose(lapply(X, FUN, ...),
                   fill = fill, ignore.empty = FALSE)
  if (type.convert)
    ans = lapply(ans, type.convert, as.is = TRUE)
  if (give.names)
    setattr(ans, "names", paste("V", seq_along(ans), sep = ""))
  ans
}
