#' RDS shortcuts
#'
#' @description These functions allow the user to type less when using readRDS
#' and saveRDS. The object name followed by RDS is appended to the file
#' destination.
#'
#' @param x R object to serialize.
#' @param dir The file storage location.
#' @param envir The environment to which readnamesRDS assign the R object. The
#' default is NULL in which case it reads in the R object without assigning it.
#'
#' @return all functions return either TRUE or FALSE.
#'
#' @section Notes:
#' These functions are for convenience only. Does not work with \code{matrix()}
#' since \code{length(matrix)} does not equal 0.
#'
#' @examples
#' \dontrun{
#' x1 <- c(1, 2, 3)
#' x2 <- data.frame(v1 = 1:10)
#' savenameRDS(x = x1, dir = getwd())
#' readnameRDS(x = x1, dir = getwd())
#' rm(x1)
#' readnameRDS(x = "x1", dir = getwd(), env = .GlobalEnv)
#' }
#' @name nameRDS
NULL

#' @rdname nameRDS
#' @export
savenameRDS <- function(x, dir) {
  obj_name <- substitute(x)
  saveRDS(x, file.path(dir, paste0(obj_name, '.RDS')))
}

#' @rdname nameRDS
#' @export
readnameRDS <- function(x, dir, envir = NULL) {
  obj_name <- substitute(x)
  result <- readRDS(file.path(dir, paste0(obj_name, '.RDS')))
  if (!is.null(envir)) {
    assign(x, value = result, envir = envir)
  } else {
    return(result)
  }
}

