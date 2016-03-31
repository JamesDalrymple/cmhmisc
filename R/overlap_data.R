#' @title Overlap Example Dataset
#'
#' @description An example dataset that (hopefully) incorporates all potential
#' problems to be fixed by overlap functions.
#'
#' @docType data
#' @format A data.table object.
#' @keywords datasets
#' @source Made up data, similar to CMH admission data.
#'
#' @examples
#' \dontrun{
#' data(overlap_dt)
#' rm("overlap_dt")
#' overlap_dt
#' # wccmh::overlap_dt
#' }
#' @name overlap_data
NULL
#' @rdname overlap_data
"overlap_dt"

# fb_unit_type <-
#   readRDS("C:/Users/dalrymplej/Documents/GitHub/wccmh/data/fb_unit_type.rdata")
# fb_unit_type[mod=="", mod := NA]
# save(fb_unit_type, file = "fb_unit_type.rda")
