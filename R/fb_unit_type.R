#' Funding Bucket Unit Type Data
#'
#' Encompass database is lacking unit type data for SALs, this fixes that
#' issue. Subject to update when necessary. Last updated 11/3/2015.
#'
#' @docType data
#'
#' @usage data(fb_unit_type)
#'
#' @format A data.table object.
#'
#' @keywords datasets
#'
#' @source Funding Bucket Report from PCE.
#'
#' @examples
#' \dontrun{
#' data(fb_unit_type)
#' rm("fb_unit_type")
#' fb_unit_type
#' wccmh::fb_unit_type
#' }
"fb_unit_type"

# fb_unit_type <-
#   readRDS("C:/Users/dalrymplej/Documents/GitHub/wccmh/data/fb_unit_type.rdata")
# fb_unit_type[mod=="", mod := NA]
# saveRDS(fb_unit_type, "fb_unit_type.RData")
