#' @title Funding Bucket Data
#'
#' @description Encompass database is lacking unit type data for SALs,
#' this fixes that issue; last updated 11/3/2015.
#'
#' Encompass data.table is lacking costs for SALs, this fixes that issue.
#' Last updated 11/23/2015.
#'
#' @docType data
#' @format A data.table object.
#' @keywords datasets
#' @source Funding Bucket Report from PCE.
#'
#' @examples
#' \dontrun{
#' data(fb_unit_type)
#' data(fb_cpt_mod_cost)
#' rm("fb_unit_type", "fb_cpt_mod_cost")
#' fb_unit_type
#' fb_cpt_mod_cost
#' wccmh::fb_unit_type
#' wccmh::fb_cpt_mod_cost
#' }
#' @name fb_data
NULL
#' @rdname fb_data
"fb_unit_type"
#' @rdname fb_data
"fb_cpt_mod_cost"

# fb_unit_type <-
#   readRDS("C:/Users/dalrymplej/Documents/GitHub/wccmh/data/fb_unit_type.rdata")
# fb_unit_type[mod=="", mod := NA]
# save(fb_unit_type, file = "fb_unit_type.rda")
