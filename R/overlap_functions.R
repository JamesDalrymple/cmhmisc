#' @title WCCMH admission functions
#' @description These functions are for datasets that have integer or date
#' columns that need to be combined by some unique grouping or with the
#' additional constraint of a priority column when the priority column causes
#' an overlap outside of the unique grouping but within the priority column.
#'
#' @param data A data.table object with collapsable records.
# param case_col the case_no or main field distiguisher.
#' @param group_cols A group of columns which will collectively make a key on
#' which to group by when reducing/collapsing the data.table object.
#' @param priority_col A single column which will have a priority assignment.
#' @param start_col The start date for record. Blanks not allowed.
#' @param end_col The end date for the record. Blanks will be replaced, see
#' @param overlap_int An integer, default value of 1, to find consecutive
#' records.
#' @param analysis_date If end_col has missing values, they will be replaced
#' with this value. Defaults to Sys.Date().
#'
#' @return overlap_combine returns a reduced/collapsed data.table object based
#' on group_cols.
#' priority_overlap returns a data.table with fixed records based on priority
#' team assignment. May contain more rows than original dataset.
#'
#' @note Do not have data columns named strdt, enddt, pk_1, pk_2, ... You have
#' been warned! MAJOR rewrite coming eventually... hopefully backward compatible
#'
#' @examples
#' # how to fix if priorities are not to be accounted for...
#' test1 <- overlap_combine(data = copy(ex_overlap), group_cols = c("case_no",
#' "team"), start_col = "start_date", end_col = "end_date",
#'  overlap_int = 1L, analysis_date = Sys.Date() + 1e3)
#' print(test1)
#'
#'\dontrun{
#' # priority overlap is broken, WIP
#'  # how to fix if priorities are to be accounted for...
#' test2 <- priority_overlap(data = copy(ex_overlap),
#'                  group_cols = c("case_no", "type"),
#'                  priority_col = "team",
#'                  start_col = "start_date",
#'                  end_col = "end_date",
#'                  overlap_int = 1L,
#'                  analysis_date = Sys.Date())
#' print(test2)
#' }
#'
#' @import data.table
#' @importFrom Hmisc Cs
#' @importFrom EmiscDev p_warn grepv d.t
#' @name overlap_functions
NULL


# sql_adm <- if (FALSE) {
#   sql <- list(
#     channel = odbcConnect("WSHSQL002"),
#     query = list()
#   )
#   # admissions
#   sql$query$adm <-
#     sprintf(
#       "select distinct
#       adm.case_no, adm.provider_eff as team_eff, adm.provider_exp as team_exp,
#       adm.provider as team, adm.assigned_staff as staff, adm.staff_eff,
#       adm.staff_exp, adm.adm_effdt, adm.adm_expdt
#       from encompass.dbo.tblE2_Adm_Consumers as adm
#       where adm.county = 'Washtenaw' and adm.provider in
#       ('WSH - ACT', 'WSH - ATO' , 'WSH - Children''s Services',
#       'WSH - Children''s Services - Home Based', 'WSH - DD Adult',
#       'WSH - Access/Engagement', 'Washtenaw County Community Mental Health',
#       'Washtenaw County Community Mental Health-External',
#       'WSH - MI - Adult')
#       and adm.providertype = 'Direct Provider'
#       and adm.provider_eff <= '%2$s' and
#       (adm.provider_exp >= '%1$s' or adm.provider_exp is null)",
#       "10/1/2010", "5/30/2016")
#   sql$adm <- sqlQuery(query = sql$query$adm,
#     channel = sql$channel, stringsAsFactors = FALSE)
#   sql$adm <- data.table(sql$adm)
#   adm <- copy(sql$adm)
#   adm[, Cs(staff_eff, staff_exp, staff) := NULL]
#   wccmh::overlap_combine(
#     data = adm, group_cols = Cs(team, adm_effdt, adm_expdt),
#     start_col = "team_eff", end_col = "team_exp", overlap_int = 1L,
#     analysis_date = Sys.Date()
#   )
# }


# R CMD checker appeasement ---
index <- i.index <- i.start_date <- start_date <- i.end_col <- end_date <-
  ovr_vec <- xid <- yid <- i.priority <- ovr_pairs <- i.end_date <- i.team <-
  remove_record <- p_col <- i.p_col <- grp_id <- .GRP <- grp_n <-
  add_record <- new_index <- p_col <- NULL

# older version (outdated 4/2/2016) -------------------------------------------
#' @export
#' @rdname overlap_functions
overlap_combine <-
  function(data, group_cols, start_col, end_col, overlap_int = 1L,
           analysis_date = Sys.Date()) {
    d <- copy(data)
    if (any(names(d) == "end_col")) {
      d[, end_col := NULL]
      p_stop("You had a column labeled end_col which conflicts with
             overlap_comb.")
    }
    d[, end_col := get(end_col)]
    if (d[, class(end_col)] != "Date") {
      p_warn("end_col was not supplied as a Date; as.Date was applied but please
             submit end_col as Date class to avoid potential Date conversion
             errors.")
      d[, end_col := as.Date(end_col)]
    }
    d[, end_col := end_col + overlap_int]
    # sd_cols <- c(start_col, "end_col")
    d[is.na(end_col), end_col := analysis_date]
    # note: if end_col becomes < start_col due to overlap_int,
    # we assign end_col <- start_col
    if (nrow(d[end_col - get(start_col) < 0]) > 0) {
      d[end_col - get(start_col) < 0, c("end_col", end_col)
        := get(start_col)]
      p_warn("you had start_col and end_col out of order")
    }
    d[, index := .I]
    setnames(d, start_col, "start_date")
    setnames(d, end_col, "end_date")
    # finding overlapping combinations via vectors of indices ---
    c_overlap <-
      d[d[, unique(.SD), .SDcols =
            c(group_cols, "start_date", "end_col", "index")],
        on = group_cols, allow.cartesian = TRUE]
    c_overlap <- c_overlap[i.index != index]
    c_overlap[between(i.start_date, start_date, end_col) |
                between(i.end_col, start_date, end_col),
              ovr_vec := list(list(unique(c(index, i.index)))),
              by = c(group_cols, "start_date")]

    if (!is.null(c_overlap$ovr_vec)) {
      ovr_l <- c_overlap[, ovr_vec]
      ovr_l <- Filter(Negate(function(x) is.null(unlist(x))), ovr_l)
      ovr_l <- unique(ovr_l)
      # find list of reduced vectors which we need to MIN/MAX ---
      ovr_red_l <- list()
      for (i in seq_along(ovr_l)) {
        tmp_inter <- unique(as.vector(unlist(sapply(
          ovr_l,
          FUN = function(x) {
            if (length(intersect(unlist(x), unlist(ovr_l[i]))) > 0) {
              result <- union(unlist(x), unlist(ovr_l[i]))
              return(result)
            } else {
              return(ovr_l[i])
            }
          }
        ))))
        ovr_red_l[[i]] <- sort(tmp_inter)
      }
      ovr_red_l <- unique(ovr_red_l)

      for (i in seq(ovr_red_l)) {
        setkey(d, index)[ovr_red_l[[i]],
                         c("start_date", "end_date", "end_col") :=
                           list(min(start_date), max(end_date), max(end_col))]
      }
    }
    d[, Cs(index) := NULL]
    d <- unique(d)
    setnames(d, "start_date", start_col)
    setnames(d, "end_date", end_col)
    return(d)
    }

# WIP <- if (FALSE) {
# case <- uN <- fl_pk <- gs_i <- pk <- fdate <- ldate <- NULL
# overlap_combine2 <-
#   function(data,
#            case_col,
#            group_cols,
#            start_col,
#            end_col,
#            overlap_int = 1L,
#            analysis_date = Sys.Date()) {
#     focus_flds  <- c(start_col, end_col, case_col, group_cols)
#     remand_flds <- setdiff(names(data), focus_flds)
#     d <- copy(data)[, .SD, .SDc = c(focus_flds, remand_flds)]
#     GS_v <- group_cols
#     SD_v <- c(srt_date_col = "strcol", end_date_col = "endcol")
#     CS_v <- Cs(case)
#     setnames(d, c(SD_v, CS_v, GS_v, remand_flds))
#     set(d,
#         j = SD_v,
#         value = lapply(d[, SD_v, with = FALSE], as.Date, format = '%m/%d/%Y'))
#     if (!inherits(analysis_date, what = "Date"))
#       analysis_date <- as.Date(analysis_date)
#     d[, uN := nrow(.SD), by = c(CS_v, GS_v)]
#     # d[uN > 3]
#     # setorderv(d, c(CS_v, GS_v, SD_v))
#     setkeyv(d, c(SD_v))
#     # d[, fl_pk := 1]
#     # d[case==1126484]
#     # Rcpp::sourceCpp("./src/overlap.cpp")
#     # d[case==244779]
#     # casenum <- 10499
#     # casenum <- 244779
#     # casenum <- 1126484
#     d[, fl_pk := NA_integer_]
#     folp <-
#       function(SD1,
#                SD2,
#                type = "any",
#                which = TRUE,
#                mult = "all") {
#         flopz <<-
#           foverlaps(SD1,
#                     SD2,
#                     type = type,
#                     which = which,
#                     mult = mult)
#         z <- copy(flopz)
#         zset <- d.t(xid = stunq(z$xid), key = "xid")
#         z[xid > yid, `:=`(yid = xid, xid = yid)]
#         z <- z[!duplicated(z)][order(xid, yid)] # & xid != yid
#         if (z[,!any(xid != yid)] & nrow(z) == nrow(zset)) {
#           return(z[, .(as.integer(yid))])
#         } else if (z[, any(xid != yid)]) {
#           rct <- copy(z[xid != yid])
#           for (i in rev(seq_row(rct))) {
#             # i=4 i=3 i=2 i=1
#             rcs <- paste0(rct[i, yid], "=", rct[i, xid])
#             z[, Cs(xid, yid) := lapply(.SD, recode, rcs)]
#           }
#           setkey(z, xid)
#           return(z[zset, uni(.SD), roll = TRUE][, .(as.integer(yid))])
#         }
#         stop("The flop function has no idea how to handle a condition.")
#       }
#     #       d[uN > 1 & case == casenum]
#     #        #& cmh_team == "Child"
#     #       d[uN > 1 & case == casenum , folp(.SD, .SD), .SDc = c(SD_v), by = c(CS_v, GS_v)]
#
#     d[uN > 1, fl_pk := folp(.SD, .SD), .SDc = c(SD_v), by = c(CS_v, GS_v)]
#     d[is.na(fl_pk), fl_pk := 1L]
#
#     setorderv(d, c(CS_v, "fl_pk", GS_v))
#     d[, gs_i := seq(nrow(.SD)), by = c(CS_v, "fl_pk"), .SDc = GS_v]
#     spntf_mnchr_v <- d[, unlist(.(
#       case = max(nchar(case)),
#       ugrp = max(nchar(as.character(gs_i))),
#       ufol = max(nchar(fl_pk))
#     ))]
#     fmt <- paste0("%",
#                   spntf_mnchr_v['case'],
#                   ".0f-%",
#                   spntf_mnchr_v['ugrp'],
#                   ".0f-%",
#                   spntf_mnchr_v['ufol'],
#                   ".0f")
#     d[, pk := gsub(" ", "0", sprintf(fmt, case, gs_i, fl_pk))]
#     d[, fdate := min(unlist(.SD)), by = pk, .SDc = SD_v['srt_date_col']]
#     d[, ldate := max(unlist(.SD)), by = pk, .SDc = SD_v['end_date_col']]
#     dn_v <- names(d)
#     for (j in grepv('date', dn_v))
#       set(d, j = j, value = as.Date(d[[j]], origin = "1970-01-01"))
#     output_vec <-
#       c(CS_v, GS_v, grepv('date', dn_v), 'pk', remand_flds)
#     d <- d[, unique(.SD), .SDc = output_vec]
#     return(d)
#   }
# }

# priority column with overlapping date records -------------------------------
#' @export
#' @rdname overlap_functions
priority_overlap <- function(data,
                             group_cols,
                             priority_col,
                             start_col,
                             end_col,
                             overlap_int = 1L,
                             analysis_date = Sys.Date()+999) {
  # data = copy(data)
  # group_cols = Cs(case_no, cmh_effdt)
  # priority_col = "cmh_team"
  # start_col = "team_effdt"
  # end_col = "team_expdt"

  # fix 'easier' issues first with simple min/max
  d <- overlap_combine(
    data = data,
    group_cols = c(group_cols, priority_col),
    start_col = start_col,
    end_col = end_col,
    overlap_int = overlap_int,
    analysis_date = analysis_date
  )
  group_cols <- setdiff(group_cols, priority_col)
  setnames(d, priority_col, "p_col")
  d[, p_col := as.int(p_col)]
  stopifnot(d[, class(p_col)] == "integer")
  d[!is.na(end_date), end_col := end_date]
  setkeyv(d, c(group_cols, "start_date", "end_col"))
  overlap_pairs_dt <-
    foverlaps(
      d[, .SD, .SDcols = c(group_cols, Cs(start_date, end_col))],
      d[, .SD, .SDcols = c(group_cols, Cs(start_date, end_col))],
      by.x = c(group_cols, "start_date", "end_col"),
      by.y = c(group_cols, "start_date", "end_col"),
      which = TRUE)[xid != yid]
  overlap_pairs_dt[, index := .I]
  overlap_pairs_dt[, ovr_pairs := list(list(c(xid, yid))), by = index]
  ovr_pairs_l <- overlap_pairs_dt[, ovr_pairs]
  ovr_pairs_l <- unique(rapply(ovr_pairs_l, sort, how = "list"))
  # combine/reduce pair list if any indice is overlapping ---
  repeat {
    initial_length <- length(ovr_pairs_l)
    tmp_pairs_l <- list()
    for (i in seq(ovr_pairs_l)) {
      for_inter <-
        unique(as.vector(unlist(sapply(ovr_pairs_l, function(x) {
          if (length(intersect(unlist(x), unlist(ovr_pairs_l[i]))) > 0) {
            result <- union(unlist(x), unlist(ovr_pairs_l[i]))
            return(result)
          } else {
            return(ovr_pairs_l[i])
          }
        })))) # end of fn and sapply
      tmp_pairs_l[[i]] <- sort(for_inter)
    } # end of repeat loop
    ovr_pairs_l <- unique(tmp_pairs_l)
    post_length <- length(ovr_pairs_l)
    if (post_length - initial_length == 0)
      break
  } # end of while
  d[, index := .I]
  messy_ovr_dt <- setkey(d, index)[unlist(ovr_pairs_l)]
  clean_dt <- setkey(d, index)[!unlist(ovr_pairs_l)]
  retain_cols <- setdiff(names(d),
                         c("start_date", "end_date", "date_value", "end_col"))
  messy_ovr_dt[is.na(end_date), end_date := analysis_date]
  # self join by overlap (too bad we cant add conditions here) ---
  # messy_ovr_dt[cmh_priority_dt, priority := i.priority, on = "team"]
  setkeyv(messy_ovr_dt, c(group_cols, "start_date", "end_date"))
  messy_ovr_dt <- foverlaps(
    messy_ovr_dt, messy_ovr_dt,
    by.x = c(group_cols, "start_date", "end_date"),
    by.y = c(group_cols, "start_date", "end_date"))
  # remove records that have 'lower' p_col and are completely 'within'
  messy_ovr_dt[, remove_record := ifelse(p_col > i.p_col &
                                           start_date > i.start_date & end_date < i.end_date, TRUE, FALSE)]
  messy_ovr_dt[start_date >= i.start_date & end_date <= i.end_date &
                 p_col > i.p_col, remove_record := TRUE]
  # keep non-duplicate + needed records
  messy_ovr_dt[, grp_id := .GRP, by = c(group_cols, "p_col")]
  messy_ovr_dt[, grp_n := .N, by = c(group_cols, "p_col")]
  messy_ovr_dt[grp_n > 1 & index == i.index, remove_record := TRUE]
  messy_ovr_dt[, Cs(grp_id, grp_n) := NULL]
  messy_ovr_dt <- messy_ovr_dt[remove_record == FALSE | is.na(remove_record)]
  messy_ovr_dt[, add_record := NA_character_]
  # higher priority does not affect lower priority
  messy_ovr_dt[p_col < i.p_col, add_record := "do not change"]
  # lower priority followed by overlapping higher priority
  messy_ovr_dt[p_col < i.p_col & start_date < i.start_date &
                 end_date >= i.start_date, add_record := "do not change"]
  # lower priority 'within' higher priority
  messy_ovr_dt[p_col > i.p_col & start_date < i.start_date &
                 i.end_date < end_date, add_record := "split record both sides"]
  # case 3: higher priority followed by overlapping lower
  messy_ovr_dt[p_col > i.p_col & start_date < i.start_date &
                 end_date <= i.end_date & i.start_date <= end_date,
               add_record := "shorten right side"]
  # case 4b: lower priority followed by overlapping higher priority
  messy_ovr_dt[p_col > i.p_col & start_date > i.start_date &
                 start_date <= i.end_date & i.end_date < end_date,
               add_record := "shorten left side"]
  # case 5: no overlap (shouldnt really show up)
  messy_ovr_dt[end_date < i.start_date, add_record := "no overlap"]
  # messy_ovr_dt[p_col < i.p_col & i.start_date < start_date &
  # end_date <= i.end_date, add_record := "add record left of p_col"]
  # messy_ovr_dt[p_col < i.p_col & start_date < i.end_date &
  # end_date > i.end_date, add_record := "add record left of p_col"]
  messy_ovr_dt[, new_index := .I]
  # cases in the middle of a split need to be discarded
  setkeyv(messy_ovr_dt, c(group_cols, "start_date", "end_date", "add_record"))
  rm_index <- messy_ovr_dt[messy_ovr_dt[add_record == "split record both sides",
                                        unique(.SD), .SDcols = c(group_cols, "start_date", "end_date")]][
                                          add_record == "do not change", new_index]
  messy_ovr_dt <- setkey(messy_ovr_dt, new_index)[!rm_index]
  # cases in the right side need to have the rule consistently applied
  setkeyv(messy_ovr_dt, c(group_cols, "start_date", "end_date", "add_record"))
  change_index <- messy_ovr_dt[messy_ovr_dt[add_record == "shorten right side",
                                            unique(.SD), .SDcols = c(group_cols, "start_date", "end_date")]][
                                              is.na(add_record), new_index]
  setkey(messy_ovr_dt, new_index)[change_index, add_record := "shorten right side"]
  rm(change_index)
  # cases in the left side need to have the rule consistently applied
  setkeyv(messy_ovr_dt, c(group_cols, "start_date", "end_date", "add_record"))
  change_index <- messy_ovr_dt[messy_ovr_dt[add_record == "shorten left side",
                                            unique(.SD), .SDcols = c(group_cols, "start_date", "end_date")]][
                                              is.na(add_record), new_index]
  setkey(messy_ovr_dt, new_index)[change_index, add_record := "shorten left side"]
  rm(change_index, rm_index)
  setkey(messy_ovr_dt, NULL)
  split_recs <- messy_ovr_dt[add_record == "split record both sides"]
  split_recs[, index := -index]
  messy_ovr_dt <- messy_ovr_dt[add_record %nin% "split record both sides"]
  # dealing with each split separately is the safe/right way to do this
  split_right <- copy(split_recs)[, `:=`(add_record = "split right")]
  split_right[, end_date := i.start_date - 1]
  split_right[, end_date := min(end_date), by = index]
  split_left <- copy(split_recs)[, `:=`(add_record = "split left")]
  split_left[, start_date := i.end_date + 1]
  split_left[, start_date := max(start_date), by = index]
  split_comb <- rbindlist(list(split_right, split_left), use.names = TRUE)
  split_comb[, c(grep(x = names(split_comb), pattern = "[.]", value = TRUE),
                 Cs(new_index, remove_record, add_record, index)) := NULL]
  # applying date fixes
  messy_ovr_dt[add_record == "shorten left side",
               start_date := i.end_date + 1]
  messy_ovr_dt[add_record == "shorten right side",
               end_date := i.start_date - 1]
  # records were separated via foverlaps; rejoining now ---
  messy_ovr_dt[,
               Cs(start_date, end_date) :=
                 list(max(start_date),
                      min(end_date)),
               by = c(group_cols, "p_col", "index")]

  messy_ovr_dt[, setdiff(names(messy_ovr_dt), names(split_comb)) := NULL]
  messy_ovr_dt <- unique(messy_ovr_dt)
  split_comb <- unique(split_comb)
  clean_dt[, setdiff(names(clean_dt), names(split_comb)) := NULL]
  fixed_dt <- rbindlist(list(messy_ovr_dt, split_comb, clean_dt), use.names = TRUE)
  fixed_dt[, end_col := NULL]
  setnames(fixed_dt, old = "p_col", new = priority_col)
  setnames(fixed_dt, "start_date", start_col)
  setnames(fixed_dt, "end_date", end_col)
  return(fixed_dt)
}
