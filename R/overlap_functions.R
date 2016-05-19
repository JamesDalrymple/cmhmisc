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
#' @param priority_value An integer column, where lower priorities override
#' higher priorities. May be numeric if coerceable to integer.
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
#' been warned!
#'
#' @examples#'
#' # how to fix if priorities are not to be accounted for...
#' test1 <- overlap_combine(data = copy(ex_overlap), group_cols = c("case_no",
#' "team"), start_col = "start_date", end_col = "end_date",
#'  overlap_int = 1L, analysis_date = Sys.Date() + 1e3)#'
#' print(test1)
#'
#'  # how to fix if priorities are to be accounted for...
#' test2 <- priority_overlap(data = copy(ex_overlap),
#'                  group_cols = c("case_no", "type"),
#'                  priority_col = "team",
#'                  priority_value = "priority",
#'                  start_col = "start_date",
#'                  end_col = "end_date",
#'                  overlap_int = 1L,
#'                  analysis_date = Sys.Date())
#' print(test2)
#'
#' @import data.table
#' @importFrom Hmisc Cs
#' @importFrom EquaPac p_warn
#' @name overlap_functions
NULL

# R CMD checker appeasement ---

index <- i.index <- i.start_date <- start_date <- i.end_col <- end_date <-
  ovr_vec <- xid <- yid <- i.priority <- ovr_pairs <- i.end_date <- i.team <-
  remove_record <- p_col <- i.p_col <- grp_id <- .GRP <- grp_n <-
  add_record <- new_index <- p_col <- NULL

# trouble_cases <- c(10450, 11660, 10563, 11091, 220766)


# WIP <- if (FALSE) {
#   data = copy(ex_overlap)
#   group_cols = c(group_cols, priority_col)
#   start_col = start_col
#   end_col = end_col
#   overlap_int = overlap_int
#   analysis_date = analysis_date
# }


# older version (outdated 4/2/2016) -------------------------------------------
#' @export
#' @rdname overlap_functions
overlap_combine <-
  function(data, group_cols, start_col, end_col, overlap_int = 1L,
           analysis_date = Sys.Date()) {
    d <- copy(data)
    if (any(names(d) == "end_col")) {
      d[, end_col := NULL]
      p_warn("You had a column labeled end_col which conflicts with
             overlap_comb. It was deleted and re-created based on the end_col
             parameter.")
    }
    d[, end_col := get(end_col) + overlap_int]
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
    return(d)
    }

if (FALSE) {
  # overlap_dt <- fread("C:/Users/dalrymplej/Documents/GitHub/wccmh/data/overlap_dt.csv")
  # overlap_dt[, start_date := as.Date(start_date, format = 'm/%d/%Y' )]
  # overlap_dt[, end_date := as.Date(end_date, format = "%m/%d/%Y")]
  # overlap_dt[, priority := as.int(priority)]
  # save(overlap_dt,
  #  file = "C:/Users/dalrymplej/Documents/GitHub/wccmh/data/overlap_dt.rda")
  # load("C:/Users/dalrymplej/Documents/GitHub/wccmh/data/overlap_dt.rda")
  # data("ex_overlap")

  # how to fix if priorities are not to be accounted for...


WIP <- if (FALSE) {
    # copy(overlap_dt)
    data <- readRDS(file.path("C:/Dropbox/github_clone/EToPac",
                              "data-sets/ex_admissions.rds"))
    data[, Cs(other1, other2) := .(seq_row(data), rev(seq_row(data)))]


    a <- proc.time()
    test1 <- overlap_combine(data,
                             group_cols =
                          c("case_no", "cmh_team","cmh_effdt", "cmh_expdt"),
                             start_col = "team_effdt",
                             end_col = "team_expdt",
                             overlap_int = 1L,
                             analysis_date = Sys.Date() + 1e3)
    b <- proc.time() ; b - a
    a <- proc.time()
    test2 <- overlap_combine2(data,
                              case_col = c("case_no"),
                    group_cols = c("cmh_team","cmh_effdt", "cmh_expdt"),
                    start_col = "team_effdt",
                    end_col = "team_expdt",
                    overlap_int = 1L,
                    analysis_date = Sys.Date() + 1e3)
    b <- proc.time() ; b - a
    library(data.table)
    library(EquaPac)
    # sourceCpp("overlap.cpp")
    options(showWarnCalls = TRUE,
            max.print = 10000,
            # scipen = 0,
            # warn = 1,
            warn = 1, # Turns warnings to errors
            stringsAsFactors = FALSE,
            error = NULL,
            # error = recover,
            # error = browser,
            # browserNLdisabled = TRUE, # Enter: repeats the previous command.
            # warn = 2, # Turns warnings to errors
            # show.error.locations = TRUE,
            show.error.locations = 'top',
            showErrorCalls = TRUE
    )

    # xid = c(1L, 1L, 2L, 2L, 2L, 3L, 3L)
    # yid = c(1L, 2L, 1L, 2L, 3L, 2L, 3L)
    #
    # overlap_groups(xid, yid)

    # z <- d.t(xid = x[seq(x)%%2==1], yid = x[seq(x)%%2==0], key = "xid")
    # zet <- stunq(unlist(z))
    # z[xid > yid, `:=`(yid = xid, xid = yid)]
    # zm <- z[!duplicated(z) & xid != yid][order(xid, yid)]
    # zm
    # fs <- function(SD) t(apply(SD, 1, sort))
    # z[xid == 1 | yid == 1,  t(apply(fs(.SD), 2, list))]
    # lapply(xxx, 2, list)
    # as.list(xxx)
    # xdc <- dcast(melt(x, id.vars = Cs(xid, yid)), yid ~ xid)
    # merge(xdc, xdc, by = Cs(yid))
    # x[, yid, by = xid]
    # sourceCpp(file = "fasterLm.cpp")
    # library(Rcpp)

    # test1$case,test2$case
    # dm <- merge(test1, test2, by.x = Cs(case_no, group_cols), by.y = )

    # data <- fread(file.path(".", "data","ex_overlap.csv"))
    # data <- copy(data(ex_overlap))
    # data[, start_date := as.Date(.I)]
    case_col = c("case_no")
    group_cols = c("cmh_team","cmh_effdt", "cmh_expdt")
    start_col = "team_effdt"
    end_col = "team_expdt"
    overlap_int = 1L
    analysis_date = Sys.Date() + 1e3
    # ex_overlap - package data example
    data = copy(ex_overlap)
    case_col = "case_no"
    group_cols = c("team")
    start_col = "start_date"
    end_col = "end_date"
    overlap_int = 1L
    analysis_date = Sys.Date() + 999


  }

  overlap_combine2 <-
    function(data, case_col, group_cols, start_col, end_col,
             overlap_int = 1L, analysis_date = Sys.Date()) {
      focus_flds  <- c(start_col, end_col, case_col, group_cols)
      remand_flds <- setdiff(names(data), focus_flds)
      d <- copy(data)[, .SD, .SDc = c(focus_flds, remand_flds)]
      # GS_v <- paste0("grp", seq(group_cols))
      GS_v <- group_cols
      SD_v <- c(srt_date_col = "strcol", end_date_col = "endcol")
      CS_v <- Cs(case)
      setnames(d, c(SD_v, CS_v, GS_v, remand_flds))
      set(d, j = SD_v, value =
            lapply(d[,SD_v, with = FALSE], as.Date, format = '%m/%d/%Y'))
      if (!inherits(analysis_date, what = "Date"))
        analysis_date <- as.Date(analysis_date)
      d[, uN := nrow(.SD), by = c(CS_v, GS_v)]
      # d[uN > 3]
      # setorderv(d, c(CS_v, GS_v, SD_v))
      setkeyv(d, c(SD_v))
      # d[, fl_pk := 1]
      # d[case==1126484]
      # Rcpp::sourceCpp("./src/overlap.cpp")
      # d[case==244779]
      # casenum <- 10499
      # casenum <- 244779
      # casenum <- 1126484
      d[, fl_pk := NA_integer_]
      folp <- function(SD1, SD2, type = "any", which = TRUE, mult = "all") {
        flopz <<- foverlaps(SD1, SD2, type = type, which = which, mult = mult)
        z <- copy(flopz)
        zset <- d.t(xid = stunq(z$xid), key = "xid")
        z[xid > yid, `:=`(yid = xid, xid = yid)]
        z <- z[!duplicated(z)][order(xid, yid)] # & xid != yid
        if (z[, !any(xid != yid)] & nrow(z) == nrow(zset)) {
          return(z[, .(as.integer(yid))])
        } else if (z[, any(xid != yid)]) {
          recode <- car::recode
          rct <- copy(z[xid != yid])
          for (i in rev(seq_row(rct))) { # i=4 i=3 i=2 i=1
            rcs <- paste0(rct[i, yid], "=", rct[i, xid])
            z[, Cs(xid, yid) := lapply(.SD, recode, rcs)]
          }
          setkey(z, xid)
          return(z[zset, uni(.SD), roll = TRUE][,.(as.integer(yid))])
        }
        stop("The flop function has no idea how to handle a condition.")

      }

#       d[uN > 1 & case == casenum]
#        #& cmh_team == "Child"
#       d[uN > 1 & case == casenum , folp(.SD, .SD), .SDc = c(SD_v), by = c(CS_v, GS_v)]

      d[uN > 1, fl_pk := folp(.SD, .SD), .SDc = c(SD_v), by = c(CS_v, GS_v)]
      d[is.na(fl_pk), fl_pk := 1L]

      setorderv(d, c(CS_v, "fl_pk", GS_v))
      d[, gs_i := seq(nrow(.SD)), by = c(CS_v, "fl_pk"), .SDc = GS_v]
      spntf_mnchr_v <- d[,unlist(
        .(case = max(nchar(case)),
          ugrp = max(nchar(as.character(gs_i))),
          ufol = max(nchar(fl_pk))))]
      fmt <- paste0("%",
                    spntf_mnchr_v['case'], ".0f-%",
                    spntf_mnchr_v['ugrp'], ".0f-%",
                    spntf_mnchr_v['ufol'], ".0f")
      d[, pk := gsub(" ", "0", sprintf(fmt, case, gs_i, fl_pk))]
      d[, fdate := min(unlist(.SD)), by = pk, .SDc = SD_v['srt_date_col']]
      d[, ldate := max(unlist(.SD)), by = pk, .SDc = SD_v['end_date_col']]
      dn_v <- names(d)
      for(j in grepv('date', dn_v))
        set(d, j=j, value = as.Date(d[[j]], origin = "1970-01-01"))
      output_vec <- c(CS_v, GS_v, grepv('date', dn_v), 'pk', remand_flds)
      d <- d[, unique(.SD), .SDc = output_vec]
      return(d)
    }

  # how to fix if priorities are to be accounted for...
  test2 <- priority_overlap(overlap_dt = copy(overlap_dt),
                            group_cols = "person_ID",
                            priority_col = "team",
                            priority_value = "priority",
                            start_col = "start_date",
                            end_col = "end_date",
                            overlap_int = 1L,
                            replace_blanks = Sys.Date())

}


    # figure out which intervals are overlapping with different team priorities
    # overlap_dt2 <- copy(overlap_dt)
    # overlap_dt <- copy(overlap_dt2)

# priority column with overlapping date records -------------------------------
#' @export
#' @rdname overlap_functions
priority_overlap <- function(data,
                             group_cols,
                             priority_col,
                             priority_value,
                             start_col,
                             end_col,
                             overlap_int = 1L,
                             analysis_date = Sys.Date()+999) {
  # data = copy(data)
  # group_cols = Cs(case_no, cmh_effdt)
  # priority_col = "cmh_team"
  # priority_value = "priority"
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
  # setnames(d, priority_value, "p_integer")
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
  # setnames(fixed_dt, old = "p_col", new = priority_value)
  setnames(fixed_dt, "start_date", start_col)
  setnames(fixed_dt, "end_date", end_col)
  return(fixed_dt)
}
