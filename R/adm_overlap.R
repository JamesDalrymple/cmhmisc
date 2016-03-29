#' @title WCCMH admission functions
#' @description standardize admission data
#'
#' @param overlap_dt A data.table object.
#' @param id_cols A unique ID column(s) which will be used to group records.
#' @param team_col A single column; the admission record team.
#' @param start_col The start date for the admission record. Blanks not allowed.
#' @param end_col The end date for the admission record. Blanks will be
#' replaced, see parameter replace_blanks
#' @param overlap_int An integer, default value of 1, to find consecutive records.
#' @param replace_blanks If end_col has missing values, they will be replaced
#' with this value. Defaults to Sys.Date().
#'
#' @return A data.table with fixed admission records. May contain more rows
#' than original dataset.
#'
#' @note This could be cleaned. This could be a part II to EquaPac::overlap_fix_dt
#'
#' @examples
#' \dontrun{
#' # EXAMPLE IS OUTDATED ---
#' input <- data.table(
#' person_ID = c(rep(98723, 4), rep(8534, 2), 11223, rep(22446, 3)),
#' team = c(rep("A", 4), rep("B", 2), "A", "B", "A", "C"),
#' start_date = as.Date(c("2009-10-1", "2011-11-21", "2012-1-23", "2013-3-2",
#' "2009-11-14", "2010-1-1", "2012-1-2", "2011-2-2", "2012-4-3", "2010-01-09")),
#' end_date = as.Date(c("2010-5-23", NA, "2015-01-02", "2013-3-2", "2009-12-31",
#' "2010-3-1", "2015-03-22", "2016-1-2", "2014-9-30", "2016-3-22")))
#' team_priority <- data.table(team = c("A", "B", "C"), priority = c(1, 2, 3))
#' input[team_priority, priority := i.priority, on = "team"]
#'
#' test <- overlap_combine(overlap_dt = copy(input[team!="C"]), id_cols = "person_ID",
#'  team_col = "team", start_col = "start_date", end_col = "end_date",
#'  overlap_int = 1L, replace_blanks = Sys.Date() + 1e3,
#'  priority_col = "priority")
#'  }
#' @importFrom data.table data.table := rbindlist copy setnames setorderv dcast shift melt between setkeyv foverlaps .N
#' @importFrom Hmisc Cs
#'
#' @name admission_functions
NULL

# R CMD checker appeasement
.SD <- .SDcols <- team <- bound_ok <- variable <- boundaries <-
  date_value <- priority <- ovr_shift <- prior_shift <- nested_ovr <-
  overlap_combine <- index <- i.index <- i.start_date <- i.end_col <-
  start_date <- end_date <- ovr_vec <- date_group <- xid <- yid <-
  i.priority <- ovr_pairs <- i.end_date <- i.team <- remove_record <-
  n_index <- n_na <- NULL

#' @export
#' @rdname admission_functions
overlap_combine <-
  function(overlap_dt, id_cols, team_col, start_col, end_col, overlap_int = 1L,
           replace_blanks = Sys.Date()) {
    # overlap_dt = modify$cmh_core[case_no == 10450]
    # options(warn=2)
    # overlap_dt = copy(modify$cmh_core[case_no == 11660])
    # overlap_dt = copy(modify$cmh_core[case_no == 10563])
    # overlap_dt = copy(modify$cmh_core[case_no == 11091])
    # overlap_dt = copy(modify$cmh_core[case_no == 220766])


    setorderv(overlap_dt, c(id_cols, team_col, start_col))
    if (any(names(overlap_dt) == "end_col")) {
      overlap_dt[, end_col := NULL]
    }
    overlap_dt[, end_col := get(end_col) + overlap_int]
    sd_cols <- c(start_col, "end_col")
    overlap_dt[is.na(end_col), end_col := replace_blanks]
    # note: if end_col becomes < start_col due to overlap_int,
    # we assign end_col <- start_col
    overlap_dt[end_col - get(start_col) < 0, end_col := start_col]
    overlap_dt[, index := .I]
    setnames(overlap_dt, start_col, "start_date")
    setnames(overlap_dt, end_col, "end_date")
    setnames(overlap_dt, team_col, "team")
    # finding overlapping combinations via vectors of indices ---
    c_overlap <-
      overlap_dt[overlap_dt[, unique(.SD), .SDcols =
                              c(id_cols, "team", "start_date", "end_col", "index")],
                 on = c(id_cols, "team"), allow.cartesian = TRUE]
    c_overlap <- c_overlap[i.index != index]
    c_overlap[between(i.start_date, start_date, end_col) |
                between(i.end_col, start_date, end_col),
              ovr_vec := list(list(unique(c(index, i.index)))),
              by = c(id_cols, "team", "start_date")]

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
        setkey(overlap_dt, index)[ovr_red_l[[i]],
                                  c("start_date", "end_date", "end_col") :=
                                    list(min(start_date), max(end_date), max(end_col))]
      }
    }
    overlap_dt[, index := NULL]
    overlap_dt <- unique(overlap_dt)

    # figure out which intervals are overlapping with different team priorities
    # overlap_dt2 <- copy(overlap_dt)
    # overlap_dt <- copy(overlap_dt2)

    # PART II: Even messier records -------------------------------------------
    overlap_dt[!is.na(end_date), end_col := end_date ]
    setkeyv(overlap_dt, c(id_cols, "start_date", "end_col"))
    overlap_pairs_dt <-
      foverlaps(overlap_dt[, .SD, .SDcols = c(id_cols, Cs(start_date, end_col))],
              overlap_dt[, .SD, .SDcols = c(id_cols, Cs(start_date, end_col))],
              by.x = c(id_cols, "start_date", "end_col"),
              by.y = c(id_cols, "start_date", "end_col"),
              which = TRUE)[xid != yid]
    overlap_pairs_dt[, index := .I]
    overlap_pairs_dt[, ovr_pairs := list(list(c(xid, yid))), by = index]
    ovr_pairs_l <- overlap_pairs_dt[, ovr_pairs]
    ovr_pairs_l <- unique(rapply(ovr_pairs_l, sort, how = "list"))

    repeat {
      initial_length <- length(ovr_pairs_l)
      tmp_pairs_l <- list()
      for (i in seq(ovr_pairs_l)) {
        for_inter <- unique(as.vector(unlist(sapply(ovr_pairs_l, function(x) {
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
      if (post_length - initial_length == 0) break
    } # end of while

    overlap_dt[, index := .I]
    messy_ovr_dt <- setkey(overlap_dt, index)[unlist(ovr_pairs_l)]
    clean_dt <- setkey(overlap_dt, index)[!unlist(ovr_pairs_l)]
    retain_cols <- setdiff(names(overlap_dt),
      c("start_date", "end_date", "date_value", "end_col"))
    messy_ovr_dt[is.na(end_date), end_date := Sys.Date()+999]
    # self join by overlap (too bad we cant add conditions here)
    messy_ovr_dt[cmh_priority_dt, priority := i.priority, on = "team"]
    setkeyv(messy_ovr_dt, c(id_cols, "start_date", "end_date"))
    messy_ovr_dt <- foverlaps(messy_ovr_dt, messy_ovr_dt,
              by.x = c(id_cols, "start_date", "end_date"),
              by.y = c(id_cols, "start_date", "end_date")
    )
    # remove records that have 'lower' priority and are completely 'within'
    messy_ovr_dt[, remove_record := ifelse(priority > i.priority &
      start_date > i.start_date & end_date < i.end_date, TRUE, FALSE)]
    messy_ovr_dt[start_date >= i.start_date & end_date <= i.end_date &
                   priority > i.priority, remove_record := TRUE]
    # keep non-duplicate + needed records
    messy_ovr_dt[(index == i.index | priority <= i.priority) & !(i.start_date >
      start_date & i.end_date < end_date & priority > i.priority),
      c(grep(x = names(messy_ovr_dt), pattern = "[.]", value = TRUE)) := NA]
    messy_ovr_dt <- unique(messy_ovr_dt)
    # records that have 'higher' priority and are completely 'within'
    messy_add_dt <- messy_ovr_dt[(i.start_date > start_date &
      i.end_date < end_date & priority > i.priority)]
    if (nrow(messy_add_dt) > 0) {
      messy_ovr_dt <- messy_ovr_dt[index %nin% messy_add_dt[, unique(c(index, i.index))]]
      messy_add_dt <- rbindlist(list(
        messy_add_dt[, list(start_date = start_date,
                            end_date = pmin(end_date, i.start_date-1)), by = c(retain_cols)],
        messy_add_dt[, list(start_date = pmax(start_date, i.end_date + 1),
                            end_date = end_date), by = c(retain_cols)],
        messy_add_dt[, list(start_date = i.start_date, end_date = i.end_date,
                            team = i.team), by = setdiff(retain_cols, "team")]
      ), use.names = TRUE)
      messy_add_dt[, index := -.I]
      messy_ovr_dt <-
        rbindlist(list(messy_ovr_dt, messy_add_dt), use.names = TRUE,fill = TRUE)
    }
    messy_ovr_dt[, n_index := .N, by = index]
    messy_ovr_dt[, n_na := sum(is.na(i.start_date)), by = index]
    messy_ovr_dt[n_index > 1 & is.na(i.start_date) & n_na < n_index,
                 remove_record := TRUE]
    messy_ovr_dt <- messy_ovr_dt[remove_record==FALSE | is.na(remove_record)]
    messy_ovr_dt[, Cs(remove_record, n_index, n_na) := NULL]
    messy_ovr_dt <- unique(messy_ovr_dt)
    # give partial overlap records fixed start/end dates
    # case 1
    messy_ovr_dt[priority > i.priority &
                start_date <= i.end_date &
                start_date >= i.start_date &
                end_date > i.start_date,
                start_date := i.end_date + 1, by = index]
    # case 2
    messy_ovr_dt[priority > i.priority &
                 start_date < i.start_date & end_date >= i.start_date,
                 end_date := i.start_date - 1, by = index]
    # case 3: dealing with case 1 + 2 having 2+ overlaps
    messy_ovr_dt <- messy_ovr_dt[, list(start_date = max(start_date),
                        end_date = min(end_date),
                        end_col = min(end_date)),
                 by = c(retain_cols)]
    fixed_dt <- rbindlist(list(clean_dt, messy_ovr_dt), use.names = TRUE)
    fixed_dt[, index := NULL]
    setorderv(fixed_dt, c(id_cols, "start_date", "team"))
    setnames(fixed_dt, "team", team_col)
    setnames(fixed_dt, "start_date", start_col)
    setnames(fixed_dt, "end_date", end_col)
    return(fixed_dt)
  }

