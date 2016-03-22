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
#' @param priority_col The name of the field in overlap_dt corresponding to
#' priority. Each team needs to have a predefined unique priority.
#'
#' @return A data.table with fixed admission records. May contain more rows
#' than original dataset.
#'
#' @note This could be cleaned. This could be a part II to EquaPac::overlap_fix_dt
#'
#' @examples
#' input <- data.table(
#' person_ID = c(rep(98723, 4), rep(8534, 2), 11223, rep(22446, 3)),
#' team = c(rep("A", 4), rep("B", 2), "A", "B", "A", "C"),
#' start_date = as.Date(c("2009-10-1", "2011-11-21", "2012-1-23", "2013-3-2",
#' "2009-11-14", "2010-1-1", "2012-1-2", "2011-2-2", "2012-4-3", "2010-01-09")),
#' end_date = as.Date(c("2010-5-23", NA, "2015-01-02", "2013-3-2", "2009-12-31",
#' "2010-3-1", "2015-03-22", "2016-1-2", "2014-9-30", "2016-3-22")))
#' team_priority <- data.table(team = c("A", "B", "C"), priority = c(1, 2, 3))
#' input[team_priority, priority := i.priority, on = "team"]
#' overlap_combine(overlap_dt = copy(input[team!="C"]), id_cols = "person_ID",
#'  team_col = "team", start_col = "start_date", end_col = "end_date",
#'  overlap_int = 1L, replace_blanks = Sys.Date() + 1e3,
#'  priority_col = "priority")
#' @importFrom data.table data.table := rbindlist copy setnames setorderv dcast shift melt between setkeyv
#'
#' @name admission_functions
NULL

.SD <- .SDcols <- team <- bound_ok <- variable <- boundaries <-
  date_value <- priority <- ovr_shift <- prior_shift <- nested_ovr <-
  overlap_combine <- index <- i.index <- i.start_date <- i.end_col <-
  start_date <- end_date <- ovr_vec <- date_group <- NULL

#' @export
#' @rdname admission_functions
overlap_combine <-
  function(overlap_dt, id_cols, team_col, start_col, end_col, overlap_int = 1L,
           replace_blanks = Sys.Date(), priority_col = "priority") {
    setorderv(overlap_dt, c(id_cols, team_col, start_col))
    overlap_dt[, end_col := get(end_col) + overlap_int]
    sd_cols <- c(start_col, "end_col")
    # foverlaps cannot deal with blanks
    overlap_dt[is.na(end_col), end_col := replace_blanks]
    # note: if end_col becomes < start_col due to overlap_int,
    # we assign end_col <- start_col
    overlap_dt[end_col - get(start_col) < 0, end_col := start_col]
    overlap_dt[, index := .I]
    setnames(overlap_dt, start_col, "start_date")
    # setnames(overlap_dt, team_col, "team")
    # finding overlapping combinations via vectors of indices ---
    c_overlap <-
      overlap_dt[overlap_dt[, unique(.SD), .SDcols =
                              c(id_cols, team_col, "start_date", "end_col", "index")],
                 on = c(id_cols, team_col), allow.cartesian = TRUE]
    c_overlap <- c_overlap[i.index != index]
    c_overlap[between(i.start_date, start_date, end_col) |
                between(i.end_col, start_date, end_col),
              ovr_vec := list(list(unique(c(index, i.index)))),
              by = c(id_cols, team_col, "start_date")]
    ovr_l <- c_overlap[, ovr_vec]
    ovr_l <- Filter(Negate(function(x) is.null(unlist(x))), ovr_l)
    ovr_l <- unique(ovr_l)
    # find list of reduced vectors which we need to MIN/MAX ---
    ovr_red_l <- list()
    for (i in seq_along(ovr_l)) {
      tmp_inter <- unique(unlist(sapply(
        ovr_l,
        FUN = function(x) {
          if (length(intersect(unlist(x), unlist(ovr_l[i]))) > 0) {
            result <- union(unlist(x), unlist(ovr_l[i]))
            return(result)
          } else {
            return(ovr_l[i])
          }
        }
      )))
      ovr_red_l[[i]] <- sort(tmp_inter)
    }
    ovr_red_l <- unique(ovr_red_l)

    for (i in seq(ovr_red_l)) {
      setkey(overlap_dt, index)[ovr_red_l[[i]],
                                c("start_date", "end_date", "end_col") :=
                                  list(min(start_date), max(end_date), max(end_col))]
    }
    overlap_dt[, index := NULL]
    overlap_dt <- unique(overlap_dt)
    setkeyv(overlap_dt, c(id_cols, "start_date"))
    # figure out which intervals are nested, separate them out & deal with them
    overlap_dt[, index := .I]
    overlap_dt[, prior_shift := priority - shift(priority, n = 1), by = id_cols]
    overlap_dt[between(shift(start_date, n = 1), start_date, end_col), ovr_shift := 1, by = id_cols]
    overlap_dt[shift(ovr_shift) == 1 & prior_shift < 0, nested_ovr := 1]
    overlap_dt[index %in% overlap_dt[nested_ovr == 1, index - 1], nested_ovr := 1]
    overlap_nested_dt <- overlap_dt[nested_ovr == 1]
    overlap_dt <- overlap_dt[is.na(nested_ovr)]
    nested_melt_dt <-
      melt(overlap_nested_dt, id.vars = c(id_cols, team_col, priority_col),
           measure.vars = c("start_date", "end_col"), value.name = "date_value")
    setkeyv(nested_melt_dt, c(id_cols, "date_value"))
    nested_melt_dt[, boundaries := cumsum(ifelse(variable == "start_date", 1, -1))]
    nested_melt_dt[boundaries==1 & variable == "start_date", bound_ok := 1]
    nested_melt_dt[boundaries==0 & variable == "end_col", bound_ok := 1]
    # create new records that need to be added ---
    add_dt <- nested_melt_dt[0, ]
    if (nrow(nested_melt_dt)>0) {
      for (i in 1:nrow(nested_melt_dt))
        if (nested_melt_dt[i, is.na(bound_ok)]) {
          if (nested_melt_dt[i, variable] == "start_date") {
            add_dt <- rbindlist(list(add_dt,
              data.table(nested_melt_dt[i, id_cols, with = FALSE],
                         nested_melt_dt[i-1, team],
                         nested_melt_dt[i-1, priority],
                         "end_col",
                         nested_melt_dt[i, date_value - 1],
                         NA, NA)))
          } else if (nested_melt_dt[i, variable] == "end_col") {
            add_dt <- rbindlist(list(add_dt,
              data.table(nested_melt_dt[i, id_cols, with = FALSE],
                         nested_melt_dt[i+1, team],
                         nested_melt_dt[i+1, priority],
                         "start_date",
                         nested_melt_dt[i, date_value + 1],
                         NA, NA)))
          }
        }
    }
    # add new records back in
    nested_melt_dt <- rbindlist(list(nested_melt_dt, add_dt))
    setkeyv(nested_melt_dt, c(id_cols, "date_value"))
    nested_melt_dt[, c("bound_ok", "boundaries") := NULL]
    nested_melt_dt[, date_group := .I, by = id_cols]
    nested_melt_dt[variable == "end_col", date_group := date_group - 1L]
    nested_melt_dt <-
      dcast(nested_melt_dt, paste0(paste(id_cols, sep = "+"), "+", team_col, "+",
                                   priority_col, "+", "date_group ~ variable"), value.var = "date_value")
    nested_melt_dt[, end_date := end_col]
    nested_melt_dt[, c("date_group") := NULL]
    overlap_dt[,  c("index", "prior_shift", "ovr_shift", "nested_ovr") := NULL]
    overlap_dt <- rbindlist(list(overlap_dt, nested_melt_dt), use.names = TRUE)
    setorderv(overlap_dt, c(id_cols, start_col, team_col))
    return(overlap_dt)
  }

