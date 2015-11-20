#' @title WCCMH file utility functions
#' @description csv_to_rds reads
#'
#' @param work_dir the working direction containing either csv and/or rds
#' files.
#' @param csv_file the name of the file with the full file location.
#' @param keep_cols the column names you want to retain.
#' @param rename_cols column names to rename, defaults to NULL where no
#' changes take effect.
#' @param rm.original logical TRUE/FALSE whether to delete original csv file.
#' @param file_pattern what file should be selected. Please make the file
#' pattern select a unique file.
#'
#' @return A vector of recoded fund names.
#'
#' @note if you can get away from csv file storage, this is recommended. If you
#' can get even further away from file storage on a hard drive and use a
#' database, that is ideal.
#'
# #' @examples
#'
#' @seealso file.path
#'
#' @importFrom data.table fread := setnames
#'
#' @name file_utility
NULL

#' @rdname file_utility
csv_to_rds <-
  function(csv_file, keep_cols, rename_cols, rm.original) {
    csv_data <- fread(csv_file, select = keep_cols)
    rds_file <- gsub(x = csv_file, pattern = ".csv", replacement = ".rds")
    if (is.null(rename_cols) || missing(rename_cols)) {
      setnames(csv_data, colnames(csv_data), rename_cols)
    }
    saveRDS(object = csv_data, file = rds_file)
    if (rm.original) {
      unlink(x = csv_file); print("original file deleted")
    }
    result <- readRDS(file = rds_file)
  }

#' @rdname file_utility
#' @export
load_data <-
  function(work_dir, file_pattern, keep_cols = NULL, rename_cols = NULL,
           rm.original = FALSE) {
    csv_files <- list.files(work_dir, pattern = ".csv")
    rds_files <- list.files(work_dir, pattern = ".rds")
    data_csv <- grep(
      x = csv_files, pattern = file_pattern,
      ignore.case = TRUE, value = TRUE, perl = TRUE
    )
    data_rds <- grep(
      x = rds_files, pattern = file_pattern,
      ignore.case = TRUE, value = TRUE
    )
    if (length(data_csv) == 0 &&
        length(data_rds) == 0) {
      stop("No file found with given pattern (csv or rds).")
    }
    if (length(data_csv) > 1) {
      stop(paste("too many csv", file_pattern, "files!", sep = " - "))
    }
    if (length(data_csv) == 1) {
      # load data_csv, keep only wanted columns, save as rds file,
      # then load rds file, then delete data_csv file
      result <- csv_to_rds(
        csv_file = data_csv,
        keep_cols = keep_cols,
        rename_cols = rename_cols,
        rm.original = rm.original
      )
    }  else if (length(data_rds) == 1) {
      # load rds file
      result <- readRDS(file = data_rds)
      if (length(colnames(result)) > length(rename_cols)) {
        result[, setdiff(colnames(result), rename_cols) := NULL]
        saveRDS(object = result, file = data_rds)
      }
    }
    return(result)
  }

