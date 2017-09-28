#' @title dig_dec
#'
#' @description Conviences tool to obtain a functions digits and decimals
#' @usage
#' dig_dec(x)
#' @param x Single numeric value or string that is coercible to numeric.
#' @return Named numeric vector with the components
#'  \item{digits}{Number of digets left of the decimal}
#'  \item{decimals}{Number of digets right of the decimal}
#'  \item{round}{The smartest place to round for readablity}
#' @examples
#' dig_dec(3949.1221291)
#' dig_dec("3523.642")
#' dig_dec(00000.0002)
#' dig_dec(x = 9.2)
#' @note Rmpfr::format, Rmpfr::mpfr may contain better solutions.
#' @section TODO:
#' \itemize{
#'    \item Rename dig_dec
#'    \item Rmpfr::format, Rmpfr::mpfr may contain better solutions.
#' }
#' @export
dig_dec <- function(x) {
  pre_settings <- list(digits = getOption("digits"),
                       scipen = getOption("scipen"))
  on.exit(options(pre_settings))

  temp_settings <- list(digits = 16, scipen = 10)
  options(temp_settings)

  x <- as.numeric(x)

  ## Stops if not a number ##
  if (is.na(x)) {
    stop("(dig_dec) x cannot be coerced to numeric (RLB)")
  }
  string_x <- format(x, scientific = FALSE, digits = 15)
  ## Count decimals ##
  if (suppressWarnings(identical((x %% 1), 0))) {
    string_x <- gsub("^[[:punct:]]", "", string_x)
    dig_pl <- nchar(string_x)
    dec_pl <- 0
  } else {
    string_x <- gsub("^[[:punct:]]", "", string_x)
    N_x <- nchar(string_x)
    s_x <- strsplit(string_x, "\\.")
    dig_pl <- nchar(s_x[[1]][1])
    if (dig_pl == 1 & s_x[[1]][1] == "0") {
      dig_pl <- 0
    }
    dec_pl <- nchar(s_x[[1]][2])
  }
  # dig_pl=4  dig_pl=3  dig_pl=2  dig_pl=1
  if (dig_pl > 0) {
    pretty_round <- ifelse(3 - dig_pl < 0, 0, 3 - dig_pl)
  } else {
    pretty_round <- dec_pl
  }
  c("digits" = dig_pl, "decimals" = dec_pl, "round" = pretty_round)
}