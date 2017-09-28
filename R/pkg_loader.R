#' Auto Package Loader
#'
#' @description This function is used to automatically update out-of-date
#' existing packages and install new packages prior to loading them. The
#' packages are provided in either a list of characters or character vector.
#' Base packages are not updated, download the newest version of R to fix
#' base packages being out-of-date. It is a wrapper for both base R packages
#' and pacman, which is a package for dealing with package management.
#'
#' @param ... Arguments to be passed in from
#' \code{\link[utils]{install.packages}}, \code{\link[utils]{old.packages}},
#' and \code{\link[utils]{update.packages}}.
#'
#' @param packages Takes in either a vector of package name strings, or a list
#'   of package names.
#'
#' @param lib The installation working directory for packages. The first item
#' of .libPaths() is our default choice. If you do not have a user created
#' library, R will likely prompt you to create one.
#'
#' @param repos This should default to your Rstudio setting or profile
#' setting, we recommend setting one if you have not already. Our default
#' choice is "https://cran.rstudio.com/".
#'
#' @param update When set to TRUE, an attempt is made to update any outdated
#' 'packages' input(s) to the most up-to-date CRAN packages. Note you cannot
#' update base R packages if they are in an locked-down OS generated folder.
#'
#' @param sys.override Default value of FALSE, as out-of-date system packages
#'    may be an indication of an out-of-date R installation. However,
#'    if you really want to update system packages, set this argument to TRUE.
#'    It will install system packages in the R user library, as installing
#'    them in the system library is error-prone as your OS could prevent this.
#'
#' @param verbose Allow user to see more messages. Defaults to FALSE, which
#' gives minimal messages (some are just too important to turn off). You can
#' turn off all package messages in your options if desired.
#'
#' @details This is just a wrapper to provide full automation.
#'
#' @return A list with multiple items: installed for packages installed,
#' loaded for packages loaded, updated for packages updated, install_fail
#' for packages that failed to install, load_fail for packages that failed
#' to load, updated_fail for packages that failed to update.
#'

#' @note Prospective Features: Adding version updates for non-cran github
#' repos.
#'
#' @examples
#' \dontrun{
#' pkg_loader("splines")
#' packages = list("data.table", "reshape2")
#' pkg_loader(packages)
#' packages = c("data.table", "reshape2")
#' pkg_loader(packages)
#' pkg_loader(c("fakePackage", "fake2")) # causes error
#' pkg_loader(packages = "data.table", update = TRUE)
#' pkg_loader(packages = c("MASS", "data.table", "splines"),
#'            update = TRUE, sys.override = TRUE)
#' aux <- new.env(parent = .GlobalEnv)
#' aux$packages <- c("xtable", "knitr", "data.table",
#' "ggplot2", "zoo", "xlsx", "RODBC")
#' debugonce(pkg_loader)
#' pkg_loader(aux$packages, verbose = TRUE,
#' repos = "cran.cnr.berkeley.edu")
#' }
#'
#'
#' @importFrom pacman p_iscran p_isinstalled
#' @importFrom utils install.packages available.packages old.packages update.packages installed.packages
#'
#' @section TODO:
#' \itemize{
#'    \item Currently working on extended robust version.
#' }
#' @name pkg_loader
NULL

pkgs_outdated_success <- pkgs_outdated_fail <-
  install_pkgs_success <- install_pkgs_fail <- NULL # R checker appeasement

#' @rdname pkg_loader
#' @export
pkg_loader <- function(packages, lib = NULL, repos = NULL, update = FALSE,
                       verbose = FALSE, sys.override = FALSE, ...) {
  # input must be of character (or list) ... exit function if false
  switch(typeof(packages),
         "list" = {
           packages <- unlist(packages)
           if (!is.character(packages)) {
             p_stop('"unlist(packages)" is not a character list/vector:',
                    unlist(packages))
           }
         },
         "character" = {
           NULL
         },
         p_stop('"packages" must either be a list or a character vector:',
                packages))
  if (missing(lib) || is.null(lib)) lib <-  .libPaths()[1L]
  if (missing(repos) || is.null(repos)) {
    repos <- getOption("repos")
    if (!grepl(x = repos[1], pattern = "http")) {
      p_stop("Please give repos a CRAN mirror or set one up in your default
                options.")
    }
  }
  if (missing(update)) update <- FALSE
  if (missing(verbose)) verbose <- FALSE
  if (missing(sys.override)) sys.override <- FALSE

  stopifnot(
    typeof(lib) == "character",
    file.exists(lib),
    typeof(update) == "logical",
    typeof(verbose) == "logical",
    typeof(sys.override) == "logical")
  ### package categories ###
  # note: 'base R' packages will not be updated w/o override=TRUE
  auxillary_l <- list(
    cran_pkgs = packages[pacman::p_iscran(packages)])
  pkg_cat_l <- list(
    input_packages = packages,
    base_R_pkgs = intersect(packages, auxillary_l$base_R_pkgs),
    cran_pkgs = auxillary_l$cran_pkgs)

  auxillary_l$fake_pkgs <- setdiff(
    packages, # user input
    c(rownames(utils::available.packages()), # CRAN packages
      list.files(file.path(Sys.getenv("R_HOME"), "library")))) # R Home dir.
  if (length(auxillary_l$fake_pkgs)) {
    p_stop("These package(s) are not listed on CRAN or in your R Home
             directory:", auxillary_l$fake_pkgs)
  }

  auxillary_l <- list(
    auxillary_l,
    pkgs_install_status = pacman::p_isinstalled(pkg_cat_l$cran_pkgs),
    old_pkgs = as.character(utils::old.packages()[, 1]))

  pkg_cat_l <- c(
    pkg_cat_l,
    pkgs_installed = list(pkg_cat_l$cran_pkgs[auxillary_l$pkgs_install_status]),
    new_pkgs = pkg_cat_l$cran_pkgs[!auxillary_l$pkgs_install_status],
    base_outdated = intersect(pkg_cat_l$base_R_pkgs, auxillary_l$old_pkgs),
    pkgs_outdated = intersect(pkg_cat_l$cran_pkgs, auxillary_l$old_pkgs))
  # update packages if update = TRUE ---
  if (update & length(pkg_cat_l$pkgs_outdated) == 0 & verbose) {
    p_msg("No packages to update :)")
  }
  if (update & length(pkg_cat_l$pkgs_outdated) > 0) {
    if (verbose) {
      p_msg("Updating these packages:",
            pkg_cat_l$pkgs_outdated)
    }
    utils::update.packages(lib.loc = lib, repos = repos,
                    oldPkgs = pkg_cat_l$pkgs_outdated, ask = FALSE, ...)
    # see if packages actually updated
    auxillary_l$old_pkgs <- as.character(utils::old.packages()[, 1])
    pkg_cat_l <- c(pkg_cat_l,
                   pkgs_outdated_fail =
                     intersect(pkg_cat_l$pkgs_outdated, auxillary_l$old_pkgs),
                   pkgs_outdated_sucess =
              setdiff(auxillary_l$pkgs_outdated_success, auxillary_l$old_pkgs)
    )
  } else if (!update & length(pkg_cat_l$pkgs_outdated) > 0) {
    p_warn("The following packages:",
           pkg_cat_l$pkgs_outdated, "are out-of-date. Consider using argument
      'update = TRUE' to update them.")
  }
  if (length(pkg_cat_l$base_outdated) > 0) {
    p_warn("These base R packages are out-of-date:",
           pkg_cat_l$base_outdated, "Verify R is up-to-date.")
  }

  ### install packages ###
  if (verbose & length(pkg_cat_l$new_pkgs) > 0) {
    p_msg("Trying to install these packages:",
          pkg_cat_l$new_pkgs,
          paste("installation path:", sQuote(normalizePath(lib))))
  }

  if (length(pkg_cat_l$new_pkgs) > 0) {
    install.packages(pkgs = pkg_cat_l$new_pkgs,
                     lib = lib,
                     repos = repos, ...)
  }
  pkg_cat_l <- c(pkg_cat_l,
                 install_pkgs_success =
                   intersect(pkg_cat_l$new_pkgs, list.files(lib)),
            install_pkgs_fail = setdiff(pkg_cat_l$new_pkgs, list.files(lib)))

  ### load packages
  result <- suppressPackageStartupMessages(vapply(
    X = packages,
    FUN = require,
    character.only = TRUE,
    FUN.VALUE = logical(1)
  ))

  pkg_cat_l <- c(pkg_cat_l,
                 loaded_pkgs = list(names(result[result])),
                 load_fail = list(names(result[!result])))
  if (verbose) {
    p_msg("You loaded these packages:",
          pkg_cat_l$loaded_pkgs,
          paste("library load location:", sQuote(normalizePath(lib))))
  }
  if (length(pkg_cat_l$load_fail) > 0) {
    warning("These packages did not load:",
            pkg_cat_l$load_fail)
  }

  # not showing empty categories ... load_fail required this line
  pkg_cat_l <- Filter(f = function(x) length(x) > 0, pkg_cat_l)
  if (verbose) return(pkg_cat_l)
}

REINO <- if (FALSE) {
    if (FALSE) {
      options(showWarnCalls = TRUE,
              max.print = 10000,
              # scipen = 0,
              warn = 1,
              # warn = 2, # Turns warnings to errors
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
      load <- c('data.table', 'tidyr')
      inst_only = c("jsonlite", "validate")
      update = TRUE
      verbose = TRUE
      library(pacman)
      # library(installr)
      ## @importFrom Pacman p_iscran p_depends, p_exists
    }
auto_package_initializer <- function(load = NULL, inst_only = NULL,
                                     github_repos = NULL, update = FALSE,
                                     sys.override = FALSE, ...,
                                     verbose = FALSE) {
  W <- NULL ; E <- NULL      # withRestarts()
  w.handler <- function(w) { # warning handler
    W <<- w ; invokeRestart("muffleWarning")}
  e.handler <- function(e) { # error handler
    E <<- e ; invokeRestart("muffleError")}
  depends <- c("devtools", "utils", "methods", "stats", "pacman")
  installs <- depends[!p_isinstalled(depends)]
  if (length(installs)) {
    for (pkg in installs) {
      q <- quote(install.packages(pkg))
      withCallingHandlers(
        tryCatch({eval(q)}, error = e.handler, warning = w.handler),
        warning = W, error = E)
      if (!is.null(W) | !is.null(E)) message(W, " | ", E)
    }
  }
  updates <- intersect(depends, p_old()$Package)
  if (length(updates)) {
    for (pkg in updates) {
      q <- quote(utils::update.packages(oldPkgs = pkg))
      withCallingHandlers(
        tryCatch({eval(q)}, error = e.handler, warning = w.handler),
        warning = W, error = E)
      if (!is.null(W) | !is.null(E)) message(W, " | ", E)
    }
  }

  pkgs <- c(load, inst_only)
  installs <- pkgs[!p_isinstalled(pkgs)]
  for(pkg in installs)
    eval(substitute(p_install(p), list(p = pkg)))
  if (update)
    for(pkg in pkgs)
      eval(substitute(p_update(oldPkgs = p), list(p = pkg)))
  for(pkg in load)
    eval(substitute(p_load(oldPkgs = p), list(p = pkg)))
}

pkgs_info <- as.data.table(utills::installed.packages())
pkgs_info[,.(Package, LibPath)][grepl("Program Files", LibPath)]
pkgs_info[grepl("nlme", Package)]

remove.packages("nlme", file.path(Sys.getenv("R_HOME"), "library"))
Sys.chmod(file.path(Sys.getenv("R_HOME"), "library"))

pacman::p_information()
pacman::p_base()
pacman::p_info()
pacman::p_interactive()
  utils::available.packages()


grep("nlme", utils::available.packages()[, "Package"])

.libPaths()
Sys.getenv("R_LIBS_USER")
Sys.getenv("R_HOME")
Sys.chmod(file.path(Sys.getenv("R_HOME"), "library"))

file.path(Sys.getenv("R_HOME"), "library")
p_install("nlme")
  debugonce("p_update")
  debugonce("install.packages")
  debugonce("getExportedValue")
  debugonce("get0")
  get0(oNam, envir = ns)
  get0 <- function (x, envir = pos.to.env(-1L),
                    mode = "any", inherits = TRUE,
                    ifnotfound = NULL)
    .Internal(get0(x, envir, mode, inherits, ifnotfound))

  # doWithOneRestart(return(expr), restart)
  p_depends('devtools')
  p_update(oldPkgs = "nlme")
  p_delete("nlme")
  library(pacman)
  Sys.getenv("R_LIBS_USER")
  dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)
  p_install("tidyr")

  dir.create("C:/Users/private/Desktop/test/test")
  Sys.chmod("C:/Users/private/Desktop/test/test")

  Sys.chmod(file.path(Sys.getenv("R_HOME"), "library"))

  dir.exists(file.path(Sys.getenv("R_HOME"), "library"))

  file.info("C:/Program Files/R/R-3.2.4/library")
  file.mode(p_path()[2])

  # chmod -R u+rwX,g-rwx,o-rx

  Sys.getenv("R_HOME")


  p_version_diff(pkgs[3])


  p_load_current_gh(github_repos)

  pacman::p_install()
  pacman::p_install_gh()

  github_repos
  # pacman::p_isinstalled()
  pacman::p_load_current_gh()
  packs <- c("trinker/gofastr","trinker/termco","trinker/textshape",
             "Dasonk/Dmisc", "trinker/clustext", "trinker/termco")
  pacman::p_load_current_gh(packs)


  pacman::p_loaded()
  pacman::p_load()
  pacman::p_old()
  pacman::p_update()
  pacman::p_version_diff()
  pacman::p_vignette()

  pkgs[pacman::p_iscran(pkgs)]
  pacman::p_depends("pacman")
  # pacman::p_exists()


  utils::update.packages(lib.loc = lib, repos = repos,
                  oldPkgs = pkg_cat_l$pkgs_outdated, ask = FALSE, ...)

  utils::install.packages(pkgs = pkg_cat_l$new_pkgs,
                   lib = lib,
                   repos = repos, ...)
  github_repos


}