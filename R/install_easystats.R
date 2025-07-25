#' Install the easystats suite from R-universe (GitHub) or CRAN
#'
#' This function can be used to install all the **easystats** packages, either
#' latest development versions (from R-universe/GitHub) or the current
#' versions from CRAN. If the development versions are installed, packages
#' will be installed from the stable branch (master/main) for each package.
#'
#' @param source Character. Either `"development"`, `"cran"` or `"github"`. If
#'   `"cran"`, packages will be installed from the default CRAN mirror returned
#'   by `getOption("repos")['CRAN']`. If `"development"` (the default), packages
#'   are installed from the R-universe repository
#'   (<https://easystats.r-universe.dev/>). `"github"` installs the latest
#'   version from the GitHub-repositories main-branch. However, this option is
#'   usually not needed, because R-universe provides latest binaries. Use the
#'   `"github"` option only when R-universe servers are unavailable.
#' @param packages Character vector, indicating which packages to be installed.
#'   By default, the option `"all"` will install all **easystats** packages.
#' @param force Logical, if `FALSE`, only those packages with a newer
#'   version number will be installed. Use `force=TRUE` to force
#'   installation of all packages, even if the version number for the locally
#'   installed packages is identical to the latest development-versions. Only
#'   applies when `source="development"`.
#' @param verbose Toggle messages.
#'
#' @return Invisible `NULL`.
#'
#' @examplesIf FALSE
#' # install latest development-version of easystats packages from
#' # the r-universe repository, but only those packages that have newer
#' # versions available
#' install_latest()
#'
#' # install all latest development-version of easystats packages from
#' # the r-universe repository, no matter whether local installations
#' # are up to date or not.
#' install_latest(force = TRUE)
#' @export
install_latest <- function(source = "development",
                           packages = "all",
                           force = FALSE,
                           verbose = TRUE) {
  source <- insight::validate_argument(source, c("development", "cran", "github"))
  pkg <- easystats_packages()
  install_all_packages <- FALSE

  # update all packages?
  if (length(packages) == 1L && packages == "all") {
    install_all_packages <- TRUE
  }

  if (install_all_packages) {
    packages <- pkg
  } else {
    packages <- intersect(packages, pkg)
  }

  # set repository for download, depending on source
  if (source == "development") {
    repos <- "https://easystats.r-universe.dev"
  } else if (source == "cran") {
    repos <- getOption("repos")["CRAN"]
  } else {
    # for GitHub, we for now just check if pak is installed
    insight::check_if_installed("pak")
  }

  # only install newer versions?
  if (isFALSE(force) && source == "development") {
    insight::check_if_installed("jsonlite", reason = "to check for updates among development packages")
    if (isTRUE(verbose)) {
      insight::print_color("Looking for newer package versions...\n", "blue")
    }
    # get current CRAN and local versions
    easy_pkgs <- .easystats_version()
    # for development versions, overwrite CRAN version with r-universe version
    for (i in packages) {
      js <- jsonlite::fromJSON(paste0("https://easystats.r-universe.dev/api/packages/", i))
      easy_pkgs$cran[easy_pkgs$package == i] <- js$Version[1]
    }
    easy_pkgs$behind <- package_version(easy_pkgs$cran) > package_version(easy_pkgs$local)
    packages <- easy_pkgs$package[easy_pkgs$behind[stats::na.omit(match(packages, easy_pkgs$package))]]

    if (isTRUE(verbose) && !is.null(packages) && length(packages)) {
      colnames(easy_pkgs) <- c("Package", "Latest", "Installed", "behind")
      easy_pkgs <- easy_pkgs[easy_pkgs$behind, ]
      cat(insight::print_color("\nInstalling following packages:\n\n", "blue"))
      cat(insight::export_table(easy_pkgs[c("Package", "Installed", "Latest")]))
      cat("\n\n")
    }
  }

  if (is.null(packages) || !length(packages)) {
    if (isTRUE(verbose)) {
      insight::print_color("All easystats-packages are up to date!\n", "green")
    }
    return(invisible())
  }

  # for pak, we do not install from a repository
  if (source == "github") {
    pak::pkg_install(file.path("easystats", packages), dependencies = FALSE)
    return(invisible())
  }

  utils::install.packages(packages, repos = repos)
}
