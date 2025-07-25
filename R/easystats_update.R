#' Update *easystats*-packages and its dependencies from CRAN, if necessary.
#'
#' @param which String, indicates whether easystats-packages (`which = "core"`),
#'   dependencies (`which = "deps"`) or both (`which = "all"`) should be
#'   checked for available updates.
#'
#' @return Invisible `NULL`.
#'
#' @details If package `{pak}` is installed, `pak::pkg_install()` will be used
#' to install packages. Else, `utils::install.packages()` is used.
#'
#' @examplesIf FALSE
#' # check which local easystats-packages (and their dependencies)
#' # are out of date and install updates from CRAN
#' easystats_update()
#'
#' # update only easystats core-packages
#' easystats_update("core")
#'
#' @export
easystats_update <- function(which = "all") {
  which <- insight::validate_argument(which, c("all", "core", "deps"))

  if (which %in% c("all", "core")) {
    core <- .easystats_version()
    behind <- core[core$behind, ]

    if (nrow(behind) == 0) {
      insight::print_color("All easystats-packages are up to date!\n", "green")
      return(invisible())
    }

    message("The following packages are out of date:")
    message(paste0(" * ", format(behind$package), " (", behind$local, " -> ", behind$cran, ")", collapse = "\n"))

    message("Update now?")
    do_it <- utils::menu(c("Yes", "No")) == 1L

    if (!do_it) {
      return(invisible())
    }

    # detach packages before installing
    lapply(behind$package, unloadNamespace)

    if (insight::check_if_installed("pak", quietly = TRUE)) {
      pak::pkg_install(behind$package)
    } else {
      utils::install.packages(
        behind$package,
        quiet = TRUE,
        dependencies = FALSE
      )
    }
  }

  if (which %in% c("all", "deps")) {
    deps <- .easystats_deps()
    behind <- deps[deps$behind, ]

    if (nrow(behind) == 0) {
      insight::print_color("All easystats-dependencies are up to date!\n", "green")
      return(invisible())
    }

    message("The following packages are out of date:")
    message(" * ", format(behind$package), " (", behind$local, " -> ", behind$cran, ")", collapse = "\n")

    message("Update now?")
    do_it <- utils::menu(c("Yes", "No")) == 1

    if (!do_it) {
      return(invisible())
    }

    # detach packages before installing
    lapply(behind$package, unloadNamespace)

    if (insight::check_if_installed("pak", quietly = TRUE)) {
      pak::pkg_install(behind$package)
    } else {
      utils::install.packages(
        behind$package,
        quiet = TRUE,
        dependencies = FALSE
      )
    }
  }

  invisible()
}
