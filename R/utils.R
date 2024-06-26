.easystats_deps <- function() {
  pkgs <- utils::available.packages()
  deps <- tools::package_dependencies(.packages_on_cran(), pkgs, recursive = FALSE)
  pkg_deps <- unique(sort(unlist(deps)))

  base_pkgs <- c(
    "base", "compiler", "datasets", "graphics", "grDevices", "grid",
    "methods", "parallel", "splines", "stats", "stats4", "tools", "tcltk",
    "utils"
  )
  pkg_deps <- setdiff(pkg_deps, base_pkgs)

  cran_version <- lapply(pkgs[pkg_deps, "Version"], package_version)
  local_version <- lapply(pkg_deps, utils::packageVersion)

  behind <- unlist(Map(">", cran_version, local_version))

  data.frame(
    package = pkg_deps,
    cran = vapply(cran_version, as.character, FUN.VALUE = character(1L)),
    local = vapply(local_version, as.character, FUN.VALUE = character(1L)),
    behind = behind,
    stringsAsFactors = FALSE
  )
}



.easystats_version <- function() {
  pkgs <- tryCatch(
    {
      utils::available.packages(
        contriburl = utils::contrib.url("https://cloud.r-project.org", type = getOption("pkgType"))
      )
    },
    warning = function(w) {
      NULL
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(pkgs)) {
    easystats_pkgs <- .packages_on_github()
    easystats_on_cran <- .packages_on_cran()
    easystats_not_on_cran <- setdiff(easystats_pkgs, easystats_on_cran)

    local_version <- lapply(easystats_on_cran, utils::packageVersion)

    out <- data.frame(
      package = easystats_on_cran,
      cran = NA,
      local = vapply(local_version, as.character, FUN.VALUE = character(1L)),
      behind = FALSE,
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    .add_easystats_dev_pkgs(out, easystats_not_on_cran)
  } else {
    easystats_pkgs <- .packages_on_github()
    easystats_on_cran <- intersect(easystats_pkgs, rownames(pkgs))
    easystats_not_on_cran <- setdiff(easystats_pkgs, easystats_on_cran)

    cran_version <- lapply(pkgs[easystats_on_cran, "Version"], package_version)
    local_version <- lapply(easystats_on_cran, utils::packageVersion)

    behind <- unlist(Map(">", cran_version, local_version))

    out <- data.frame(
      package = easystats_on_cran,
      cran = vapply(cran_version, as.character, FUN.VALUE = character(1L)),
      local = vapply(local_version, as.character, FUN.VALUE = character(1L)),
      behind = behind,
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    .add_easystats_dev_pkgs(out, easystats_not_on_cran)
  }
}


.add_easystats_dev_pkgs <- function(out, easystats_not_on_cran) {
  if (length(easystats_not_on_cran) > 0L) {
    # check if any dev-version is actually installed
    easystats_not_on_cran <- vapply(
      easystats_not_on_cran,
      function(i) {
        p <- try(find.package(i, verbose = FALSE, quiet = TRUE))
        if (!inherits(p, "try-error") && length(p) > 0L) {
          i
        } else {
          ""
        }
      },
      FUN.VALUE = character(1L)
    )

    # remove empty
    easystats_not_on_cran <- easystats_not_on_cran[nzchar(easystats_not_on_cran, keepNA = TRUE)]

    # only check for dev-versions when these are actually installed
    if (length(easystats_not_on_cran) > 0L) {
      local_version_dev <- lapply(easystats_not_on_cran, utils::packageVersion)

      out <- rbind(
        out,
        data.frame(
          package = easystats_not_on_cran,
          cran = NA,
          local = vapply(local_version_dev, as.character, FUN.VALUE = character(1L)),
          behind = FALSE,
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      )
    }
  }

  out
}




.packages_on_cran <- function() {
  c(
    "bayestestR", "correlation", "datawizard", "easystats", "effectsize",
    "insight", "modelbased", "performance", "parameters", "report", "see"
  )
}

.packages_on_github <- .packages_on_cran

.installed_packages <- function() {
  lib.loc <- .libPaths()
  all_pkgs <- list.files(lib.loc)
  all_pkgs[all_pkgs != "_cache"]
}
