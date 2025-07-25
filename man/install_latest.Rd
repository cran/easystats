% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install_easystats.R
\name{install_latest}
\alias{install_latest}
\title{Install the easystats suite from R-universe (GitHub) or CRAN}
\usage{
install_latest(
  source = "development",
  packages = "all",
  force = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{source}{Character. Either \code{"development"}, \code{"cran"} or \code{"github"}. If
\code{"cran"}, packages will be installed from the default CRAN mirror returned
by \code{getOption("repos")['CRAN']}. If \code{"development"} (the default), packages
are installed from the R-universe repository
(\url{https://easystats.r-universe.dev/}). \code{"github"} installs the latest
version from the GitHub-repositories main-branch. However, this option is
usually not needed, because R-universe provides latest binaries. Use the
\code{"github"} option only when R-universe servers are unavailable.}

\item{packages}{Character vector, indicating which packages to be installed.
By default, the option \code{"all"} will install all \strong{easystats} packages.}

\item{force}{Logical, if \code{FALSE}, only those packages with a newer
version number will be installed. Use \code{force=TRUE} to force
installation of all packages, even if the version number for the locally
installed packages is identical to the latest development-versions. Only
applies when \code{source="development"}.}

\item{verbose}{Toggle messages.}
}
\value{
Invisible \code{NULL}.
}
\description{
This function can be used to install all the \strong{easystats} packages, either
latest development versions (from R-universe/GitHub) or the current
versions from CRAN. If the development versions are installed, packages
will be installed from the stable branch (master/main) for each package.
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# install latest development-version of easystats packages from
# the r-universe repository, but only those packages that have newer
# versions available
install_latest()

# install all latest development-version of easystats packages from
# the r-universe repository, no matter whether local installations
# are up to date or not.
install_latest(force = TRUE)
\dontshow{\}) # examplesIf}
}
