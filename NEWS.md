# easystats 0.7.5

* New function `easystats_citations()` to report the total number of Google
  Scholar citations for `easystats` publications.

* The `source` argument in `install_latest()` gets a `"github"` option, to
  install the latest version from the GitHub-repositories main-branch. Usually,
  this is not needed, because r-universe provides latest binaries.

# easystats 0.7.4

* Added a new vignette, _A complete workflow using easystats_.

* `install_suggested()` did not install required hard-dependencies for
  suggested packages. This is fixed now, thanks to @viv-analytics.

* Minor changes, fixes to broken URLs.

# easystats 0.7.3

* New function `easystats_packages()` that produces a character vector with
  the names of all packages in the `easystats` ecosystem (#417).

* Updated `citation()` information.

* `easystats_update()` and `install_suggested()` now use the _pak_ package, if
  this is installed.

# easystats 0.7.2

* Add a vignette on the R version policy for all packages in the **easystats**
  ecosystem (#403).

* Fixed CRAN check issues.

# easystats 0.7.1

## Bug fix

* Fixed issue with detecting correct version of development packages when version
  numbers on CRAN and GitHub were identical.

# easystats 0.7.0

## Major Changes

- Given their significant contributions throughout the ecosystem, the package
  gains two new authors: [Etienne Bacher](https://github.com/etiennebacher)
  and [Rémi Thériault](https://github.com/rempsyc). Welcome on-board!

- This release changes the licensing model of `{easystats}` to an MIT license.

- All component packages have been bumped to their latest versions.

## Minor Changes

- `model_dashboard()` gains a new argument `browse_html` to control if the
  rendered dashboard is opened in the browser.

# easystats 0.6.0

## Breaking Changes

- The minimum needed R version has been bumped to `3.6`.

* Following functions have been removed since they are not expected to be of
  any relevance for the users:

  - `CRAN_checks()`
  - `on_CRAN()`

## Major Changes

* All hard dependencies have been bumped to their latest versions.

* The `model_dashboard()` is now less verbose when some of the various functions
  do not support the provided model class, to keep the output clean.

* The `model_dashboard()` now better captures errors and providing more
  informative messages on how to seek help.

* Fixed NOTEs in CRAN checks.

# easystats 0.5.2

* Initial CRAN submission.

# easystats 0.5.1

* `install_latest()` gains a `force` argument, to either force all packages
  to be updates, or only update locally installed packages where newer
  versions are available.

# easystats 0.5.0

* Adds new `model_dashboard()` function to generate a dashboard with regression
  model summary from `{easystats}`.

* Bumps needed minimum R version to `3.5`.

* Updates minimum needed versions of all packages to the latest versions.

# easystats 0.4.1

* Adds *datawizard* package.

* Adds a new function `install_suggested()` to install suggested packages.

# easystats 0.4.0

* Includes a new package for automated reporting of statistical analyses:
  `report`.

* Moves `rvest`, `xml2`, and `remotes` from `Imports` to `Suggests`.

# easystats 0.3.0

* Revised startup-messages, so these can be suppressed by using
  `suppressPackageStartupMessages()`.

* The CRAN-check for errors or warnings in `easystats`-packages is no longer
  shown by default on startup. Use `easystats::check_cran_status()` to see them.
