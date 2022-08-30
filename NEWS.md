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
  shown by default on startup. Use `easystats::CRAN_checks()` to see them.
