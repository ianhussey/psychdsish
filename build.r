library(roxygen2)
#setwd("~/git/")
#devtools::create("psychdsish")
setwd("~/git/psychdsish")

devtools::document()

devtools::check()
# rcmdcheck::rcmdcheck(args = "--as-cran")

# once you have the package updated, you can use it to build the vignettes, check the whole thing, and reinstall again
devtools::build_vignettes()

devtools::install(build_vignettes = TRUE)

# # or from github, after push
# devtools::install_github("ianhussey/psychdsish")

library(psychdsish)

?psychdsish
vignette("psychdsish")


detach("package:psychdsish", unload = TRUE)


# cran checks
# win-builder
library(devtools)
check_win_devel() # emails results to the maintainer address in DESCRIPTION

# R-hub
library(rhub)
rhub_check() # v2: runs on GitHub Actions in your repo
