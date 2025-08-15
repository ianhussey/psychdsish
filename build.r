
library(roxygen2)
#setwd("~/git/")
#devtools::create("psychdsish")
setwd("~/git/psychdsish")

devtools::document()

devtools::check(vignettes = FALSE)

#devtools::install()
# or from github, after push
devtools::install_github("ianhussey/psychdsish")

library(psychdsish)

?psychdsish

detach("package:psychdsish", unload=TRUE)

# once you have the package updated, you can use it to build the vignettes, check the whole thing, and reinstall again
devtools::build_vignettes()
devtools::check()

