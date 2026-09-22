# Development and release script for {psychdsish}.
# Run the sections in order; the CRAN section is only needed when releasing.

setwd("~/git/psychdsish")

library(devtools)


# development ------------------------------------------------------------

# regenerate NAMESPACE and the .Rd files from the roxygen comments
document()

# run the test suite (fast; skips the tests that need Quarto unless NOT_CRAN is
# set, which devtools does set)
test()

# test coverage, by function or by file
# covr::package_coverage()
# covr::report()  # opens an annotated HTML report

# style the package's own code
# styler::style_pkg()

# build the vignettes, check everything, and reinstall
build_vignettes()
install(build_vignettes = TRUE)

library(psychdsish)
?psychdsish
vignette("psychdsish")
detach("package:psychdsish", unload = TRUE)


# before a CRAN submission -----------------------------------------------

# 1. bump Version in DESCRIPTION and in CITATION.cff, and add the changes to
#    NEWS.md. Use x.y.0 for new features, x.y.z for fixes.

# 2. the full CRAN-style check, including the PDF manual (needs LaTeX; TinyTeX
#    is enough: tinytex::install_tinytex()). Aim for 0 errors, 0 warnings,
#    0 notes. --run-donttest also runs the \donttest{} examples, as CRAN does.
check(manual = TRUE, args = c("--as-cran", "--run-donttest"))

# 3. spelling: add any correctly spelled new words to inst/WORDLIST
spelling::spell_check_package()
# spelling::update_wordlist()

# 4. URLs: CRAN rejects broken or redirecting links in DESCRIPTION, README,
#    the vignette, and the .Rd files
urlchecker::url_check()

# 5. commit and push, then check that the GitHub Actions R-CMD-check workflow
#    passes on macOS, Windows, and Ubuntu (devel, release, oldrel-1)

# 6. CRAN's own build servers. Each uploads the package and checks it there.
#    win-builder emails the results to the maintainer address in DESCRIPTION
#    (15-30 minutes); mac-builder prints a URL to open (5-10 minutes).
check_win_devel()
check_win_release()
check_mac_release()

# optional: more platforms (e.g. Linux with clang, older R), run on GitHub
# Actions in this repo. rhub_setup() only needs to be run once.
# rhub::rhub_setup()
# rhub::rhub_check()

# 7. record the results of steps 2 and 6 in cran-comments.md, and note there
#    anything a reviewer should know (e.g. why a NOTE is expected). For a new
#    package, the expected NOTE is "New submission", sometimes together with
#    words its spell-checker does not recognise.


# submitting to CRAN -----------------------------------------------------

# builds the source .tar.gz and uploads it to the CRAN submission form, using
# DESCRIPTION and cran-comments.md. CRAN then emails a confirmation link to the
# maintainer address: nothing is submitted until that link is clicked.
submit_cran()

# release() does the same but first walks through a longer checklist
# release()

# to build the .tar.gz without submitting (e.g. to upload it by hand at
# https://cran.r-project.org/submit.html)
# build()

# if the reviewer asks for changes: make them, bump the version (e.g. 0.2.1),
# describe the changes at the top of cran-comments.md, and run submit_cran()
# again. Reply by resubmitting, not by email, unless you are asked a question.


# after acceptance -------------------------------------------------------

# 1. tag the release on GitHub: usethis::use_github_release()
# 2. set date-released (and the DOI, after the Zenodo release) in CITATION.cff
# 3. change the install instructions from remotes::install_github() to
#    install.packages("psychdsish") in README.md, in the README that
#    create_project_skeleton() generates, and in the vignette
