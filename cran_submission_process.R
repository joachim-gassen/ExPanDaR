# So, you think that you are done and ready to release a new version to CRAN?

# Run a local check on your main development environment

devtools::check(args = c('--as-cran'))

# Check whether NEWS.md contains all material changes

# Do a spell-check because, why not?

devtools::spell_check()

# Build and install your local package to make sure that it is being
# used to build the documentation

# Build the final documentation for the development version

pkgdown::build_site()

# Check whether all vignettes and help pages render as expected
# (aka as poor man testing)

# Check the examples in 'ExPanDaR_examples.R'. Do they work as expected?

# Commit the current status of the pre-release candidate to Github.
# Use a message like "getting ready for CRAN".

# Deploy the shinyapps.io ExPanD versions for the pre-release candidate
# by installing it from Github and then running the code that is
# included in 'shinyapps.R'

# If everything is well (if not fix and start over) it is time to prepare
# the release version. First, bump the version number in DESCRIPTION
# and NEWS.md

# Build and install the release candidate locally.

# Build the new documentation for the release candidate

pkgdown::build_site()

# Run another local check on your main development environment as
# you might have broken something..

devtools::check(args = c('--as-cran'))

# Test windows platforms on win_builder

devtools::check_win_oldrelease()
devtools::check_win_release()
devtools::check_win_devel()

# Test unix platform on rhub - this tends to block the console

devtools::check_rhub(platforms = c(
  "ubuntu-gcc-release", "ubuntu-gcc-devel", "linux-x86_64-rocker-gcc-san",
  "debian-clang-devel"
))

# In the meantime, you can start drafting the 'cran-comments.md' file.

# Win builder is likely to cause wrong positives about SSL handshakes
# with shinyapps.io. See the notes in cran-comments.md about that.
# Everything else should ho smoothly. If not, rinse and repeat ;-)

# Once you are happy with the check feedback is it time to commit the
# release candidate to Github.

# After committing, pull the release candidate to your other development
# environments and run local checks there. Document the success in
# cran-comments.md.

devtools::check(args = c('--as-cran'))

# Wait to hear what Travis has to say and document the success in
# cran-comments.md.

# Time to release!

devtools::release()

# After the release was accepted to CRAN, tag the GitHub commit.

# Then delete CRAN-RELEASE file created by 'devools' and version bump
# DESCRIPTION and NEWS.md to their development version (add .9000) and prepare
# the documentation for the new development version.

pkgdown::build_site()

# Commit to github and have fun finding the first bug in the released version!
