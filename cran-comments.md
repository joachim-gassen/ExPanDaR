## Info

This is a minor release that, besides some small tweaks mentioned in NEWS.md,
has removed the dependencies to wbstats and lfe as these packages were removed 
from CRAN and this caused ExPanDaR to be removed from CRAN as well.

Thus, I receive the following NOTE on checking

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Joachim Gassen <gassen@wiwi.hu-berlin.de>’

New submission

Package was archived on CRAN

Version contains large components (0.5.2)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-12-04 as requires archived package
    'lfe'.


## Test environments
* local OS X install, R 3.6.0 (OK)
* local WIN install, R 3.4.3 (OK)
* local Ubuntu 18.04, 3.6.3 (OK)
* travis-ci: R 3.3, 3.4, release, devel (OK)
* win-builder: 
  - NOTE: Sometimes win-builder generates NOTEs related to libcurl error #35 
    indicating problems with the SSL handshake to shinapps.io URLs 
    (e.g., http://jgassen.shinyapps.io/expand/) that are included in the 
    documentation. This happens for different builds (devel/release/unstable) 
    and is irreproducable to me, meaning that sometimes it happens and sometimes 
    it does not. My guess is that this is related to timeouts when shinyapps.io 
    takes a while to respond (starting the app). In any case, the URLs are 
    accessible and work fine.
  - devel (unstable) (2020-12-04 r79564) (OK)
  - release 4.0.3 (2020-10-10) (OK) 
  - oldrelease R version 3.6.3 (2020-02-29) (OK)
* rhub
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit (OK with R_COMPILE_AND_INSTALL_PACKAGES = "always")
  - Ubuntu Linux 16.04 LTS, R-release, GCC (OK)
  - Fedora Linux, R-devel, clang, gfortran (OK)


## R CMD check results

0 errors | 0 warnings | 0 note
  
