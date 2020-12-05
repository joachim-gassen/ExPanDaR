## Info

This is a minor release that, besides some small tweaks mentioned in NEWS.md,
has removed the dependencies to wbstats and lfe as these packages were removed 
from CRAN and this caused ExPanDaR to be removed from CRAN as well.


## Test environments
* local OS X install, R 3.6.0 (OK)
* local WIN install, R 3.4.3 (OK)
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
  - devel (unstable) (2020-01-28 r77738) (OK)
  - release 3.6.2 (OK) 
  - oldrelease R version 3.5.3 (2019-03-11) (OK)
* rhub
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit (OK)
  - Ubuntu Linux 16.04 LTS, R-release, GCC (OK)
  - Fedora Linux, R-devel, clang, gfortran (OK)


## R CMD check results

0 errors | 0 warnings | 0 note
  
