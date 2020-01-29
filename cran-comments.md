## Info

This is a minor release that also addresses the following spurious NOTE in CRAN 
checks:

Check: dependencies in R code 
Result: NOTE 
    Namespace in Imports field not imported from: ‘zip’
     All declared Imports should be used. 
Flavors: r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc, r-patched-solaris-x86, r-release-osx-x86_64, r-oldrel-osx-x86_64

This is triggered by calls to the zip package in inst/app directory not being
captured by 'R cmd check', I guess. Fixed by including 'importFrom(zip, zip)' 
call in 'NAMESPACE'.


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
  
