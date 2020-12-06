## Info

Apologies, but this is a quick bug fix release as a CRAN check fix 
(adding 'stats' package reference to lm() call) broke downstream behavior 
in stargazer::stargazer() without me noticing. This is a breaking bug that
needs fixing quickly.

So, you will see the NOTE: Days since last update: 1

Sorry!


## Test environments
* local OS X install, R 3.6.0 (OK)
* local WIN install, R 3.4.3 (OK)
* local Ubuntu 18.04, R 3.6.3 (OK)
* travis-ci: R 3.3, 3.4, release, devel (OK)
* win-builder: 
  - NOTE: Sometimes win-builder generates NOTEs related to libcurl 
    error #35 indicating problems with the SSL handshake to shinapps.io URLs 
    (e.g., http://jgassen.shinyapps.io/expand/) that are included in the 
    documentation. This happens for different builds (devel/release/unstable) 
    and is irreproducable to me, meaning that sometimes it happens and
    sometimes it does not. My guess is that this is related to timeouts 
    when shinyapps.io takes a while to respond (starting the app).
    In any case, the URLs are accessible and work fine.
  - devel (unstable) (2020-12-04 r79564) (OK)
  - release 4.0.3 (2020-10-10) (OK) 
  - oldrelease R version 3.6.3 (2020-02-29) (OK)
* rhub
  - Currenty, rhub acts up with the Unix based builts. I got a set of
    PREERROR messages from Ubuntu builds because of dependencied that could
    not be installed on the images (e.g., 'openssl', 'roxygen', 'xml2',
    'kableExtra'). As my local checks and the travis-ci builds are OK I 
    uess that we are fine.
  - Debian Linux, R-devel, clang, ISO-8859-15 locale (OK)



## R CMD check results

0 errors | 0 warnings | 0 note
  
