# ExPanDaR 0.5.3.9000

Extensions:

* Added a `save_settings_option` to `ExPanD()` defaulting to `TRUE` so that users can start the app without the settings dialog at the bottom (issue #8)


Minor Issues:

* Moved to `fixest` and `modelsummary` for `prepare_regression_table()`. Both packages are faster, more versatile and more actively supported than their predecessors `lfe` and `stargazer`. Please note that the move to `modelsummary` implies changes in the table layout, meaning that it might break downstream code that depends on the table structure, e.g. code that customizes latex output. 


# ExPanDaR 0.5.3

Bug fixes:

* Fixed bug that caused stargazer not to render plain OLS models


# ExPanDaR 0.5.2

Extensions:

* Added a `prepare_by_group_trend_graph()` function to plot time trends by
grouping variable

* Included grouped time trend plots in `ExPanD()`


Bug fixes:

* Fixed notebook code for sub sampling (failed on sampling variables containing `NA`s) 

* Fixed a small issue in the Notebook generation code for the time trend and
quantile trend chunks

* Fixed display of `prepare_regression_table()` for logit model by group tables
with fixed effects


Minor Issues:

* Replaced `lfe` with `plm` package for fixed effect models as `lfe` has been
removed from CRAN

* Removed `wbstats` from Suggests as it is currently not on CRAN

* Removed `tidyquant` package from Suggests as it depends on a package that
requires R >= 3.5.0

* Spelling


# ExPanDaR 0.5.1

Extensions:

* Added a vignette to explain the notebook export option of `ExPanD()` and the 
resulting notebook


Bug fixes:

* Fixed bug that stopped `ExPanD()` displays from rendering on user uploaded 
panel data


Minor Issues: 

* Updated World Bank data

* Allowed `ExPanD()` to be called with `ts_id == NULL` and `cs_id != NULL` so
that data frame variables can be specified as cross-sectional identifiers
in the cross-sectional mode

* Removed some artefacts in cross-sectional mode where cross-sectional and
time series identifiers were still provided as variable options in the 
`ExPanD()` app

* Included manual zip package import in `NAMESPACE` to make `devtoools::check()` 
happy on various CRAN systems

* Changed shinyapps.io URLs in vignettes to plain HTTP as win-builder seems to
have an issue with the SSL handshake of shinyapps.io


# ExPanDaR 0.5.0

Extensions:

* Modified `ExPanD()` to process cross-sectional data with useful defaults

* Implemented an export option that allows users to download a 
zip file containing the data and a R notebook based variant of the ExPanD
analysis (`export_nb_option = TRUE`)


Minor issues:

* Added an option 'binary' to `prepare_missing_values_graph()` to visualize 
whether any values are missing

* Typo fixes


# ExPanDaR 0.4.0

Extensions:

* Introduced `html_block` tag in `components` to allow users to add 
self-designed html content to `ExPanD()`

* Added `sample_selection`, `subset_factor`, `grouping` and `udvars` variables 
to `components` in `ExPanD()` so that these parts can also be re-arranged 
or omitted

* Allowed user defined variables to be built on analysis variables and used
in simple_mode

* Added new vignette to explain new features


Bug fixes:

* Made Stata import more robust

* Fixed hover code in correlation and scatter plots to adjust for 
`img_css_ratio`


Minor issues:

* Removed non-ASCII characters from `NAMESPACE`

* Allowed more flexible handling of binary variables in `ExPanD()`

* Added additional examples to Github repository (not in package)

* Updated World Bank data

* Switched to openssl package (Thank you to Jeroen Ooms for the PR)

* Marginally improved error checking for config upload


# ExPanDaR 0.3.0

Extensions:

* Removed the requirement for at least one non-numerical variable

* Added by group violin plot

* Included `!` and `is.na()` as allowed functions for user defined variables

* Introduced the option to change the order of reported components and to 
exclude selected components

* Added clustered standard errors and fixed effects for logit models in 
`prepare_regression_table()`

* Implemented binary response logit models in `ExPanD()`


Bug fixes:

* Fixed a sorting bug in long variable definition construction

* Fixed a bug in `ExPanD()` extreme obs plot (was sorting on grouping variable)


Minor issues:

* Removed the dependency with the CodeDepends package

* Added rio package to the imports list

* Added packages used by `ExPanD()` to `NAMESPACE` to make CRAN checks happy

* UI cleanups

* Fixed the definition of `oint_ta` in `r3` dataset

* Added a check to verify that `ts_id` is provided as an ordered vector 

* Fixed a typo in `worldbank_var_def`

* Removed the `drop_undersore` parameter in `prepare_regression_table()` 
(no longer needed)

* Work-around for special characters in stargazer `column.labels`

* Marginally improved error handling for user provided data files

* Allowed `treat_outliers()` to route parameters through to `stats::quantile()` 
(needed for type parameter)

* Changed `NA` handling in `prepare_graph` type functions


# ExPanDaR 0.2.0

* Initial version on CRAN

