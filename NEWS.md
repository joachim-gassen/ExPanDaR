# ExPanDaR 0.5.0

Extensions:

* Modified ExPanD() to process cross-sectional data with useful defaults

* Implemented an export option that allows users to download a 
zip file containing the data and a R notebook based variant of the ExPanD
analysis (`export_nb_option = TRUE`)

Minor issues:

* Added an option 'binary' to prepare_missing_values_graph() to visualize whether any values are missing

* Typo fixes


# ExPanDaR 0.4.0

Extensions:

* Introduced `html_block` tag in `components` to allow users to add self-designed
html content to `ExPanD()`

* Added `sample_selection`, `subset_factor`, `grouping` and `udvars` variables 
to `components` in `ExPanD()` so that these parts can also be re-arranged 
or omitted

* Allowed user defined variables to be built on analysis variables and used
in simple_mode

* Added new vignette to explain new features


Bug fixes:

* Made Stata import more robust

* Fixed hover code in correlation and scatter plots to adjust for `img_css_ratio`


Minor issues:

* Removed non-ASCII characters from NAMESPACE

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

* Introduced the option to change the order of reported components and to exclude selected components

* Added clustered standard errors and fixed effects for logit models in `prepare_regression_table()`

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

* Added a check to verify that `ts_ids` provided as ordered vectors 

* Fixed a typo in `worldbank_var_def`

* Removed the `drop_undersore` parameter in `prepare_regression_table()` (no longer needed)

* Work-around for special characters in stargazer `column.labels`

* Marginally improved error handling for user provided data files

* Allowed `treat_outliers()` to route parameters through to `stats::quantile()` (needed for type parameter)

* Changed `NA` handling in `prepare_graph` type functions


# ExPanDaR 0.2.0

* Initial version on CRAN

