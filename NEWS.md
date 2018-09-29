# ExPanDaR 0.2.0.9000

Extensions:

* Removed the requirement for at least one non-numerical variable

* Added by group violin plot

* Included '!' and 'is.na()' as allowed functions for user defined variables

* Introduced the option to change the order of reported components and to exclude selected components


Bug fixes:

* Fixed a sorting bug in long variable definition construction

* Fixed a bug in ExPanD() extreme obs plot (was sorting on grouping variable)


Minor issues:

* Removed the dependency with the CodeDepends package

* Added rio package to the imports list

* Added packages used by ExPanD() to NAMESPACE to make CRAN checks happy

* UI cleanups

* Fixed the definition of oint_ta in r3 dataset

* Added a check to verify that ts_ids provided as ordered vectors 

* Fixed a typo in worldbank_var_def

* Removed the drop_undersore parameter in prepare_regression_table() (no longer needed)

* Work-around for special characters in stargazer column.labels
