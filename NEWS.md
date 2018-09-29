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

<<<<<<< HEAD
* Fixed a typo in worldbank_var_def

=======
* Added a check to verify that ts_ids provided as ordered vectors 
>>>>>>> 11b4f4b9f6e17a1649a85fcfc2982013fe70f402

