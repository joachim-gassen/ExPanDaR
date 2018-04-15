ExPanDaR: Exploring Panel Data with R
================
Joachim Gassen
2018-04-15

Explore Panel Data with R (ExPanDaR)
------------------------------------

You are visiting the github repository of the ExPanDaR (Explore Panel Data with R) package. I develop ExPanDaR to provide the code base for the ExPanD web app. ExPanD is a shiny based app supporting interactive exploratory data analysis.

I designed the ExPanD for the following two purposes:

-   Enable users to assess the robustness of empirical evidence without providing them with access to the underlying data.
-   Provide a toolbox for researchers to explore panel data on the fly.

While I hope that ExPanD might be particularly helpful in the review and publication process I also think that it is convenient for typical exploratory data analysis workflows. In addition, it has already proven helpful to me in the classroom on multiple occasions.

This is what ExPanD looks like:

<img src="vignettes/figures/ExPanD_simple_03.png" width="90%" style="display: block; margin: auto;" />

If you are interested to see what ExPanD has to offer without diving into R, click [here](https://jgassen.shinyapps.io/expand/) to explore an instance of ExPanD that hosts an international panel of financial accounting and stock returns data.

If you want to analyze your own panel data instead, you can also access a variant of ExPanD app [here](https://jgassen.shinyapps.io/expand/) that allows user-side data uploads.

Finally, if you are in for the full treat and want to test ExPanD from within R, run the following in your R session to start exploring World Bank data.

``` r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("joachim-gassen/ExPanDaR")
library(ExPanDaR)

ExPanD(df = worldbank,  
       df_def = worldbank_data_def, 
       var_def = worldbank_var_def,
       df_name = "World Bank Data",
       config_list = ExPanD_config_worldbank)
```

To learn more about how to use the ExPanD app and its accompanying functions, read the vignettes that come with the ExPanDaR package:

-   [Using ExPanD for Panel Data Exploration](%22vignettes/use_ExPanD.md%22)
-   [Using the functions of the ExPanDaR package](%22vignettes/ExPanDaR-functions.md%22)

Enjoy!
