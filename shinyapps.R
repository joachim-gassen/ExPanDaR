# Code to generate the shinyapps.io applications
# Deploy on MacOS (seems to be less prone to stupid locale and enconding issues)
devtools::install_github("joachim-gassen/ExPanDaR")
library(ExPanDaR)

# expand
ExPanD(export_nb_option = TRUE)

# expand_r3
ExPanD(df = russell_3000, df_def = russell_3000_data_def,
       config_list = ExPanD_config_russell_3000,
       abstract = "The data for this sample has been collected from Google Finance and Yahoo Finance.",
       export_nb_option = TRUE)

# expand_wb
ExPanD(worldbank, df_def = worldbank_data_def, var_def = worldbank_var_def,
       config_list = ExPanD_config_worldbank, df_name = "World Bank Data",
       abstract = "The data for this sample has been collected using the <a href=https://data.worldbank.org>World Bank API</a>.".
       export_nb_option = TRUE)
