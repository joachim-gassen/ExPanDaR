---
title: "Using the functions of the ExPanDaR package"
author: "Joachim Gassen"
date: "2018-04-17"
output: 
  html_document:
    keep_md: TRUE
---



While the main purpose of the ExPanDaR package is to provide the infrastructure for the ExPanD app, the auxiliary functions of the package can also be used to support your exploratory data analysis workflow in your own code. All functions are relatively thin wrappers around established R packages for graphics and HTML table presentation (ggplot2, kableExtra, stargazer). 
While I developed them to support analysis steps that are common with empirical archival research projects in the area of accounting and finance (which happens to be my field),
I hope that they are generally useful for exploratory data analysis.

To see what ExPanDaR has to offer, let's take a quick tour. For more detailed guidance on how to use a specific function presented below, take a look at the respective function's help page.

## Data Preparation

ExPanDaR is designed for exploratory panel data analysis (hence the name).
Thus, while you can also use some functions on cross-sectional data, for most functions you will need a data frame containing your panel data. 
ExPanDaR expects the data to be organized in long format. 
This implies that each observation (a row) is identified by cross-sectional and time series identifiers and that variables are organized by columns. 
While you can have a vector of variables jointly determining the cross-section, the time-series needs to be identified by a unique variable. 
The ExPanDaR functions treat cross-sectional identifiers as factors and expect the time-series identifier to be coercible into an ordered factor.

For this walk-through I will use the data set russell_3000, which comes with the package. 
It contains some financial reporting and stock return data of Russell 3000 firms from Google Finance and Yahoo Finance and has been collected using the tidyquant package in the summer of 2017. 
A word of caution: While the data appears to be relatively decent quality I would advise against using this data for scientific work without verifying its integrity first. 
These are the variables included in the data.


```r
kable(data.frame(Variable=russell_3000_data_def$var_name, 
                 Definition=russell_3000_data_def$var_def), row.names = FALSE, escape = TRUE) 
```



|Variable   |Definition                                                                      |
|:----------|:-------------------------------------------------------------------------------|
|coid       |Company identifier                                                              |
|period     |Fiscal year                                                                     |
|coname     |Company name                                                                    |
|sector     |Sector                                                                          |
|industry   |Industry                                                                        |
|toas       |Total assets at period end (M US-$)                                             |
|sales      |Sales of the period (M US-$)                                                    |
|equity     |Total equity at period end (M US-$)                                             |
|debt_ta    |Total debt (% total assets)                                                     |
|eq_ta      |Total equity (% total assets)                                                   |
|gw_ta      |Goodwill (% total assets)                                                       |
|oint_ta    |Other intangible assets (% total assets)                                        |
|ppe_ta     |Property, plant and equipment (% total assets)                                  |
|ca_ta      |Current assets (% total assets)                                                 |
|cash_ta    |Cash (% total assets)                                                           |
|roe        |Return on equity (net income divided by average equity)                         |
|roa        |Return on assets (earnings before interest and taxes divided by average equity) |
|nioa       |Net income divided by average assets                                            |
|cfoa       |Cash flow from operations divided by average assets                             |
|accoa      |Total accruals divided by average assets                                        |
|cogs_sales |Cost of goods sold divided by sales                                             |
|ebit_sales |Earnings before interest and taxes divided by sales                             |
|ni_sales   |Net income divided by sales                                                     |
|return     |Stock return of the period (%)                                                  |

You can infer from the variable definition that `coid` seems to identify the cross-section (a Russell 3000 firm) while `period` identifies the time-series (a fiscal year).
In addition, `coname` also sounds like it mighty identify a firm but we cannot be sure whether there are duplicate company names. 
In addition, we want to verify that there are no duplicate `coid`/`period` pairs.
Let's check.


```r
cs_ids <- unique(russell_3000[,c("coid", "coname")])
identical(cs_ids$coid, unique(russell_3000$coid))
```

```
## [1] TRUE
```

```r
identical(cs_ids$coname, unique(russell_3000$coname))
```

```
## [1] TRUE
```

The first test verifies that there are no two observations that share the same `coid` but a different `coname`.
The second makes sure that there are firms with the same `coname` but a different `coid`. Thus, we can use both, `coname` and `coid`, or either as cross-sectional identifier.

The following test establishes whether in combination `coid` and `period` identify a panel observation.  


```r
any(duplicated(russell_3000[,c("coid", "period")]))
```

```
## [1] FALSE
```

This seems to be the case. 

As a next step, let's use ExPanDaR's function `prepare_missing_values_graph()` to eyeball how frequently observations are missing in the data set.


```r
prepare_missing_values_graph(russell_3000, ts_id = "period")
```

<img src="figure/missing_obs-1.png" title="plot of chunk missing_obs" alt="plot of chunk missing_obs" style="display: block; margin: auto;" />

OK. This does not look too bad. Only FY2013 seems odd, as some variables are completely missing. Guess why? They are calculated using lagged values of total assets. So, in the following, let's focus on the variables that we care about and on the fiscal years 2014 to 2016 (a short panel, I know). Time to check the descriptive statistics using the `prepare_descriptive_table()` function.


```r
r3 <- droplevels(russell_3000[russell_3000$period > "FY2013",
                              c("coid", "coname", "period", "sector", "toas",
                                "nioa", "cfoa", "accoa", "return")])
t <- prepare_descriptive_table(r3)
t$kable_ret  %>%
  kable_styling("condensed", full_width = F, position = "center")
```

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Descriptive Statistics</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Std. dev. </th>
   <th style="text-align:right;"> Min. </th>
   <th style="text-align:right;"> 25 % </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> 75 % </th>
   <th style="text-align:right;"> Max. </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> toas </td>
   <td style="text-align:right;"> 6,643 </td>
   <td style="text-align:right;"> 8,722.510 </td>
   <td style="text-align:right;"> 35,846.713 </td>
   <td style="text-align:right;"> 0.800 </td>
   <td style="text-align:right;"> 463.225 </td>
   <td style="text-align:right;"> 1,600.050 </td>
   <td style="text-align:right;"> 5,100.500 </td>
   <td style="text-align:right;"> 861,395.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nioa </td>
   <td style="text-align:right;"> 6,399 </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:right;"> 0.096 </td>
   <td style="text-align:right;"> -2.692 </td>
   <td style="text-align:right;"> -0.002 </td>
   <td style="text-align:right;"> 0.017 </td>
   <td style="text-align:right;"> 0.037 </td>
   <td style="text-align:right;"> 0.463 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cfoa </td>
   <td style="text-align:right;"> 6,399 </td>
   <td style="text-align:right;"> 0.033 </td>
   <td style="text-align:right;"> 0.084 </td>
   <td style="text-align:right;"> -2.121 </td>
   <td style="text-align:right;"> 0.021 </td>
   <td style="text-align:right;"> 0.041 </td>
   <td style="text-align:right;"> 0.065 </td>
   <td style="text-align:right;"> 0.460 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> accoa </td>
   <td style="text-align:right;"> 6,399 </td>
   <td style="text-align:right;"> -0.032 </td>
   <td style="text-align:right;"> 0.052 </td>
   <td style="text-align:right;"> -0.712 </td>
   <td style="text-align:right;"> -0.046 </td>
   <td style="text-align:right;"> -0.026 </td>
   <td style="text-align:right;"> -0.012 </td>
   <td style="text-align:right;"> 0.621 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> return </td>
   <td style="text-align:right;"> 6,009 </td>
   <td style="text-align:right;"> 0.097 </td>
   <td style="text-align:right;"> 0.433 </td>
   <td style="text-align:right;"> -0.938 </td>
   <td style="text-align:right;"> -0.136 </td>
   <td style="text-align:right;"> 0.065 </td>
   <td style="text-align:right;"> 0.269 </td>
   <td style="text-align:right;"> 6.346 </td>
  </tr>
</tbody>
</table>


Take a look at the minima and the maxima of some of the variables (e.g., net income over assets (`nioa`)). Normally, it should be around -50 % to + 50%. Our measure has a minimum way below -50 %. One thing that comes very handy when dealing with outliers is a quick way to observe extreme values. `prepare_ext_obs_table()` might be helpful here.


```r
t <- prepare_ext_obs_table(na.omit(r3[c("coname", "period", "nioa")]))
t$kable_ret %>%
  kable_styling("condensed", full_width = F, position = "center")
```

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> coname </th>
   <th style="text-align:left;"> period </th>
   <th style="text-align:right;"> nioa </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Gamco Investors, Inc. </td>
   <td style="text-align:left;"> FY2016 </td>
   <td style="text-align:right;"> 0.463 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Gaia, Inc. </td>
   <td style="text-align:left;"> FY2016 </td>
   <td style="text-align:right;"> 0.369 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Five Prime Therapeutics, Inc. </td>
   <td style="text-align:left;"> FY2015 </td>
   <td style="text-align:right;"> 0.355 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NewLink Genetics Corporation </td>
   <td style="text-align:left;"> FY2014 </td>
   <td style="text-align:right;"> 0.318 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ligand Pharmaceuticals Incorporated </td>
   <td style="text-align:left;"> FY2015 </td>
   <td style="text-align:right;"> 0.302 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:right;"> ... </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Proteostasis Therapeutics, Inc. </td>
   <td style="text-align:left;"> FY2015 </td>
   <td style="text-align:right;"> -0.822 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Proteostasis Therapeutics, Inc. </td>
   <td style="text-align:left;"> FY2014 </td>
   <td style="text-align:right;"> -0.830 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Omeros Corporation </td>
   <td style="text-align:left;"> FY2015 </td>
   <td style="text-align:right;"> -1.255 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vTv Therapeutics Inc. </td>
   <td style="text-align:left;"> FY2014 </td>
   <td style="text-align:right;"> -1.269 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Omeros Corporation </td>
   <td style="text-align:left;"> FY2014 </td>
   <td style="text-align:right;"> -2.692 </td>
  </tr>
</tbody>
</table>

In a real life research situation, you might want to take a break and check your data as well as the actual financial statements to see what is going on. 
In most cases, you will see that the outliers are caused by very small denominators (average total assets in this case). To reduce the effect of these outliers on your analysis, you can winsorize (or truncate) them by using the `treat_outliers()` function.


```r
r3win <- treat_outliers(r3, percentile = 0.01)
t <- prepare_ext_obs_table(na.omit(r3win[c("coname", "period", "nioa")]))
t$kable_ret %>%
  kable_styling("condensed", full_width = F, position = "center")
```

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> coname </th>
   <th style="text-align:left;"> period </th>
   <th style="text-align:right;"> nioa </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> ABIOMED, Inc. </td>
   <td style="text-align:left;"> FY2015 </td>
   <td style="text-align:right;"> 0.147 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Acacia Communications, Inc. </td>
   <td style="text-align:left;"> FY2015 </td>
   <td style="text-align:right;"> 0.147 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Acacia Communications, Inc. </td>
   <td style="text-align:left;"> FY2016 </td>
   <td style="text-align:right;"> 0.147 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Aspen Technology, Inc. </td>
   <td style="text-align:left;"> FY2015 </td>
   <td style="text-align:right;"> 0.147 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Aspen Technology, Inc. </td>
   <td style="text-align:left;"> FY2016 </td>
   <td style="text-align:right;"> 0.147 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:left;"> ... </td>
   <td style="text-align:right;"> ... </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Workhorse Group, Inc. </td>
   <td style="text-align:left;"> FY2015 </td>
   <td style="text-align:right;"> -0.355 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Workhorse Group, Inc. </td>
   <td style="text-align:left;"> FY2016 </td>
   <td style="text-align:right;"> -0.355 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EXCO Resources NL </td>
   <td style="text-align:left;"> FY2015 </td>
   <td style="text-align:right;"> -0.355 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ZIOPHARM Oncology Inc </td>
   <td style="text-align:left;"> FY2015 </td>
   <td style="text-align:right;"> -0.355 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ZIOPHARM Oncology Inc </td>
   <td style="text-align:left;"> FY2016 </td>
   <td style="text-align:right;"> -0.355 </td>
  </tr>
</tbody>
</table>


## Descriptive Statistics

This looks better. Let's look at the winsorized descriptive statistics.


```r
t <- prepare_descriptive_table(r3win)
t$kable_ret  %>%
  kable_styling("condensed", full_width = F, position = "center")
```

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Descriptive Statistics</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> N </th>
   <th style="text-align:right;"> Mean </th>
   <th style="text-align:right;"> Std. dev. </th>
   <th style="text-align:right;"> Min. </th>
   <th style="text-align:right;"> 25 % </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> 75 % </th>
   <th style="text-align:right;"> Max. </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> toas </td>
   <td style="text-align:right;"> 6,643 </td>
   <td style="text-align:right;"> 7,198.735 </td>
   <td style="text-align:right;"> 17,632.076 </td>
   <td style="text-align:right;"> 45.400 </td>
   <td style="text-align:right;"> 463.225 </td>
   <td style="text-align:right;"> 1,600.050 </td>
   <td style="text-align:right;"> 5,100.500 </td>
   <td style="text-align:right;"> 122,418.180 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nioa </td>
   <td style="text-align:right;"> 6,399 </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 0.077 </td>
   <td style="text-align:right;"> -0.355 </td>
   <td style="text-align:right;"> -0.002 </td>
   <td style="text-align:right;"> 0.017 </td>
   <td style="text-align:right;"> 0.037 </td>
   <td style="text-align:right;"> 0.147 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cfoa </td>
   <td style="text-align:right;"> 6,399 </td>
   <td style="text-align:right;"> 0.034 </td>
   <td style="text-align:right;"> 0.068 </td>
   <td style="text-align:right;"> -0.280 </td>
   <td style="text-align:right;"> 0.021 </td>
   <td style="text-align:right;"> 0.041 </td>
   <td style="text-align:right;"> 0.065 </td>
   <td style="text-align:right;"> 0.178 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> accoa </td>
   <td style="text-align:right;"> 6,399 </td>
   <td style="text-align:right;"> -0.032 </td>
   <td style="text-align:right;"> 0.043 </td>
   <td style="text-align:right;"> -0.209 </td>
   <td style="text-align:right;"> -0.046 </td>
   <td style="text-align:right;"> -0.026 </td>
   <td style="text-align:right;"> -0.012 </td>
   <td style="text-align:right;"> 0.092 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> return </td>
   <td style="text-align:right;"> 6,009 </td>
   <td style="text-align:right;"> 0.088 </td>
   <td style="text-align:right;"> 0.375 </td>
   <td style="text-align:right;"> -0.707 </td>
   <td style="text-align:right;"> -0.136 </td>
   <td style="text-align:right;"> 0.065 </td>
   <td style="text-align:right;"> 0.269 </td>
   <td style="text-align:right;"> 1.568 </td>
  </tr>
</tbody>
</table>

I am sure that you won't care but I am a big fan of correlation tables. `prepare_correlation_table()` prepares a table reporting Pearson correlations above and Spearman correlations below the diagonal.


```r
t<- prepare_correlation_table(r3win, bold = 0.01, format="html")
t$kable_ret %>%
  kable_styling("condensed", full_width = F, position = "center")
```

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> A </th>
   <th style="text-align:right;"> B </th>
   <th style="text-align:right;"> C </th>
   <th style="text-align:right;"> D </th>
   <th style="text-align:right;"> E </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> A: toas </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.10</span> </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.07</span> </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.08</span> </td>
   <td style="text-align:right;"> <span style="     ">-0.00</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> B: nioa </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.17</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.80</span> </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.49</span> </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.15</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> C: cfoa </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.10</span> </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.66</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">-0.09</span> </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.12</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> D: accoa </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.20</span> </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.38</span> </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">-0.30</span> </td>
   <td style="text-align:right;">  </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.07</span> </td>
  </tr>
  <tr>
   <td style="text-align:left;"> E: return </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.04</span> </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.20</span> </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.14</span> </td>
   <td style="text-align:right;"> <span style=" font-weight: bold;    ">0.08</span> </td>
   <td style="text-align:right;">  </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; border: 0;" colspan="100%">
<sup></sup> This table reports Pearson correlations above and Spearman correlations below the diagonal. The number of observations ranges from 6002 to 6643. Correlations with significance levels below 1% appear in bold print.</td></tr></tfoot>
</table>

In fact, I like correlations so much that especially for samples containing many variables I use `prepare_correlation_graph()` to display a graphic variant based on the corrplot package. See for yourself. 


```r
ret <- prepare_correlation_graph(r3win)
```

<img src="figure/correlation_graph-1.png" title="plot of chunk correlation_graph" alt="plot of chunk correlation_graph" style="display: block; margin: auto;" />



## Visuals

Additional visuals are available for exploring time trends. `prepare_trend_graph()` can be used for comparing variables... 


```r
graph <- prepare_trend_graph(r3win[c("period", "nioa", "cfoa", "accoa")], "period")
graph$plot
```

<img src="figure/time_trend_plot-1.png" title="plot of chunk time_trend_plot" alt="plot of chunk time_trend_plot" style="display: block; margin: auto;" />

... and for eyeballing the distributional properties of a single variable over time you have `prepare_quantile_trend_graph()`.


```r
graph <- prepare_quantile_trend_graph(r3win[c("period", "return")], "period", c(0.05, 0.25, 0.5, 0.75, 0.95))
graph$plot
```

<img src="figure/quantile_plot-1.png" title="plot of chunk quantile_plot" alt="plot of chunk quantile_plot" style="display: block; margin: auto;" />

Nothing special going on here (not really surprising, given the short time span that the sample covers).

Finally, `prepare_scatter_plot()` produces the mother of all plots, the scatter plot.


```r
prepare_scatter_plot(r3win, x="nioa", y="return", color="sector", size="toas", loess = 1)
```

<img src="figure/scatter_plot-1.png" title="plot of chunk scatter_plot" alt="plot of chunk scatter_plot" style="display: block; margin: auto;" />

Do you see the structural break around nioa == 0? Researchers in the area of accounting tend to like that kind of stuff.

## Regression Tables

Finally, if you happen to be a fan of starred numbers, you can also quickly produce regression tables by using the function `prepare_regression_table()` that calls `lfe::felm()` for OLS and `glm()` for binary logit models. The tables are then constructed by calling `stargazer::stargazer()`, allowing for plain text, html and latex output.

You can construct tables by mixing different models...


```r
dvs <- c("return", "return", "return", "return", "return", "return")
idvs <- list(c("nioa"), 
             c("cfoa"), 
             c("accoa"), 
             c("cfoa", "accoa"), 
             c("nioa", "accoa"), 
             c("nioa", "accoa")) 
feffects <- list("period", "period", "period", 
                 c("period", "coid"), c("period", "coid"), c("period", "coid"))
clusters <- list("", "", "", c("coid"), c("coid"), c("period", "coid"))
t <- prepare_regression_table(r3win, dvs, idvs, feffects, clusters)
htmltools::HTML(t$table)
```

<!--html_preserve--> <table style="text-align:center"><tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="6"><em>Dependent variable:</em></td></tr> <tr><td></td><td colspan="6" style="border-bottom: 1px solid black"></td></tr> <tr><td style="text-align:left"></td><td>return</td><td>return</td><td>return</td><td>return</td><td colspan="2">return</td></tr> <tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td><td>(5)</td><td>(6)</td></tr> <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">nioa</td><td>0.772<sup>***</sup></td><td></td><td></td><td></td><td>1.794<sup>***</sup></td><td>1.794<sup>***</sup></td></tr> <tr><td style="text-align:left"></td><td>(0.064)</td><td></td><td></td><td></td><td>(0.384)</td><td>(0.511)</td></tr> <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr> <tr><td style="text-align:left">cfoa</td><td></td><td>0.705<sup>***</sup></td><td></td><td>1.582<sup>***</sup></td><td></td><td></td></tr> <tr><td style="text-align:left"></td><td></td><td>(0.073)</td><td></td><td>(0.410)</td><td></td><td></td></tr> <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr> <tr><td style="text-align:left">accoa</td><td></td><td></td><td>0.534<sup>***</sup></td><td>0.876<sup>**</sup></td><td>-0.890<sup>**</sup></td><td>-0.890</td></tr> <tr><td style="text-align:left"></td><td></td><td></td><td>(0.116)</td><td>(0.381)</td><td>(0.395)</td><td>(0.553)</td></tr> <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td><td></td><td></td></tr> <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Fixed effects</td><td>period</td><td>period</td><td>period</td><td>period, coid</td><td>period, coid</td><td>period, coid</td></tr> <tr><td style="text-align:left">Std. errors clustered</td><td>No</td><td>No</td><td>No</td><td>coid</td><td>coid</td><td>period, coid</td></tr> <tr><td style="text-align:left">Observations</td><td>6,002</td><td>6,002</td><td>6,002</td><td>6,002</td><td>6,002</td><td>6,002</td></tr> <tr><td style="text-align:left">R<sup>2</sup></td><td>0.054</td><td>0.047</td><td>0.035</td><td>0.341</td><td>0.344</td><td>0.344</td></tr> <tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.054</td><td>0.046</td><td>0.035</td><td>-0.044</td><td>-0.039</td><td>-0.039</td></tr> <tr><td colspan="7" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="6" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr> </table><!--/html_preserve-->

... or by applying one model on different sub-samples.


```r
t <- prepare_regression_table(r3win, "return", c("nioa", "accoa"), byvar="period")
htmltools::HTML(t$table)
```

<!--html_preserve--> <table style="text-align:center"><tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="4"><em>Dependent variable:</em></td></tr> <tr><td></td><td colspan="4" style="border-bottom: 1px solid black"></td></tr> <tr><td style="text-align:left"></td><td colspan="4">return</td></tr> <tr><td style="text-align:left"></td><td>Full Sample</td><td>FY2014</td><td>FY2015</td><td>FY2016</td></tr> <tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td><td>(4)</td></tr> <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">nioa</td><td>0.801<sup>***</sup></td><td>0.234<sup>*</sup></td><td>0.607<sup>***</sup></td><td>1.375<sup>***</sup></td></tr> <tr><td style="text-align:left"></td><td>(0.074)</td><td>(0.139)</td><td>(0.123)</td><td>(0.119)</td></tr> <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr> <tr><td style="text-align:left">accoa</td><td>-0.067</td><td>0.014</td><td>0.638<sup>***</sup></td><td>-1.224<sup>***</sup></td></tr> <tr><td style="text-align:left"></td><td>(0.132)</td><td>(0.240)</td><td>(0.205)</td><td>(0.233)</td></tr> <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr> <tr><td style="text-align:left">Constant</td><td>0.082<sup>***</sup></td><td>0.130<sup>***</sup></td><td>0.014</td><td>0.096<sup>***</sup></td></tr> <tr><td style="text-align:left"></td><td>(0.006)</td><td>(0.011)</td><td>(0.011)</td><td>(0.011)</td></tr> <tr><td style="text-align:left"></td><td></td><td></td><td></td><td></td></tr> <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Fixed effects</td><td>None</td><td>None</td><td>None</td><td>None</td></tr> <tr><td style="text-align:left">Std. errors clustered</td><td>No</td><td>No</td><td>No</td><td>No</td></tr> <tr><td style="text-align:left">Observations</td><td>6,002</td><td>1,817</td><td>2,033</td><td>2,152</td></tr> <tr><td style="text-align:left">R<sup>2</sup></td><td>0.023</td><td>0.002</td><td>0.033</td><td>0.058</td></tr> <tr><td style="text-align:left">Adjusted R<sup>2</sup></td><td>0.023</td><td>0.001</td><td>0.032</td><td>0.057</td></tr> <tr><td colspan="5" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="4" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr> </table><!--/html_preserve-->


## Conclusion

This is all there is (currently).
All these functions are rather simple wrappers around established R functions. 
They can easily be modified to fit your needs and taste. 
Take look at the [github repository of the ExPanDaR package](https://github.com/joachim-gassen/ExPanDaR) for the code.
Have fun!
