ExPanDaR: An Intro
================
Joachim Gassen
January 13, 2018

Explore Panel Data with R (ExPanDaR)
------------------------------------

You are visiting the github repository of the ExPanDaR (Explore Panel Data with R) package. ExPanDaR is a small and extremly early stage R package that is being developed to provide the code base for the ExPanD web app. ExPanD is a shiny based app designed to enable users with little or no statistical programming experience to explore panel data. In addition, it serves as a front end to assess the robustness of empirical archival research work.

If you want to try ExPanD just run the following in your R session

``` r
if (!require("devtools")) {
  install.packages("devtools")
  library("devtools")
}
install_github("joachim-gassen/ExPanDaR")
library(ExPanDaR)
data(russell_3000)
ExPanD(russell_3000, c("coid", "coname"), "period", ExPanD_config_russell_3000)
```

Or, if you do not like financial accounting data (who doesn't?), you can try your luck with something somewhat more intuitive.

``` r
if (!require("gapminder")) {
  install.packages("gapminder")
  library("gapminder")
}
data(gapminder)
gapminder$year <- ordered(gapminder$year)
ExPanD(gapminder, "country", "year")
```

Enjoy!

ExPanDaR Package Functions
--------------------------

The auxiliary functions of the ExPanDaR package can also be used for rapid prototyping data analysis. The functions provided by ExPanDaR are designed to support analysis steps that are common with empirical archival research projects in the area of accounting and finance (which happens to be my field).

To see what ExPanDaR has to offer, let's take a quick tour.

### Data Preparation

The ExPanDaR package expects you to start with a data frame containing your panel data (meaning data with a cross-sectional dimension and something like a time dimension). However, you can also use some functions on simple cross-sectional data. ExPanDaR expects the cross-sectional identifiers to be factors and the time-series identifier to be an ordered factor. For this walk-through I will use the data set russel\_3000, which comes with the package. It contains some financial reporting and capital market data of Russell 3000 firms from Google Finance and Yahoo Finance and has been collected using the tidyquant package in the summer of 2017. The data was collected to showcase the functions of ExPanDaR in its natural habitat but I would advise against using this data for scientific work. These are the variables included in the data.

``` r
kable(data.frame(Variable=names(russell_3000), Definition=Hmisc::label(russell_3000)), row.names = FALSE) 
```

| Variable    | Definition                                                                       |
|:------------|:---------------------------------------------------------------------------------|
| coid        | Company identifier                                                               |
| period      | Fiscal year                                                                      |
| coname      | Company name                                                                     |
| sector      | Sector                                                                           |
| industry    | Industry                                                                         |
| toas        | Total assets at period end (M US-$)                                              |
| sales       | Sales of the period (M US-$)                                                     |
| equity      | Total equity at period end (M US-$)                                              |
| mktcap      | Market capitalization of publicly outstanding equity at period end (M US-$)      |
| debt\_ta    | Total debt (% total assets)                                                      |
| eq\_ta      | Total equity (% total assets)                                                    |
| gw\_ta      | Goodwill (% total assets)                                                        |
| oint\_ta    | Other intangible assets (% total assets)                                         |
| ppe\_ta     | Property, plant and equipment (% total assets)                                   |
| ca\_ta      | Current assets (% total assets)                                                  |
| cash\_ta    | Cash (% total assets)                                                            |
| roe         | Return on equity (net income divided by average equity)                          |
| roa         | Return on assets (earnings before interest and taxes divided by average equity)  |
| nioa        | Net income divided by average assets                                             |
| cfoa        | Cash flow from operations divided by average assets                              |
| accoa       | Total accruals divided by average assets                                         |
| cogs\_sales | Net income divided by average assets                                             |
| ebit\_sales | Earnings before interest and taxes divided by sales                              |
| ni\_sales   | Net income divided by sales                                                      |
| mtb         | Market-to-book (Market capitalization of equity divided by book value of equity) |
| per         | Price earnings ratio (Market capitalization of equity divided by net income)     |
| return      | Stock return of the period (%)                                                   |

First, let's eyeball how frequently observations are missing in the data set.

``` r
data("russell_3000")
prepare_missing_values_graph(russell_3000, period = "period")
```

<img src="README_files/figure-markdown_github/missing_obs-1.png" style="display: block; margin: auto;" />

OK. This does not look too bad. Only FY2013 seems odd, as some variables are completely missing. Guess why? They are calculated using lagged values of total assets. So, in the following, let's focus on the variables that we care about and on the fiscal years 2014 to 2016 (a short panel, I know). Time to check the descriptive statistics.

``` r
r3 <- russell_3000[russell_3000$period > "FY2013",
                   c("coid", "coname", "period", "sector", "toas", "sales","mktcap", 
            "eq_ta", "roe", "nioa", "cfoa", "accoa", "return")]
t <- prepare_descriptive_table(r3)
t$kable_ret  %>%
  kable_styling("condensed", full_width = F, position = "center")
```

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Descriptive Statistics
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
N
</th>
<th style="text-align:right;">
Mean
</th>
<th style="text-align:right;">
Std. dev.
</th>
<th style="text-align:right;">
Min.
</th>
<th style="text-align:right;">
25 %
</th>
<th style="text-align:right;">
Median
</th>
<th style="text-align:right;">
75 %
</th>
<th style="text-align:right;">
Max.
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
toas
</td>
<td style="text-align:right;">
6,664
</td>
<td style="text-align:right;">
8,817.822
</td>
<td style="text-align:right;">
35,926.884
</td>
<td style="text-align:right;">
0.800
</td>
<td style="text-align:right;">
464.957
</td>
<td style="text-align:right;">
1,615.220
</td>
<td style="text-align:right;">
5,142.557
</td>
<td style="text-align:right;">
861,395.000
</td>
</tr>
<tr>
<td style="text-align:left;">
sales
</td>
<td style="text-align:right;">
6,668
</td>
<td style="text-align:right;">
5,117.248
</td>
<td style="text-align:right;">
18,804.014
</td>
<td style="text-align:right;">
0.010
</td>
<td style="text-align:right;">
299.330
</td>
<td style="text-align:right;">
960.745
</td>
<td style="text-align:right;">
3,175.418
</td>
<td style="text-align:right;">
485,651.000
</td>
</tr>
<tr>
<td style="text-align:left;">
mktcap
</td>
<td style="text-align:right;">
6,662
</td>
<td style="text-align:right;">
10,055.857
</td>
<td style="text-align:right;">
37,434.753
</td>
<td style="text-align:right;">
18.670
</td>
<td style="text-align:right;">
621.020
</td>
<td style="text-align:right;">
1,860.000
</td>
<td style="text-align:right;">
5,900.000
</td>
<td style="text-align:right;">
753,720.000
</td>
</tr>
<tr>
<td style="text-align:left;">
eq\_ta
</td>
<td style="text-align:right;">
6,664
</td>
<td style="text-align:right;">
0.426
</td>
<td style="text-align:right;">
0.315
</td>
<td style="text-align:right;">
-5.262
</td>
<td style="text-align:right;">
0.275
</td>
<td style="text-align:right;">
0.440
</td>
<td style="text-align:right;">
0.615
</td>
<td style="text-align:right;">
1.172
</td>
</tr>
<tr>
<td style="text-align:left;">
roe
</td>
<td style="text-align:right;">
6,500
</td>
<td style="text-align:right;">
-0.184
</td>
<td style="text-align:right;">
12.934
</td>
<td style="text-align:right;">
-978.000
</td>
<td style="text-align:right;">
-0.015
</td>
<td style="text-align:right;">
0.086
</td>
<td style="text-align:right;">
0.173
</td>
<td style="text-align:right;">
121.620
</td>
</tr>
<tr>
<td style="text-align:left;">
nioa
</td>
<td style="text-align:right;">
6,501
</td>
<td style="text-align:right;">
0.002
</td>
<td style="text-align:right;">
0.193
</td>
<td style="text-align:right;">
-5.383
</td>
<td style="text-align:right;">
-0.003
</td>
<td style="text-align:right;">
0.034
</td>
<td style="text-align:right;">
0.074
</td>
<td style="text-align:right;">
0.925
</td>
</tr>
<tr>
<td style="text-align:left;">
cfoa
</td>
<td style="text-align:right;">
6,501
</td>
<td style="text-align:right;">
0.066
</td>
<td style="text-align:right;">
0.168
</td>
<td style="text-align:right;">
-4.241
</td>
<td style="text-align:right;">
0.043
</td>
<td style="text-align:right;">
0.082
</td>
<td style="text-align:right;">
0.130
</td>
<td style="text-align:right;">
0.921
</td>
</tr>
<tr>
<td style="text-align:left;">
accoa
</td>
<td style="text-align:right;">
6,501
</td>
<td style="text-align:right;">
-0.064
</td>
<td style="text-align:right;">
0.105
</td>
<td style="text-align:right;">
-1.424
</td>
<td style="text-align:right;">
-0.091
</td>
<td style="text-align:right;">
-0.051
</td>
<td style="text-align:right;">
-0.023
</td>
<td style="text-align:right;">
1.242
</td>
</tr>
<tr>
<td style="text-align:left;">
return
</td>
<td style="text-align:right;">
6,100
</td>
<td style="text-align:right;">
0.097
</td>
<td style="text-align:right;">
0.437
</td>
<td style="text-align:right;">
-0.938
</td>
<td style="text-align:right;">
-0.138
</td>
<td style="text-align:right;">
0.064
</td>
<td style="text-align:right;">
0.268
</td>
<td style="text-align:right;">
6.346
</td>
</tr>
</tbody>
</table>
Take a look at the minima and the maxima of some of the variables (e.g., return on equity (roe)). This does not look nice. One thing that comes very handy when dealing with outliers is a quick way to observe extreme values.

``` r
t <- prepare_ext_obs_table(na.omit(r3[c("coname", "period", "roe")]))
t$kable_ret %>%
  kable_styling("condensed", full_width = F, position = "center")
```

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
coname
</th>
<th style="text-align:left;">
period
</th>
<th style="text-align:right;">
roe
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
W&T Offshore, Inc.
</td>
<td style="text-align:left;">
FY2015
</td>
<td style="text-align:right;">
121.62049
</td>
</tr>
<tr>
<td style="text-align:left;">
CytomX Therapeutics, Inc.
</td>
<td style="text-align:left;">
FY2014
</td>
<td style="text-align:right;">
43.61151
</td>
</tr>
<tr>
<td style="text-align:left;">
Pinnacle Entertainment, Inc.
</td>
<td style="text-align:left;">
FY2016
</td>
<td style="text-align:right;">
30.82278
</td>
</tr>
<tr>
<td style="text-align:left;">
EXCO Resources NL
</td>
<td style="text-align:left;">
FY2015
</td>
<td style="text-align:right;">
15.65625
</td>
</tr>
<tr>
<td style="text-align:left;">
Allegion plc
</td>
<td style="text-align:left;">
FY2015
</td>
<td style="text-align:right;">
14.79808
</td>
</tr>
<tr>
<td style="text-align:left;">
...
</td>
<td style="text-align:left;">
...
</td>
<td style="text-align:right;">
...
</td>
</tr>
<tr>
<td style="text-align:left;">
Argos Therapeutics, Inc.
</td>
<td style="text-align:left;">
FY2015
</td>
<td style="text-align:right;">
-47.48571
</td>
</tr>
<tr>
<td style="text-align:left;">
Cheniere Energy, Inc.
</td>
<td style="text-align:left;">
FY2014
</td>
<td style="text-align:right;">
-70.70065
</td>
</tr>
<tr>
<td style="text-align:left;">
HD Supply Holdings, Inc.
</td>
<td style="text-align:left;">
FY2016
</td>
<td style="text-align:right;">
-184.00000
</td>
</tr>
<tr>
<td style="text-align:left;">
Advanced Micro Devices, Inc.
</td>
<td style="text-align:left;">
FY2016
</td>
<td style="text-align:right;">
-248.50000
</td>
</tr>
<tr>
<td style="text-align:left;">
Workhorse Group, Inc.
</td>
<td style="text-align:left;">
FY2016
</td>
<td style="text-align:right;">
-978.00000
</td>
</tr>
</tbody>
</table>
In a real life research situation, you might want to take a break and check your data as well as the actual financial statements to see what is going on. In most cases, you will see that the outliers are caused by very small denominators (lagged equity values in this case). To reduce the effect of these outliers on your analysis, you can winsorize (or truncate) them.

``` r
r3win <- as.data.frame(treat_outliers(r3, percentile = 0.01))
t <- prepare_ext_obs_table(na.omit(r3win[c("coname", "period", "roe")]))
t$kable_ret %>%
  kable_styling("condensed", full_width = F, position = "center")
```

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
coname
</th>
<th style="text-align:left;">
period
</th>
<th style="text-align:right;">
roe
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Allegion plc
</td>
<td style="text-align:left;">
FY2015
</td>
<td style="text-align:right;">
2.182
</td>
</tr>
<tr>
<td style="text-align:left;">
Allegion plc
</td>
<td style="text-align:left;">
FY2016
</td>
<td style="text-align:right;">
2.182
</td>
</tr>
<tr>
<td style="text-align:left;">
Advanced Micro Devices, Inc.
</td>
<td style="text-align:left;">
FY2015
</td>
<td style="text-align:right;">
2.182
</td>
</tr>
<tr>
<td style="text-align:left;">
Argos Therapeutics, Inc.
</td>
<td style="text-align:left;">
FY2016
</td>
<td style="text-align:right;">
2.182
</td>
</tr>
<tr>
<td style="text-align:left;">
Array BioPharma Inc.
</td>
<td style="text-align:left;">
FY2014
</td>
<td style="text-align:right;">
2.182
</td>
</tr>
<tr>
<td style="text-align:left;">
...
</td>
<td style="text-align:left;">
...
</td>
<td style="text-align:right;">
...
</td>
</tr>
<tr>
<td style="text-align:left;">
Winmark Corporation
</td>
<td style="text-align:left;">
FY2015
</td>
<td style="text-align:right;">
-2.772
</td>
</tr>
<tr>
<td style="text-align:left;">
Workiva Inc.
</td>
<td style="text-align:left;">
FY2016
</td>
<td style="text-align:right;">
-2.772
</td>
</tr>
<tr>
<td style="text-align:left;">
Workhorse Group, Inc.
</td>
<td style="text-align:left;">
FY2016
</td>
<td style="text-align:right;">
-2.772
</td>
</tr>
<tr>
<td style="text-align:left;">
Wynn Resorts, Limited
</td>
<td style="text-align:left;">
FY2014
</td>
<td style="text-align:right;">
-2.772
</td>
</tr>
<tr>
<td style="text-align:left;">
Wynn Resorts, Limited
</td>
<td style="text-align:left;">
FY2015
</td>
<td style="text-align:right;">
-2.772
</td>
</tr>
</tbody>
</table>
### Descriptive Statistics

Still rather extreme values (Return on equity = 217 %) but let's move on and look at the winsorized descriptive statistics.

``` r
t <- prepare_descriptive_table(r3win)
t$kable_ret  %>%
  kable_styling("condensed", full_width = F, position = "center")
```

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Descriptive Statistics
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
N
</th>
<th style="text-align:right;">
Mean
</th>
<th style="text-align:right;">
Std. dev.
</th>
<th style="text-align:right;">
Min.
</th>
<th style="text-align:right;">
25 %
</th>
<th style="text-align:right;">
Median
</th>
<th style="text-align:right;">
75 %
</th>
<th style="text-align:right;">
Max.
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
toas
</td>
<td style="text-align:right;">
6,664
</td>
<td style="text-align:right;">
7,343.392
</td>
<td style="text-align:right;">
18,170.457
</td>
<td style="text-align:right;">
45.479
</td>
<td style="text-align:right;">
464.957
</td>
<td style="text-align:right;">
1,615.220
</td>
<td style="text-align:right;">
5,142.557
</td>
<td style="text-align:right;">
127,894.870
</td>
</tr>
<tr>
<td style="text-align:left;">
sales
</td>
<td style="text-align:right;">
6,668
</td>
<td style="text-align:right;">
4,427.692
</td>
<td style="text-align:right;">
11,137.037
</td>
<td style="text-align:right;">
0.670
</td>
<td style="text-align:right;">
299.330
</td>
<td style="text-align:right;">
960.745
</td>
<td style="text-align:right;">
3,175.418
</td>
<td style="text-align:right;">
80,666.340
</td>
</tr>
<tr>
<td style="text-align:left;">
mktcap
</td>
<td style="text-align:right;">
6,662
</td>
<td style="text-align:right;">
8,752.487
</td>
<td style="text-align:right;">
23,193.551
</td>
<td style="text-align:right;">
86.227
</td>
<td style="text-align:right;">
621.020
</td>
<td style="text-align:right;">
1,860.000
</td>
<td style="text-align:right;">
5,900.000
</td>
<td style="text-align:right;">
174,450.000
</td>
</tr>
<tr>
<td style="text-align:left;">
eq\_ta
</td>
<td style="text-align:right;">
6,664
</td>
<td style="text-align:right;">
0.433
</td>
<td style="text-align:right;">
0.263
</td>
<td style="text-align:right;">
-0.456
</td>
<td style="text-align:right;">
0.275
</td>
<td style="text-align:right;">
0.440
</td>
<td style="text-align:right;">
0.615
</td>
<td style="text-align:right;">
0.931
</td>
</tr>
<tr>
<td style="text-align:left;">
roe
</td>
<td style="text-align:right;">
6,500
</td>
<td style="text-align:right;">
0.034
</td>
<td style="text-align:right;">
0.517
</td>
<td style="text-align:right;">
-2.772
</td>
<td style="text-align:right;">
-0.015
</td>
<td style="text-align:right;">
0.086
</td>
<td style="text-align:right;">
0.173
</td>
<td style="text-align:right;">
2.182
</td>
</tr>
<tr>
<td style="text-align:left;">
nioa
</td>
<td style="text-align:right;">
6,501
</td>
<td style="text-align:right;">
0.005
</td>
<td style="text-align:right;">
0.155
</td>
<td style="text-align:right;">
-0.718
</td>
<td style="text-align:right;">
-0.003
</td>
<td style="text-align:right;">
0.034
</td>
<td style="text-align:right;">
0.074
</td>
<td style="text-align:right;">
0.295
</td>
</tr>
<tr>
<td style="text-align:left;">
cfoa
</td>
<td style="text-align:right;">
6,501
</td>
<td style="text-align:right;">
0.068
</td>
<td style="text-align:right;">
0.137
</td>
<td style="text-align:right;">
-0.561
</td>
<td style="text-align:right;">
0.043
</td>
<td style="text-align:right;">
0.082
</td>
<td style="text-align:right;">
0.130
</td>
<td style="text-align:right;">
0.356
</td>
</tr>
<tr>
<td style="text-align:left;">
accoa
</td>
<td style="text-align:right;">
6,501
</td>
<td style="text-align:right;">
-0.064
</td>
<td style="text-align:right;">
0.086
</td>
<td style="text-align:right;">
-0.426
</td>
<td style="text-align:right;">
-0.091
</td>
<td style="text-align:right;">
-0.051
</td>
<td style="text-align:right;">
-0.023
</td>
<td style="text-align:right;">
0.184
</td>
</tr>
<tr>
<td style="text-align:left;">
return
</td>
<td style="text-align:right;">
6,100
</td>
<td style="text-align:right;">
0.088
</td>
<td style="text-align:right;">
0.375
</td>
<td style="text-align:right;">
-0.706
</td>
<td style="text-align:right;">
-0.138
</td>
<td style="text-align:right;">
0.064
</td>
<td style="text-align:right;">
0.268
</td>
<td style="text-align:right;">
1.577
</td>
</tr>
</tbody>
</table>
This looks better. I am sure that you won't care but I am a big fan of correlation tables.

``` r
t<- prepare_correlation_table(r3win, bold = 0.01, format="html")
t$kable_ret %>%
  kable_styling("condensed", full_width = F, position = "center")
```

<table class="table table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
A
</th>
<th style="text-align:right;">
B
</th>
<th style="text-align:right;">
C
</th>
<th style="text-align:right;">
D
</th>
<th style="text-align:right;">
E
</th>
<th style="text-align:right;">
F
</th>
<th style="text-align:right;">
G
</th>
<th style="text-align:right;">
H
</th>
<th style="text-align:right;">
I
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
A: toas
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.77</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.79</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">-0.16</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.09</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.10</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.07</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.08</span>
</td>
<td style="text-align:right;">
<span style="     ">-0.00</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
B: sales
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.84</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.76</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">-0.15</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.11</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.14</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.12</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.07</span>
</td>
<td style="text-align:right;">
<span style="     ">0.01</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
C: mktcap
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.80</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.74</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">-0.08</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.12</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.15</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.14</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.06</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.04</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
D: eq\_ta
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">-0.42</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">-0.36</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">-0.20</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">-0.04</span>
</td>
<td style="text-align:right;">
<span style="     ">-0.01</span>
</td>
<td style="text-align:right;">
<span style="     ">-0.02</span>
</td>
<td style="text-align:right;">
<span style="     ">0.03</span>
</td>
<td style="text-align:right;">
<span style="     ">0.03</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
E: roe
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.26</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.38</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.34</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">-0.13</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.50</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.37</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.28</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.09</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
F: nioa
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.17</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.35</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.33</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.10</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.79</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.80</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.49</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.15</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
G: cfoa
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.09</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.29</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.27</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.07</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.50</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.66</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">-0.09</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.12</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
H: accoa
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.20</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.14</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.12</span>
</td>
<td style="text-align:right;">
<span style="     ">-0.02</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.34</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.38</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">-0.30</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.06</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
I: return
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.04</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.03</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.17</span>
</td>
<td style="text-align:right;">
<span style="     ">0.01</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.17</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.20</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.14</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;    ">0.07</span>
</td>
<td style="text-align:right;">
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; border: 0;" colspan="100%">
<sup></sup> This table reports Pearson correlations above and Spearman correlations below the diagonal. The number of observations ranges from 6093 to 6668. Correlations with significance levels below 1% appear in bold print.
</td>
</tr>
</tfoot>
</table>
In fact, I like correlations so much that I sometimes use a graphic variant based on the corrplot package. See for yourself.

``` r
ret <- prepare_correlation_graph(r3win)
```

<img src="README_files/figure-markdown_github/correlation_graph-1.png" style="display: block; margin: auto;" />

### Visuals

Additional visuals are available for exploring time trends. For comparing variables...

``` r
graph <- prepare_trend_graph(r3win[c("period", "nioa", "cfoa", "accoa")], "period")
graph$plot
```

<img src="README_files/figure-markdown_github/time_trend_plot-1.png" style="display: block; margin: auto;" />

... and for eyeballing the distributional properties of a single variable over time.

``` r
graph <- prepare_quantile_trend_graph(r3win[c("period", "return")], "period", c(0.05, 0.25, 0.5, 0.75, 0.95))
graph$plot
```

<img src="README_files/figure-markdown_github/quantile_plot-1.png" style="display: block; margin: auto;" />

And, of course, the mother of all plots, the scatter plot. Do you see the structural break around nioa == 0? Accountants like that kind of stuff.

``` r
prepare_scatter_plot(r3win, x="nioa", y="return", color="sector", size="toas", loess = 1)
```

    ## Warning: Removed 575 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 575 rows containing missing values (geom_point).

<img src="README_files/figure-markdown_github/scatter_plot-1.png" style="display: block; margin: auto;" />

### Regression Tables

And, if you happen to be a fan of starred numbers, you can also quickly produce regression tables. Both, by mixing different models...

``` r
dvs <- list("return", "return", "return", "return", "return", "return")
idvs <- list(c("nioa"), 
             c("cfoa"), 
             c("cfoa", "accoa"), 
             c("cfoa", "accoa"), 
             c("cfoa", "accoa"), 
             c("cfoa", "accoa")) 
feffects <- list("period", "period", "period", c("period", "sector"), c("period", "coid"), c("period", "coid"))
clusters <- list("", "", "", "period", c("period", "sector"), c("period", "coid"))
t <- prepare_regression_table(r3win, dvs, idvs, feffects, clusters)
htmltools::HTML(t$table)
```

<!--html_preserve-->
<table style="text-align:center">
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="6">
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="6" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
return
</td>
<td>
return
</td>
<td colspan="4">
return
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
<td>
(5)
</td>
<td>
(6)
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
nioa
</td>
<td>
0.376<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.032)
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
cfoa
</td>
<td>
</td>
<td>
0.355<sup>\*\*\*</sup>
</td>
<td>
0.379<sup>\*\*\*</sup>
</td>
<td>
0.466<sup>\*\*\*</sup>
</td>
<td>
0.840<sup>\*\*\*</sup>
</td>
<td>
0.840<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.036)
</td>
<td>
(0.036)
</td>
<td>
(0.074)
</td>
<td>
(0.313)
</td>
<td>
(0.252)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
accoa
</td>
<td>
</td>
<td>
</td>
<td>
0.318<sup>\*\*\*</sup>
</td>
<td>
0.297<sup>\*</sup>
</td>
<td>
0.463
</td>
<td>
0.463
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
(0.057)
</td>
<td>
(0.175)
</td>
<td>
(0.457)
</td>
<td>
(0.471)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Fixed effects
</td>
<td>
period
</td>
<td>
period
</td>
<td>
period
</td>
<td>
period, sector
</td>
<td>
period, coid
</td>
<td>
period, coid
</td>
</tr>
<tr>
<td style="text-align:left">
Std. errors clustered
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
period
</td>
<td>
period, sector
</td>
<td>
period, coid
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
6,093
</td>
<td>
6,093
</td>
<td>
6,093
</td>
<td>
6,093
</td>
<td>
6,093
</td>
<td>
6,093
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.052
</td>
<td>
0.045
</td>
<td>
0.050
</td>
<td>
0.060
</td>
<td>
0.335
</td>
<td>
0.335
</td>
</tr>
<tr>
<td style="text-align:left">
Adjusted R<sup>2</sup>
</td>
<td>
0.051
</td>
<td>
0.045
</td>
<td>
0.050
</td>
<td>
0.058
</td>
<td>
-0.047
</td>
<td>
-0.047
</td>
</tr>
<tr>
<td colspan="7" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td colspan="6" style="text-align:right">
<sup>*</sup>p&lt;0.1; <sup>**</sup>p&lt;0.05; <sup>***</sup>p&lt;0.01
</td>
</tr>
</table>
<!--/html_preserve-->
... or by applying one model on different sub-samples.

``` r
t <- prepare_regression_table(r3win, "return", c("cfoa", "accoa"), byvar="period")
htmltools::HTML(t$table)
```

<!--html_preserve-->
<table style="text-align:center">
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="4">
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="4" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="4">
return
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
Full Sample
</td>
<td>
FY2013
</td>
<td>
FY2014
</td>
<td>
FY2015
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
cfoa
</td>
<td>
0.370<sup>\*\*\*</sup>
</td>
<td>
0.078
</td>
<td>
0.275<sup>\*\*\*</sup>
</td>
<td>
0.682<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.037)
</td>
<td>
(0.068)
</td>
<td>
(0.062)
</td>
<td>
(0.059)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
accoa
</td>
<td>
0.349<sup>\*\*\*</sup>
</td>
<td>
0.111
</td>
<td>
0.616<sup>\*\*\*</sup>
</td>
<td>
0.031
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.058)
</td>
<td>
(0.112)
</td>
<td>
(0.088)
</td>
<td>
(0.101)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Constant
</td>
<td>
0.083<sup>\*\*\*</sup>
</td>
<td>
0.132<sup>\*\*\*</sup>
</td>
<td>
0.018<sup>\*</sup>
</td>
<td>
0.092<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.006)
</td>
<td>
(0.011)
</td>
<td>
(0.011)
</td>
<td>
(0.011)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Fixed effects
</td>
<td>
None
</td>
<td>
None
</td>
<td>
None
</td>
<td>
None
</td>
</tr>
<tr>
<td style="text-align:left">
Std. errors clustered
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
6,093
</td>
<td>
1,822
</td>
<td>
2,083
</td>
<td>
2,188
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.020
</td>
<td>
0.001
</td>
<td>
0.029
</td>
<td>
0.058
</td>
</tr>
<tr>
<td style="text-align:left">
Adjusted R<sup>2</sup>
</td>
<td>
0.020
</td>
<td>
-0.00004
</td>
<td>
0.028
</td>
<td>
0.057
</td>
</tr>
<tr>
<td colspan="5" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td colspan="4" style="text-align:right">
<sup>*</sup>p&lt;0.1; <sup>**</sup>p&lt;0.05; <sup>***</sup>p&lt;0.01
</td>
</tr>
</table>
<!--/html_preserve-->
### Conclusion

This is all there is (currently). All these functions are rather simple wrappers around established R functions. They can easily be modified to fit your needs and taste. Enjoy!
