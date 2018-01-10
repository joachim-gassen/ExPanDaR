ExPanDaR: An Intro
================
Joachim Gassen
January 7, 2018

ExPanDaR Package Functions
--------------------------

You are visiting the gihub repository of the ExPanDaR (Explore Panel Data with R) package. ExPanDaR is a small and extremly early stage R package that is being developed to provide the code base for the ExPanD web app. ExPanD is a shiny based app designed to enable users with little or no statistical programming experience to explore panel data. In addition, it will serve as a front end to assess the robustness of empirical archival research work.

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
| sic         | Standard industry classifier                                                     |
| sector      | Industry sector                                                                  |
| toas        | Total assets at period end (M US-$)                                              |
| sales       | Sales of the period (M US-$)                                                     |
| equity      | Total equity at period end (M US-$)                                              |
| mktcap      | Market capitalization of publicly outstanding equity at period end (M US-$)      |
| debt\_ta    | Total debt (% total assets)                                                      |
| eq\_ta      | Total equity (% total assets)                                                    |
| gw\_ta      | Goodwill (% total assets)                                                        |
| int\_ta     | Intangible assets (% total assets)                                               |
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

OK. This does not look to bad. Only FY2013 seems odd, as some variables are completely missing. Guess why? They are calculated using lagged values of total assets. So, in the following, let's focus on the variables that we care about and the sample on the fiscal years 2014 to 2016 (a short panel, I know). Also, let's take a quick look at the descriptive statistics.

``` r
r3 <- russell_3000[russell_3000$period > "FY2013",
                   c("coid", "coname", "period", "sector", "toas", "sales","mktcap", 
            "eq_ta", "int_ta", "roe", "nioa", "cfoa", "accoa", "return")]
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
6,703
</td>
<td style="text-align:right;">
9,036.222
</td>
<td style="text-align:right;">
37,725.783
</td>
<td style="text-align:right;">
0.800
</td>
<td style="text-align:right;">
467.435
</td>
<td style="text-align:right;">
1,629.320
</td>
<td style="text-align:right;">
5,135.450
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
6,707
</td>
<td style="text-align:right;">
5,189.938
</td>
<td style="text-align:right;">
19,245.733
</td>
<td style="text-align:right;">
0.010
</td>
<td style="text-align:right;">
300.975
</td>
<td style="text-align:right;">
968.610
</td>
<td style="text-align:right;">
3,170.275
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
6,677
</td>
<td style="text-align:right;">
10,038.695
</td>
<td style="text-align:right;">
37,394.508
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
5,890.000
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
6,703
</td>
<td style="text-align:right;">
0.426
</td>
<td style="text-align:right;">
0.314
</td>
<td style="text-align:right;">
-5.262
</td>
<td style="text-align:right;">
0.275
</td>
<td style="text-align:right;">
0.439
</td>
<td style="text-align:right;">
0.614
</td>
<td style="text-align:right;">
1.172
</td>
</tr>
<tr>
<td style="text-align:left;">
int\_ta
</td>
<td style="text-align:right;">
5,157
</td>
<td style="text-align:right;">
0.098
</td>
<td style="text-align:right;">
0.117
</td>
<td style="text-align:right;">
-0.008
</td>
<td style="text-align:right;">
0.018
</td>
<td style="text-align:right;">
0.057
</td>
<td style="text-align:right;">
0.139
</td>
<td style="text-align:right;">
0.871
</td>
</tr>
<tr>
<td style="text-align:left;">
roe
</td>
<td style="text-align:right;">
6,539
</td>
<td style="text-align:right;">
-0.182
</td>
<td style="text-align:right;">
12.896
</td>
<td style="text-align:right;">
-978.000
</td>
<td style="text-align:right;">
-0.014
</td>
<td style="text-align:right;">
0.087
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
6,540
</td>
<td style="text-align:right;">
0.002
</td>
<td style="text-align:right;">
0.192
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
6,540
</td>
<td style="text-align:right;">
0.066
</td>
<td style="text-align:right;">
0.167
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
6,540
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
6,115
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
0.065
</td>
<td style="text-align:right;">
0.269
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
2.171
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
2.171
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
2.171
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
2.171
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
2.171
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
-2.771
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
-2.771
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
-2.771
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
-2.771
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
-2.771
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
6,703
</td>
<td style="text-align:right;">
7,390.704
</td>
<td style="text-align:right;">
18,408.996
</td>
<td style="text-align:right;">
45.622
</td>
<td style="text-align:right;">
467.435
</td>
<td style="text-align:right;">
1,629.320
</td>
<td style="text-align:right;">
5,135.450
</td>
<td style="text-align:right;">
129,488.840
</td>
</tr>
<tr>
<td style="text-align:left;">
sales
</td>
<td style="text-align:right;">
6,707
</td>
<td style="text-align:right;">
4,471.466
</td>
<td style="text-align:right;">
11,393.165
</td>
<td style="text-align:right;">
0.680
</td>
<td style="text-align:right;">
300.975
</td>
<td style="text-align:right;">
968.610
</td>
<td style="text-align:right;">
3,170.275
</td>
<td style="text-align:right;">
83,089.900
</td>
</tr>
<tr>
<td style="text-align:left;">
mktcap
</td>
<td style="text-align:right;">
6,677
</td>
<td style="text-align:right;">
8,738.255
</td>
<td style="text-align:right;">
23,169.558
</td>
<td style="text-align:right;">
86.428
</td>
<td style="text-align:right;">
621.020
</td>
<td style="text-align:right;">
1,860.000
</td>
<td style="text-align:right;">
5,890.000
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
6,703
</td>
<td style="text-align:right;">
0.433
</td>
<td style="text-align:right;">
0.263
</td>
<td style="text-align:right;">
-0.455
</td>
<td style="text-align:right;">
0.275
</td>
<td style="text-align:right;">
0.439
</td>
<td style="text-align:right;">
0.614
</td>
<td style="text-align:right;">
0.931
</td>
</tr>
<tr>
<td style="text-align:left;">
int\_ta
</td>
<td style="text-align:right;">
5,157
</td>
<td style="text-align:right;">
0.097
</td>
<td style="text-align:right;">
0.111
</td>
<td style="text-align:right;">
0.000
</td>
<td style="text-align:right;">
0.018
</td>
<td style="text-align:right;">
0.057
</td>
<td style="text-align:right;">
0.139
</td>
<td style="text-align:right;">
0.576
</td>
</tr>
<tr>
<td style="text-align:left;">
roe
</td>
<td style="text-align:right;">
6,539
</td>
<td style="text-align:right;">
0.034
</td>
<td style="text-align:right;">
0.515
</td>
<td style="text-align:right;">
-2.771
</td>
<td style="text-align:right;">
-0.014
</td>
<td style="text-align:right;">
0.087
</td>
<td style="text-align:right;">
0.173
</td>
<td style="text-align:right;">
2.171
</td>
</tr>
<tr>
<td style="text-align:left;">
nioa
</td>
<td style="text-align:right;">
6,540
</td>
<td style="text-align:right;">
0.005
</td>
<td style="text-align:right;">
0.154
</td>
<td style="text-align:right;">
-0.715
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
0.294
</td>
</tr>
<tr>
<td style="text-align:left;">
cfoa
</td>
<td style="text-align:right;">
6,540
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
6,540
</td>
<td style="text-align:right;">
-0.063
</td>
<td style="text-align:right;">
0.086
</td>
<td style="text-align:right;">
-0.423
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
6,115
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
0.065
</td>
<td style="text-align:right;">
0.269
</td>
<td style="text-align:right;">
1.576
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
<th style="text-align:right;">
J
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
<span style=" font-weight: bold;  ">0.77</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.79</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">-0.16</span>
</td>
<td style="text-align:right;">
<span style="   ">0.02</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.09</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.10</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.06</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.08</span>
</td>
<td style="text-align:right;">
<span style="   ">-0.00</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
B: sales
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.83</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.76</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">-0.15</span>
</td>
<td style="text-align:right;">
<span style="   ">-0.01</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.11</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.13</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.11</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.07</span>
</td>
<td style="text-align:right;">
<span style="   ">0.01</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
C: mktcap
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.80</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.74</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">-0.08</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.06</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.12</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.15</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.14</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.06</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.04</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
D: eq\_ta
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">-0.42</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">-0.36</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">-0.20</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">-0.10</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">-0.04</span>
</td>
<td style="text-align:right;">
<span style="   ">-0.01</span>
</td>
<td style="text-align:right;">
<span style="   ">-0.02</span>
</td>
<td style="text-align:right;">
<span style="   ">0.03</span>
</td>
<td style="text-align:right;">
<span style="   ">0.03</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
E: int\_ta
</td>
<td style="text-align:right;">
<span style="   ">0.03</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.06</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.05</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">-0.06</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style="   ">0.02</span>
</td>
<td style="text-align:right;">
<span style="   ">0.01</span>
</td>
<td style="text-align:right;">
<span style="   ">0.00</span>
</td>
<td style="text-align:right;">
<span style="   ">0.02</span>
</td>
<td style="text-align:right;">
<span style="   ">-0.02</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
F: roe
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.26</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.38</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.34</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">-0.13</span>
</td>
<td style="text-align:right;">
<span style="   ">0.03</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.50</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.37</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.28</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.09</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
G: nioa
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.17</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.35</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.33</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.10</span>
</td>
<td style="text-align:right;">
<span style="   ">0.03</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.79</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.80</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.49</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.15</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
H: cfoa
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.09</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.29</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.27</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.07</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.05</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.50</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.66</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">-0.09</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.12</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
I: accoa
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.20</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.14</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.12</span>
</td>
<td style="text-align:right;">
<span style="   ">-0.02</span>
</td>
<td style="text-align:right;">
<span style="   ">-0.02</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.34</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.38</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">-0.30</span>
</td>
<td style="text-align:right;">
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.06</span>
</td>
</tr>
<tr>
<td style="text-align:left;">
J: return
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.04</span>
</td>
<td style="text-align:right;">
<span style="   ">0.03</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.17</span>
</td>
<td style="text-align:right;">
<span style="   ">0.01</span>
</td>
<td style="text-align:right;">
<span style="   ">0.01</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.17</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.19</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.14</span>
</td>
<td style="text-align:right;">
<span style=" font-weight: bold;  ">0.07</span>
</td>
<td style="text-align:right;">
</td>
</tr>
</tbody>
<tfoot>
<tr>
<td style="padding: 0; border: 0;" colspan="100%">
<sup></sup> This table reports Pearson correlations above and Spearman correlations below the diagonal. The number of observations ranges from 4743 to 6707. Correlations with significance levels below 1% appear in bold print.
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

    ## Warning: Removed 599 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 599 rows containing missing values (geom_point).

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
0.377<sup>\*\*\*</sup>
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
0.356<sup>\*\*\*</sup>
</td>
<td>
0.380<sup>\*\*\*</sup>
</td>
<td>
0.466<sup>\*\*\*</sup>
</td>
<td>
0.840<sup>\*\*\*</sup>
</td>
<td>
0.841<sup>\*\*\*</sup>
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
0.317<sup>\*\*\*</sup>
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
(0.472)
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
6,108
</td>
<td>
6,108
</td>
<td>
6,108
</td>
<td>
6,093
</td>
<td>
6,093
</td>
<td>
6,108
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
0.046
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
0.336
</td>
</tr>
<tr>
<td style="text-align:left">
Adjusted R<sup>2</sup>
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
0.058
</td>
<td>
-0.047
</td>
<td>
-0.046
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
0.371<sup>\*\*\*</sup>
</td>
<td>
0.079
</td>
<td>
0.277<sup>\*\*\*</sup>
</td>
<td>
0.683<sup>\*\*\*</sup>
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
0.347<sup>\*\*\*</sup>
</td>
<td>
0.109
</td>
<td>
0.615<sup>\*\*\*</sup>
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
0.093<sup>\*\*\*</sup>
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
6,108
</td>
<td>
1,827
</td>
<td>
2,088
</td>
<td>
2,193
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
0.028
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
0.027
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
