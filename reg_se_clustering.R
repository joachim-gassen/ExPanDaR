library(plm)
library(lmtest)
library(ExPanDaR)

# See Millo (2017): https://www.jstatsoft.org/article/view/v082i03
# and https://blog.theleapjournal.org/2016/06/sophisticated-clustered-standard-errors.html

data(petersen, package = "multiwayvcov")

df <- pdata.frame(petersen, index=c("firmid", "year"))
pooled <- plm(y ~ x, df, model = "pooling")
fe.firm <- plm(y ~ x, df, model = "within", effect = "individual")
fe.year <- plm(y ~ x, df, model = "within", effect = "time")
fe.fyear <- plm(y ~ x, df, model = "within", effect = "twoways")

summary(fe.fyear)

# "sss" gives the small sample correction that is being used by Stata
# See Millo(2017): 22f.

# Clustered by time
coeftest(fe.fyear, vcov=vcovHC(fe.firm, type="sss", cluster="time"))

# Clustered by firm
coeftest(fe.fyear, vcov=vcovHC(fe.firm, type="sss", cluster="group"))

# Clustered by firm and year
coeftest(fe.fyear, vcov=vcovDC(pooled.ols, type="sss"))

prepare_regression_table(
  petersen, rep("y", 7), rep("x", 7),
  feffects = list(
    "", "firmid", "year", c("firmid", "year"),
    "firmid", "year", c("firmid", "year")
  ),
  cluster = list(
    "", "", "", "",
    "firmid", "year", c("firmid", "year")
  ),
  format = "text"
)
