# Clean the memory
rm(list = ls()) 

# Clean the screen
cat("\014")   # Equivalent to "ctrl+L"

# Set the working directory
setwd("C:/aMonash/Quant Fin/Assignment 2")

# Load required library
library(quantmod)
library(fBasics)
library(jtools)
library(stats)
library(forecast)
library(rugarch)

###1. Data collection
# compute returns
getSymbols("NVDA",from="2018-09-01",to="2024-08-31",periodicity = 'daily') 
logprices <- log(NVDA$NVDA.Adjusted)
returns <- diff(logprices)*100 
returns <- na.omit(returns) 

head(returns)
tail(returns)

basicStats(returns)

###2. Risk Analysis:

# define a new xts object using only data until 31 Aug 2022
returns_is <- returns["/2022-08-31"]
head(returns_is)
tail(returns_is)

# Get all data from 1 Sep 2018 to 31 Aug 2022 as in-sample data for estimation
T1 <- length(returns_is)
T2 <- length(returns["2022-09-01/"])

# specify the 1st model we want to use to estimate the returns
spec_arch2 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,0)),
                  mean.model = list(armaOrder = c(0, 0)),
                  distribution.model = "norm")
# fit the model
arch2 = ugarchfit(spec = spec_arch2, data = returns,out.sample = T2)
show(arch2)

# choose 11 for acf of squared standardized residuals (z^2)
plot(arch2)
11
0
dev.copy(pdf,'Figures/squared_residual_arch2.pdf', width = 8, height = 5,paper='special')
dev.off()

# Specify the 2nd model we want to use to estimate the returns
# GARCH(1,1)
# specify the model we want to use to estimate the returns
spec_garch11 = ugarchspec(variance.model = list(model="sGARCH",garchOrder = c(1,1)),
                  mean.model = list(armaOrder = c(0,0)),
                  distribution.model="norm")
# fit the model
garch11 = ugarchfit(spec=spec_garch11,data=returns,out.sample=T2)
show(garch11)

# choose 11 for acf of squared standardized residuals (z^2)
plot(garch11)
11
0
dev.copy(pdf,'Figures/squared_residual_garch11.pdf', width = 8, height = 5,paper='special')
dev.off()

# Specify the 3rd model we want to use to estimate the returns
# GJR-GARCH(1,1)-t(v)
spec_gjr11t = ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")
# fit the model
gjr11t = ugarchfit(spec = spec_gjr11t, data = returns,out.sample = T2)
show(gjr11t)


### Conduct one-step-ahead out-of-sample volatility forecasts using two models

##### out-of-sample comparison
dates_oos = index(returns[(T1+1):(T1+T2)]) # specify the out-of-sample dates

### rolling window estimation for first model
roll_garch11 = ugarchroll(spec_garch11, returns, n.start = T1, refit.every = 10,
                        refit.window = "moving")
# retrieve one-day-ahead volatility forecast
vol_garch11_roll = roll_garch11@forecast$density$Sigma
# redefine an xts object to store volatility forecasts
vol_garch11_roll = xts(vol_garch11_roll,order.by = dates_oos)

plot(vol_garch11_roll)
dev.copy(pdf,'Figures/vol_garch11_roll.pdf', width = 8, height = 5,paper='special')
dev.off()

### rolling window estimation for second model
roll_gjr11t = ugarchroll(spec_gjr11t, returns, n.start = T1, refit.every = 10,
                       refit.window = "moving")
# retrieve one-day-ahead volatility forecast
vol_gjr11t_roll = roll_gjr11t@forecast$density$Sigma
# redefine an xts object to store volatility forecasts
vol_gj11tr_roll = xts(vol_gjr11t_roll,order.by = dates_oos)

plot(vol_gj11tr_roll)
dev.copy(pdf,'Figures/vol_gj11tr_roll.pdf', width = 8, height = 5,paper='special')
dev.off()

# save the realized returns in the out-of-sample period to use as a proxy
realizedret = returns[(T1+1):(T1+T2)]

# 1% Value-at-risk (VaR) & Back-testing
str(roll_garch11@forecast)
# retrieve one-day-ahead 1% VaR forecast for First Model
var1_garch11 = roll_garch11@forecast$VaR$`alpha(1%)`
  # redefine an xts object to store volatility forecasts
  var1_garch11 = xts(var1_garch11,order.by = dates_oos)

  # plot the VaR and the realized return
  VaRplot(alpha=0.01,realizedret,var1_garch11)
  dev.copy(pdf,'Figures/VaRplot_garch11.pdf', width = 8, height = 5,paper='special')
  dev.off()

# retrieve one-day-ahead 1% VaR forecast for Second Model
var1_gjr11t = roll_gjr11t@forecast$VaR$`alpha(1%)`
  # redefine an xts object to store volatility forecasts
  var1_gjr11t = xts(var1_gjr11t,order.by = dates_oos)

# save the realized returns in the out-of-sample period 
realizedret = returns[(T1+1):(T1+T2)]

  # plot the VaR and the realized return
  VaRplot(alpha=0.01,realizedret,var1_gjr11t)
  dev.copy(pdf,'Figures/VaRplot_gjr11t.pdf', width = 8, height = 5,paper='special')
  dev.off()

# evaluating the two VaR forecasts
VaRTest(alpha=0.01,realizedret,var1_garch11)
VaRTest(alpha=0.01,realizedret,var1_gjr11t)
