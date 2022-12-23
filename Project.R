#The aim of this project is to forecast various variables related to India’s Population like the
#total population, Yearly percentage change, The number of migrants in the country, the median
#age, the Country’s share of the World population, etc.

#The outcome of this project is to be able to do the following tasks:
  
#1) Visualize how the population of India changed over time.
#2) Show what is the effect on the fertility rate with respect to population.
#3) Analyze the change in the number of migrants and median age in the country over years.
#4) Come up with assumptions specific to the population change over the years
#5) Estimate and forecast countries’ share of the world population over time.

#Since the data is a time series, we would first check for trends and seasonality after cleaning. 
#Then, using a variety of methods frequently employed in Time Series data models, 
#we would check each additional column or feature and investigate the connections they have with the other

#Simple Exponential Smoothening, Holtz Linear Trend method, and ETS will do better than the others. 
#The ETS statistical framework has the enormous benefit of allowing model selection using the information criteria. 
#Which ETS model is best for a particular time series can be determined using the AIC, AICC, and BIC.

#1. https://www.worldometers.info/world-population/india-population/
#2. https://www.kaggle.com/datasets/anandhuh/population-data-india 
#3. https://www.worldometers.info/about/
################################################################################

library(fpp2)
library(stats)
library(lmtest)
library(dplyr)


plot(india_population)
head(india_population)
length(india_population)
frequency(india_population)

ds = ts(india_population, frequency = 1, start = 2003)
autoplot(ds, facets = T)
round(cor(ds), digits = 2)
#just considering the least and most correlated variables

popul = ts(india_population[, "Population"], frequency = 1, start = 2003)
FR = ts(india_population[, "Fertility_Rate"], frequency = 1, start = 2003)
Dens = ts(india_population[, "Density_(P/Km¬≤)"], frequency = 1, start = 2003)
UPpercent = ts(india_population[, "Urban_Pop %"], frequency = 1, start = 2003)
UP = ts(india_population[, "Urban_Population"], frequency = 1, start = 2003)
WP = ts(india_population[, "World_Population"], frequency = 1, start = 2003)

ggtsdisplay(popul)
lambda_value = BoxCox.lambda(popul)
ggtsdisplay(BoxCox(popul, lambda = lambda_value))
#Box.Cox transform does not provides much difference thus lets apply log transform to smoothen
ggtsdisplay(log(popul))

ggtsdisplay(FR)
lambda_value = BoxCox.lambda(FR)
ggtsdisplay(BoxCox(FR, lambda = lambda_value))
#no need to of log transform as Box.Cox smooth out the variance

ggtsdisplay(Dens)
lambda_value = BoxCox.lambda(Dens)
ggtsdisplay(BoxCox(Dens, lambda = lambda_value))
#Box.Cox transform does not provides much difference thus lets apply log transform to smoothen
ggtsdisplay(log(Dens))

ggtsdisplay(UPpercent)
lambda_value = BoxCox.lambda(UPpercent)
ggtsdisplay(BoxCox(UPpercent, lambda = lambda_value))

ggtsdisplay(UP)
lambda_value = BoxCox.lambda(UP)
ggtsdisplay(BoxCox(UP, lambda = lambda_value))

ggtsdisplay(WP)
lambda_value = BoxCox.lambda(WP)
ggtsdisplay(BoxCox(WP, lambda = lambda_value))



Popul = window(popul, end=c(2017))
Ferti = window(FR, end=c(2017))
denst = window(Dens, end=c(2017))
Uppercen = window(UPpercent, end=c(2017))
Upop = window(UP, end=c(2017))
Wpop = window(WP, end=c(2017))

popultest = window(popul, start=c(2018))
FRtest = window(FR, start=c(2018))
Denstest = window(Dens, start=c(2018))
UPpercenttest = window(UPpercent, start=c(2018))
UPtest = window(UP, start=c(2018))
WPtest = window(WP, start=c(2018))

ets(Popul)    #ETS(M,A,N)       
ets(Ferti)    #ETS(A,A,N)       
ets(denst)    #ETS(M,A,N)       
ets(Uppercen)    #ETS(A,A,N)       
ets(Upop)    #ETS(A,A,N)       
ets(Wpop)    #ETS(M,A,N)       

ets_popul = forecast(ets(Popul), h = 6)
snaive_popul = snaive(Popul, h = 6)
drift_popul = rwf(Popul, h = 6, drift = T)
holt_popul = holt(Popul,h = 6, damped = T)

ets_ferti = forecast(ets(Ferti), h = 6)
snaive_ferti = snaive(Ferti, h = 6)
drift_ferti = rwf(Ferti, h = 6, drift = T)
holt_ferti = holt(Ferti,h = 6, damped = T)

ets_Dens = forecast(ets(denst), h = 6)
snaive_Dens = snaive(denst, h = 6)
drift_Dens = rwf(denst, h = 6, drift = T)
holt_Dens = holt(denst, h = 6, damped = T)

ets_UPercen = forecast(ets(Uppercen), h = 6)
snaive_UPercen = snaive(Uppercen, h = 6)
drift_UPercen = rwf(Uppercen, h = 6, drift = T)
holt_UPercen = holt(Uppercen, h = 6, damped = T)

ets_upop = forecast(ets(Upop), h = 6)
snaive_upop = snaive(Upop, h = 6)
drift_upop = rwf(Upop, h = 6, drift = T)
holt_upop = holt(Upop, h = 6, damped = T)

ets_wp = forecast(ets(Wpop), h = 6)
snaive_wp = snaive(Wpop, h = 6)
drift_wp = rwf(Wpop, h = 6, drift = T)
holt_wp = holt(Wpop, h = 6, damped = T)

autoplot(Popul)+
  autolayer(ets_popul, PI = F, series = 'ETS')+
  autolayer(snaive_popul, PI = F, series = 'Snaive')+
  autolayer(drift_popul, PI = F, series = 'Drift')+
  autolayer(holt_popul, PI = F,series = "Damped trend")

autoplot(Ferti)+
  autolayer(ets_ferti, PI = F, series = 'ETS')+
  autolayer(snaive_ferti, PI = F, series = 'Snaive')+
  autolayer(drift_ferti, PI = F, series = 'Drift')+
  autolayer(holt_ferti, PI = F,series = "Damped trend")

autoplot(denst)+
  autolayer(ets_Dens, PI = F, series = 'ETS')+
  autolayer(snaive_Dens, PI = F, series = 'Snaive')+
  autolayer(drift_Dens, PI = F, series = 'Drift')+
  autolayer(holt_Dens, PI = F,series = "Damped trend")

autoplot(Uppercen)+
  autolayer(ets_UPercen, PI = F, series = 'ETS')+
  autolayer(snaive_UPercen, PI = F, series = 'Snaive')+
  autolayer(drift_UPercen, PI = F, series = 'Drift')+
  autolayer(holt_UPercen, PI = F,series = "Damped trend")

autoplot(Upop)+
  autolayer(ets_upop, PI = F, series = 'ETS')+
  autolayer(snaive_upop, PI = F, series = 'Snaive')+
  autolayer(drift_upop, PI = F, series = 'Drift')+
  autolayer(holt_upop, PI = F,series = "Damped trend")

autoplot(Wpop)+
  autolayer(ets_wp, PI = F, series = 'ETS')+
  autolayer(snaive_wp, PI = F, series = 'Snaive')+
  autolayer(drift_wp, PI = F, series = 'Drift')+
  autolayer(holt_wp, PI = F,series = "Damped trend")

accuracy(holt_popul, popultest)
accuracy(holt_ferti, FRtest)
accuracy(holt_Dens, Denstest)
accuracy(holt_UPercen, UPpercenttest)
accuracy(holt_upop, UPtest)
accuracy(holt_wp, WPtest)