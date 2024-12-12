# title: "California Avocado ARIMA Modeling"
# date: "12/12/2024"
# author: Leonard Genders
# class: BUAD 5052 Decision Modeling

## ARIMA Model to avocado data
library(fpp3)
library(tidyverse)
cali <- read_csv("https://jagelves.github.io/Data/CaliforniaAvocado.csv")

# view
glimpse(cali)

# to tsibbles
cali %>% 
  as_tsibble(key=geography, index=date, regular=T) %>%
  filter_index("2015-01-04"~"2018-12-02") -> cali

# train set
cali %>% 
  as_tsibble(key=geography, index=date, regular=T) %>%
  filter_index("2015-01-04"~"2018-06-02") -> calits_train

# test set
cali %>% 
  as_tsibble(key=geography, index=date, regular=T) %>%
  filter_index("2018-06-02"~"2018-12-02") -> calits_test

# Estimate LS, ETS, AR, and ARIMA models
fit <- model(calits_train,
             ETS=ETS(average_price),
             ARIMA=ARIMA(average_price,approximation=F), # approximation=F so search for best ARIMA model is exhaustive
             AR=AR(average_price~order(1)),
             LS=TSLM(average_price~trend()))

# retrieve coefficients
fit %>% coef()
# ETS model suggested is a SES with a smoothing parameter of approx. 0.65
# ARIMA(2,0,2) process suggested

# plot the ACF and PACF of the avg price of avocados
calits_train %>% 
  ACF(lag_max = 12, average_price) %>%
  autoplot() + theme_classic() +
  labs(x="", y="", title="ACF For Avocado Average Price")

calits_train %>% 
  PACF(lag_max = 12, average_price) %>%
  autoplot() + theme_classic() +
  labs(x="", y="", title="PACF For Avocado Average Price")
# patterns in the plots are AR(1), we need to include this in the model set of candidates

fit %>% 
  forecast(h=27) %>% autoplot(level=NULL) + theme_classic() +
  autolayer(cali, average_price) +
  labs(y="", title = "California's Forecasting Average Price of Avocados",
       subtitle="Jan 4, 2015 - Dec 2, 2018", x="")
# AR(1) and LS perform the best

## Combination of Models (Ensembles)
fit %>%
  mutate(Ensemble = 0.5*LS + 0.5*AR) -> fit2

# plot the forecasts for the ensemble, the AR1 and the LS model
fit2 %>%
  select(AR, LS, Ensemble) %>%
  forecast(h=27) %>%
  autoplot(level=NULL) +
  autolayer(cali, average_price) + theme_classic() +
  labs(x="", y="", title="California's Forecasted Average Price of Avocados",
       subtitle = "Jan 4, 2015 - Dec 2, 2018")

# now confirm the accuracy of the ensemble to the test set
fit2 %>%
  forecast(h=27) %>%
  accuracy(cali)
