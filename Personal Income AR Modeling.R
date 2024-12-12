# title: "ARIMA Modeling"
# date: "12/11/2024"
# author: Leonard Genders

# White Noise
# use norm dist with mean 0 and constant variance

library(fpp3)
set.seed(10)
wn<-tsibble(x=rnorm(100), period=seq(1:100), index=period)

# observe the white noise with autoplot()
wn %>%
  autoplot() + theme_classic() +
  labs(title="White Noise Process",
       subtitle="Mean=0 and Standard Devia tion=1",
       x="", y="") +
  geom_hline(yintercept = 0, col="blue", lwd=1, linetype="dashed",
             alpha=0.4)
# behavior is erratic but fluctuates around mean of 0 and keeps stdev of 1

# Many time series models, including the ARIMA model, require a stationary time series. 
# These models make predictions about future values based on past values, and the statistical 
# properties of the past values are used to inform these predictions. If the time 
# series’ statistical properties change over time, then the models may not work well
# as the assumptions underlying them would not be met.

# use tesla dataset
tesla<-tsibble(
  period=yearquarter(c("2016:Q1","2016:Q2","2016:Q3","2016:Q4",
                       "2017:Q1","2017:Q2","2017:Q3","2017:Q4",
                       "2018:Q1","2018:Q2","2018:Q3","2018:Q4",
                       "2019:Q1","2019:Q2","2019:Q3","2019:Q4",
                       "2020:Q1","2020:Q2","2020:Q3","2020:Q4",
                       "2021:Q1","2021:Q2","2021:Q3","2021:Q4",
                       "2022:Q1","2022:Q2","2022:Q3","2022:Q4",
                       "2023:Q1","2023:Q2")),
  deliveries=c(14.8,14.4,24.5,22.2,
               25,22,26.2,29.9,
               30,40.7,83.5,90.7,
               63,95.2,97,112,
               88.4,90.7,139.3,180.6,
               184.82,201.25,241.3,308.6,
               310.5,254.7,343.8,405.3,
               422.9,466.1),
  index=period     # This is the time variable
)


# view before modeling and forecasting to determine whether series is stationary
tesla %>%
  autoplot(.vars=deliveries) + theme_classic() +
  labs(title = "Tesla Car Deliveries",
       subtitle = 'Q1 2017 to Q2 2023') +
  xlab("Quarter") + ylab(" ")
# steady upward trend, not stationary - crosses blue line and never revisits it


# need no trend, constant mean, variance, and auto-covariance
tesla %>%
  as_tsibble(index=period, regular=T) %>%
  autoplot(difference(deliveries)) + theme_classic() + # difference() function to eliminate trend in series
  labs(title="Change in Tesla's Vehicle Deliveries",
       subtitle = "Q1 2016 - Q4 2022", x="", y="") + 
  geom_hline(yintercept=mean(difference(tesla$deliveries), na.rm=TRUE), col="blue", linetype="dashed", lwd=1, alpha=0.4)
  # now fluctuates closer to mean, but behaves less erratic

# we can also see a fluctuation in the variance (heteroskedasicity or increasing variance)
# variance is low from 2016 to 2018 while significantly hugher for the period a fter
# to normalize variance of the series, we can conduct a Box-Cox transformation
lambda <- tesla %>%
  as_tsibble(index=period, regular=T) %>%
  features(deliveries, features = guerrero) %>%
  pull(lambda_guerrero) 

tesla %>%
  as_tsibble(index=period, regular=T) %>%
  autoplot(box_cox(difference(deliveries), lambda)) +
  labs(y="") + theme_classic() +
  labs(title="Box-Cox Transformation of Tesla's Vehicle Deliveries", 
       subtitle = "Q1 2016 - Q4 2022", x="", y="")
# variance is more uniform now

# statistical features to determine the stationary of a series (unitroot_kpss and unitroo_ndiffs)
# low p-value allows us to reject the null hypothesis of stationarity
# test the features with the non-stationary Tesla deliveries
tesla %>%
  as_tsibble(index=period, regular=T) %>%
  features(deliveries, features = c(unitroot_kpss, unitroot_ndiffs))
# not actual p-val, but since output below .01, we verify that the Tesla deliveries are non-stationary
# and that two differences are required to make the data stationary (ndiff=2)


# The Autocorrelation Function
# autocorrelations indicate the degree of similarity between a time series and a lagged
# version of itself (a previous period), help id patterns and trends in data so we 
# can predict future values of a series
library(tidyverse)
PI <- read_csv("https://jagelves.github.io/Data/PersonalIncome.csv")

PI %>%
  as_tsibble(index=Date) %>%
  filter_index(1970~2005) -> PI_train # train set

PI %>%
  as_tsibble(index=Date) %>%
  filter_index(2006~2011) -> PI_test

# use the ACF()_ function and plot with autoplot
PI_train %>%
  ACF(lag_max = 12, PI_Growth) %>%
  autoplot() + theme_bw() +
  labs(x="", y="",
       title="ACF Personal Income Growth in California")
# plot reveals that the correlation of the series with its first lag is strongest
# Interpretation: positive income growth in the previous period correlates with positive
# income growth in the current period. Plot also shows decay in the strength of the 
# correlation as the lags get larger, a positive income growth two or three periods 
# ago still positively influences the current period's income growth, but less than 
# the immediate previous period

# Blue lines help determine which autocorrelations are statistically different from zero (significant)
# at the 5% level and so we know that lags 1-4 are positively correlated with the 
# series and are statistically significant

# White Noise
# we expect wn to show no correlation with its lags since the series is constructed from independent 
# draws from a norm dist with constant variance -- prev periods do not affect current 
# or future periods

wn %>% ACF(x) %>%
  autoplot() + theme_bw() +
  labs(x="", y="ACF", title="ACF White Noice Process")
# here lag #14 shows a positive correlation with the series


## Partial Autocorrelation Function (PACF)
# summarizes the relationships between a series and its lags
# the sample PAC at lag k is the correlation that results after removing the effect of any 
# correlations due to the terms at shorter lags
PI_train %>%
  PACF(lag_max = 12, PI_Growth) %>%
  autoplot() + theme_bw() + labs(x="", y="PACF") +
  labs(x="", y="", title="PACF Personal Income Growth in California")
# Interpretation: graph shows that series has a strong correlation only with the first lag
# lag 2,3,4 seem to have been correlated with the series (see ACF) - likely because of influence
# from lag 1 though

# now inspecting the white noise processes to confirm that there are no more patterns
wn %>%
  PACF(x) %>% 
  autoplot() + theme_classic() + labs(x="", y="PACF", title= "PACF White Noise Process")


## Personal Income in California with AR(1) Model
# previous sesction we determined that the growth of personal income in California has
# a decaying ACF and a single significant spike (at lag 1) in the PACF

# simulate some data based on the AR(1) model
y<-c(0)
phi<-0.7
const<-1
nrep<-100

for (i in 2:nrep){
  y[i] = const + phi*y[i-1] +rnorm(1,0,0.5)
}

tsibble(y=y,period=seq(1,length(y)),index=period) %>%
  ACF(lag_max = 12, y) %>%
  autoplot() + theme_bw() +
  labs(x="", y="", title="ACF For Simulated AR(1) phi=.7 c=1")
# use the generated data to see what the ACF looks like for an AR(1) process
# Interpretation: Decaying ACF, strong correlation at lag 1 and subsequent lower
# correlations as the lags get larger

# view PACF
tsibble(y=y, period=seq(1,length(y)), index=period) %>%
  PACF(lag_max = 12, y) %>%
  autoplot() + theme_bw() +
  labs(x="", y="PACF", title="PACF For simulated AR(1) phi=0.7, c=1")
# Interpretation: significant spike at lag 1 and all other partial autocorrelations
# are not statistically different from zero, so we confirm we can model with AR(1) process


## Modeling and Residuals
# using AR(1) model and least squares model to compare
PI_fit <- PI_train %>%
  model(AR1 = AR(PI_Growth ~ order(1)),
        LS = TSLM(PI_Growth ~ trend()))
coef(PI_fit)
# if we change the estimated coefficient to 1.8 (from table), the simulation
# will resemble the personal income growth better - the AR model selects the constant
# and the lag coefficient (phi) such that it fits the data

# if the AR(1) process correctly describes the personal income series then the errors
# should behave like white noise
errors_PI <- augment(PI_fit)

errors_PI %>% 
  select(.resid) %>%  ACF(.resid) %>%
  autoplot() + theme_bw() +
  labs(title="ACF for Model Errors of AR(1) and LS", x="")
# Interpretation: errors from AR(1) model behave like white noise, suggesting that we correctly
# identified the systematic component of the series

# check PACF
errors_PI %>%
  select(.resid) %>% PACF(.resid) %>%
  autoplot() + theme_bw()
# confirmed that no pattern in residuals for AR(1) model


## Model Selection
glance(PI_fit) %>% arrange(AICc) %>% select(.model:BIC)
# AR(1) performs better in all of the metrics since they are lower than LS model

# test accuracy on the test set
PI_fc <- PI_fit %>% forecast(new_data = PI_test)
PI_fc %>% accuracy(PI_test)


PI_fc %>%
  filter(.model =="AR1") %>% autoplot(level=95) + theme_classic() +
  autolayer(PI_train, PI_Growth) +
  autolayer(PI_test, PI_Growth) +
  labs(title="Personal Income Growth AR(1) Forecast Accuracy",
       subtitle="1970-2021", y="", x="")



## MA(1) Model
# MA process assumes that the current obs of a time series is a linear combo of the 
# weighted sum of past error terms and that error terms caputre the unpredictable and random
# fluctuations in the time series that are not accounted for by the AR model

# simulate an MA(1) process and generate the ACF and PACF plots
set.seed(13)
e_t <- rnorm(300,0,0.5)
y_t = 2 + 0.95*dplyr::lag(e_t) + e_t

tsibble(y=y_t, period=seq(1, length(y_t)), index=period) %>%
  ACF(lag_max = 12, y) %>% autoplot() + theme_bw() +
  labs(x="", y="", title="ACF For Simulated MA(1)")

tsibble(y=y_t, period=seq(1, length(y_t)), index=period) %>%
  PACF(lag_max = 12, y) %>% autoplot() + theme_bw() +
  labs(x="", y="", title="PACF For Simulated MA(1)")
# pattern shown is one significant spike in first lag of ACF and a decaying PACF as lags are larger