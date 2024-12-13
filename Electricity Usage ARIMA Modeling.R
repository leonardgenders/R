# title: "Electricity Usage ARIMA Modeling"
# date: "12/12/2024"
# author: Leonard Genders

## Q2.1 Loading Packages and Data
# Load the fpp3 and tidyverse packages and save the data into an obj called Usage
library(fpp3)
library(tidyverse)
Usage <- read_csv("https://jagelves.github.io/Data/ElectricityBill.csv")

# view
glimpse(Usage)


## Q2.2 Date Variables
# Create a variable called date using the mutate(), yearmonth(), and seq() functions
# Wrap the seq() function with the yearmonth() function
# from arg to ymd("2021-06-01"), length.out arg to 38 and by arg to "1 month"
Usage %>%
  mutate(date = yearmonth(seq(from=ymd("2021-06-01"), length.out = 38, by="1 month"))) -> Usage


## Q2.3 Tsibble
Usage %>%
  as_tsibble(index=date, regular=T) %>%
  rename(kwh = `Usage (kWh)`) %>%
  select(date, kwh) -> usage_ts

# view
glimpse(usage_ts)


## Q2.4 Plot
# Create a plot of the kwh variable in the usage_ts tsibble
usage_ts %>%
  autoplot() + theme_classic() +
  labs(title = "Electric Usage") # only title of Electric Usage and theme_classic()


## Q2.5 Test and Train
# Create test and training sets using the filter_index() function
usage_ts %>%
  filter_index("2021 Jul"~"2023 Apr") -> usagets_train # jul 21 to apr 23

# view train set
glimpse(usagets_train)

# rest of the data for test
usage_ts %>%
  filter_index("2023 May"~"2024 Jul") -> usagets_test

# view test set
glimpse(usagets_test)


## Q2.6 Fit
# Create obj called fit that estimates the ARIMA(), ETS, and a TSLM() model with trend() and seasonality()
# by using the training set, no args besides kwh should be passed in the ARIMA or ETS models
fit <- usagets_train %>%
  model(
    ARIMA=ARIMA(kwh),
    ETS=ETS(kwh),
    TSLM=TSLM(kwh ~ trend() + season())
  )

# view
glimpse(fit)



## Q2.7 Accuracy
# Use the fit object to retreive accuracy measures of the forecast relative to the test set (no cv)
fit %>% 
  accuracy() %>% 
  arrange(RMSE) # arrange the table in ascending order by RMSE
# TSLM has the lowest RMSE

## Q2.8 Information Criterion
# use the fit object and the glance() function to retrieve the AIC, AICc, and BIC
fit %>%
  glance() %>%
  select('.model',"AIC","AICc","BIC")
# TSLM has lowest AIC, AICc and BIC


## Q2.9 Forecast Plot
usage_ts %>%
  model(TSLM=TSLM(kwh ~ trend() + season())) %>% # use model fx to estimate best model according to Q2.7 and Q2.8 (TSLM)
  forecast(h=4) %>% # forecast four periods ahead (h=4)
  autoplot(level=95) + theme_classic() + # autoplot() with level = 95, theme_classic()
  autolayer(usage_ts, kwh) + 
  labs(title="Monthly Electric Usage in kWh", x="", y="")
