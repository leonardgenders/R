# title: "M6 Assignment" 
# date: "12/7/2024" 
# author: Leonard Genders

## Q2.1 Loading Packages and Data
# Load the fpp3 and tidyverse packages and create an obj taht captures the data called Netflix
library(fpp3)
library(tidyverse)

Netflix <- read_csv("https://jagelves.github.io/Data/Netflix.csv")

# view
glimpse(Netflix)


## Q2.2 Creating a tsibble
# Use piping to:
Netflix %>%
     mutate(Quarter = yearquarter(Quarter)) %>% # coerce the Quarter variable to a date that captures the yr/qtr
     as_tsibble(index=Quarter) -> Netflix_ts # name the resulting tsibbble Netflix_ts

# view changes
glimpse(Netflix)


## Q2.3 Plot
# Use autoplot() to plot the Subscribers time series

Netflix_ts %>%
     autoplot(.vars = Subscribers) + theme_classic() +
     labs(title = "Netflix World Wide Subscribers (Millions)", # Add a title that reads "Netflix World Wide Subscribers (Millions)"
          subtitle = "2013 Q1 to 2023 Q2") # Add a subtitle that reads "2013 Q1 to 2023 Q2"


## Q2.4 Test and Train Sets
# Use the filter_index() function to create train and test sets
# name the sets train_net and test_net

# train set should go from the first quarter of 2013 to the fourth quarter of 2021
train_net <- filter_index(.data=Netflix_ts, "2013 Q1" ~ "2021 Q4")

# view
glimpse(train_net)

# test set should go from the first quarter of 2022 to the second quarter of 2023
test_net <- filter_index(.data = Netflix_ts, "2022 Q1" ~ "2023 Q2")

# view 
glimpse(test_net)



## 2.5 CV
# Perform cross-validation using the train set. Use the stretch_tsibble() function

library(gt) # creating a table for submission

train_net %>%
     stretch_tsibble(.init=12, .step=4) %>% # with the .init arg set to 12 and the .step arg set to four.
     model(
          ETS=ETS(Subscribers ~ error("A") + trend("Ad") + season("A")), # ETS with additive error, damped additive trend, and additive seasonality
          ALGO=ETS(Subscribers), # ETS with no arguments. Algorithm (ALGO)
          LS=TSLM(Subscribers ~ trend() + I(trend()^2)), # TSLM with trend and trend squared, least squares model
          LSS=TSLM(Subscribers ~ trend() + I(trend()^2) + season())) %>% # TSLM with trend, trend squared, and seasonality
     forecast(h = 4) %>% # forecast four periods
     accuracy(Netflix_ts) # test accuracy against the test set


## Q2.7 Fit Object
# Use the Netflix_ts data to create an obj called fit that estimates only the top ETS
# model and the top TSLM by RMSE
# top models are ETS for ETS models and LSS for TSLM models

fit <- Netflix_ts %>%
     model(
          ETS=ETS(Subscribers ~ error("A") + trend("Ad") + season("A")),
          LSS=TSLM(Subscribers ~ trend() + I(trend()^2) + season())
     )

# view
glimpse(fit)


## Q2.8 Information Criterion
# Use fit obj to obtain AIC, AICc, and BIC. Start with fit obj and pipe to the glance()
# function to do this.

fit %>% 
     glance() %>%
     select(`.model`, "AIC", "AICc", "BIC")



## Q2.9 Forecast
# Use fit obj to forecast four quarters into the future (h=4)
fit %>%
     forecast(h = 4)


## Q2.10 Plot2
# Netflix subscribers and the ETS and LSTM forecasts below
fit %>%
     forecast(h = 4) %>%
     autoplot(Netflix_ts, level=90) + # 90% prediction intervals
     labs(x = "", y="", # omit x and y labels
          title = "Netflix Subscribers", # title Netflix Subscribers
          subtitle = "2013 Q1 to 2024 Q2 (estimated)") + theme_classic() # subtitle 2013 Q1 to 2024 Q2 (estimated)
