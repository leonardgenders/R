# title: "Netflix Subscriber Forecasting in Q4 2024"
# date: "12/13/2024"
# author: Leonard Genders

## Q3 Netflix
# Update with current information - Q3 2024 information available and at 282.72 million subscribers
# Citation: Stoll, J. (20204, October 18). Quarterly Netflix subscribers count worldwide 2013-2024. Retreived on December 13, 2024
# from https://www.statista.com/statistics/250934/quarterly-number-of-netflix-streaming-subscribers-worldwide/

# Method: Retrieved the dataset provided by Dr. Gelves from https://jagelves.github.io/Data/Netflix.csv and saved the csv
# file locally, added the Q3 2024 information for 282.72 million subscribers and saved again locally to use for Q3.

# load the updated data
Netflix <- read_csv("Netflix_updated.csv")

# view
glimpse(Netflix)
# Quarter not in yearquarter() format

# updated Quarter variable and coerce into a date that captures the yr/qtr
# create tsibble
Netflix %>%
  mutate(Quarter = yearquarter(Quarter)) %>% 
  as_tsibble(index=Quarter) -> Netflix_ts # naming the resulting tsibbble Netflix_ts

# view 
glimpse(Netflix_ts)

# view plot of Netflix_ts with updated 2024 Q3 info
Netflix_ts %>%
  autoplot(.vars = Subscribers) + theme_classic() +
  labs(title = "Netflix World Wide Subscribers (Millions)",
       subtitle = "2013 Q1 to 2024 Q3")

# Creating test and train sets
## Q2.4 Test and Train Sets
# Use the filter_index() function to create train and test sets
# name the sets train_net and test_net

# train set should go from the first quarter of 2013 to the fourth quarter of 2022
Netflix_train <- filter_index(.data=Netflix_ts, "2013 Q1" ~ "2022 Q4")

# view
glimpse(Netflix_train)
# 40 rows, 2 cols


# test set should go from the first quarter of 2023 to the third quarter of 2024
Netflix_test <- filter_index(.data = Netflix_ts, "2023 Q1" ~ "2024 Q3")

# view 
glimpse(Netflix_test)
# 7 rows, 2 cols


# Cross-validation using the Netflix_train set and the stretch_tsibble() function
# Will select the top to forecast
Netflix_train %>%
  stretch_tsibble(.init=12, .step=4) %>% # with the .init arg set to 12 for one year, and step for one quarter at a time
  model(
    ETS=ETS(Subscribers ~ error("A") + trend("Ad") + season("A")), # ETS with additive error, dampened additive trend, and additive seasonality
    ALGO=ETS(Subscribers), # ETS with no arguments for baseline, Algorithm (ALGO)
    LS=TSLM(Subscribers ~ trend() + I(trend()^2)), # TSLM with trend and trend squared, least squares model
    ARIMA=ARIMA(Subscribers)) %>% # ARIMA with no args to select the best one
  forecast(h = 1) %>% # forecasting 8 periods or two years (8 quarters)
  accuracy(Netflix_ts) %>% # test accuracy against the test set
  arrange(RMSE) # arrange by lowest RMSE

# best RMSEs in order are ETS, ALGO, ARIMA, LS
# interesting note: first try was ETS(A,A,A) with RMSE of 4.75, added dampened additive trend on second time and RMSE dropped to 4.69, keeping "Ad" trend

# now fitting with top model
fit <- Netflix_ts %>%
  model(
    ETS=ETS(Subscribers ~ error("A") + trend("Ad") + season("A"))) # ETS with additive error, dampened additive trend, and additive seasonality

# view
glimpse(fit)
# ETS = ETS(A,Ad,A)


# obtain AIC, AICc, and BIC
fit %>% 
  glance() %>%
  select(".model", "AIC", "AICc", "BIC")


# ASSIGNMENT: Provide a forecast of Netflix world-wide subsribers for the upcoming quarter
fit %>%
  forecast(h=1)
# Forecast for Netflix subscribers is a mean of 292M and standard deviation of 9.3M subscribers. 


# Visualize
fit %>%
  forecast(h = 1) %>%
  autoplot(Netflix_ts, level=95) + # 95% prediction intervals
  labs(x = "", y="", # no x or y labels
       title = "Netflix Subscribers", # title Netflix Subscribers
       subtitle = "2013 Q1 to 2024 Q3 (estimated)") + theme_classic() # subtitle 2013 Q1 to 2024 Q3 (estimated)

