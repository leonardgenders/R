# title: M6 Notes
# author: Leonard Genders
# date: 12/2/2024

# Error, Trend, and Seasonality (ETS) modeling
# using to provide a flexible approach for modeling and forecasting
# time sereis data by incorporating elements for error, trend, and seasonality

# Simple Exponential Smoothing (SES) Model - give more weight to recent obs 
# and less to older obs

# Tesla Problem - use ETS to forecast Tesla deliveries

# data from https://ir.tesla.com/#quarterly-disclosure
library(fpp3)

# Create tsibble
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

# create a plot of the deliveries using the autoplot() function
tesla %>%
     autoplot(.vars=deliveries) + theme_classic() +
     labs(title = "Tesla Car Deliveries",
          subtitle = 'Q1 2017 to Q2 2023') +
     xlab("Quarter") + ylab(" ")
# shows an exponential trend with a slight seasonal component in Q4 that other quarters

# STL decomposition (seasonal and trend decomp using Loess - for nonlinear relationships)
tesla %>%
     model(STL(deliveries~trend() + season())) %>%
     components() %>%
     autoplot() + theme_classic()

# Now model the data and create the appropriate forecasts
# Generate test and training sets from the available Tesla data
train_tesla <- filter_index(.data=tesla, "2016 Q1" ~ "2021 Q4")
test_tesla <- filter_index(.data=tesla, "2022 Q1" ~ "2023 Q2")

# Estimate the models and the results of the cross validation
library(gt) # table viewing

train_tesla %>% stretch_tsibble(.init = 8, .step = 4) %>% # reshape the tsibnlle to accomodate for cross validation
     model(
          SES=ETS(deliveries ~ error("A") + trend("N") + season("N")), # SES with additive errors
          HOLT=ETS(deliveries ~ error("A") + trend("A") + season("N")), # Holt model that includes an additive trend
          DAMPED=ETS(deliveries ~ error("A") + trend("Ad") + season("N")), # Dampened trend model
          DAMPEDS=ETS(deliveries ~ error("A") + trend("Ad") + season("A")), # Dampened model with seasonality
          ALGO=ETS(deliveries),
          LS=TSLM(deliveries ~ trend() + I(trend()^2)), # time series linear model, least squares
          LSS=TSLM(deliveries ~ trend() + I(trend()^2) + season())) %>% # quadratic model with seasonality dummies
     forecast(h = 4) %>% # 4 steps (1 year)
     accuracy(tesla) %>% # accuracy measures for tesla
     select(-"ACF1") # all but the ACF1 metrics

 # accuracy measure reveal the DAMPEDS and LSS models perform consistently well
# continue with the DAMPEDS and LSS models as the trend seems to be exponential and there
# seems to be evidence of seasonality
# models are estimated and saved into object called fit below
fit <- tesla %>%
     model(
          DAMPEDS = ETS(deliveries ~ error("A") + trend("Ad") + season("A")),
          LSS = TSLM(deliveries ~ trend() + I(trend()^2) + season())
     )

# retrieve model coefficients by using tidy() or coef()
# retrieve the coefficients of the least squares model with seasonality
tidy(fit) %>%
     filter(.model == "LSS") %>%
     select(-".model")
# output reveals that the seasonality dummy for Q4 is statistically significant at the 10% level
# confirming the seasonal pattern found in the decomposition 

# plot to show the fit of the models with the blue being LSS and red being DAMPEDS
tesla %>% 
     autoplot(deliveries, lwd=1.2, alpha=0.5) + theme_classic() +
     geom_line(aes(y = .fitted), col='blue',
               data = augment(fit) %>% filter(`.model`=='LSS')) +
     geom_line(aes(y = .fitted), col='red',
               data = augment(fit) %>% filter(`.model`=="DAMPEDS")) + 
     labs(title = "Tesla Car Deliveries Fitted Values",
          subtitle = "Q1 2017 to Q2 2023") +
     xlab("Quarter") + ylab(" ")


# Attempt to select models via the AIC, AICc, or BIC. Summarize the measure for the models considred
train_tesla %>%
  model(
    SES=ETS(deliveries ~ error("A") + trend("N") + season("N")),
    HOLT=ETS(deliveries ~ error("A") + trend("A") + season("N")),
    DAMPED=ETS(deliveries ~ error("A") + trend("Ad") + season("N")),
    DAMPEDS=ETS(deliveries ~ error("A") + trend("Ad") + season("A")),
ALGO=ETS(deliveries),
LS = TSLM(deliveries ~ trend()+I(trend()^2)),
LSS = TSLM(deliveries ~ trend()+I(trend()^2)+season())) %>% 
  report()  %>%
  select('.model',"AIC","AICc","BIC")
# interpretation - LSS model appears to perform the best as it provides the lowest values
# among the ETS models, the ALGO model now stands out - the ALGO model is designed to select ETS
# components that minimize the AIC

# forecast four quarters using the forecast() function
library(gt)
fit %>%
     forecast(h = 4) %>%
     select(-'.model') -> deliveries_fc
deliveries_fc

# plot the forecasts for both models along with the 95% prediction intervals
fit %>%
     forecast(h = 4) %>%
     autoplot(tesla, level=95) +
     labs(x = "Quarter", y="",
          title = "Tesla Car Delivieries Forecasts",
          subtitle = "Q1 2017 to Q2 2023") + theme_classic()


# On October 9, 2023, Tesla announced 435,059 deliveries for Q3, signaling strong performance. However, during the previous earnings call, Tesla had already cautioned investors about potential delivery impacts due to planned factory shutdowns and upgrades. These shutdowns are necessary to retool production lines for their upcoming vehicles, the Highland Model 3 and Cybertruck. Interestingly, a similar factory shutdown occurred in Q2 2022, giving us valuable historical data to make more precise predictions.

# To factor in the effect of these shutdowns, we can introduce a dummy variable, where we assign a value of 1 during shutdown quarters and 0 otherwise.
tesla <- mutate(tesla,
                Down=case_when(
                     period==yearquarter("2022 Q2") ~ 1,
                     TRUE ~ 0))
# Down var captures the impact of factory shutdowns on deliveries
# now we can incorporate this info into our TSLM to estimate Tesla's deliveries more accurately
fit <- tesla %>%
  model(
    LSS = TSLM(deliveries~trend()+
                 I(trend()^2)+season()+Down)
  )

# now explore 2 scenarios - where Tesla undergoes a factory shutdown and another where
# production continues uninterrupted
# use the scenarios() function to create a dataset for forecasting over the next four periods, simulating both situations
Down_Scenarios <- scenarios(
     Factory_Down = new_data(tesla, 4) %>%
          mutate(Down=rep(1,4)),
     Factory_Up = new_data(tesla, 4) %>%
          mutate(Down=rep(0,4)),
     names_to = "Scenario")

# Visualize how each scenario affects Tesla's future delivery forecasts
tesla %>%
     autoplot(deliveries) +
     autolayer(forecast(fit, new_data=Down_Scenarios), 
                level=NULL) + theme_classic() +
     labs(x="Quarter", y=" ",
          title = 'Tesla Car Deliveries Forecasts',
          subtitle = "Scenario Forecast")
# The new forecast of 429,404 is now closer to the reported delivery number of 435,059 for Q3 2023 when compared to the analyst consensus estimate of 454,809. For the 4th quarter of 2023 the analyst consensus was 480,500, which is inline with the forecast of the LSS model (about 480,000).

# Determine a 95% confidence interval using the hilo() function
fit %>% forecast(new_data=Down_Scenarios) %>%
     hilo(95) %>% unpack_hilo("95%") %>%
     as_tibble() %>%
     select(Down, period, .mean, `95%_lower`, `95%_upper`) %>%
     gt() %>% fmt_number(columns=c(.mean, `95%_lower`, `95%_upper`),
                         decimals = 2)
