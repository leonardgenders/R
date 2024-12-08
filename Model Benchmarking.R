# title: M5Leonard_Genders.R
# author: Leonard Genders
# date: 11/29/2024

## Q2.1 Loading Packages and Data
# load fpp3 and tidyverse packages
library(fpp3)
library(tidyverse)

# load the avocado2020.csv dataset using the read_csv() function, save to obj called avocado
avocado <- read_csv("https://jagelves.github.io/Data/avocado2020.csv")

# view
glimpse(avocado)


## Q2.2 Piping1
# start with the avocado object
avocado %>%
  
  # remove the observations with a date of 1/1/18 using the filter(function)
  # CHECK before first step
  # filter(date == "1/1/18") # 98 rows
  filter(date != "1/1/18") %>%

  # use the mutate() and replace() functions to replace observations with a date of 1/7/19 to 1/6/19  
  # CHECK before next step
  # filter(date == "1/7/19") # 98 rows with 1/7/19
  mutate(date = replace(date, date == "1/7/19", "1/6/19")) %>%

  # use the mutate() and replace() functions to replace observations with a date of 1/6/20 to 1/5/20
  # CHECK before next step
  # filter(date == "1/6/20") # 98 rows with 1/6/20
  mutate(date = replace(date, date == "1/6/20", "1/5/20")) %>%
  
  # filter the data so that the geography included is "Richmond/Norfolk" and the type of avocado is "organic
  filter(geography == "Richmond/Norfolk", type == "organic") %>%
  
  # select only the date and average_price variables
  select(date, average_price) -> RVA # save to new object called RVA

# view - should have 305 observations and two variables
glimpse(RVA)


## Q2.3 Piping2
# start with the RVA object
RVA %>%
  
  # use the mutate() function to coercce the date variable to a date that has the year and wk by nesting the mdy() function inside of the yearweek() function
  mutate(date = yearweek(mdy(date))) %>%
  
  # coerce the data to a tsibble using the as_tsibble() function
  as_tsibble(index = date) %>%
  
  # filter the period "2019-01-07" to "2020-11-29" by using the filter_index() function
  filter_index("2019-01-07"~"2020-11-29") -> RVA_ts # save result in an object called RVA_ts

# view - should have 99 obs and two vars
glimpse(RVA_ts)


## Q2.4 Train and Test Sets
# create a test and training set and them them into objects called train_RVA and test_RVA
# training set should include the period 2019-01-06 to 2020-10-25
RVA_ts %>%
  filter_index("2019-01-06"~"2020-10-25") -> train_RVA
# view
glimpse(train_RVA) # 94 obs

# test set should include the period 2020-11-01 to 2020-11-29
RVA_ts %>%
  filter_index("2020-11-01"~"2020-11-29") -> test_RVA
# view
glimpse(test_RVA) # 5 obs


## Q2.5 Autoplot
# plot the avg price of avocadoes using RVA_ts in Richmond/Norfolk using the autoplot() function
RVA_ts %>%
  autoplot(average_price) + theme_classic() +
  labs(y='', title="Weekly Avocado Price in Richmond/Norfolk",
       subtitle = "2019 to 2020",
       x='')


## Q2.6 Fit Models
# fit the following models using the mdoel() and TSLM() functions on the training set
train_RVA %>% model(LS=TSLM(average_price~trend()),
                    LS3=TSLM(average_price~trend()+I(trend()^2)+I(trend()^3)),
                    LS6=TSLM(average_price~trend()+I(trend()^2)+I(trend()^3)+I(trend()^4)+I(trend()^5)+I(trend()^6))) -> fit # save in obj called fit
# view
glimpse(fit)


## Q2.7 Plot1
# create a plot that shows the training set and the fitted values of all the models estimated (LS, LS3, and LS6)
# red line for LS model, orange line for LS3 model, and blue line for LS6 model
train_RVA %>%
  autoplot(average_price) + theme_classic() + # training set
  geom_line(aes(y = .fitted), col = "red",
            data = augment(fit) %>% 
              filter(`.model`=="LS")) +
  geom_line(aes(y = .fitted), col="orange",
            data = augment(fit) %>%
              filter(`.model`=="LS3")) + 
  geom_line(aes(y = .fitted), col = "blue",
            data = augment(fit) %>%
              filter(`.model` =="LS6")) +
  labs(y='', title="Weekly Avocado Price in Richmond/Norfolk",
       subtitle = "2019 to 2020",
       x='')


## Q2.8 gt Package
# load the gt package
library(gt)

# use piping to accomplish the following tasks and start with the fit object
fit %>% 
  accuracy() %>% # use the accuracy() function to find the model's fit for the training set
  select(.model, RMSE) %>% # select the variables .model and RMSE
  arrange(RMSE) %>% # arrange the data using the arrange() function by the RMSE
  gt() %>% # use the gt package
  tab_style(locations=cells_body(rows = RMSE == min(RMSE)),
            style=cell_fill(color="lightgreen")) # highlight the row(model) with the lowest RMSE


## Q2.9 Information Criterion
fit %>% 
  glance() %>%
  select('.model',"AIC","AICc","BIC") # lowest BIC is LS6 model at -346


## Q2.10 CV
# Use piping to perform cross-validation. Start with the train_RVA obj
train_RVA %>%
  # use the stretch_tsibble() function with the argument .init set to 20 and the .step arg set to five
  stretch_tsibble(.init=20, .step=5) %>%
  
  # estimate the LS, LS3, and LS6 models once again using the model() and TSLM() function
  model(LS=TSLM(average_price~trend()),
        LS3=TSLM(average_price~trend()+I(trend()^2)+I(trend()^3)),
        LS6=TSLM(average_price~trend()+I(trend()^2)+I(trend()^3)+I(trend()^4)+I(trend()^5)+I(trend()^6))) %>%

  # forecast five periods (h=5) using the forecast() function
  forecast(h=5) %>%
  
  # use the accuracy() function with the RVA_ts as only input
  accuracy(RVA_ts)

