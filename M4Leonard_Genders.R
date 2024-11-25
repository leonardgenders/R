# title: "M4 Assignment" 
# date: "11/24/2024" 
# author: Leonard Genders 

# Q2.1 Loading packages and data
# load fpp3 and tidyverse packages
library(fpp3)
library(tidyverse)

# Load HomePrices.csv dataset with the read_csv() function and save it in an object called Homes
Homes <- read_csv("https://jagelves.github.io/Data/HomePrices.csv")

# Use the glimpse() function to preview the data
glimpse(Homes)


# Q2.3 Piping
# Start with Homes and use the pipe operator
# filter the data so that only 'two-bedroom' houses are included
Homes %>% filter(type =='house', bedrooms == 2) %>%
     # coerce the saledate variable to a date using the dmy() function, using mutate
     mutate(saledate = dmy(saledate)) %>% 
     # format the saledate variable to quarters using the yearquarter() function
     mutate(saledate = yearquarter(saledate)) %>%
     # remove the bedrooms and type variables
     select(saledate, Price) -> h2 # save to a new object called h2
head(h2, 5) # view to confirm


# Q2.4 tsibble
# Coerce the h2 tibble into a tsibble using the as_tsibble() function
h2 %>%
     as_tsibble(index=saledate, regular=T) -> h2_ts # save the new tsibble in an obj called h2_ts

# view
head(h2_ts, 5)


# Q2.5 Plot
# lineplot of the Price variable using the ggplot() function
ggplot(data=h2_ts) +
     geom_line(mapping=aes(x=saledate, y=Price), color='black') +
     theme_classic() +
     labs(x = '',
          y='',
          title='Quarterly Home Prices', # title as "Quarterly Home Prices"
          subtitle='2007 Q3 to 2019 Q3') # subtitle that reads "2007 Q3 to 2019 Q3"


# Q2.6 STL
h2_ts %>% 
     model(STL = STL(Price~trend(window=11) + season(window='periodic')))


# Q2.7 STL Components
h2_ts %>% 
     model(STL = STL(Price~trend(window=11) + season(window='periodic'))) %>% 
     components()


# Q2.8 STL Plot
h2_ts %>% 
     model(STL = STL(Price~trend(window=11) + season(window='periodic'))) %>% 
     components() %>%
     autoplot() + theme_classic()
