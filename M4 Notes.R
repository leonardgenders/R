## M3 Notes

# Ch2 Example Vid
install.packages('fpp3')
library(fpp3)

mydata <- tsibble(
     year=2015:2019,
     y = c(123, 39, 78, 52, 110),
     index = year
)

mydata <-tibble(
     year = 2015:2019,
     y = c(123, 39, 78, 52, 110),
) |> as_tsibble(index = year)

mydata

global_economy
tourism

prison <- readr::read_csv("data/prison_population.csv") |>
     mutate(Quarter = yearquarter(date)) |>
     select(-date) |>
     as_tsibble(
          index = Quarter,
          key = c(state, gender, legal, indigenous) # important to specify unique combinations
     )

PBS

# pull out only A10 drugs
PBS |>
     filter(ATC2 == "A10") |>
     select(Month, Concession, Type, Cost) |> # only keep the columns that we need
     summarise(TotalC = sum(Cost)) |> # summarise across the different concession groups and type
     mutate(Cost = TotalC/1e6) -> a10 # create a new variable Cost and put in millions, then assign to new variable a10



us_retail_employment <- us_employment |>
     filter(year(Month) >= 1990, Title == "Retail Trade") |>
     select(-Series_ID)

# Unfixed over time
us_retail_employment |>
     model(STL(Employed)) |>
     components() |>
     autoplot() + labs(title = "STL decomposition: US retail employment")

# fixed over time
us_retail_employment |>
     model(STL(Employed ~ season(window = 15) + trend(window = 15), robust = TRUE)) |> # seasonal pattern can be controlled with season argument
     components() |>
     autoplot() + labs(title = "STL decomposition: US retail employment")
     


# Avocado Data Set
library(tidyverse)

# load in the avocado dataset
avocado<- read_csv('avocado2020.csv')

# use spec() or glimpse() to view
spec(avocado)
# we can see that the date variable is a character type so we need to lubridate to coerce into a date
library(lubridate)
avocado$date<-mdy(avocado$date) # in 1/4/15 format (mdy)

glimpse(avocado)


# select the average_price and geography variables
# arrange in descending order by average_price and show the top 5
avocado %>% select(c(average_price, geography)) %>%
     arrange(desc(average_price)) %>% head(5)

# as noted on the insights page 1, there is an additional date in the data set that is between
# weeks (2018-01-01) - we need to remove this using the filter() function
avocado %>% filter(date != ymd("2018-01-01")) -> avocado

avocado %>% head(5)

# Use the filter() and select() functions to retrieve California's average price of organic avocados for 2015-2018
avocado %>% 
     filter(geography=="California", type=='organic',
            year<=2018) %>%
     select(date, average_price, geography) -> cali

# show the number of days between observations
cali %>% pull(date) %>% diff()



# dplyr insights video
library(tidyverse)

# Electric dataset from professor
Electric<-read_csv("https://jagelves.github.io/Data/Electric.csv")
glimpse(Electric)

# show only cars with range above 300km
filter(Electric, Range_km > 300)

# now add cars that cost less than 50,000 pounds
filter(Electric, Range_km > 300, Price_pounds < 50000)

# select variables
select(Electric, -Range_km)

# mutate - create another variable - pounds to dollars at 1.2 dollars per pound
mutate(Electric, Price_dollars=Price_pounds*1.2)

# if we want to add this to our Electric tibble, add Electric <- in front
Electric <- mutate(Electric, Price_dollars=Price_pounds*1.2)

# pipe operator
filter(Electric, Range_km > 300, Price_pounds < 50000) %>% 
     select(Make, Model, Price_pounds, Range_km)

# another piping example
# summarize data by make of car
Electric %>% group_by(Make) %>%
     summarise(Count=n(),
               Mean_Range=mean(Range_km)) # n() counts number of elements in a grou



# Let's visualize the data now
ggplot(data=cali) +
     geom_line(mapping=aes(x=date,y=average_price, group=geography), color="black") +
     theme_classic() +
     labs(x="",
          y="Average Price",
          title="Organic Avocado Price in California",
          subtitle="2015-2018")
# Interpretation: The average price of avocados in California increased during the period considered. It reached a maximum of about 2.60 in 2017 and a minimum of 1.10 in 2015. There is also a seasonal pattern with low prices at the beginning of the year and peaks mid-year. As we will see in upcoming modules, these are patterns that can be extrapolated and used to forecast time series.


# ggplot
# Electric dataset from professor
Electric<-read_csv("https://jagelves.github.io/Data/Electric.csv")

# view
glimpse(Electric)

# visualize range and price
Electric %>% ggplot() + 
     geom_point(aes(x=Range_km, y=Price_pounds)) # scatterplot

# graph mean range has increased over the years
Electric %>% group_by(Year)



avocado %>% 
     filter(geography=="Orlando", type=="organic", 
            year==2016) %>%
     select(average_price)
