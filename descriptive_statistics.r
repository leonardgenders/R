# CHALLENGE #1: Histogram and Skewness
# Using the dataset 'airquality' from base R, load the dataset
# and use the variable 'Temp' - renamed 'airqual'
library(tidyverse)
library(semTools)
airqual <- airquality

# Define and determine mean of Temp
Temp <- airqual$Temp
mean(Temp)

# Find median of Temp
median(Temp)

# Find mode of Temp, low to high with first value returned
names(x = sort(x = Table(Temp), decreasing = TRUE))[1]

# Evaluate skewness of Temp variable
semTools::skew(object = airqual$Temp)
min(airqual$Temp)
max(airqual$Temp)

# Visually evaluate the skewness of Temp with a histogram
airqual %>%
  ggplot(aes(x = Temp)) + geom_histogram(binwidth = 5, boundary = 55, 
  color = "black", fill = "blue") + labs(title = "Histogram of Temperatures
  at La Guardia Airport", x = "Temperature in degrees Farenheit", y = "Frequency")


# CHALLENGE #2: Summary commands, dataset sorting and ordering, data filtering

# Read in the 'Emp.csv' dataset from the working directory
empdata <- read_csv("Emp.csv")
summary(empdata)

