# Task 1: Histogram and Skewness
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


# Task #2: Summary commands, dataset sorting and ordering, data filtering
# Read in the 'Emp.csv' dataset from the working directory
empdata <- read_csv("Emp.csv")
summary(empdata)

# Create an object 'sortAns' that saves ordered data of the entire dataset by variable EDUC. Decreasing = FALSE. Print head of sortAns.
sortAns<-sort(x =table(empdata$EDUC),decreasing =FALSE)head(sortAns)

# Create an object 'filterAns' that filters gender by
# equaling Male and print the head of filterAns
filterAns<-filter(empdata, Gender=="Male")
head(filterAns)


# Task #3: Table frequencies
# Load the Pima.tr dataset from the MASS library and 
# summarize the data
library(MASS)
pimaData <- Pima.tr
summary(pimaData)

# Calculate the frequencies of the _type_ var, suggesting whether the person was diabetic
# according to WHO criteria and save as 'Frequencies'
Frequencies <- table(pimaData$type)
Frequencies

# Calc and retrieve the relative frequencies of the _type_ variable and output the information
# to decimal values
prop.table(Frequencies)
# There are 132# observations of people who were not diabetic and 68 people who were
# diabetic according to the WHO criteria. The relative frequency of
# not being diabetic is 66% or 0.66 and of being diabetic is 34% or
# 0.34. The relative frequency is found by using prop.table() by
# dividing the _type_ variable categories of No and Yes by the total
# sample size of 20


# Task 4: Fully Summarize a Vector
# Calc the three types of central tendency for the age var
# in pimaData and report the answers
mean.age <- mean(pimaData$age)
median.age <- median(pimaData$age)
mode.age <- names(sort(x = table(pimaData$age), decreasing = TRUE))[1]
mean.age
median.age
mode.age

# Give the spread with report to the mean with var, std, and kurtosis.
var.age<-var(pimaData$age,na.rm =TRUE)
sd.age<-sd(pimaData$age,na.rm =TRUE)
krtsis.age<-kurtosis(pimaData$age)
var.age
sd.age
krtsis.age
# Variance = 120.4602, STD DEV = 10.97544, Kurtosis = 0.3043919, since the 
# Kurtosis() function subtracts 3 from the kurtosis, a positive value indicates
# a leptokurtic distribution. Since the z value is within -3.29 to 3.29 at 
# 0.8787036, it is not problematic.

# Give the spread with report to the median, including the range, and the IQR. 
range.age <- range(pimaData$age, na.rm = TRUE)
iqr.age <- IQR(pimaData$age, na.rm = TRUE)
range.age
iqr.age
# Range = 21 - 63, IQR = 16.25

# Give the spread with report to the mode, including the B index of the age 
# variable
qualvar::B(x = table(pimaData$age))
# Bindex = 0.2726595

# Evaluate the skewness of the age variable visually witha histogram and 
# using the skew() function
semTools:skew(object = pimaData$age)
min(pimaData$age)
max(pimaData$age)
# Skew = 1.096218
pimData %>%
  ggplot(aes(x = age)) + geom_histogram(bindwidth = 5, boundary = 20, color = "red", fill = "turquoise")
  + labs(title = "Histogram of Pima Indian Women who were tested for Diabetes", x = "Age (Years)", y = "Frequency")
# z-value = 6.32910; since data size is between 50-300 normal is considered between -3.29 and 3.29. Since the z-value
# is larger than 3.29, the data is not normal. Examining the mean, median, and mode of the 
# age variable reveals that the mean of 32.11 is greater than the median of 28 and the mode
# of 21. Therefore, we can condluce the age variable is right-skewed. 
