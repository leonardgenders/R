# CHALLENGE #1: Creating a Data Frame from three variables
name <- c("Patient1", "Patient2", "Patient3")
age <- c(18, 21, 23)
covidResult <- c("Negative", "Positive", "Negative")

# Combine the new vectors into a df names healthScreening
healthScreening <- data.frame(name, age, covidResult)

# Determine the data types for name, age, covidResult
summary(object = healthScreening)

# Update covidResult as factor type
healthScreening$covidResult <- as.factor(x = healthScreening$covidResult)
class(x = healthScreening$covidResult)

# Summarize the df
summary(object = healthScreening)


# CHALLENGE #2: Identifying Missing Data, calculate mean of immunization variable ignoring blanks,
# produce the number of obs in WHO data set, omit the NAs and save as new obj. Produce number of 
# obs in new data set after NAs removed and list in comments how many were removed overall.

# Read the WHO data
whoDATA <- read.csv("WHO.csv", stringsAsFactors = TRUE, na.strings = "NA")

# Display the dimensions
dim(whoDATA)

# Identify which obs in 'Life' col are NA
which(is.na(whoDATA$Life))

# Calculate mean of immunization variable w/o blanks
mean(whoDATA$immunization, na.rm = TRUE)

# Produce the number of observations in whoDATA
nrow(whoDATA)

# Omit NAs and save as a new object called who_small
who_small <- na.omit(whoDATA)

# Produce the number of observations in who_small
nrow(who_small)


# CHALLENGE #3: Calculating Immunizaions levels greater than 90%, calculating the length after 
# filtering by AdRestrictionsTVBeer being banned, sorting data by variables

# Calculate the number of countries whose immunization level is >90%
length(which(who_small$immunization > 90)

# Calculate the length after filtering by AdRestrictionsTVBeer banned
length(which(who_small$AdRestrictionsTVBeer == 'ban))

# Sort the data from high to low using GDP as the sorted variable
# Save the answer in an object called sortGDP
sortGDP <- who_small[order(who_small$GDP, decreasing = TRUE), ]


# CHALLENGE #4: Vectors, calculating maximum and minimum values, sorting data

# Create a vector x with 92, 76, 31, 67, 27, 65, 13, 76, 72, 69
x <- c(92, 76, 31, 67, 27, 65, 13, 76, 72, 69)

# Calculate the max value of x
max(x)

# Calculate the minimum value of x
min(x)

# Sort x from low to high
sort(x)

