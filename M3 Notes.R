## M3 Notes ##
# Insights 1 Problem

# Determine amount of fish needed
# triang dist below
# at least 20
# can seat 50
# likely 30

# purchase quantity = 9/16
# fish entitlement = 5 oz
# miso soup = $300

# charge per entry = $20

library(extraDistr)
Order_Oz<-160 # Decision Variable

Price_Fish_Oz <- 9/16 # Input
Price_Miso <- 300 # Input
Entry_Fee <- 20 # Input
Fish_Entitled_Oz <- 5 # Input

set.seed(20)
# Determine how many people we expect to attend
Attendance <- round(rtriang(1,20,50,30), 0) # Random Variable
print(Attendance)

# Determine how much fish we expect to be consumed
Consumption <- Attendance * Fish_Entitled_Oz # Calculated
print(Consumption)

Available <- Order_Oz # Calculated

Profit <- min(Consumption, Available)/Fish_Entitled_Oz*Entry_Fee - Order_Oz*Price_Fish_Oz - Price_Miso # Outcome

print(Profit)




# Insights 2 Redoing Problem 1
n<-10000
V_Order_Oz<- rep(Order_Oz, n)
V_Price_Fish_Oz<-rep(Price_Fish_Oz, n)
V_Price_Miso<-rep(Price_Miso, n)
V_Entry_Fee<-rep(Entry_Fee, n)
V_Fish_Entitled_Oz<-rep(Fish_Entitled_Oz, n)

set.seed(12)
V_Attendance<-round(rtriang(n,20,50,30), 0)
print(V_Attendance)
Consumption<-V_Attendance*V_Fish_Entitled_Oz
print(Consumption)
Available<-V_Order_Oz
print(Available)

V_Profit <- pmin(Consumption, Available)/V_Fish_Entitled_Oz*V_Entry_Fee - V_Order_Oz*V_Price_Fish_Oz - V_Price_Miso
print(mean(V_Profit))

# view all simulation results
hist(V_Profit, main="Expected Profits of an Order of 160 Ounces", xlab='')
abline(v=mean(V_Profit), lwd=2)

# Now we need to determine the optimal order amount

Order_Oz <- seq(160,240,10) # from 160 to 240 by 10
Price_Fish_Oz <- 9/16 # Input
Price_Miso <- 300 # Input
Entry_Fee <- 20 # Input
Fish_Entitled_Oz <- 5 #Input

# Profits vector
Profits <- c()

for (i in Order_Oz){
     n <- 100000
     V_Order_Oz <- rep(i,n) # replicate i n times
     V_Price_Fish_Oz <- rep(Price_Fish_Oz, n)
     V_Price_Miso <- rep(Price_Miso, n)
     V_Entry_Fee <- rep(Entry_Fee,n)
     V_Fish_Entitled_Oz <- rep(Fish_Entitled_Oz, n)
     
     set.seed(12)
     V_Attendance <- round(rtriang(n,20,50,30), 0) # Random Variable
     Consumption <- V_Attendance * V_Fish_Entitled_Oz # Calculated
     Available <- V_Order_Oz # Calculated
     
     V_Profit <- pmin(Consumption, Available)/V_Fish_Entitled_Oz*V_Entry_Fee - V_Order_Oz*V_Price_Fish_Oz - V_Price_Miso
     Profits <- c(Profits, mean(V_Profit))
}

(results <- data.frame(Order = Order_Oz, Profits = Profits))
# We can see that the optimal order is not 160, but instead 200 oz for a profit of 243.1558

# Sensitivity Analysis
# What if we offer 6 oz of fish?
# Now we need to determine the optimal order amount

Order_Oz <- seq(160,240,10) # from 160 to 240 by 10
Price_Fish_Oz <- 9/16 # Input
Price_Miso <- 300 # Input
Entry_Fee <- 20 # Input
Fish_Entitled_Oz <- 6 #Input

# Profits vector
Profits <- c()

for (i in Order_Oz){
     n <- 100000
     V_Order_Oz <- rep(i,n) # replicate i n times
     V_Price_Fish_Oz <- rep(Price_Fish_Oz, n)
     V_Price_Miso <- rep(Price_Miso, n)
     V_Entry_Fee <- rep(Entry_Fee,n)
     V_Fish_Entitled_Oz <- rep(Fish_Entitled_Oz, n)
     
     set.seed(12)
     V_Attendance <- round(rtriang(n,20,50,30), 0) # Random Variable
     Consumption <- V_Attendance * V_Fish_Entitled_Oz # Calculated
     Available <- V_Order_Oz # Calculated
     
     V_Profit <- pmin(Consumption, Available)/V_Fish_Entitled_Oz*V_Entry_Fee - V_Order_Oz*V_Price_Fish_Oz - V_Price_Miso
     Profits <- c(Profits, mean(V_Profit))
}

(results <- data.frame(Order = Order_Oz, Profits = Profits))
# We can see that the optimal order is no longer 200 oz with 6 oz offered per customer but now 240 oz and the profit is $220
# This is lower than the expected $243 profit from 5 oz per person

# Well, what if we wanted to show 7 oz per person?
# Now we need to determine the optimal order amount

Order_Oz <- seq(160,240,10) # from 160 to 240 by 10
Price_Fish_Oz <- 9/16 # Input
Price_Miso <- 300 # Input
Entry_Fee <- 20 # Input
Fish_Entitled_Oz <- 7 #Input

# Profits vector
Profits <- c()

for (i in Order_Oz){
     n <- 100000
     V_Order_Oz <- rep(i,n) # replicate i n times
     V_Price_Fish_Oz <- rep(Price_Fish_Oz, n)
     V_Price_Miso <- rep(Price_Miso, n)
     V_Entry_Fee <- rep(Entry_Fee,n)
     V_Fish_Entitled_Oz <- rep(Fish_Entitled_Oz, n)
     
     set.seed(12)
     V_Attendance <- round(rtriang(n,20,50,30), 0) # Random Variable
     Consumption <- V_Attendance * V_Fish_Entitled_Oz # Calculated
     Available <- V_Order_Oz # Calculated
     
     V_Profit <- pmin(Consumption, Available)/V_Fish_Entitled_Oz*V_Entry_Fee - V_Order_Oz*V_Price_Fish_Oz - V_Price_Miso
     Profits <- c(Profits, mean(V_Profit))
}

(results <- data.frame(Order = Order_Oz, Profits = Profits))
# Best profit is now with 240 oz for $189, so profiving each guest with more fish is a bad ideas as profits are 
# highly sensitive to every ounce increase




# Exercise - Question 1
# Now we need to determine the optimal order amount

Fixed <- 20000 # Input
Variable <- 15 # Per unit produced
Revenue <- 100 # Per unit sold

Production <- 10000 # producing 10,000 units
n <- 100000000 # 100,000 replications

# Demand
Demand_Probabilities <- c(.10, .20, .30, .25, .15)
Demand_Values <- c(6000,7000,8000,9000,10000)
# Simulate demand
simulated_demand <- sample(Demand_Values, n, replace = TRUE, prob = Demand_Probabilities)

Profit <- pmin(simulated_demand, Production)*Revenue - Variable*Production - Fixed
print(mean(Profit))

# Standard Deviation
print(sd(Profit))

# 90% Confidence Interval
standard_error <- sd(Profit)/sqrt(n)
print(standard_error)
multiple <- 2

# Quantile
CI <- quantile(Profit, probs=c(.05, .95))
print(CI)





# Exercise - Question 2 
# Load necessary library
library(extraDistr)
# Normal dist 
mean <- 20
stdev <- 5

# unused seat = $300
# overbooked seat = $600
# n = 10,000 simulation

# optimal seats from 10 to 30 by 5
Seats <- seq(10,30,1) # from 10 to 30 by 5 (go by 1 to see all seat options)
Price_Underbook <- 300 # Input
Price_Overbook <- 600 # Input

n <- 10000 # 10,000 replications
V_Arrivals <- round(rnorm(n, 20, 5), 0) # Random Variable

# Costs vector
Costs <- c()

for (i in Seats){
     V_Seats <- rep(i, n) # simulate number of seats
     
     # Calculate Under and Over booking costs
     Underbooking <- ifelse(V_Arrivals < V_Seats, (V_Seats - V_Arrivals) * Price_Underbook, 0) # if the arrivals are less than seats, we underbooked, otherwise 0
     Overbooking <- ifelse(V_Arrivals > V_Seats, (V_Arrivals - V_Seats) * Price_Overbook, 0) # if the arrivals are greater than seats, we overbooked, otherwise 0
     
     # total costs now
     Total_Cost <- Underbooking + Overbooking
     Costs <- c(Costs, mean(Total_Cost))
}

(results_air <- data.frame(Seats = Seats, Costs = Costs))
print(results_air)

#  Costs     Seats
# 1 6020.31    10
# 2 3363.57    15
# 3 1789.26    20
# 4 1875.63    25
# 5 3044.25    30




# Exercise - Question 3
# Import necessary libraries
library(extraDistr)
library(tidyverse)
library(gt)
rm(list=ls())

# Part 1: Demand modeled by a Triangular Distribution
min <- 500
mode <- 1500
max <- 2500

# Part 2: Supply modeled by a Beta Distribution
alpha <- 2
beta <- 5
max_capacity <- 3000

# Cost and revenue parameters
fixed_cost <- 10000
variable_cost <- 15
selling_price <- 100
salvage_value <- 30

# number sims
nsims <- 1000000

# Production targets we want to test
production_targets <- seq(1000, 3000, by = 500)

# empty vector to collect the profits generated by each production target suggested
profits <- c()

for (i in production_targets){
simulation <- tibble(
     demand = round(rtriang(nsims, min, max, mode), 0),
     max_production = round(rbeta(nsims,alpha,beta)*3000, 0),
     actual_production = map2_dbl(.x = max_production,
                                       .y = i,
                                       .f = ~min(.x, .y)),
     units_sold = map2_dbl(.x = demand,
                                .y = actual_production,
                                .f = ~ min(.x, .y)),
     leftover_units = map2_dbl(.x = actual_production,
                                    .y = units_sold,
                                    .f = ~ max(.x-.y, 0)),
     revenue = units_sold * selling_price,
     production_cost = fixed_cost + actual_production*variable_cost,
     salvage_revenue = map2_dbl(.x = leftover_units,
                                     .y = 1000,
                                     .f = ~ min(.x, .y)*salvage_value),
     profit = revenue - production_cost + salvage_value)
     profits<-c(profits, mean(simulation$profit))
}

(results <- data.frame(production_targets=production_targets, profits=profits))

# Quantile
CI_3 <- quantile(profits)
print(CI_3)




## Exercise - Question 4
initial <- 1000 # input
mean <- 0.1 # 10% annually
standard_dev <- 0.18 # 18% standard deviation
investment <- 6000 # annual investment
period <- 20 # over 20 years

# Calculate return
nsimulations <- 10000

# capture the results annually
portfolio <- c()

# account for year 0, starting
portfolio <- initial

# collect the results
status <- c()

for (i in 1:20){
     # norm dist annual return
     annual_return <- rnorm(nsimulations, mean, standard_dev)
     portfolio <- (portfolio + investment)*(1 + annual_return)
     status <- c(status, i, mean(portfolio))
}


print(mean(portfolio)) # Expected End Value after 20 years

summary(portfolio)


# Exercise - Question 5
