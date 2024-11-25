'title: "M3 Assignment"
author: "Leonard Genders"
date: "14 November 2024"'

## Q2.1 Basic Model
# Decision Variable
num_reservations <- 5000 # number of reservations

# Inputs
regular_cost <- 150 # cost per room input
late_cost <- 250 # cost per late room input

# Random Variable
set.seed(10)
attendance <- round(rnorm(10000,5000,1000)) # random variable (return 10,000 numbers, mean 5000, stdev 1000)


# Q2.2 Extra Rooms
extra_rooms <- c() # record the number of extra rooms needed

# use a for loop to populate the vector by comparing the attendance
# and num_reservations objects

for (i in 1:10000){
     if(attendance[i] > num_reservations){
          extra_rooms <- c(extra_rooms, attendance[i] - num_reservations)
     } else {
          extra_rooms <-c(extra_rooms, 0)
     }
}

# view extra_rooms vector
extra_rooms

# confirm 10,000 elements in extra_rooms
length(extra_rooms)


## Q2.3 Reservation Cost
reservation_cost <- rep(regular_cost * num_reservations, 10000)

# view reservation_cost
reservation_cost

# check 10,000 reservation costs
length(reservation_cost)


## Q2.4 Total Cost
late_reservation_cost <- c()

# loop over the extra_rooms to get the late_reservation_cost and capture in late_reservation_cost object

for (i in 1:10000){
     late_reservation_cost <- c(late_reservation_cost, extra_rooms[i] * late_cost)
}

# view late_reservation_cost
late_reservation_cost

# check length for 10,000 elements
length(late_reservation_cost)

# check for index 1
extra_rooms[1] * late_cost # 19 * 250 = 4750 confirmed

# final object that caputres the total cost
total_cost <- reservation_cost + late_reservation_cost

# view total_cost
total_cost

# check index 1
reservation_cost[1] + late_reservation_cost[1] # 750,000 + 4750 = 754,750 confirmed


## Q2.5 Statistics
# Mean
mean(total_cost) # 850344.8

# Standard Deviation
sd(total_cost) # 147069.1


## Q2.6 Optimal Number of rooms
# 5,000 rooms = $850,344.80
# 4,900 rooms = $848,316.40
# 4,800 rooms = $847,273.10
# 4,700 rooms = $847,239.30 # lowest cost
# 4,600 rooms = $848,207.30
# 4,500 rooms = $850,104.50
# 4,400 rooms = $852,849.20
# 4,300 rooms = $856,414.90
# 4,200 rooms = $860,786.60
# 4,100 rooms = $865,914.10
# 4,000 rooms = $871,646.40 # highest cost

# what if we just use the mean - what is the tradeoff
850344.8 - 847239.3

