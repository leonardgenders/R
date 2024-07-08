# Task 1: Boxplot and Violin
# Load the chickwts dataset from R, which has two vars
# weight (continuous var) and feed (factor)
library(tidyverse)
library(ggplot2)
chickwts.data <- chickwts
summary(chickwts.data)

# Create a boxplot using ggplot with colors of your choice.
box.chickwts.data <-chickwts.data %>% 
  ggplot(aes(x =reorder(x =feed,X =-weight),y =weight))
  +geom_boxplot(aes(fill =feed),alpha =0.7)+theme_minimal()+labs(x ="Feed type",y ="Weight in grams of 6 wk old chick")
  +coord_flip()+scale_fill_manual(values =c(horsebean ="red4",linseed ="orange4",soybean ="yellow4",meatmeal ="green4",casein ="green3",sunflower ="green",guide =FALSE))
box.chickwts.data

# Create violin plot from boxplot
violin.chickwts.data<-chickwts.data %>%
  ggplot(aes(x =reorder(x =feed,X =-weight),y =weight))
  +geom_violin(aes(fill =feed))+theme_minimal()+labs(x ="Feed type",y ="Weight in grams of 6 wk old chick")
  +scale_fill_manual(values =c(horsebean ="blue",linseed ="gray",soybean ="gray",meatmeal ="gray",casein ="gray",sunflower ="blue",guide =FALSE))
violin.chickwts.data

# Identify any outliers and in which categories. Why/why not?
# There are three outliers within sunflower, one approx. 225 grams,
# one approx. 290 grams, and the other approx. 420 grams. Outliers 
# are more than 1.5 times the IQR past the edge of the box for sunflower.
# The sunflower box extends from the 25th percentile to the 75th percentile
# and contains the middle 50% of the data for the weight of the 
# 6 week old chicks and the points indicate the outliers. 
