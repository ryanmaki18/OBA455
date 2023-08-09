
# clear data from workspace

rm(list = ls())



# load tidyverse package in the current session

library(tidyverse)



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "g.linear_regression" folder and click "Open"



#### simple linear regression on advertising data ####


# read advertising data
# assign it to an object "advertising"

advertising = read.csv("Advertising.csv")



# structure of the advertising object

str(advertising)



# simple linear regression with output "sales" and predictor "tv"
# store the model results in an object "sales.slr"

sales.slr = lm(sales~tv, advertising)



# summary of the simple linear regression model

summary(sales.slr)



# new data to predict sales
# assign it to an object "newdata"

newdata = as_tibble(list(tv = c(75, 150, 220)))



# predict of sales for the new data

predict()



rm(sales.slr, newdata)



#### multiple linear regression on advertising data ####


# multiple linear regression with output "sales" and predictors "tv", "radio", "newspaper"
# store the model results in an object "sales.mlr"

sales.mlr = lm(sales~tv + radio + newspaper, advertising)



# summary of the multiple linear regression model

summary(sales.mlr)



# new data to predict sales
# assign it to an object "newdata"

newdata = as_tibble(list(tv = c(75, 150, 220), radio = c(10, 25, 40), newspaper = c(10, 15, 20)))



# predict sales for the new data

predict()



rm(sales.mlr, newdata)



#### multiple linear regression on the toyota data ####


# clear data from workspace

rm(list = ls())



# read toyota corolla data
# assign it to an object "toyota"

toyota = read.csv("toyota_corolla.csv")



# we will work on all observations and the following variables
# price, age_08_04, km, fuel_type, hp, met_color, automatic, cc, doors, quarterly_tax, weight
# rename age_08_04 to age
# price is the output variable and the remaining are predictors/input variables

toyota  = toyota %>%
  select (price, age_08_04, km, fuel_type, hp, met_color, automatic, cc, doors, quarterly_tax, weight) %>%
  rename (age = age_08_04)



# multiple linear regression model
# store the model results in an object "toyota.mlr"

toyota.mlr = lm(price ~ age + km + fuel_type + hp + met_color + automatic + cc + doors + quarterly_tax + weight, toyota)



# summary of the multiple linear regression model

summary(toyota.mlr)



# new data to predict price
# assign it to an object "newdata"

newdata = as_tibble(list(age = c(44, 60, 70), km = c(43000, 70000, 90000), fuel_type = c("CNG", "Diesel", "Petrol"),
                         hp = c(90, 100, 110), met_color = c(0, 0, 1), automatic = c(0, 1, 0),
                         cc = c(1400, 1500, 1600), doors = c(2, 3, 4), quarterly_tax = c(70, 80, 90),
                         weight = c(1040,1060,1080)))



# predict price for the new data

predict()



##### selective levels of character variables in the model : approach 1 ####


# convert to a factor variable and define the order of levels

toyota = toyota %>%
  mutate()



# multiple linear regression model

toyota.mlr = lm(price ~ age + km + fuel_type + hp + met_color + automatic + 
                  cc + doors + quarterly_tax + weight, toyota)



# summary of the multiple linear regression model

summary()



##### selective levels of character variables in the model : approach 2 ####


# create dummy variables 

toyota = toyota %>%
  mutate()



# multiple linear regression model

toyota.mlr = lm()



# summary of the multiple linear regression model

summary()



# clear data from workspace

rm(list = ls())