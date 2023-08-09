
# clear data from workspace

rm(list = ls())



# load tidyverse package in the current session

library("tidyverse")



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "h.model_evaluation_regression" folder and click "Open"



#### recap of multiple linear regression on toyota data ####


# read toyota corolla data
# assign it to an object "toyota"

toyota = read_csv("toyota_corolla.csv")



# we will work on all observations and the following variables
# price, age_08_04, km, fuel_type, hp, met_color, automatic, cc, doors, quarterly_tax, weight
# rename age_08_04 to age
# price is the output variable and the remaining are predictors/input variables

toyota  = toyota %>%
  select(price, age_08_04, km, fuel_type, hp, met_color, automatic, cc, doors, quarterly_tax, weight) %>%
  rename(age = age_08_04)



# multiple linear regression model on the toyota data
# store the model results in an object "toyota.mlr"

toyota.mlr = lm(price ~ age + km + fuel_type + hp + met_color + automatic + 
                  cc + doors + quarterly_tax + weight, toyota)



# summary of the multiple linear regression model

summary(toyota.mlr)



# new data to predict price
# assign it to an object "newdata"

newdata = as_tibble(list(age = c(44, 60, 70), km = c(43000, 70000, 90000), fuel_type = c("CNG", "Diesel", "Petrol"),
                         hp = c(90, 100, 110), met_color = c(0, 0, 1), automatic = c(0, 1, 0),
                         cc = c(1400, 1500, 1600), doors = c(2, 3, 4), quarterly_tax = c(70, 80, 90),
                         weight = c(1040,1060,1080)))



# predict price for the new data

predict(toyota.mlr, newdata)



rm(toyota.mlr, newdata)



##### evaluate predictive performance of the multiple linear regression ####


# create a new variable "id" that reflects the row number
# toyota object reflects the "entire data"

toyota  = toyota %>%
  mutate(id = 1:nrow(toyota))



# rename the output variable "price" to "price_actual"

toyota  = toyota %>%
  rename(price_actual = price)



# data partition - train & validation


# set the seed to 30 

set.seed(30)



# randomly draw 70% of the entire data (toyota) 
# assign it to an object "train"

train = toyota %>%
  sample_frac(0.7)



# extract the remaining 30% of the entire data
# assign it to an object "validation"

validation = toyota %>%
  slice(setdiff(toyota$id, train$id))



# model build on the train data
# multiple linear regression on the train data

train.mlr = lm(price_actual ~ age + km + fuel_type + hp + met_color + 
                 automatic + cc + doors +  quarterly_tax + weight, train)



# summary of the model

summary(train.mlr)



# predict output (price) in the validation data 
# create a new variable "price_prediction" in the validation data
# price_prediction reflects the predicted price

validation = validation %>%
  mutate(price_prediction = predict(train.mlr, validation))



#### error/accuracy measures ####


# mean error (ME)
# mean absolute error (MAE)
# mean percentage error (MPE)
# mean absolute percentage error (MAPE)
# root mean square error (RMSE) 


#### error measures for validation data ####


# create a new variable "error" in the validation data
# error reflects the difference between price_actual & price_prediction

validation = validation %>%
  mutate(error = price_actual - price_prediction)



# mean error (ME)

mean(validation$error)



# mean absolute error (MAE)

mean(abs(validation$error))



# mean percentage error (MPE)

mean(validation$error/validation$price_actual) * 100



# mean absolute percentage error (MAPE)

mean(abs(validation$error/validation$price_actual)) * 100



# root mean square error (RMSE)

sqrt(mean((validation$error)^2))



# generate all errors measures by a single function


# install "forecast" package

# Miscellaneous window -> Packages -> Install -> forecast



# load the forecast package in the current session

library("forecast")



# function to generate error measures
# accuracy(prediction values, actual values)

accuracy(validation$price_prediction, validation$price_actual)



#### error measures for train data ####


# predict output (price) in the train data 
# create a new variable "price_prediction" in the train data
# price_prediction reflects the predicted price
# create a new variable "error" in the train data
# error reflects the difference between price_actual & price_prediction

train = train %>%
  mutate(price_prediction = predict(train.mlr, train),
         error = price_actual - price_prediction)



# function to generate error measures
# accuracy(prediction values, actual values)

accuracy(train$price_prediction, train$price_actual)



# clear data from workspace

rm(list = ls())