
# clear data from workspace

rm(list = ls())



# load tidyverse package in the current session

library("tidyverse")



# load caret package in the current session

library("caret")



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "k. logistics_regression" folder & click "Open"



#### logistics regression on delays data ####


# read airline delays data
# assign it to an object "delays"

delays = read_csv("delays.csv")



# "Day_Week" variable appears as numeric
# machine will treat "Day_Week" variable as a numeric value
# need to convert into character/factor variable
# label 1 as Mon, 2 as Tue and so on...

delays = delays %>%
  mutate(Day_Week = factor(Day_Week, levels = c(1:7), labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))



# "Dep_Time" variable appears as numeric
# how to incorporate departure time as input into the model?
# create a new variable "Dep_Hour" that represents the hour the flight departed
# need to convert into factor variable

delays = delays %>%
  mutate(Dep_Hour = floor(Dep_Time/100)) %>%
  mutate(Dep_Hour = factor(Dep_Hour, levels = c(6:21)))



# convert Origin, Destination, Carrier variables into factor variables

delays = delays %>%
  mutate(Origin = factor(Origin, levels = c("IAD", "BWI","DCA")),
         Destination = factor(Destination, levels = c("LGA", "EWR", "JFK")),
         Carrier = factor(Carrier, levels = c("US", "CO", "DH", "DL", "MQ", "OH", "RU", "UA")))



# logistic regression model 


# what is the class of the output variable? 
# create a variable "Flight_Status_actual"
# Flight_Status_actual takes 1 if Flight_Status is delayed, 0 otherwise
# create a new variable "id" that reflects the row number
# delays object reflects the "entire data"

delays = delays %>%
  mutate(Flight_Status_actual = if_else(Flight_Status == "delayed", 1, 0)) %>%
  mutate(id = 1:nrow(delays))



# data partition - train & validation


# set the seed to 30 

set.seed(30)



# randomly draw 70% of the entire data (delays) 
# assign it to an object "train"

train = delays %>%
  sample_frac(0.7)



# extract the remaining 30% of the entire data
# assign it to an object "validation"

validation = delays %>%
  slice(setdiff(delays$id, train$id))



# model build on the train data
# logistics regression on the train data

train.lr = glm(Flight_Status_actual ~ Day_Week + Dep_Hour + Origin + Destination + Carrier + Weather, 
               train, family = "binomial")



# summary of the model

summary(train.lr)



# predict output in the validation data
# note that the logistics regression model predicts the Pr(Y = 1)

validation = validation %>%
  mutate(prob_prediction = predict(train.lr, validation, type = "response"))



#### evaluate predictive performance from validation data ####


# rule - if prob_prediction >= 0.5, prediction is 1 (delayed), else 0 (on-time)
# create a new variable "Flight_Status_prediction" in the validation data
# Flight_Status_prediction reflects the predicted flight status

validation = validation %>%
  mutate(Flight_Status_prediction = if_else(prob_prediction >= 0.5, 1, 0))



# function to generate error/accuracy measures
# confusionMatrix(prediction variable, actual variable)
# function requires both the prediction & actual variables to be "factor" class
# convert the Flight_Status_prediction & Flight_Status_actual variables to factor class

validation = validation %>%
  mutate(Flight_Status_actual = factor(Flight_Status_actual, c(0,1)),
         Flight_Status_prediction = factor(Flight_Status_prediction, c(0,1)))



# confusion matrix, accuracy, sensitivity & specificity

confusionMatrix(validation$Flight_Status_prediction,validation$Flight_Status_actual)



rm(train, validation, train.lr)



#### logistics regression model with new levels ####


# Excessive levels in "Day_Week" variable
# create a new variable "Day_Type" that signifies weekday or weekend
# number of levels reduced to 2
# need to convert into factor variable

delays = delays %>%
  mutate(Day_Type = if_else(Day_Week %in% c("Sat", "Sun"), "weekend", "weekday")) %>%
  mutate(Day_Type = factor(Day_Type, levels = c("weekend", "weekday")))



# Excessive levels in "Day_Hour" variable
# create a new variable "Time_Day" that signifies morning (6am-12pm), afternoon (12pm-5pm) & evening (5pm-10pm)
# number of levels reduced to 3
# need to convert into factor variable

delays = delays %>%
  mutate(Time_Day = case_when(Dep_Hour %in% c("6", "7","8", "9", "10", "11") ~ "morning",
                              Dep_Hour %in% c("12", "13","14", "15", "16") ~ "afternoon",
                              Dep_Hour %in% c("17", "18","19", "20", "21") ~ "evening")) %>%
  mutate(Time_Day = factor(Time_Day, levels = c("morning", "afternoon", "evening")))



# Excessive levels in "Carrier" variable
# group all insignificant carries into a level
# need to convert into factor variable

delays = delays %>%
  mutate(Carrier_New = if_else(Carrier %in% c("CO", "DH", "MQ", "RU"), "CO_DH_MQ_RU", "US_DL_OH_UA")) %>%
  mutate(Carrier_New = factor(Carrier_New, levels = c("US_DL_OH_UA", "CO_DH_MQ_RU")))



# data partition - train & validation


# set the seed to 30 

set.seed(30)



# randomly draw 70% of the entire data (delays) 
# assign it to an object "train"

train = delays %>%
  sample_frac(0.7)



# extract the remaining 30% of the entire data
# assign it to an object "validation"

validation = delays %>%
  slice(setdiff(delays$id, train$id))



# model build on the train data
# logistics regression on the train data

train.lr = glm(Flight_Status_actual ~ Day_Type + Time_Day + Origin + Destination + Carrier_New + Weather, 
               train, family = "binomial")



# summary of the model

summary(train.lr)



# predict output in the validation data
# note that the logistics regression model predicts the Pr(Y = 1)

validation = validation %>%
  mutate(prob_prediction = predict(train.lr, validation, type = "response"))



#### evaluating predictive performance from validation data ####


# rule - if prob_prediction >= 0.5, prediction is 1 (delayed), else 0 (on-time)
# create a new variable "Flight_Status_prediction" in the validation data
# Flight_Status_prediction reflects the predicted flight status

validation = validation %>%
  mutate(Flight_Status_prediction = if_else(prob_prediction >= 0.5, 1, 0))



# function to generate error/accuracy measures
# confusionMatrix(prediction variable, actual variable)
# function requires both the prediction & actual variables to be "factor" class
# convert the Flight_Status_prediction & Flight_Status_actual variables to factor class

validation = validation %>%
  mutate(Flight_Status_actual = factor(Flight_Status_actual, c(0,1)),
         Flight_Status_prediction = factor(Flight_Status_prediction, c(0,1)))



# confusion matrix, accuracy, sensitivity & specificity

confusionMatrix(validation$Flight_Status_prediction,validation$Flight_Status_actual)



# clear data from workspace

rm(delays, train, train.lr, validation)

rm(list = ls())
