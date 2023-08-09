
# clear data from workspace

rm(list = ls())



# load tidyverse package in the current session

library("tidyverse")



# install "lubridate" package

# Miscellaneous window -> Packages -> Install -> lubridate



# load lubridate package in the current session

library("lubridate")



# load forecast package in the current session

library("forecast")



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "m. regression_forecasting" folder & click "Open"



#### time series forecasting on amtrak data ####


# read amtrak data
# assign it to an object "amtrak"

amtrak = read_csv("amtrak.csv")



# structure of the "amtrak" data

str(amtrak)



# rename "ridership" variable to "ridership_actual"

amtrak = amtrak %>%
  rename(ridership_actual = ridership)



# change "month" variable from character to mdy date format

amtrak = amtrak %>%
  mutate(month = mdy(month))



# data partition - train & validation


# train data from jan 1991 (row 1) to mar 2001(row 123)

train = amtrak %>%
  slice(1:123)



# validation data from apr 2001 (row 124) to mar 2004 (row 159)

validation = amtrak %>%
  slice(124:159)



#### linear trend model ####


# ridership_actual only as a function of time

train.lt = lm(ridership_actual~time_index, train)



# summary of the linear trend model

summary(train.lt)



# predict the ridership in train data

train = train %>%
  mutate(ridership_prediction = predict(train.lt, train))



# accuracy measures - RMSE and MAPE for train data

accuracy(train$ridership_prediction, train$ridership_actual)[c(2,5)]



# predict the ridership in validation data

validation = validation %>%
  mutate(ridership_prediction = predict(train.lt, validation))



# accuracy measures - RMSE and MAPE for validation data

accuracy(validation$ridership_prediction, validation$ridership_actual)[c(2,5)]



#### polynomial trend model ####


# ridership_actual as a linear and quadratic function of time

train.pt = lm(ridership_actual~time_index + I(time_index^2), train)



# summary of the polynomial trend model

summary(train.pt)



# predict the ridership in train data

train = train %>%
  mutate(ridership_prediction = predict(train.pt, train))



# accuracy measures - RMSE and MAPE for train data

accuracy(train$ridership_prediction, train$ridership_actual)[c(2,5)]



# predict the ridership in validation data

validation = validation %>%
  mutate(ridership_prediction = predict(train.pt, validation))



# accuracy measures - RMSE and MAPE for validation data

accuracy(validation$ridership_prediction, validation$ridership_actual)[c(2,5)]



#### seasonal model ####


# ridership_actual as a function of month

train.st = lm(ridership_actual~month_abbr, train)



# summary of the seasonal model

summary(train.st)



# predict the ridership in train data

train = train %>%
  mutate(ridership_prediction = predict(train.st, train))



# accuracy measures - RMSE and MAPE for train data

accuracy(train$ridership_prediction, train$ridership_actual)[c(2,5)]



# predict the ridership in validation data

validation = validation %>%
  mutate(ridership_prediction = predict(train.st, validation))



# accuracy measures - RMSE and MAPE for validation data

accuracy(validation$ridership_prediction, validation$ridership_actual)[c(2,5)]



#### seasonal + polynomial trend model ####


# ridership_actual as a function of time, time^2 and month

train.spt = lm(ridership_actual~time_index + I(time_index^2) + month_abbr, train)



# summary of the seasonal + polynomial trend model

summary(train.spt)



# predict the ridership in train data

train = train %>%
  mutate(ridership_prediction = predict(train.spt, train))



# accuracy measures - RMSE and MAPE for train data

accuracy(train$ridership_prediction, train$ridership_actual)[c(2,5)]



# predict the ridership in validation data

validation = validation %>%
  mutate(ridership_prediction = predict(train.spt, validation))



# accuracy measures - RMSE and MAPE for validation data

accuracy(validation$ridership_prediction, validation$ridership_actual)[c(2,5)]
