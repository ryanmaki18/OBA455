
# clear data from workspace

rm(list = ls())



# load tidyverse package in the current session

library("tidyverse")



# load forecast package in the current session

library("forecast")



# load caret package in the current session

library(caret)



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "j. cross_validation" folder & click "Open"



#### preparing resources for leave-one-out-cross-validation (loocv) ####


# read toyota corolla data
# assign it to an object "toyota"

toyota = read.csv("toyota_corolla.csv")



# we will work on all observations and the following variables
# price, age_08_04, km, fuel_type, hp, met_color, automatic, cc, doors, quarterly_tax, weight
# rename age_08_04 to age
# price is the output variable and the remaining are predictors/input variables

toyota  = toyota %>%
  select(price, age_08_04, km, fuel_type, hp, met_color, automatic, cc, doors, quarterly_tax, weight) %>%
  rename(age = age_08_04)



# create dummy (binary) variables for "fuel_type" variable

toyota = toyota %>%
  mutate(fuel_type_Diesel = if_else(fuel_type == "Diesel", 1, 0),
         fuel_type_Petrol = if_else(fuel_type == "Petrol", 1, 0),
         fuel_type_CNG = if_else(fuel_type == "CNG", 1, 0))



#### leave-one-out-cross-validation (loocv) ####


# create a new variable "id" that reflects the row number
# rename the output variable "price" to "price_actual"

toyota  = toyota %>%
  mutate(id = 1:nrow(toyota)) %>%
  rename (price_actual = price)


# store the number of rows in the data in an object "n"

n = nrow(toyota)


# iterative process

temp = as_tibble()

for(obs_num in 1:n)
{
  
  # filter all observations whose "id" value is not obs_num. save it in object "train"
  
  train = toyota %>%
    filter(id !=obs_num)
  
  # filter the observation whose "id" value is obs_num. save it in object "validation"
  
  validation = toyota %>%
    filter(id == obs_num)
  
  # linear regression model on the train data
  
  train.mlr = lm (price_actual~age + km + fuel_type_Diesel + fuel_type_Petrol + hp + met_color + automatic + cc + doors + quarterly_tax + weight, train)
  
  # predict output (price) in the validation data 
  # store the prediction in a new variable "price_prediction"
  
  validation = validation %>%
    mutate (price_prediction = predict(train.mlr, validation))
  
  # save the validation data error measures in an object "am"
  
  am = accuracy(validation$price_prediction, validation$price_actual)
  
  # append current error measures to past memory of temp data
  
  temp = temp %>%
    bind_rows(as_tibble(list(run = obs_num, RMSE = am[2], MAPE = am[5])))
  
  print(paste("iteration", obs_num, "completed"), sep = " ")
  
}



# summary of MAPE measures of all iterations

temp %>%
  summarise(mean_MAPE = mean(MAPE), sd_MAPE = sd(MAPE))



# summary of RMSE measures of all iterations

temp %>%
  summarise(mean_RMSE = mean(RMSE), sd_RMSE = sd(RMSE))



#### logistics regression on personal loan data ####


# clear data from workspace

rm(list = ls())



# read personal loan data
# assign it to an object "loan"

loan = read.csv("personal_loan.csv")



# rename the output variable "loan_status" to "loan_status_actual"
# create a new variable "id" that reflects the row number
# loan object reflects the "entire data"

loan  = loan %>%
  rename(loan_status_actual = loan_status) %>%
  mutate (id = 1:nrow(loan))



# create dummy (binary) variables for "education" variable

loan = loan %>%
  mutate(education_undergrad = if_else(education == "undergrad", 1, 0),
         education_graduate = if_else(education == "graduate", 1, 0),
         education_advanced = if_else(education == "advanced", 1, 0))



# data partition - train & validation


# set the seed to 30 

set.seed(30)



# randomly draw 70% of the entire data (loan) 
# assign it to an object "train"

train = loan %>%
  sample_frac(0.7)
  



# extract the remaining 30% of the entire data
# assign it to an object "validation"

validation = loan %>%
  slice(setdiff(loan$id, train$id))



# model build on the train data
# logistics regression on the train data

train.lr = glm(loan_status_actual~age + experience + income+ family + ccavg + education_graduate + education_advanced + mortgage + securities_account + cd_account + online + credit_card, train, family = "binomial")



# summary of the model

summary(train.lr)



# predict output in the validation data
# note that the logistics regression model predicts the Pr(Y = 1)

validation = 



#### evaluating predictive performance from validation data ####


# rule - if prob_prediction >= 0.5, prediction is 1 (accept), else 0 (reject)
# create a new variable "loan_status_prediction" in the validation data
# loan_status_prediction reflects the predicted loan_status

validation = 



# function to generate error/accuracy measures
# confusionMatrix(prediction variable, actual variable)
# function requires both the prediction & actual variables to be "factor" class
# convert the loan_status_prediction & loan_status_actual variables to factor class

validation = validation %>%
  mutate(loan_status_actual = factor(loan_status_actual, c(0,1)),
         loan_status_prediction = factor(loan_status_prediction, c(0,1)))



# confusion matrix, accuracy, sensitivity & specificity

confusionMatrix()



# clear data from workspace

rm(list = ls())
