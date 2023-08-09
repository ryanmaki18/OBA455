
# clear data from workspace

rm(list = ls())



# load tidyverse package in the current session

library("tidyverse")



# load FNN package in the current session

library("FNN")



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "i.model_evaluation_classification" folder & click "Open"



#### k-NN as classifier on riding movers data ####


# read riding mowers data
# assign it to an object "ridmov". ridmov object reflects the "entire data"

ridmov = read.csv("ridingmowers.csv")



# create a new variable "id" that reflects the row number
# rename the output variable "Ownership" to "Ownership_actual"

ridmov = ridmov %>%
  mutate(id = 1:nrow(ridmov)) %>%
  rename(Ownership_actual = Ownership)




# set the seed to 30 

set.seed(30)



# randomly draw 60% of the entire data (ridmov) 
# assign it to an object "train"

train = ridmov %>%
  sample_frac(0.6)



# extract the remaining 40% of the  data
# assign it to an object "validation"

validation = ridmov %>%
  slice(setdiff(ridmov$id, train$id))


# model build on train data


### step1 : train data
# standardize the input numeric variables 
# convert input character variables into dummy (binary) variables


# mean & standard deviation of income

a1 = mean(train$Income)

a2 = sd(train$Income)


# mean & standard deviation of lot size

b1 = mean(train$Lot_Size)

b2 = sd(train$Lot_Size)


# create standardized variables in the train data
# name the new variables ending with "_norm" to distinguish from original variables

train = train %>%
  mutate(Income_norm = (Income-a1)/a2, Lot_Size_norm = (Lot_Size -b1)/b2)


# there are no input character variables. go to step 2



#### step 2: pick only the standardized input numeric & dummy variables in the train data
# assign it to a new object "train_input_norm"
# referred as "standardized train data"

train_input_norm = train %>%
  select(Income_norm, Lot_Size_norm)



### step3 : validation data - prediction of interest
# standardize the input numeric variables 
# convert input character variables into dummy variables


# create standardized variables in the validation data
# note to use mean & standard deviation of variables in the train data
# name the new variables ending with "_norm" to distinguish from original variables

validation = validation %>%
  mutate(Income_norm = (Income-a1)/a2, Lot_Size_norm  = (Lot_Size-b1)/b2)


# there are no input character variables. go to step 4



### step 4: pick only the standardized input numeric & dummy variables in the validation data
# assign it to a new object "validation_input_norm"
# referred as "standardized validation data"

validation_input_norm = validation %>%
  select(Income_norm, Lot_Size_norm)



### step 5 : track the output variable in the train data
# assign it to a new object "train_output"
# referred as "train data output"

train_output = train$Ownership_actual



### step 6 : predict the category for validation data
# function to use for k-NN application as classification is "knn"
# knn(standardized train data, standardized validation data , train data output, choice of k)

pred = knn(train_input_norm, validation_input_norm, train_output, 3)



# store the ownership status in a new variable "Ownership_prediction" in the validation data 

validation = validation %>%
  mutate(Ownership_prediction = pred)

validation %>%
  select(Ownership_actual, Ownership_prediction)



#### evaluating predictive performance of knn as classifier ####


# confusion matrix, accuracy, Sensitivity, Specificity


# confusion matrix

validation %>%
  group_by(Ownership_prediction, Ownership_actual) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  pivot_wider (names_from = Ownership_actual, values_from = count, values_fill = 0)


# accuracy

validation %>%
  mutate (flag = if_else(Ownership_actual == Ownership_prediction, 1, 0)) %>%
  summarise(accuracy = sum(flag), err_miscl_rate = 1 - accuracy)



# Sensitivity

validation %>%
  filter (Ownership_actual == "Nonowner") %>%
  mutate (flag = if_else(Ownership_actual == Ownership_prediction, 1, 0)) %>%
  summarise(specificity = sum(flag)/length(flag))



# Specificity

validation %>%
  filter (Ownership_actual == "Owner") %>%
  mutate (flag = if_else(Ownership_actual == Ownership_prediction, 1, 0)) %>%
  summarise(specificity = sum(flag)/length(flag))



# generate all error/accuracy measures by a single function


# install "caret" package

# Miscellaneous window -> Packages -> Install -> caret



# load the caret package in the current session

library("caret")



# function to generate error/accuracy measures
# confusionMatrix(prediction variable, actual variable)
# function requires both the prediction & actual variables to be of "factor" class
# convert the Ownership_prediction & Ownership_actual variables to factor class

validation = validation %>%
  mutate(Ownership_actual = factor(Ownership_actual, levels = c("Nonowner", "Owner")),
         Ownership_prediction = factor(Ownership_prediction, levels = c("Nonowner", "Owner")))



# confusion matrix, accuracy, sensitivity & specificity

m = confusionMatrix(validation$Ownership_prediction, validation$Ownership_actual)

m 


# how to access only the accuracy value





#### optimal choice of k for best predictive performance ####

options(warn = -1)

for (k in 1:14)
{
  # k = 1
  
  # predict output in validation data for chosen k
  
  a = knn(train_input_norm, validation_input_norm, train_output, k)
  
  # store confusion matrix results in object "cm"
  
  cm = confusionMatrix(a,validation$Ownership_actual)
  
  # print the accuracy for chosen k
  
  print(paste("The accuracy for k = ", k, "is", cm$overall[1])) 
  
}



# clearing data from workspace

rm(list = ls())