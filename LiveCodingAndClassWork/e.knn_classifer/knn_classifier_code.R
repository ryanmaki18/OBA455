
# clearing data from workspace

rm(list = ls())



# load tidyverse package in the current session

library("tidyverse")



# install "FNN" package

# Miscellaneous window -> Packages -> Install -> FNN



# load FNN package in the current session

library("FNN")



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "e.knn_classifer" folder and click "Open"



#### k-NN as classifier on riding movers data ####


# read riding mowers data (main data)

ridmov = read_csv("ridingmowers.csv")



### step1 : main data
# standardize the input numeric variables 
# convert input character variables into dummy (binary) variables


# mean and standard deviation of income

a1 = mean(ridmov$Income)

a2 = sd(ridmov$Income) 


# mean and standard deviation of lot size

b1 = mean(ridmov$Lot_Size)

b2 = sd(ridmov$Income)


# create standardized variables in the main data
# name the new variables ending with "_norm" to distinguish from original variables

ridmov = ridmov %>%
  mutate(Income_norm = (Income-a1)/a2, Lot_Size_norm = (Lot_Size-b1)/b2)


# there are no input character variables. go to step 2



#### step 2: pick only the standardized input numeric and dummy variables in the main data
# assign it to a new object "ridmov_input_norm"
# referred as "standardized main data"

ridmov_input_norm = ridmov %>%
  select(Income_norm, Lot_Size_norm)



### step3 : new data - prediction of interest
# standardize the input numeric variables 
# convert input character variables into dummy variables


# consider the following observation you wish to predict the output
# assign the observation to object "newdata"

newdata = as_tibble(list(Income = 75, Lot_Size = 20))


# create standardized variables in the new data
# note to use mean and standard deviation of variables in the main data
# name the new variables ending with "_norm" to distinguish from original variables

newdata = newdata %>%
  mutate(Income_norm = (Income-a1)/a2, Lot_Size_norm = (Lot_Size - b1)/b2)


# there are no input character variables. go to step 4



### step 4: pick only the standardized input numeric and dummy variables in the new data
# assign it to a new object "newdata_input_norm"
# referred as "standardized new data"

newdata_input_norm = newdata %>%
  select (Income_norm, Lot_Size_norm)



### step 5 : track the output variable in the main data
# assign it to a new object "ridmov_output"
# referred as "main data output"

ridmov_output = ridmov$Ownership



### step 6 : predict the category for new data
# function to use for k-NN application as classification is "knn"
# knn(standardized main data, standardized new data , main data output, choice of k)

 



# clearing data from workspace

rm(list = ls())
