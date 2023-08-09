
# clearing data from workspace

rm(list = ls())



# load tidyverse package in the current session

library("tidyverse")



# load FNN package in the current session

library("FNN")



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "f.knn_regression" folder and click "Open"



#### k-NN as regression on toyota corolla data ####


# read toyota corolla data (main data)

toyota = read.csv("toyota_corolla.csv")



# the data has numerous variables. we will work on selective records and variables to illustrate the model
# consider observations with price <= 30000 and age_08_04 >= 20
# rename age_08_04 to age
# pick the variables age, km, price and only first 50 rows of data

toyota = toyota %>%
  filter(price<=30000 & age_08_04 >= 20) %>%
  rename( age = age_08_04) %>%
  select(age, km, price) %>%
  slice(1:50)




### step1 : main data
# standardize the input numeric variables 
# convert input character variables into dummy (binary) variables


# mean and standard deviation of age

a1 = mean(toyota$age)

a2 = sd(toyota$age)


# mean and standard deviation of km

b1 = mean(toyota$km)

b2 = sd(toyota$km)


# create standardized variables in the main data
# name the new variables ending with "_norm" to distinguish from original variables

toyota = toyota %>%
  mutate(age_norm=(age - a1)/a2, km_norm = (km - b1)/b2)


# there are no input character variables. go to step 2



#### step 2: pick only the standardized input numeric and dummy variables in the main data
# assign it to a new object "toyota_input_norm"
# referred as "standardized main data"

toyota_input_norm = toyota %>%
  select(age_norm, km_norm)



### step3 : new data - prediction of interest
# standardize the input numeric variables 
# convert input character variables into dummy variables


# consider the following observation you wish to predict the output
# assign the observation to object "newdata"

newdata = as_tibble(list(age = 23, km = 30000))


# create standardized variables in the new data
# note to use mean and standard deviation of variables in the main data
# name the new variables ending with "_norm" to distinguish from original variables

newdata = newdata %>%
  mutate(age_norm = (age - a1)/a2, km_norm = (km - b1)/b2)


# there are no input character variables. go to step 4



### step 4: pick only the standardized input numeric and dummy variables in the new data
# assign it to a new object "newdata_input_norm"
# referred as "standardized new data"

newdata_input_norm = newdata %>%
  select(age_norm, km_norm)



### step 5 : track the output variable in the main data
# assign it to a new object "toyota_output"
# referred as "main data output"

toyota_output = toyota$price



### step 6 : predict the category for new data
# function to use for k-NN application as classification is "knn.reg"
# knn.reg(standardized main data, standardized new data , main data output, choice of k)

knn.reg(toyota_input_norm, newdata_input_norm, toyota_output, 1)

knn.reg(toyota_input_norm, newdata_input_norm, toyota_output, 2)

knn.reg(toyota_input_norm, newdata_input_norm, toyota_output, 3)



# clearing data from workspace

rm(list = ls())



#### k-NN as regression on larger and more variables of toyota corolla data ####


# read toyota corolla data (main data)

toyota = read_csv("toyota_corolla.csv")



# we will work on all observations and following variables
# price, age_08_04, km, fuel_type, hp, met_color, automatic
# rename age_08_04 to age
# pick the variables age, km, fuel_type, hp, met_color, automatic, price
  
toyota = toyota %>%
  rename(age = age_08_04) %>%
  select (age, km, fuel_type, hp, met_color, automatic, price)



### step1 : main data
# standardize the input numeric variables 
# convert input character variables into dummy (binary) variables


# mean and standard deviation of age

a1 = mean(toyota$age)

a2 = sd(toyota$age)


# mean and standard deviation of km

b1 = mean(toyota$km)

b2 = sd(toyota$km)


# mean and standard deviation of hp variable

c1 = mean(toyota$hp)

c2 = sd(toyota$hp)


# create standardized variables in the main data
# name the new variables ending with "_norm" to distinguish from original variables

toyota = toyota %>%
  mutate(age_norm = (age - a1)/a2, km_norm = (km-b1)/b2, hp_norm = (hp-c1)/c2)


# there is one character variable - fuel_type
# create dummy variables for fuel_type

toyota = toyota %>%
  mutate(fuel_type_CNG = if_else(fuel_type == "CNG", 1,0),
         fuel_type_Diesel = if_else(fuel_type == "Diesel", 1, 0),
         fuel_type_Petrol = if_else(fuel_type == "Petrol", 1, 0))



#### step 2: pick only the standardized input numeric and dummy variables in the main data
# assign it to a new object "toyota_input_norm"
# referred as "standardized main data"

toyota_input_norm = toyota %>%
  select(age_norm, km_norm, hp_norm, met_color, automatic, 
         fuel_type_CNG, fuel_type_Diesel, fuel_type_Petrol)



### step3 : new data - prediction of interest
# standardize the input numeric variables 
# convert input character variables into dummy variables


# consider the following observation you wish to predict the output
# assign the observation to object "newdata"

newdata = as_tibble(list(age = 60, km = 70000, fuel_type = "Diesel", hp = 110, met_color = 1, automatic = 0))


# create standardized variables in the new data
# note to use mean and standard deviation of variables in the main data
# name the new variables ending with "_norm" to distinguish from original variables
# there is one character variable - fuel_type
# create dummy variables for fuel_type

newdata = newdata %>%
  mutate(age_norm = (age-a1)/a2, km_norm = (km-b1)/b2, hp_norm = (hp-c1)/c2,
         fuel_type_CNG = if_else(fuel_type == "CNG", 1,0),
         fuel_type_Diesel = if_else(fuel_type == "Diesel", 1, 0),
         fuel_type_Petrol = if_else(fuel_type == "Petrol", 1, 0))



### step 4: pick only the standardized input numeric and dummy variables in the new data
# assign it to a new object "newdata_input_norm"
# referred as "standardized new data"

newdata_input_norm = newdata %>%
  select(age_norm, km_norm, hp_norm, met_color, automatic, 
         fuel_type_CNG, fuel_type_Diesel, fuel_type_Petrol)



### step 5 : track the output variable in the main data
# assign it to a new object "toyota_output"
# referred as "main data output"

toyota_output = toyota$price



### step 6 : predict the category for new data
# function to use for k-NN application as classification is "knn.reg"
# knn.reg(standardized main data, standardized new data , main data output, choice of k)

knn.reg(toyota_input_norm, newdata_input_norm, toyota_output, 1)

knn.reg(toyota_input_norm, newdata_input_norm, toyota_output, 2)

knn.reg(toyota_input_norm, newdata_input_norm, toyota_output, 3)



# clearing data from workspace

rm(list = ls())