
# remove all objects in the environment 

rm(list = ls()) 



# load "tidyverse" packages in the current session

library("tidyverse")



# default working directory

getwd()



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "c.data_mgmt1" folder and click "Open"



# read toyota data

toyota = read_csv("toyota_corolla.csv")



# objects

# data class 

class(toyota)



# data structure

str(toyota)



# display first 6 rows of the data (default)

head(toyota)



# data dimension - rows followed by columns

dim(toyota) 



# summary of numeric variable

# displays minimum, 1st quantile, median, mean, 3rd quantile and maximum

summary(toyota$price)



# summary of a character variable

# count of distinct values of the character variable

table(toyota$fuel_type)



# data manipulation - summarize numeric columns and store in a new object

# calculate average sale price, average age of cars
# assign the values to "avg_price" and "avg_age"

temp1 = toyota %>% 
  summarise(avg_price = mean(price), avg_age = mean(age_08_04))

rm(temp1)



# count of different model cars

temp1 = toyota %>%
  count(model)

rm(temp1)



# summarize numeric variables by a group variable

# calculate average sale price by manufacturing year
# assign the name "avg_price" 

temp1 = toyota %>%
  group_by(mfg_year) %>%
  summarise(avg_price = mean(price)) %>%
  ungroup()

rm(temp1)



# manipulating cases - filtering observations of choice

# filter cars whose sale price is and more than 10,000 euros

temp1 = toyota %>%
  filter(price >= 10000)

rm(temp1)



# filter cars with Diesel fuel type

temp1 = toyota %>%
  filter(fuel_type == "Diesel")

rm(temp1)



# filter randomly 70% of the observations

temp1 = toyota %>%
  sample_frac(0.7)

rm(temp1)



# filter observations by position

# filter observations from 21st row to 30th row

temp1 = toyota %>%
  slice(21:30)

rm(temp1)



# sorting data by a numeric variable

# sort the data in ascending order (default) of price

temp1 = toyota %>%
  arrange(price)

rm(temp1)



# sort the data in descending order of age

temp1 = toyota %>%
  arrange(desc(age_08_04))

rm(temp1)



# manipulate variables

# pick only the model and price variables

temp1 = toyota %>%
  select(model)

rm(temp1)



# creating new variables

# the price value into dollars, kilometers to miles and pick price and distance variables

temp1 = toyota %>%
  mutate(price_dollars = price * 1.18, miles = km / 1.6) %>%
  select(price, price_dollars, km, miles)

rm(temp1)



# create a new variable whose name is "afy2000"
# afy2000 should take 1 if the car was manufactured after year 2000, 0 otherwise

temp1 = toyota %>%
  mutate(afy2000 = if_else(mfg_year > 2000, 1, 0))

rm(temp1)



# rename variables

# rename price to price_euro and km to kilometer

temp1 = toyota %>%
  rename(price_euro = price, kilometer = km)

rm(temp1)



# count the number of distinct model cars by fuel type

temp1 = toyota %>%
  group_by(fuel_type) %>%
  summarise(count = n_distinct(model)) %>%
  ungroup()

rm(temp1)



# removes all objects from the environment 

rm(list = ls()) 



#Homework 1 scratch work

as.numeric (c("1", "3", "6"))

ridmov = read_csv("RidingMowers.csv")
summary(ridmov$Income, ridmov$Lot_Size)



mutate(ridmov) %>%
  nrow(by = 3)


