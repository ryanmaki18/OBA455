
# remove all objects in the environment 

rm(list = ls()) 



# load "tidyverse" packages in the current session

library("tidyverse")



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "d.data_mgmt2" folder and click "Open"



#### quick recap of last class code ####

# read toyota data

toyota = read_csv("toyota_corolla.csv")



# data manipulation - summarize numeric columns and store in a new object

# calculate average sale price, average age of cars
# assign the values to "avg_price" and "avg_age"

temp1 = toyota %>% 
  summarise(avg_price = mean(price), avg_age = mean(age_08_04))

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

# filter cars with Diesel fuel type

temp1 = toyota %>%
  filter(fuel_type == "Diesel")

rm(temp1)



# sorting data by a numeric variable

# sort the data in ascending order (default) of price

temp1 = toyota %>%
  arrange(price)

rm(temp1)



# manipulate variables

# pick only the model and price variables

temp1 = toyota %>%
  select(model, price)

rm(temp1)



# creating new variables

# the price value into dollars, kilometers to miles and pick price and distance variables

temp1 = toyota %>%
  mutate(price_dollars = price * 1.18, miles = km / 1.6) %>%
  select(price, price_dollars, km, miles)

rm(temp1)



#### end of recap of last class code ####



# combine datasets

# column bind of datasets

# pick columns model, price and assign to temp1

temp1 = toyota %>%
  select(model, price)


# pick column age_08_04 and assign to temp2

temp2 = toyota %>%
  select(age_08_04)


# column bind temp1, temp2 and assign to temp3

temp3 = temp1 %>%
  bind_cols(temp2)

rm(temp1, temp2, temp3)



# row bind of datasets

# slice 1st to 10th row and assign to temp1

temp1 = toyota %>%
  slice(1:10)


# slice 11th to 20th row and assign to temp2

temp2 = toyota %>%
  slice(11:20)


# row bind temp1, temp2 and assign to temp3

temp3 = temp1 %>%
  bind_rows(temp2)

rm(temp1, temp2, temp3)



# merge two datasets by a common variable

# create a new variable "id" whose value indicates row number
# slice 1st to 7th row and pick variables "id" and "price"
# assign to temp1

temp1 = toyota %>%
  mutate(id = 1:nrow(toyota)) %>%
  slice(1:7) %>%
  select (id,price)



# create a new variable "id" whose value indicates row number
# slice 3rd to 10th row  and pick variables "id" and "age_08_04"
# assign to temp2

temp2 = toyota %>%
  mutate(id = 1:nrow(toyota)) %>%
  slice(3:10) %>%
  select (id,age_08_04)


rm(temp3)

# left join : keep rows of temp1, columns of both datasets (temp1 and temp2)

lj = temp1 %>%
  left_join(temp2, by = "id")



# right join : keep rows of temp2, columns of both datasets (temp1 and temp2)

rj = temp1 %>%
  right_join(temp2, by = "id")



# inner join : keep common rows and columns of both datasets (temp1 and temp2)

ij = temp1 %>%
  inner_join(temp2, by = "id")



# full join : keep all rows and columns of both datasets (temp1 and temp2)

fj = temp1 %>%
  full_join(temp2, by = "id")

rm(temp1, temp2, lj, rj, ij, fj)



# tidying data

# wide to long
# wide format - temp1, long format - temp2

temp1 = as_tibble(list(company = c("A", "B", "C"), 
                       Y1999 = c(0.7, 37, 212), 
                       Y2000 = c(2, 80, 213)))


temp2 = temp1 %>%
  pivot_longer(cols = Y1999:Y2000, names_to = "year", values_to = "cost")

rm(temp1, temp2)



# long to wide
# long format - temp1, wide format - temp2

temp1 = as_tibble(list(company = rep(c("A", "B", "C"), each = 4), 
                       year = c(1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000), 
                       type = c("cost", "revenue", "cost", "revenue","cost", "revenue", "cost", "revenue", "cost", 
                                "revenue", "cost", "revenue"), 
                       exp = c(0.7, 19, 2, 20, 37, 172, 80, 174, 212, 1000, 213, 1000)))


temp2 = temp1 %>%
  pivot_wider (names_from = type, values_from = exp)

rm(temp1, temp2)



# missing values

temp1 = as_tibble(list(V1 = c("A", "B", NA, NA, "E"), 
                       V2 = c(1, NA, NA, 3, NA)))



# drop records with missing value in any column

temp2 = temp1 %>%
  drop_na()

rm(temp2)



# impute missing values with most recent non-missing values 

# search process from top to bottom (down)

temp3 = temp1 %>%
  fill()

rm(temp3)



# search process from bottom to top (up)

temp4 = temp1 %>%
  fill()

rm(temp4)



# impute missing values with a average of non-missing values

a = mean(temp1$V2, na.rm = TRUE)


temp5 = temp1 %>%
  replace_na(list(V2 = a))


rm(a, temp1, temp5)



# graphics

# single variable - continuous

# histogram of price

p = ggplot(toyota, aes(x = price, color = "yellow")) +
  geom_histogram(binwidth = 500, fill="blue")


print(p)



# histogram of price by manufacturing year

temp = toyota %>%
  arrange(mfg_year) %>%
  select(price, mfg_year, fuel_type) %>%
  mutate(mfg_year = factor(mfg_year))


p = ggplot(temp, aes(x = price, fill = mfg_year)) +
  geom_density(alpha = 0.8, position = "identity")

print(p)



# single variable - character

# bar plot of number of cars sold by manufacturing year

p = ggplot(temp, aes(mfg_year)) +
  geom_bar(fill = "green")

print(p)



# bar plot of number of cars sold by manufacturing year and fuel type

p = ggplot(temp, aes(mfg_year, fill = fuel_type)) +
  geom_bar()

print(p)



# scatter plot of kilometers and price

temp = toyota %>%
  arrange(fuel_type) %>%
  select(km, price, fuel_type) %>%
  mutate(fuel_type = factor(fuel_type))


p = ggplot(temp, aes(x = km, y = price)) +
  geom_point()+
  geom_smooth()

print(p)



# scatter plot of kilometers and price by fuel type

p = ggplot(temp, aes(x = km, y = price, shape = fuel_type)) +
  geom_point(aes(color = fuel_type))


print(p)



# boxplot of price by manufacturing year

temp = toyota %>%
  arrange(mfg_year) %>%
  select(mfg_year, price) %>%
  mutate(mfg_year = factor(mfg_year))


p = ggplot(temp, aes(x = mfg_year, y = price)) +
  geom_boxplot()

print(p)



# remove all objects in the environment 

rm(list = ls())