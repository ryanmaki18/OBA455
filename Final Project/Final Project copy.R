rm(list = ls())
library(tidyverse)
library(FNN)

Salary = read_csv("salary.csv")

Salary = Salary %>%
  mutate (id = 1:nrow(Salary))

Salary = Salary %>%
  rename(salary_actual = salary)

Salary = Salary %>%
  rename(hpw = 'hours-per-week')

library("FNN")

##KNN CLASSIFICATION

a1 = mean(Salary$age)

a2 = sd(Salary$age) 

b1 = mean(Salary$hpw)

b2 = sd(Salary$hpw)

Salary = Salary %>%
  mutate(age_norm = (age - a1)/a2, hpw_norm = (hpw - b1)/b2)

Salary = Salary %>%
  mutate(salary_high = if_else(salary_actual == "<=50K", 1, if_else(salary_actual == "<50K",1,0))

Salary = Salary %>%
  mutate(Male = if_else(sex == "Male", 1,0),
         Female = if_else(sex == "Female",1,0))

Salary = Salary %>%
  mutate(Private = if_else(workclass == "Private", 1,0))
Salary = Salary %>%
         mutate(SelfEmployed = if_else(workclass == "Self-emp-not-inc",1, if_else(workclass == "Self-emp-inc",1,0)))
Salary = Salary %>%
         mutate(Government = if_else(workclass == "Federal-gov",1, if_else(workclass == "Local-gov", 1, if_else(workclass == "State-gov",1,0))))
Salary = Salary %>%
        mutate(NonPay = if_else(workclass == "Without-pay", 1, if_else(workclass == "Never-worked",1,0)))

Salary$education-num <- as.factor(Salary$education-num)

Salary_norm = Salary %>%
  select(age_norm, hpw_norm, salary_high, Male, Female, Private, SelfEmployed, Government, NonPay)

newdata = as_tibble(list(age = 25, hpw = 20, Male = 1, Female = 0, Private = 1, SelfEmployed = 0, Government = 0, NonPay = 0))

newdata = newdata%>%
  mutate(age_norm = (age - a1)/a2, hpw_norm = (hpw - b1)/b2),mutate(Male = if_else(sex == "Male", 1, if_else(sex == "Female",0,1)),
                mutate(Private = if_else(workclass == "Private", 1,0)), 
                SelfEmployed = if_else(workclass == "Self-emp-not-inc",1, if_else(workclass == "Self-emp-inc",1,0)),
                Government = if_else(workclass == "Federal-gov",1, if_else(workclass == "Local-gov", 1, if_else(workclass == "State-gov",1,0))),
                NonPay = if_else(workclass == "Without-pay", 1, if_else(workclass == "Never-worked",1,0))





fuel_type_CNG = if_else(fuel_type == "CNG", 1, 0), 
fuel_type_Diesel = if_else(fuel_type == "Diesel", 1, 0), 
fuel_type_Petrol = if_else(fuel_type == "Petrol", 1, 0))



newdata_input_norm = newdata%>%
  select(age_norm, hpw_norm)

Salary_output = Salary$salary_actual

knn(Salary_norm, newdata_input_norm, Salary_output,1)

knn(Salary_norm, newdata_input_norm, Salary_output,2)

knn(Salary_norm, newdata_input_norm, Salary_output,3)

knn(Salary_norm, newdata_input_norm, Salary_output,4)

knn(Salary_norm, newdata_input_norm, Salary_output,5)




