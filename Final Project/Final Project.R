
# clear data from workspace

rm(list = ls())


# load tidyverse package in the current session

library("tidyverse")

library("FNN")

library("forecast")

library("caret")

### Start Here
Salary = read_csv("salary.csv")

###Set Working Directory

##Formatting Data

Salary = Salary %>%
  mutate (id = 1:nrow(Salary))

Salary = Salary %>%
  rename(salary_actual = salary)

Salary = Salary %>%
  mutate(salary_over_50 = if_else(salary_actual == "<=50K", 1, if_else(salary_actual == "<50K",1,0)))%>%
  mutate(Male = if_else(sex == "Male", 1,0), Female = if_else(sex == "Female",1,0))

Salary1 = Salary %>%
  select(id, age, Male, education, marital.status, relationship, race, hpw, native.country.new, salary_over_50)

## Separate Validation and Train Data

set.seed(30)

train = Salary1 %>%
  sample_frac(0.7)

validation = Salary1 %>%
  slice(setdiff(Salary1$id, train$id))

## Running Logistic Regression

train.lr = glm(salary_over_50 ~ age + Male + education + marital.status + 
                 relationship + race + hpw + native.country.new, train, family = "binomial")

summary(train.lr)

# predict output in the validation data

validation = validation %>%
  mutate(prob_prediction = predict(train.lr, validation, type = "response"))

validation = validation %>%
  mutate(Salary_prediction = if_else(prob_prediction >= 0.5, 1, 0))

# function to generate error/accuracy measures
# confusionMatrix(prediction variable, actual variable)
# converting the variables to factor class

validation = validation %>%
  mutate(Salary_actual = factor(salary_over_50, c(0,1)),
         Salary_prediction = factor(Salary_prediction, c(0,1)))

## confusion matrix, accuracy, sensitivity & specificity

confusionMatrix(validation$Salary_prediction,validation$Salary_actual)




#To remove all data/values
rm(list = ls())


