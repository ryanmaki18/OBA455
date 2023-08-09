rm(list = ls())

#Homework Number 3
library(tidyverse)
library(caret)
library(FNN)

##1
#a
set.seed(30)
tayko = read.csv("Tayko.csv")

tayko = tayko %>%
  mutate(id = 1:nrow(tayko))


train = tayko %>%
  sample_frac(0.8)

validation = tayko %>%
  slice(setdiff(tayko$id, train$id))

train.lr = lm(spending~freq + last_update + web + gender + address_res + address_us, train)
summary(train.lr)

validation1 = validation %>%
  mutate(price_prediction = predict(train.lr, validation))

library("forecast")
accuracy(validation1$price_prediction, validation1$spending)

#b
library(ISLR)


rm(list = ls())
##2
#a
set.seed(30)

pl = read_csv("personal_loan.csv")

pl = pl %>%
  mutate(id = 1:nrow(pl))

pl = pl %>%
  mutate(education_undergrad = if_else(education == "undergrad", 1, 0),
         education_graduate = if_else(education == "graduate", 1, 0),
         education_advanced = if_else(education == "advanced", 1, 0))

pl = pl %>%
  mutate(ls_reject = if_else(loan_status == "reject", 1, 0),
         ls_accept = if_else(loan_status == "accept", 1, 0))

train = pl %>%
  sample_frac(0.7)

validation = pl %>%
  slice(setdiff(pl$id, train$id))

train.lr = glm(ls_reject ~ age + experience + income + ccavg + family + mortgage + securities_account + cd_account + online + credit_card + education_graduate + education_advanced,, train, family = "binomial")
summary(train.lr)


#b





