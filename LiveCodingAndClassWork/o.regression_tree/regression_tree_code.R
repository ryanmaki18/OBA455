
# clear data from workspace

rm(list = ls())



# load tidyverse, rpart, rpart.plot, caret and forecast packages in the current session. 

library("tidyverse")

library("rpart")

library("rpart.plot")

library("caret")

library("forecast")



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "o. regression_tree" folder and click "Open"



# recap from previous class


#### classification tree - loan data ####


# read personal loan data
# assign it to an object "loan"
# rename the "loan_status" variable to "loan_status_actual"
# convert "loan_status_actual" and "education" variables to a factor variables

loan = read.csv("personal_loan.csv")

loan = loan %>%
  rename(loan_status_actual = loan_status)%>%
  mutate(loan_status_actual = factor(loan_status_actual, c("accept", "reject")), 
         education = factor(education, c("undergrad", "graduate","advanced")))



# create a new variable "id" that reflects the row number

loan = loan %>%
  mutate(id = 1:nrow(loan))



#### pruning a classification tree ####


### step 1: set the seed, data partition - train & validation

# set seed to 30

set.seed(30)


# randomly draw 60% of the loan data 
# assign it to an object "train"

train = loan %>%
  sample_frac(0.6)


# extract the remaining 40% of the main data
# assign it to an object "validation"

validation = loan %>%
  slice(setdiff(loan$id, train$id))



### step2 : run a tree with options cp = 0.00001, minsplit = 5 or 10, xval = 5 or 10

# assign it to an object "loan.ct"

loan.ct = rpart(loan_status_actua ~ age + experience + income + family + ccavg + mortgage + education + secuirties_account + cd_account + online + credit_card,
                data = train,  methon = "class"
                cp = 0.00001, minsplit = 10, xval = 10)



### step 3: plot the cp or relative error

# choose the value for "size of the tree" where the relative error stabilizes

plotcp(loan.cp$cptable)



### step 4: find "nsplit" value and its associated cp value from chosen "size of the tree" value

cp.table = as_tibble()

# nsplit = size of tree -1 
# store the cp value in an object "optimal.cp"

optimal.cp = cp.table %>%
  filter(nsplit == 10)



### step 5: prune the tree with the "optimal.cp" value

pruned.ct = prune(loan$ct, cp = optimal.cp$CP)

# plot of the pruned tree

prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)



### step 6: predict the loan status for validation data

prediction = predict(pruned.ct, validation, type = "class")



### step 7: generate confusion matrix and accuracy measures

# create a new variable "loan_status_prediction" in the validation data
# loan_status_prediction reflects the predicted loan_status

validation = validation %>%
  mutate(loan_status_prediction = prediction)


# confusion matrix, accuracy, sensitivity & specificity

confusionMatrix(validation$loan_status_prediction, validation$loan_status_actual)



# clear data from workspace

rm(list = ls())



#### regression tree - toyota data ####


# read toyota data 
# assign it to an object "toyota"
# rename the "price" variable to "price_actual"
# pick variables price_actual, age_08_04, km, fuel_type, hp, met_color, automatic, cc, doors, quarterly_tax, weight

toyota = read.csv("toyota_corolla.csv") %>%
  rename(price_actual = price) %>%
  select(price_actual, 
         age_08_04, km, fuel_type, hp, met_color, automatic, cc, doors, quarterly_tax, weight)


# create a new variable "id" that reflects the row number

toyota = toyota %>%
  mutate(id = 1:nrow(toyota))



#### pruning a regression tree ####


### step 1: set the seed, data partition - train & validation

# set seed to 30

set.seed(30)


# randomly draw 70% of the toyota data 
# assign it to an object "train"

train = toyota %>%
  sample_frac(0.7)


# extract the remaining 30% of the main data
# assign it to an object "validation"

validation = toyota %>%
  slice(setdiff(toyota$id, train$id))



### step2 : run a tree with options cp = 0.001, minsplit = 5 or 10, xval = 5 or 10

# assign it to an object "toyota.rt"

toyota.rt = rpart(price_actual~age_08_04 + km + fuel_type + hp + met_color + automatic + cc + doors + quarterly_tax + weight,
                  data = train, method = "anova",
                  cp = 0.001, minsplit = 10, xval = 10)



### step 3: plot the cp or relative error

# choose the value for "size of the tree" where the relative error stabilizes

plotcp(toyota.rt)



### step 4: find "nsplit" value and its associated cp value from chosen "size of the tree" value

cp.table = as_tibble(toyota.rt$cptable)

# nsplit = size of tree -1 
# store the cp value in an object "optimal.cp"

optimal.cp = cp.table %>%
  filter(nsplit == 13)



### step 5: prune the tree with the "optimal.cp" value

pruned.ct = prune(toyota.rt, cp = optimal.cp$CP)

# plot of the pruned tree

prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)



### step 6: predict the prince for validation data

prediction = predict(pruned.ct, validation, type = "vector")



### step 7: generate accuracy measures (ME, MAE, MPE, MAPE, RMSE)

# create a new variable "price_prediction" in the validation data
# price_prediction reflects the predicted price

validation = validation %>%
  mutate(price_prediction = prediction)


# accuracy measures

accuracy(validation$price_prediction, validation$price_actual)
                                                                                                                                                                                                                                                                    
hist(train$price_actual, breaks = 100)
