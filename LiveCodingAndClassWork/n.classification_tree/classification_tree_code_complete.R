
# clear data from workspace

rm(list = ls())



# load tidyverse package in the current session

library("tidyverse")



# install rpart and rpart.plot packages

# Miscellaneous window -> Packages -> Install -> rpart, rpart.plot



# load the rpart and rpart.plot packages in the current session

library("rpart")

library("rpart.plot")



# load caret package in the current session

library("caret")



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "n. classification_tree" folder and click "Open"



#### recursive partitioning on riding mowers data ####


# read riding mowers data
# assign it to an object "ridmov"

ridmov = read_csv("RidingMowers.csv")



# default classification tree
# store the model results in an object "ridmov.dct"

ridmov.dct = rpart(Ownership ~ Income + Lot_Size, data = ridmov, method = "class")



# plot of the default classification tree

prp(ridmov.dct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)



# fully grown classification tree 
# store the model results in an object "ridmov.fct"

ridmov.fct = rpart(Ownership~ Income+Lot_Size, data = ridmov, method = "class",
                   minsplit = 1)



# plot of the fully grown classification tree

prp(ridmov.fct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)



# clear data from workspace

rm(list = ls())



# read personal loan data
# assign it to an object "loan"

loan = read_csv("personal_loan.csv")



# rename the output variable "loan_status" to "loan_status_actual"
# convert the "loan_status_actual" variable to a factor variable

loan = loan %>%
  rename(loan_status_actual = loan_status) %>%
  mutate(loan_status_actual = factor(loan_status_actual, c("accept", "reject")))



# convert the "education" variable to a factor variable

loan = loan %>%
  mutate(education = factor(education, c("undergrad", "graduate","advanced")))



# create a new variable "id" that reflects the row number

loan = loan %>%
  mutate(id = 1:nrow(loan))



#### pruning a tree ####


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

loan.ct = rpart(loan_status_actual~ age + experience + income + family + ccavg + mortgage +
                  education + securities_account + cd_account + online + credit_card, 
                data = train, 
                method = "class", cp = 0.00001, minsplit = 10, xval = 10)



### step 3: plot the cp or relative error

# choose the value for "size of the tree" where the relative error stabilizes

plotcp(loan.ct)



### step 4: find "nsplit" value and its associated cp value from "size of the tree"

cp.table = as_tibble(loan.ct$cptable)

# nsplit = size of the tree - 1

optimal.cp = cp.table %>%
  filter(nsplit == 10)



### step 5: prune the tree with the optimal cp

pruned.ct = prune(loan.ct, cp = optimal.cp$CP)

# plot of the pruned tree

prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)



### step 6: predict the loan status for validation data

results = predict(pruned.ct, validation, type = "class")



### step 7: generate confusion matrix and accuracy measures

# create a new variable "loan_status_prediction" in the validation data
# loan_status_prediction reflects the predicted loan_status

validation = validation %>%
  mutate(loan_status_prediction = results)


# confusion matrix, accuracy, sensitivity & specificity

confusionMatrix(validation$loan_status_prediction,validation$loan_status_actual)
