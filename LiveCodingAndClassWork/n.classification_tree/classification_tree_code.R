
# clear data from workspace

rm(list = ls())



# load tidyverse package in the current session

library()



# install rpart and rpart.plot packages

# Miscellaneous window -> Packages -> Install -> rpart, rpart.plot



# load the rpart and rpart.plot packages in the current session

library()

library()



# load caret package in the current session

library()



# choose a working directory

# path : Session -> Set Working directory -> Choose Directory

# select "n. classification_tree" folder and click "Open"



#### recursive partitioning on riding mowers data ####


# read riding mowers data
# assign it to an object "ridmov"

ridmov = 



# default classification tree
# store the model results in an object "ridmov.dct"

ridmov.dct = rpart()



# plot of the default classification tree

prp()



# fully grown classification tree 
# store the model results in an object "ridmov.fct"

ridmov.fct = rpart()



# plot of the fully grown classification tree

prp(ridmov.fct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)



# clear data from workspace

rm(list = ls())



# read personal loan data
# assign it to an object "loan"

loan = 



# rename the output variable "loan_status" to "loan_status_actual"
# convert the "loan_status_actual" variable to a factor variable

loan = loan %>%
  rename() %>%
  mutate()



# convert the "education" variable to a factor variable

loan = loan %>%
  mutate(education = factor(education, c("undergrad", "graduate","advanced")))



# create a new variable "id" that reflects the row number

loan = loan %>%
  mutate()



#### pruning a tree ####


### step 1: set the seed, data partition - train & validation

# set seed to 30

set.seed()


# randomly draw 60% of the loan data 
# assign it to an object "train"

train = 


# extract the remaining 40% of the main data
# assign it to an object "validation"

validation = loan %>%
  slice()



### step2 : run a tree with options cp = 0.00001, minsplit = 5 or 10, xval = 5 or 10

# assign it to an object "loan.ct"

loan.ct = rpart()



### step 3: plot the cp or relative error

# choose the value for "size of the tree" where the relative error stabilizes

plotcp()



### step 4: find "nsplit" value and its associated cp value from "size of the tree"

cp.table = as_tibble()

# nsplit = size of the tree - 1

optimal.cp = cp.table %>%
  filter()



### step 5: prune the tree with the optimal cp

pruned.ct = prune()

# plot of the pruned tree

prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = -10)



# step 6: predict the loan status for validation data

results = predict()



### step 7: generate confusion matrix and accuracy measures

# create a new variable "loan_status_prediction" in the validation data
# loan_status_prediction reflects the predicted loan_status

validation = validation %>%
  mutate()


# confusion matrix, accuracy, sensitivity & specificity

confusionMatrix(,)
