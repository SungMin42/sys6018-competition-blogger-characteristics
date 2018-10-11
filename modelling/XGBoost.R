# loading required libraries
library(tidyverse)
library(xgboost)
library(magrittr)

# Reading in the sparse matrices coefficients from glm_modelling.R
setwd("..")
coef = readRDS("data//coef.rds")
x.test = readRDS("data//x.test.rds")
x.train = readRDS("data//x.train.rds")
y.train = readRDS("data//y.train.rds")

# Subsetting the columns in x.train and x.test based on coefficients
non.zero.coef = coef@Dimnames[[1]][which(coef !=0)]

# adding other non text variables to this list
non.zero.coef = c('userid', 'gender', 'topic', 'sign', 'age', non.zero.coef)

# subsetting train and test based on non.zero.coef
x.train1 = x.train[, which(colnames(x.train) %in% non.zero.coef)]
x.train1$userid = NULL

x.test1 = x.test[, which(colnames(x.test) %in% non.zero.coef)]
user.id = x.test1$userid
x.test1$userid = NULL
# refitting sparse matrices
x.sparse.train1 = sparse.model.matrix(~., data = x.train1)
x.sparse.test1 = sparse.model.matrix(~., data = x.test1)

# xgboost modelling with cross validation
bst.cv <- xgb.cv(data = x.sparse.train1, label = y.train, nfold = 5,
                 max_depth = 8, 
                 min_child_weight = 1,
                 nrounds = 1000, 
                 objective = "reg:linear",
                 early_stopping_rounds = 50)

# So the best model seems to be at iteration 97

bst = xgboost(data = x.sparse.train1, label = y.train, nfold = 5,
              max_depth = 8, 
              min_child_weight = 1,
              nrounds = 100, 
              objective = "reg:linear")

predictions = predict(bst, x.sparse.test1)
write.csv(cbind(user.id, predictions), "prediction xgboost1.csv")
