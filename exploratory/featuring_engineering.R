library(tidyverse)
library(dplyr)
library(magrittr)
library(Amelia)

# Reading in the data
setwd("../")
df.train = read_csv("data/train.csv")
str(df.train)
# Create artificial train test dataset to make sure that dataset has all factors contained in 
# test
#   Read in test dataset
df.test = read_csv("data/test.csv")
#   Artificially created target variable to ncol matches
df.test = add_column(df.test, age = rep(0, nrow(df.test)))
#   Indicator variable for both train and test
df.train = add_column(df.train, TrainInd = rep(1, nrow(df.train)))
df.test = add_column(df.test, TrainInd = rep(0, nrow(df.test)))

#   creating train test table
df.train.test = rbind(df.train, df.test)

# Finding variables with missing values
missmap(df.train.test, main = "Missing Map")
str(df.train.test)

# More numerically, finding which variables have missing values
missing = colMeans(is.na(df.train.test))
missing = missing[missing!=0] 
missing
# We note that SaleType has a missing value

# Using the latest information of each user
# Since this information should be the most relevant
df.train.test1 = df.train.test %>% arrange(desc(post.id))
df.train.test1 = df.train.test1 %>% distinct(user.id, .keep_all = TRUE)

head(df.train.test, n = 30)
df.train.test1 %>% filter(user.id == 11869)
s
# Checking if there are missing values on this subset
missing = colMeans(is.na(df.train.test1))
missing

str(df.train.test$sign)

# For now NULL date because of its funky formatting and multiple languages
# Is there a multiple language package for dates?
df.train.test1$date = NULL

# Getting list of character variables
factor.variables = sapply(df.train.test1, class)
factor.variables = factor.variables[factor.variables == "character"]

# Taking text out factor variables
factor.variables = factor.variables[-which(names(factor.variables) == "text")]

# Class change for factor variables
df.train.test.dummy = df.train.test1[, names(factor.variables)]
df.train.test1[, names(factor.variables)] = lapply(df.train.test.dummy, factor)
str(df.train.test1)

# Imputation for character variables
df.train.test1$text[is.na(df.train.test1$text)] = "Unknown"


# # Imputation for numeric variables
# numeric.variables = which(sapply(df.train.test, is.numeric))
# df.numeric.imputed = df.train.test %>%
#   transmute_at(vars(numeric.variables), funs(func={ifelse(is.na(.), mean(., na.rm=T), .)}))
# 
# df.train.test[, names(numeric.vecs)] = df.numeric.imputed

# Rechecking if there are any missing values
missmap(df.train.test1)
# So there are no missing variables

# Now that schema and features are consistent, split back into train and test
df.train = df.train.test %>% filter(TrainInd == 1)
df.test = df.train.test %>% filter(TrainInd == 0)

# Dropping Survived and Train Ind from df.test
df.test = df.test %>% select(-TrainInd, -age)

# Dropping TrainInd from df.train and df.test
df.train = df.train %>% select(-TrainInd)

# Dropping post.id and user.id from train
df.train$post.id = NULL
df.train$user.id = NULL

################################################################################

# Standardization and normalization of numeric variables
numer.vecs <- which(sapply(df.train, is.numeric))
df.train.transf <- df.train %>% 
  mutate_at(vars(numer.vecs), funs(Standardize.vector)) %>%
  mutate_at(vars(numer.vecs), funs(Normalize.vector))

# One-hot encoding of the train data
ohe.obj <- onehot(data = df.train.transf, max_levels = 30)
df.train.ohe <- predict(object = ohe.obj, data = df.train.transf)
#ohe.obj <- onehot(data = df.train, max_levels = 30)
#df.train.ohe <- predict(object = ohe.obj, data = df.train)

# GLMnet regression for feature selection

train.resp <- log(df.train$SalePrice)
#model.glm <- glmnet(x = m.train, y = train.resp, family = "gaussian")
#predict(model.glm, newx = m.train)
#predict(model.glm, type="coef")
#plot(model.glm, xvar = "lambda")

#cv.obj <- cv.glmnet(x = m.train, y = train.resp, family = "gaussian")
#plot(predict(cv.obj,newx=m.train, s="lambda.min"))

cvob1a=cv.glmnet(m.train, train.resp, type.measure="mse")
plot(cvob1a)

coefs <- coef(cvob1a, s = "lambda.min")
coefs <- data.frame(name = coefs@Dimnames[[1]][coefs@i + 1], coefficient = coefs@x)

# Standardization and normalization of numeric variables in test set
numer.vecs <- which(sapply(df.test, is.numeric))
df.test.transf <- df.test %>% 
  mutate_at(vars(numer.vecs), funs(Standardize.vector)) %>%
  mutate_at(vars(numer.vecs), funs(Normalize.vector))

# Transform test set
ohe.obj.test <- onehot(data = df.test.transf, max_levels = 30)
df.test.ohe <- predict(object = ohe.obj.test, data = df.test.transf)
#ohe.obj.test <- onehot(data = df.test, max_levels = 30)
#df.test.ohe <- predict(object = ohe.obj.test, data = df.test)

# More exploration of the classes and thus treatment of each variable

str(df.train)

numer.vecs <- which(sapply(df.train, is.numeric))
names(numer.vecs) <- colnames(df.train)[numer.vecs]
numer.vecs

fac.vecs <- which(sapply(df.train, is.factor))
names(fac.vecs) <- colnames(df.train)[fac.vecs]
fac.vecs

col.inds <- sort(c(numer.vecs, fac.vecs))
setdiff(1:ncol(df.train), col.inds)

New.level.transformation <- function(x, threshold) {
  #Vector x and threshold is threshold
  
  interest <- names(which(table(x) < threshold))
  y <- ifelse(x %in% interest, 'Unknown', test.vector)
  
  return(y)
}
