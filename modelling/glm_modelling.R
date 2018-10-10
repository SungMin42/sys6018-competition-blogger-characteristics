
# GLM Lasso ---------------------------------------------------------------

library(glmnet)
library(magrittr)
library(dplyr)
setwd("..")

df.train.final = readRDS("data//df.train.final.rds")
df.test.final = readRDS("data//df.test.final.rds")

# Note that we will not need user.id for prediction
df.train.final$user.id = NULL

# Another useless field as it currently stands is the date and text field
df.train.final$date = NULL
df.test.final$date = NULL
df.train.final$text = NULL
df.test.final$text = NULL

# Getting rid of illegal names that do now work with sparse matrices
df.train.final$'break' = NULL
df.train.final$'next' = NULL
df.test.final$'next' = NULL
df.test.final$'break' = NULL
colnames(df.train.final)[573:582] = c('topic1', 'topic2', 'topic3', 'topic4', 'topic5', 'topic6', 'topic7', 'topic8', 'topic9', 'topic10')
colnames(df.test.final)[573:582] = c('topic1', 'topic2', 'topic3', 'topic4', 'topic5', 'topic6', 'topic7', 'topic8', 'topic9', 'topic10')

# renaming duplicate variables
colnames(df.train.final)[3] = "starsign"
colnames(df.test.final)[4] = "starsign"

# train data prep for glmnet
y.train = df.train.final$age
x.train = df.train.final
x.train$age = NULL
x.sparse.train = sparse.model.matrix(~., data = x.train)

# test data prep for glmnet
user.id = df.test.final$user.id
x.test = df.test.final
x.test$user.id = NULL
x.sparse.test = sparse.model.matrix(~., data = x.test)

# Fitting glmnet
glm.lasso.fit = glmnet(x = x.sparse.train, y = y.train, alpha = 0)

# Taking the lambda value that minimises the mean-squared error from cross validation
cvfit = cv.glmnet(x.sparse.train, y.train, alpha = 0)
plot(cvfit)
lambda = cvfit$lambda.min

# Getting test prediction
predictions = predict(cvfit, newx = x.sparse.test, s = "lambda.min")

write.csv(cbind(user.id, predictions), 'predictions.csv')
