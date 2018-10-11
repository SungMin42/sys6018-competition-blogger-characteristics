
# GLM Lasso ---------------------------------------------------------------
# Loading packages necessary for glmnet
library(glmnet)
library(magrittr)
library(dplyr)

# going to root git directory
setwd("..")

# reading in data in data folder
df.train.final = readRDS("data//df.train.final.95.rds")
df.test.final = readRDS("data//df.test.final.95.rds")

# another useless field as it currently stands is the date and text field
df.train.final$date = NULL
df.test.final$date = NULL
df.train.final$text = NULL
df.test.final$text = NULL

# getting rid of illegal names that do now work with sparse matrices
df.train.final$'break' = NULL
df.train.final$'next' = NULL
df.test.final$'next' = NULL
df.test.final$'break' = NULL

# data preparation for sparse matrix
# renaming duplicate variables
colnames(df.train.final)[which(colnames(df.train.final) == 'cant0')[1]] = 'cant1'
colnames(df.test.final)[which(colnames(df.test.final) == 'cant0')[1]] = 'cant1'
colnames(df.train.final)[which(colnames(df.train.final) == 'didnt0')[1]] = 'didnt1'
colnames(df.test.final)[which(colnames(df.test.final) == 'didnt0')[1]] = 'didnt1'
colnames(df.train.final)[which(colnames(df.train.final) == 'doesnt0')[1]] = 'doesnt1'
colnames(df.test.final)[which(colnames(df.test.final) == 'doesnt0')[1]] = 'doesnt1'
colnames(df.train.final)[which(colnames(df.train.final) == 'dont0')[1]] = 'dont1'
colnames(df.test.final)[which(colnames(df.test.final) == 'dont0')[1]] = 'dont1'
colnames(df.train.final)[which(colnames(df.train.final) == 'isnt0')[1]] = 'isnt1'
colnames(df.test.final)[which(colnames(df.test.final) == 'isnt0')[1]] = 'isnt1'
colnames(df.train.final)[which(colnames(df.train.final) == 'wasnt0')[1]] = 'wasnt1'
colnames(df.test.final)[which(colnames(df.test.final) == 'wasnt0')[1]] = 'wasnt1'
colnames(df.train.final)[which(colnames(df.train.final) == 'wont0')[1]] = 'wont1'
colnames(df.test.final)[which(colnames(df.test.final) == 'wont0')[1]] = 'wont1'

# renaming variables with spaces in them
colnames(df.train.final)[which(colnames(df.train.final) == 'Number of Words')[1]] = 'NoWords'
colnames(df.test.final)[which(colnames(df.test.final) == 'Number of Words')[1]] = 'NoWords'
colnames(df.train.final)[which(colnames(df.train.final) == 'Avg Word Length')[1]] = 'AvgWordLength'
colnames(df.test.final)[which(colnames(df.test.final) == 'Avg Word Length')[1]] = 'AvgWordLength'
colnames(df.train.final)[which(colnames(df.train.final) == 'Lexical Density')[1]] = 'LexDensity'
colnames(df.test.final)[which(colnames(df.test.final) == 'Lexical Density')[1]] = 'LexDensity'
colnames(df.train.final)[which(colnames(df.train.final) == 'Number of Unique Words')[1]] = 'NumUniqueWords'
colnames(df.test.final)[which(colnames(df.test.final) == 'Number of Unique Words')[1]] = 'NumUniqueWords'

# creating y column and x sparse matrix for train
y.train = df.train.final$age
x.train = df.train.final
x.train$age = NULL
x.sparse.train = sparse.model.matrix(~., data = x.train)

# creating x sparse matrix for test
x.test = df.test.final
x.sparse.test = sparse.model.matrix(~., data = x.test)


# fitting glmnet
glm.lasso.fit = glmnet(x = x.sparse.train, y = y.train, alpha = 1)

# taking the lambda value that minimises the mean-squared error from cross validation
cvfit = cv.glmnet(x.sparse.train, y.train, alpha = 1)
plot(cvfit)
lambda = cvfit$lambda.min
lambda

# extracting the coefficients to be used in other models
coef = coef(glm.lasso.fit, s = lambda)

# getting test prediction
predictions = predict(cvfit, newx = x.sparse.test, s = "lambda.min")
write.csv(predictions, 'predictions2.csv')

# saving coef, x.train, x.test sparse matrices and y.train
saveRDS(coef, "data//coef.rds")
saveRDS(x.train, "data//x.train.rds")
saveRDS(x.test, "data//x.test.rds")
saveRDS(y.train, "data//y.train.rds")
