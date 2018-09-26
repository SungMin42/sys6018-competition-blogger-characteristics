# Catherine Beazley
# Kaggle Competition 3

library(tidyverse)
setwd('C:/Users/cathe/Desktop/SYS6018')
blogs <- read_csv('train.csv')
test <- read_csv('test.csv')

unique(blogs$topic)
colnames(blogs)


# Doing a simple regression model to predict age for a submission by tomorrow

# Cross Validate
sub <- sample(1:length(blogs$post.id), length(blogs$post.id)/2)
train.set <-blogs[sub,]
valid.set<-blogs[-sub,]
model1 <- lm(age~topic, data=train.set)
summary(model1)
model2 <- lm(age~topic+gender, data=train.set)
summary(model2)

# model 1
preds <- predict(model1, newdata = valid.set,type="response" )
mean((valid.set$age-preds)^2)
# 45.95101

# model 2
preds2 <- predict(model2, newdata = valid.set,type="response" )
mean((valid.set$age-preds2)^2)
# 45.92698

# making final data set to submit to kaggle
preds.final <- predict(model2, newdata = test,type="response" )
preds.table <- as.data.frame(cbind(test$user.id, preds.final))
colnames(preds.table) <- c("user.id", "age")
preds2 <- preds.table %>% group_by(user.id) 
final.preds <- unique(preds2)
write.table(final.preds, file="Age-Predictions_9-26-2018.csv", row.names=F, col.names = c("user.id", "age"), sep=',')

################## Actual Text Analysis ##################################
#install.packages('NLP')
#library(NLP)

#install.packages("tidytext")
library(tidytext)
copy <- blogs

# Practicing doing text stuff on one text section
text <- copy[1,"text"]

# putting text in one token per row format
install.packages('qlcMatrix')
library(qlcMatrix)
text.vect <- splitText(text, sep='.')

# removing punctuation using regular expression
text <-  gsub("[[:punct:]]", "", text) 

# putting everything in lowercase
text <- tolower(text)



# removing stop words
text <- text %>% anti_join(get_stopwords())
get_stopwords(text)
stopwords_getlanguages('en')
