# Catherine Beazley
# Kaggle Competition 3

library(tidytext)
library(tidyverse)

setwd('C:/Users/cathe/Desktop/SYS6018')
blogs <- read_csv('train.csv')
test <- read_csv('test.csv')

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

######################### data preprocessing ############3
# frequency of post-- group by
# seasonality of post-- group by
# cleaning date

# use latest post, lateset information rn, down the line


############### Ideas for analysis ##############
# group by user, concatenate texts, topics so see all text for one user


################## Actual Text Analysis ##################################

############## FOllowing the Data Camp tutorial ###################

copy <- blogs
text <- copy[1,"text"]

# putting in all lower case
text <- tolower(text)

# removing punctuation
text <-  gsub("[[:punct:]]", "", text) 

# Fixing contractions
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}
text <- fix.contractions(text)

# Remove special characters
removeSpecials <- function(doc){
  gsub("[^a-zA-Z0-9 ]", " ", doc)
}

text <- removeSpecials(text)

# tokenization

# before this, you're suppossed to pull out 'undesirable' words
# that you know will be in the text but don't care baout
# We're not suppossed to know anything about the text so I 
# am skipping this part, except for some things

# need ll here fore all the 'll words
wordsThatDontCount <- c("ll")

# now tokenizing the text by breaking it apart into
# a dataframe with word as a column, and the input is 
# the text

# also removing stopwords in the piping
# they also remove words with less than 3 characters, I am going to keep those for now

library(tidytext)
library(tidyverse)
blogs[1,] %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% distinct() %>%
  filter(!word %in% wordsThatDontCount) 

# Need to save above, but everytime I do, it doesn't save the word part

# Should I remove numbers or replace the numbers with the word version of the numbers??
# i think numbers could be important, keeping them
# dim(wordsPerText)

# Text variables to consider
#-number of words in the post
#-lexical diversity
#-lexical density
#-frequency of each word in post

