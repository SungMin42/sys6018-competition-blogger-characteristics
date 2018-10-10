# C2-10
# Kaggle Competition 3
# SYS 6028

#setwd("~/Desktop/Fall 2018/SYS6018/Competition 3")

#install.packages("quanteda")
#install.packages("tm")

library(dplyr) #data manipulation
library(tidytext) #text mining
library(stringr) # for str_split
library(tidyr) 
library(lubridate)
library(tidyverse)
library(tm)
library(quanteda)

##### Read Data, Create DFs #####

# a file of months translated to english 
months <- as.data.frame(read.delim('months.txt', sep = ",", header = FALSE))
colnames(months) <- c("V1","V2")

# import the train and test data, removing the age (response) and binding the rows
df.train <- read_csv('train.csv')
df.test <- read_csv('test.csv')

df.test = add_column(df.test, age = rep(0, nrow(df.test)))
df.train = add_column(df.train, TrainInd = rep(1, nrow(df.train)))
df.test = add_column(df.test, TrainInd = rep(0, nrow(df.test)))

df.train.test = rbind(df.train, df.test)

##### Format the dates #####

# string split the date column, add a row Index, and fix empty Months and Years
dates <- as.data.frame(str_split_fixed(df.train.test$date, ",", n = 3))
dates <- cbind(dates,c(1:nrow(dates)))
dates$V2 <- as.character(dates$V2)
colnames(dates) <- c("day","month","year","i")
dates$month[which(dates$month == "")] <- "Unknown"
dates$year[which(dates$year == "")] <- "2005"

# Create a data frame of months that need translating
mon <- data.frame(x = as.character(dates$month), y = as.integer(dates$i))
colnames(mon) <- c("V1","i")

# Merge the translated months and re-sort by the Index
library(plyr)
mon <- join(mon, months, by = "V1", type = "left")
mon <- mon[order(mon$i),]

# Put the translated months back into the combined set
dates$month <- mon$V2
dates$Date <- paste(dates$year,dates$month,dates$day, sep = "-")
dates$Date2 <- as.Date(ymd(dates$Date))
df.train.test$date <- dates$Date2

##### Group by User ID, Concatenate Text #####

# Concatenate the text of each user's posts into one row
detach(package:plyr)
head(df.train.test)

df.train.test2 <- df.train.test %>% 
  group_by(user.id) %>% 
  summarise(gender = names(which.max(table(gender))), topic = names(which.max(table(topic))), sign = names(which.max(table(sign))), date = max(date), text = paste0(text,collapse = " "), age = max(age), TrainInd = max(TrainInd))

# Are there missing values? Yes, in the date column
missing = colMeans(is.na(df.train.test2))
missing

# Set the one missing date to a random date
df.train.test2$date[which(is.na(df.train.test2$date))] <- as.Date("01-12-2005")

##### Factor the Appropriate Columns #####

factor.variables = sapply(df.train.test2, class)
factor.variables = factor.variables[factor.variables == "character"]

# Taking text variable out of factor.variables list
factor.variables = factor.variables[-which(names(factor.variables) == "text")]

# Class change for factor variables
df.train.test.dummy = df.train.test2[, names(factor.variables)]
df.train.test2[, names(factor.variables)] = lapply(df.train.test.dummy, factor)
str(df.train.test2)

##### Separate Train and Test #####

# Now that schema and features are consistent, split back into train and test
df.train = df.train.test2 %>% filter(TrainInd == 1)
df.test = df.train.test2 %>% filter(TrainInd == 0)

# Dropping TrainInd from df.train and df.test
df.test = df.test %>% select(-TrainInd, -age)
df.train = df.train %>% select(-TrainInd)

##### Text Mining (tm package) #####

# install.packages("tm")
library(XML)
library(tm)
library(SnowballC)

df.train.blogs <- df.train
df.train.blogs <- df.train.blogs[,c("user.id", "text")]
names(df.train.blogs) = c("doc_id", "text")

# Create a VCorpus
train.blogs <- VCorpus(DataframeSource(df.train.blogs))

# regular indexing returns a sub-corpus
inspect(train.blogs[1:2])

# formatting the text data
train.blogs.clean <- tm_map(train.blogs, stripWhitespace)                          # remove extra whitespace
train.blogs.clean <- tm_map(train.blogs.clean, removeNumbers)                      # remove numbers
train.blogs.clean <- tm_map(train.blogs.clean, removePunctuation)                  # remove punctuation
train.blogs.clean <- tm_map(train.blogs.clean, content_transformer(tolower))       # ignore case
train.blogs.clean <- tm_map(train.blogs.clean, removeWords, stopwords("english"))  # remove stop words
train.blogs.clean <- tm_map(train.blogs.clean, stemDocument)                       # stem all words

##### Lexical Diversity #####

# creating a dataframe of blog posts with the blog id and the content of the corpus
df <- data.frame(text=unlist(sapply(train.blogs.clean, `[`, "content")), stringsAsFactors=F)
df$id <- df.train.blogs$user.id
rownames(df) <- NULL
df <- df[,c(2,1)]

# Adding a column for number of words per post
df$`Number of Words` <- sapply(strsplit(df$text, " "), length)

# Calculating Average Word Length per post
df$`Avg Word Length` <- (str_length(df$text)-(df$`Number of Words`-1))/df$`Number of Words`

# Determining Number of Unique Words
df$`Number of Unique Words` <- (sapply(sapply(strsplit(df$text, " "), unique), length))

# Determining Lexical Density-- this will be number of distinct words/total number of words
df$`Lexical Density`<- df$`Number of Unique Words`/df$`Number of Words`

# Determining Lexical Diversity Measures
lexdiv <- textstat_lexdiv(dfm(df$text), measure = c("all", "TTR", "C", "R", "CTTR", "U", "S", "Maas"), log.base = 10)
df <- cbind(df,lexdiv[, 2:10] )
df <- df[,c(3:15)]

##### Document Term Martix Creation #####

# Creating a Document Term Matrix, Tf and TfIdf
blogs.clean.tfidf = DocumentTermMatrix(train.blogs.clean, control = list(weighting = weightTfIdf))
blogs.clean.tf = DocumentTermMatrix(train.blogs.clean, control = list(weighting = weightTf))
blogs.clean.tfidf
blogs.clean.tf

# we've got a very sparse document-term matrix. remove sparse terms at various thresholds.
tfidf.95 = removeSparseTerms(blogs.clean.tfidf, 0.95)  # remove terms that are absent from at least 95% of documents (keep most terms)
tf.95 = removeSparseTerms(blogs.clean.tf, 0.95)


##### Topic Modeling: 10 Topics #####

# install.packages("topicmodels")
library(topicmodels)

# Getting rid of empty rows
row.sums = apply(tfidf.80, 1, sum)
train.blogs.clean = train.blogs.clean[row.sums > 0]
tfidf.80 = tfidf.80[row.sums > 0,]

# train topic model with 10 topics
topic.model = LDA(tfidf.80, 10)

# look at the top (highest probability) 10 words within the first 5 topics
terms(topic.model, 10)[,1:5]

# look at the top (highest probability) 5 topics within the first 10 documents
topics(topic.model, 5)[,1:10]


# Create the DocumentTermMatrix for the test data
testing.documents = df.test[, c("user.id", "text")]
names(testing.documents) = c("doc_id", "text")
testing.corpus = VCorpus(DataframeSource(testing.documents))
testing.corpus = tm_map(testing.corpus, stripWhitespace)                    # remove extra whitespace
testing.corpus = tm_map(testing.corpus, removeNumbers)                      # remove numbers
testing.corpus = tm_map(testing.corpus, removePunctuation)                  # remove punctuation
testing.corpus = tm_map(testing.corpus, content_transformer(tolower))       # ignore case
testing.corpus = tm_map(testing.corpus, removeWords, stopwords("english"))  # remove stop words
testing.corpus = tm_map(testing.corpus, stemDocument)                       # stem all words

##### Document Term Martix Creation, Testing #####

testing.corpus.tf = DocumentTermMatrix(testing.corpus, control = list(weighting = weightTf))
testing.corpus.tfidf = DocumentTermMatrix(testing.corpus, control = list(weighting = weightTfIdf))

# Remove sparse terms from teh test data DocumentTermMatrix
tfidf.95.test = removeSparseTerms(testing.corpus.tfidf, 0.95)  # remove terms that are absent from at least 95% of documents (keep most terms)
tf.95.test = removeSparseTerms(testing.corpus.tf, 0.95) 

##### Lexical Diversity, Test Data #####

# creating a dataframe of blog posts with the blog id and the content of the corpus
df2 <- data.frame(text=unlist(sapply(testing.corpus, `[`, "content")), stringsAsFactors=F)

# Adding a column for number of words per post
df2$`Number of Words` <- sapply(strsplit(df2$text, " "), length)

# Calculating Average Word Length per post
df2$`Avg Word Length` <- (str_length(df2$text)-(df2$`Number of Words`-1))/df2$`Number of Words`

# Determining Number of Unique Words
df2$`Number of Unique Words` <- (sapply(sapply(strsplit(df2$text, " "), unique), length))

# Determining Lexical Density-- this will be number of distinct words/total number of words
df2$`Lexical Density`<- df2$`Number of Unique Words`/df2$`Number of Words`

# Determining Lexical Diversity Measures
lexdiv <- textstat_lexdiv(dfm(df2$text), measure = c("all", "TTR", "C", "R", "CTTR", "U", "S", "Maas"), log.base = 10)
df2 <- cbind(df2,lexdiv[, 2:10] )
df2<- df2[,c(2:14)]

# infer the topic probabilities of new text using the topic model we estimated above
inferred_probabilities.test = posterior(topic.model, tf.95.test)
inferred_probabilities.test$topics

inferred_probabilities.train = posterior(topic.model, tf.95)
inferred_probabilities.train$topics

# as.data.frame(as.matrix(tfidf.95))
# save.image(file = "Competition3.RData")

##### Create the final train test data frames #####

# store the topic probabilities and the document term matrices as data frames
tfidf.95.df <- as.data.frame(as.matrix(tfidf.95))
tfidf.95.df.test <- as.data.frame(as.matrix(tfidf.95.test))
colnames(tfidf.95.df) <- paste(colnames(tfidf.95.df),".0",sep = "")
colnames(tfidf.95.df.test) <- paste(colnames(tfidf.95.df.test),".0",sep = "")

inferred.probabilities.train <- as.data.frame(inferred_probabilities.train$topics)
inferred.probabilities.test <- as.data.frame(inferred_probabilities.test$topics)
colnames(inferred.probabilities.test) <- c("topic1","topic2","topic3","topic4","topic5","topic6","topic7","topic8","topic9","topic10")
colnames(inferred.probabilities.train) <- c("topic1","topic2","topic3","topic4","topic5","topic6","topic7","topic8","topic9","topic10")

# Subset only columns that are shared between the train and the test DocumentTermMatrices
names.train <- names(as.data.frame(as.matrix(tfidf.95)))
names.test <- names(as.data.frame(as.matrix(tfidf.95.test)))
tfidf.95.df <- tfidf.95.df[,which(colnames(tfidf.95.df) %in% names.test)]
tfidf.95.df.test <- tfidf.95.df.test[,which(colnames(tfidf.95.df.test) %in% names.train)]

# Append the document term matrices and the topic probabilities
df.train.final <- cbind(df.train, tfidf.95.df)
df.train.final <- cbind(df.train.final, inferred.probabilities.train)
df.train.final <- cbind(df.train.final, df)

df.test.final <- cbind(df.test, tfidf.95.df.test)
df.test.final <- cbind(df.test.final, inferred.probabilities.test)
df.test.final <- cbind(df.test.final, df2)

# Take punctuation out of colnames
colnames(df.train.final) <- gsub(pattern = "[.]",replacement = "", x = colnames(df.train.final))
colnames(df.train.final) <- gsub(pattern = "[’]",replacement = "", x = colnames(df.train.final))

colnames(df.test.final) <- gsub(pattern = "[.]",replacement = "", x = colnames(df.test.final))
colnames(df.test.final) <- gsub(pattern = "[’]",replacement = "", x = colnames(df.test.final))

saveRDS(df.test.final, file = "df.test.final.rds")
saveRDS(df.train.final, file = "df.train.final.rds")

dim(df.train.final)
dim(df.test.final)
