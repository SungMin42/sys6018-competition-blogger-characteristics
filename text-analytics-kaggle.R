# Finding Text Variables for Text Analysis for Kaggle Competition 3
# Catherine Beazley

# For each blog post need to find:
# - number of words in the post
# - word length
# - lexical diversity
# - lexical density

#install.packages("quanteda")
library(tm)
library(dplyr)
library(quanteda)
library(tidyverse)


setwd('C:/Users/cathe/Desktop/SYS6018')
df.train <- read_csv('train.csv')

# I copied this part from Github so that I didn't have to redo it
df.train.blogs = df.train[1:10,c("post.id", "text")]
names(df.train.blogs) = c("doc_id", "text")
train.blogs = VCorpus(DataframeSource(df.train.blogs))

# Cleaning the text
train.blogs.clean = tm_map(train.blogs, stripWhitespace)                          # remove extra whitespace
train.blogs.clean = tm_map(train.blogs.clean, removeNumbers)                      # remove numbers
train.blogs.clean = tm_map(train.blogs.clean, removePunctuation)                  # remove punctuation
train.blogs.clean = tm_map(train.blogs.clean, content_transformer(tolower))       # ignore case
train.blogs.clean = tm_map(train.blogs.clean, removeWords, stopwords("english"))  # remove stop words
train.blogs.clean = tm_map(train.blogs.clean, stemDocument) # stem all words

# creating a dataframe of blog posts with the blog id and the content of the corpus
df <- data.frame(text=unlist(sapply(train.blogs.clean, `[`, "content")), stringsAsFactors=F)
df$id <- df.train.blogs$doc_id
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


