# Andrew Evans
# Kaggle Competition 3

setwd("~/Desktop/Fall 2018/SYS6018/Competition 3")


library(dplyr) #data manipulation
library(tidytext) #text mining
library(stringr)
library(tidyr)
library(lubridate)

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


