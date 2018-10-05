# Andrew Evans
# Kaggle Competition 3

setwd("~/Desktop/Fall 2018/SYS6018/Competition 3")


library(dplyr) #data manipulation
library(tidytext) #text mining
library(stringr)
library(tidyr)
library(lubridate)

# a file of months translated to english 
months <- as.data.frame(read.delim('months.txt', sep = ",", header = FALSE))
colnames(months) <- c("V1","V2")

# import the train and test data, removing the age (response) and binding the rows
blogs <- read_csv('train.csv')
test <- read_csv('test.csv')
age <- blogs$age
blogs <- blogs[-8]
both <- rbind(blogs,test)

# string split the date column, add a row Index, and fix empty Months and Years
dates <- str_split_fixed(both$date, ",", n = 3)
dates <- as.data.frame(dates)
dates <- cbind(dates,c(1:681284))
dates$V2 <- as.character(dates$V2)
colnames(dates) <- c("Day","Month","Year","Index")
dates$Month[which(dates$Month == "")] <- "Unknown"
dates$Day[which(dates$Day == "")] <- "Unknown"
dates$Year[which(dates$Year == "")] <- "2005"

# Create a data frame of months that need translating
mon <- as.data.frame(cbind(as.character(dates$Month), dates$Index))
colnames(mon) <- c("V1","Index")

# Merge the translated months and re-sort by the Index
mon <- merge(x = mon, y = months, by.x = "V1", by.y = "V1")
mon$Index <- as.numeric(mon$Index)
mon <- mon[order(mon$Index),]

# Put the translated months back into the combined set
dates$Month2 <- mon$V2
dates$Date <- paste(dates$Year,dates$Month2,dates$Day, sep = "-")
dates$Date2 <- as.Date(ymd(dates$Date))
both$date <- dates$Date2


