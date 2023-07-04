rm(list=ls())
setwd("D:/fda")
data <- read.csv("unemployment analysis.csv")
data1 <- read.csv("umemp.csv")

#install.packages("dplyr")
library(dplyr)
data_unemp <- sample_n(data,10)
summary(data_unemp)
View(data_unemp)
col1 = colMeans(data_unemp[sapply(data_unemp,is.numeric)])
col1
row = rowMeans(data_unemp[sapply(data_unemp,is.numeric)])
row

#install.packages("ggplot2") 
library(ggplot2)         
ggplot(data1,aes(x=Africa.Eastern.and.Southern,y=United.Arab.Emirates))+geom_point() #no correlation
ggplot(data1,aes(x=data1$Greece,y=data1$Spain))+geom_point() #weakly positive correlation
ggplot(data1,aes(x=data1$India,y=data1$Pakistan))+geom_point() #weakly negative correlation
barplot(col1,xlab="year",ylab="mean value",main="mean unemployement of countries",col="blue",border="red")
barplot(row,xlab="countries",ylab="mean value",main="mean unemployement of countries",col="red",border="blue")

#install.packages("VIM")
library(VIM)
# importing library plotrix for pie3D()

x1 <- c(data_unemp$X1991)
x2 <- c(data_unemp$Country.Code)
#install.packages("plotrix")
library(plotrix)
pie3D(x1, labels =x2, col = rainbow(length(x2)), main = "country code vs unmemployment in year 1991",cex = 0.8)
boxplot(data_unemp$X1991 ~ data_unemp$X2021, data = data_unemp, xlab = "2021 unemployment",ylab = "1991 unemployment", main = "unemployment data")
boxplot(data_unemp$X1991, data = data_unemp, main = "unemployment data for the year 1991")

#Create the histogram.
hist(data_unemp$X1992,xlab = "unemployement value",col = "yellow",border = "blue")
hist(data_unemp$X2002,xlab = "unemployement value",col = "red",border = "blue")
hist(data_unemp$X2010,xlab = "unemployement value",col = "green",border = "blue")
hist(data_unemp$X2021,xlab = "unemployement value",col = "orange",border = "blue")
#line plot
plot(col1,type = "o", col = "red", xlab ="year" , ylab = "unemployment percentage",main = "unemployment in various years chart")
#scatter plot using ggplot for year 1998 unemployment vs 1991 unemployment
ggplot(data = data, mapping = aes(x = X1991, y = X1998)) +
  geom_point(alpha = 0.5, color = "blue")
#scatter plot using ggplot for year 1991 unemployment vs 2010 unemployment
ggplot(data = data, mapping = aes(x = X1991, y = X2010)) +
  geom_point(alpha = 0.5, color = "red")
#scatter plot using ggplot for year 1991 unemployment vs 2021 unemployment
ggplot(data = data, mapping = aes(x = X1991, y = X2021)) +
  geom_point(alpha = 0.1, color = "green")

# from all plots we can see the correlation between the year 1991 vs 1998,2010,2021
#the plot gets scttered so correlation between the years starts to decrease it means 
#unmemployment starts to increase in subsequent years..
ggplot(data = data, aes(x = X1998, y = X2010,color="red")) +
  geom_line()
