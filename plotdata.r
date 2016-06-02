# https://github.com/mulargui/Kaggle---Walmart-forecast-challenge

#working directory
setwd("C:/mauricio/Dropbox/vagrant/apps/Kaggle---Walmart-forecast-challenge")

library("dplyr")
library("ggplot2")

#load data
dftrain <- read.csv("train.csv", header=TRUE)

#Add Seasonality Column based on factor of Date
dftrain[,"Seasonality"] <- (as.numeric(dftrain$Date) %% 52) + 1 

#Add Year column
dftrain[,"Year"] <- (as.numeric(format(as.Date(dftrain$Date), "%Y")))

# Plot total sales per week (accumulated)
dftrain_plot <- dftrain %>%
  group_by(Seasonality) %>%
  summarise(Weekly_Sales = sum(Weekly_Sales))

ggplot(dftrain_plot, aes( x = Seasonality, y = Weekly_Sales)) + geom_line() 

# Plot total sales per week per dept (accumulated)
dftrain_plot <- dftrain %>%
  group_by(Seasonality, Dept) %>%
  summarise(Weekly_Sales = sum(Weekly_Sales))

ggplot(dftrain_plot, aes(x = Seasonality, y = Weekly_Sales, color = Dept)) + geom_line() 
ggplot(dftrain_plot, aes(x = Seasonality, y = Weekly_Sales)) + geom_line() + facet_wrap(~Dept, ncol = 10)
ggplot(subset(dftrain_plot, Dept == 1), aes(x = Seasonality, y = Weekly_Sales, color = Dept)) + geom_line() 

# Plot total sales per week per store (accumulated)
dftrain_plot <- dftrain %>%
  group_by(Seasonality, Store) %>%
  summarise(Weekly_Sales = sum(Weekly_Sales))

ggplot(dftrain_plot, aes(x = Seasonality, y = Weekly_Sales, color = Store)) + geom_line() 
ggplot(dftrain_plot, aes(x = Seasonality, y = Weekly_Sales)) + geom_line() + facet_wrap(~Store, ncol = 7)
ggplot(subset(dftrain_plot, Store == 1), aes(x = Seasonality, y = Weekly_Sales, color = Store)) + geom_line() 
