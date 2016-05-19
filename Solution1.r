# https://github.com/mulargui/Kaggle---Walmart-forecast-challenge

#working directory
setwd("C:/mauricio/Dropbox/vagrant/apps/Kaggle---Walmart-forecast-challenge")

library("dplyr")

#load data
dftrain <- read.csv("train.csv", header=TRUE)
dftest <- read.csv("test.csv", header=TRUE)

#Add Seasonality Column based on factor of Date
dftrain[,"Seasonality"] <- (as.numeric(dftrain$Date) %% 52) + 1 
dftest[,"Seasonality"] <- ((as.numeric(dftest$Date) + 143) %% 52) + 1 

#avearage sales per week per dept
avg_sales <- dftrain %>%
  group_by(Store, Dept, Seasonality) %>%
  summarise(Weekly_Sales = mean(Weekly_Sales))

#predict based on average sales  
dfresult <- merge(dftest, avg_sales, by = c("Store","Dept", "Seasonality"), all.x = TRUE)

#for depts with no data use 0
dfresult$Weekly_Sales[is.na(dfresult$Weekly_Sales)] <- 0
 
#preparing the submission file
#Adding a column with store_department_date
dfresult[,"Id"] <- paste(dfresult$Store, dfresult$Dept, format(as.Date(dfresult$Date), "%Y-%m-%d" ), sep = "_") 

#save results in a csv file
write.csv(file = "Solution1.csv", x = subset(dfresult, select = c(Id, Weekly_Sales)), row.names=FALSE)