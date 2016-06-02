# https://github.com/mulargui/Kaggle---Walmart-forecast-challenge

#working directory
setwd("C:/mauricio/Dropbox/vagrant/apps/Kaggle---Walmart-forecast-challenge")

#load data
dftrain <- read.csv("train.csv", header=TRUE)
dftest <- read.csv("test.csv", header=TRUE)

#Add Seasonality Column based on factor of Date
dftrain[,"Seasonality"] <- (as.numeric(dftrain$Date) %% 52) + 1 
dftest[,"Seasonality"] <- ((as.numeric(dftest$Date) + 143) %% 52) + 1 

#Add Year column
dftrain[,"Year"] <- (as.numeric(format(as.Date(dftrain$Date), "%Y")))
dftest[,"Year"] <- (as.numeric(format(as.Date(dftest$Date), "%Y")))

#Add previous year column to the test dataset
dftest[,"PYear"] <- dftest$Year - 1

#Weekly_Sales are last year sales
dfresult <- merge(dftest, dftrain, by.x = c("Store","Dept", "Seasonality", "PYear"), 
	by.y = c("Store","Dept", "Seasonality", "Year"), all.x = TRUE)

#for depts with no data use 0 sales
dfresult$Weekly_Sales[is.na(dfresult$Weekly_Sales)] <- 0

#preparing the submission file
#Adding a column with store_department_date
dfresult[,"Id"] <- paste(dfresult$Store, dfresult$Dept, format(as.Date(dfresult$Date.x), "%Y-%m-%d" ), sep = "_") 

#save results in a csv file
write.csv(file = "Solution3.csv", x = subset(dfresult, select = c(Id, Weekly_Sales)), row.names=FALSE)