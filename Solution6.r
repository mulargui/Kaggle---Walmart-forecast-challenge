# https://github.com/mulargui/Kaggle---Walmart-forecast-challenge

#working directory
setwd("C:/mauricio/Dropbox/vagrant/apps/Kaggle---Walmart-forecast-challenge")

library("dplyr")
library("forecast")

#load data
dftrain <- read.csv("train.csv", header=TRUE)
dftest <- read.csv("test.csv", header=TRUE)

#Add Seasonality Column based on factor of Date
dftrain[,"Seasonality"] <- (as.numeric(dftrain$Date) %% 52) + 1 
dftest[,"Seasonality"] <- ((as.numeric(dftest$Date) + 143) %% 52) + 1 

#Add Week Column based on factor of Date
dftrain[,"Week"] <- as.numeric(dftrain$Date)
dftest[,"Week"] <- as.numeric(dftest$Date) + 143

#Add Year column
dftrain[,"Year"] <- (as.numeric(format(as.Date(dftrain$Date), "%Y")))
dftest[,"Year"] <- (as.numeric(format(as.Date(dftest$Date), "%Y")))

#Add previous year column to the test dataset
dftest[,"PYear"] <- dftest$Year - 1

#adjust sales in weeks 48 and 49 in 2010 to have same number of sales days in week 49 than in 2012
dfchristmas <- dftrain %>% filter(Year == 2010) %>% filter((Seasonality == 48) | (Seasonality == 49))

# for every store (45)
for (i in 1:45)
{
	#for every department (99)
	for (j in 1:99)
	{
			#do we have train data?
			dfsample <- dfchristmas %>%
				filter(Store == i) %>%
				filter(Dept == j)
			if(nrow(dfsample) == 0) next
			inc_sales <- sum(dfsample$Weekly_Sales) * 3 / 14	
			old <- dfsample$Weekly_Sales[(dfsample$Seasonality == 48)]
			dftrain$Weekly_Sales[(dftrain$Store==i)&(dftrain$Dept == j)&(dftrain$Seasonality == 48)&(dftrain$Year == 2010)] <- old - inc_sales
			old <- dfsample$Weekly_Sales[(dfsample$Seasonality == 49)]
			dftrain$Weekly_Sales[(dftrain$Store==i)&(dftrain$Dept == j)&(dftrain$Seasonality == 49)&(dftrain$Year == 2010)] <- old + inc_sales
	}
}

#adjust sales in weeks 48 and 49 in 2011 to have same number of sales days in week 49 than in 2012
dfchristmas <- dftrain %>% filter(Year == 2011) %>% filter((Seasonality == 48) | (Seasonality == 49))

# for every store (45)
for (i in 1:45)
{
	#for every department (99)
	for (j in 1:99)
	{
			#do we have train data?
			dfsample <- dfchristmas %>%
				filter(Store == i) %>%
				filter(Dept == j)
			if(nrow(dfsample) == 0) next
			inc_sales <- sum(dfsample$Weekly_Sales) * 2 / 14	
			old <- dfsample$Weekly_Sales[(dfsample$Seasonality == 48)]
			dftrain$Weekly_Sales[(dftrain$Store==i)&(dftrain$Dept == j)&(dftrain$Seasonality == 48)&(dftrain$Year == 2011)] <- old - inc_sales
			old <- dfsample$Weekly_Sales[(dfsample$Seasonality == 49)]
			dftrain$Weekly_Sales[(dftrain$Store==i)&(dftrain$Dept == j)&(dftrain$Seasonality == 49)&(dftrain$Year == 2011)] <- old + inc_sales
	}
}

# holidays have a 5x weight
dftrain$Weekly_Sales[dftrain$IsHoliday==TRUE] <- dftrain$Weekly_Sales[dftrain$IsHoliday==TRUE] * 5

#use ARIMA model
dfpredict <- data.frame(Store= integer(0), Dept=integer(0) , Week = numeric(0), Weekly_Sales = numeric(0))

# for every store (45)
for (i in 1:45)
{
	#for every department (99)
	for (j in 1:99)
	{
		#do we have train data?
		dfsample <- dftrain %>%
			filter(Store == i) %>%
			filter(Dept == j) %>%
			arrange(Week)
		if(nrow(dfsample) == 0) next
		fit <- auto.arima(dfsample$Weekly_Sales, stepwise=FALSE, approximation=FALSE)
		result <- forecast(fit, h = 39)
		dfpredict <- rbind(dfpredict, data.frame(Store=i, Dept=j, Week=c(144:182), Weekly_Sales=as.numeric(result$mean)))
	}
	print(i)
}

dfresult <- merge(dftest, dfpredict, by.x = c("Store","Dept", "Week"), 
	by.y = c("Store","Dept", "Week"), all.x = TRUE)

#for depts with no data use 0 sales
dfresult$Weekly_Sales[is.na(dfresult$Weekly_Sales)] <- 0

# desemphasize holidays
dfresult$Weekly_Sales[dfresult$IsHoliday==TRUE] <- dfresult$Weekly_Sales[dfresult$IsHoliday==TRUE] / 5

#preparing the submission file
#Adding a column with store_department_date
dfresult[,"Id"] <- paste(dfresult$Store, dfresult$Dept, format(as.Date(dfresult$Date), "%Y-%m-%d" ), sep = "_") 

#save results in a csv file
write.csv(file = "Solution6.csv", x = subset(dfresult, select = c(Id, Weekly_Sales)), row.names=FALSE)