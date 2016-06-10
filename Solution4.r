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

#Add Year column
dftrain[,"Year"] <- (as.numeric(format(as.Date(dftrain$Date), "%Y")))
dftest[,"Year"] <- (as.numeric(format(as.Date(dftest$Date), "%Y")))

#Add previous year column to the test dataset
dftest[,"PYear"] <- dftest$Year - 1

#adjust sales in weeks 48 and 49 in 2011 to have same number of sale days in week 49 than in 2012
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
			#inc_sales <- sum(dfsample$Weekly_Sales) * 2 / 8	
			old <- dfsample$Weekly_Sales[(dfsample$Seasonality == 48)]
			dftrain$Weekly_Sales[(dftrain$Store==i)&(dftrain$Dept == j)&(dftrain$Seasonality == 48)&(dftrain$Year == 2011)] <- old - inc_sales
			old <- dfsample$Weekly_Sales[(dfsample$Seasonality == 49)]
			dftrain$Weekly_Sales[(dftrain$Store==i)&(dftrain$Dept == j)&(dftrain$Seasonality == 49)&(dftrain$Year == 2011)] <- old + inc_sales
	}
}

#Weekly_Sales are last year sales
dfresult <- merge(dftest, dftrain, by.x = c("Store","Dept", "Seasonality", "PYear"), 
	by.y = c("Store","Dept", "Seasonality", "Year"), all.x = TRUE)

#for depts with no data use 0 sales
dfresult$Weekly_Sales[is.na(dfresult$Weekly_Sales)] <- 0

#preparing the submission file
#Adding a column with store_department_date
dfresult[,"Id"] <- paste(dfresult$Store, dfresult$Dept, format(as.Date(dfresult$Date.x), "%Y-%m-%d" ), sep = "_") 

#save results in a csv file
write.csv(file = "Solution4.csv", x = subset(dfresult, select = c(Id, Weekly_Sales)), row.names=FALSE)