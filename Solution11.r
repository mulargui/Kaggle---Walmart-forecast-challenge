# https://github.com/mulargui/Kaggle---Walmart-forecast-challenge

#working directory
setwd("C:/mauricio/Dropbox/vagrant/apps/Kaggle---Walmart-forecast-challenge")

library("dplyr")

#load data
dftrain <- read.csv("train.csv", header=TRUE)

#Add Seasonality Column based on factor of Date
dftrain[,"Seasonality"] <- (as.numeric(dftrain$Date) %% 52) + 1 

#Add Year column
dftrain[,"Year"] <- (as.numeric(format(as.Date(dftrain$Date), "%Y")))

#adjust sales in weeks 47, 48 and 49 in 2010 to have same number of sale days in week 49 than in 2012
dfchristmas <- dftrain %>% filter(Year == 2010) %>% filter((Seasonality == 47) | (Seasonality == 48) | (Seasonality == 49))

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
			if(nrow(dfsample) < 3) next
			inc_sales <- sum(dfsample$Weekly_Sales) * 3 / 21	
			old <- dfsample$Weekly_Sales[(dfsample$Seasonality == 47)]
			dftrain$Weekly_Sales[(dftrain$Store==i)&(dftrain$Dept == j)&(dftrain$Seasonality == 47)&(dftrain$Year == 2010)] <- old - (inc_sales / 2)
			old <- dfsample$Weekly_Sales[(dfsample$Seasonality == 48)]
			dftrain$Weekly_Sales[(dftrain$Store==i)&(dftrain$Dept == j)&(dftrain$Seasonality == 48)&(dftrain$Year == 2010)] <- old - (inc_sales / 2)
			old <- dfsample$Weekly_Sales[(dfsample$Seasonality == 49)]
			dftrain$Weekly_Sales[(dftrain$Store==i)&(dftrain$Dept == j)&(dftrain$Seasonality == 49)&(dftrain$Year == 2010)] <- old + inc_sales
	}
}

#adjust sales in weeks 47, 48 and 49 in 2011 to have same number of sale days in week 49 than in 2012
dfchristmas <- dftrain %>% filter(Year == 2011) %>% filter((Seasonality == 47) | (Seasonality == 48) | (Seasonality == 49))

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
			if(nrow(dfsample) < 3) next
			inc_sales <- sum(dfsample$Weekly_Sales) * 2 / 21	
			old <- dfsample$Weekly_Sales[(dfsample$Seasonality == 47)]
			dftrain$Weekly_Sales[(dftrain$Store==i)&(dftrain$Dept == j)&(dftrain$Seasonality == 47)&(dftrain$Year == 2011)] <- old - (inc_sales / 2)
			old <- dfsample$Weekly_Sales[(dfsample$Seasonality == 48)]
			dftrain$Weekly_Sales[(dftrain$Store==i)&(dftrain$Dept == j)&(dftrain$Seasonality == 48)&(dftrain$Year == 2011)] <- old - (inc_sales / 2)
			old <- dfsample$Weekly_Sales[(dfsample$Seasonality == 49)]
			dftrain$Weekly_Sales[(dftrain$Store==i)&(dftrain$Dept == j)&(dftrain$Seasonality == 49)&(dftrain$Year == 2011)] <- old + inc_sales
	}
}

#save results in a csv file
write.csv(file = "massagedtrain.csv", x = subset(dftrain, select = c(Store, Dept, Date, Weekly_Sales, IsHoliday)), row.names=FALSE)