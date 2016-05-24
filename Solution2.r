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

#add Weekly_Sales column to the test dataset
dftest[,"Weekly_Sales"] <- NA

# for every store (45)
for (i in 1:45)
{
	#for every department (99)
	for (j in 1:99)
	{
		# for every week (52)
		for (k in 1:52)
		{
			#do we have test data?
			dftarget <- dftest %>%
				filter(Store == i) %>%
				filter(Dept == j) %>%
				filter(Seasonality == k)
			if(nrow(dftarget) == 0)
			{
				next
			}
			#do we have train data?
			dfsample <- dftrain %>%
				filter(Store == i) %>%
				filter(Dept == j) %>%
				filter(Seasonality == k)
			if(nrow(dfsample) == 0)
			{
				next
			}
				
			#apply a linear model
			res <- lm( Weekly_Sales ~ Year, data = dfsample)

			#predict based on the model
			dftest$Weekly_Sales[(dftest$Store==i)&(dftest$Dept == j)&(dftest$Seasonality == k)] <- predict (res,dftarget, interval='prediction')
		}
	}
}

#for depts with no data use 0
dftest$Weekly_Sales[is.na(dftest$Weekly_Sales)] <- 0
 
#preparing the submission file
#Adding a column with store_department_date
dftest[,"Id"] <- paste(dftest$Store, dftest$Dept, format(as.Date(dftest$Date), "%Y-%m-%d" ), sep = "_") 

#save results in a csv file
write.csv(file = "Solution2.csv", x = subset(dftest, select = c(Id, Weekly_Sales)), row.names=FALSE)