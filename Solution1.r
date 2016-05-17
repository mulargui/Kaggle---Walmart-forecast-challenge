# https://github.com/mulargui/Kaggle---Walmart-forecast-challenge

#working directory
setwd("C:/mauricio/Dropbox/vagrant/apps/Kaggle---Walmart-forecast-challenge")

library("dplyr")

#load data
dftrain <- read.csv("train.csv", header=TRUE)
dftest <- read.csv("test.csv", header=TRUE)

#add Weekly_Sales column to the test dataset
dftest[,"Weekly_Sales"] <- NA

#create one dataset
dfrm <- rbind(dftrain, dftest)

#Add Week Column based on Date
dfrm[,"Week"] <- as.numeric(dfrm$Date)

#add Seasonality 1-52
dfrm <- transform(dfrm, Seasonality = (Week %% 52) + 1 )

#recreate the test dataset with the seasonality
dftest <- dfrm %>%
  filter(is.na(Weekly_Sales)) %>%
  select (Store, Dept, Seasonality, Date)  

#avearage sales per week per dept
avg_sales <- dfrm %>%
  filter(!is.na(Weekly_Sales)) %>%
  group_by(Store, Dept, Seasonality) %>%
  summarise(Weekly_Sales = mean(Weekly_Sales))

#predict based on average sales  
dfrm <- merge(dftest, avg_sales, by = c("Store","Dept", "Seasonality"), all.x = TRUE)

#for depts with no data use average of the data set
dfrm$Weekly_Sales[is.na(dfrm$Weekly_Sales)] <- mean(dfrm$Weekly_Sales,na.rm=TRUE)

#preparing the submission file
#Adding a column with store_department_date
dfrm[,"Id"] <- paste(dfrm$Store, dfrm$Dept, format(as.Date(dfrm$Date), "%Y-%m-%d" ), sep = "_") 

#save results in a csv file
write.csv(file = "Solution1.csv", x = subset(dfrm, select = c(Id, Weekly_Sales)), row.names=FALSE)
