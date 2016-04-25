
# Experiment 12
#working directory
setwd("c:/mauricio/dropbox/revel/walmart competition")

#load data
dftrain <- read.csv("train.csv", header=TRUE)
dffeatures <- read.csv("features.csv", header=TRUE)
dfstores <- read.csv("stores.csv", header=TRUE)
dftest <- read.csv("test.csv", header=TRUE)

#add Weekly_Sales column to the test dataset
dftest[,"Weekly_Sales"] <- NA

#create one dataset
dftmp1 <- rbind(dftrain, dftest)
dftmp2 <- merge(dftmp1, dfstores, by = "Store")
dfrm <- merge(dftmp2, dffeatures, by = c("Store","Date", "IsHoliday"))

#replace NAs by mean value
dfrm$MarkDown1[is.na(dfrm$MarkDown1)] <- mean(dfrm$MarkDown1,na.rm=TRUE)
dfrm$MarkDown2[is.na(dfrm$MarkDown2)] <- mean(dfrm$MarkDown2,na.rm=TRUE)
dfrm$MarkDown3[is.na(dfrm$MarkDown3)] <- mean(dfrm$MarkDown3,na.rm=TRUE)
dfrm$MarkDown4[is.na(dfrm$MarkDown4)] <- mean(dfrm$MarkDown4,na.rm=TRUE)
dfrm$MarkDown5[is.na(dfrm$MarkDown5)] <- mean(dfrm$MarkDown5,na.rm=TRUE)
dfrm$CPI[is.na(dfrm$CPI)] <- mean(dfrm$CPI,na.rm=TRUE)
dfrm$Unemployment[is.na(dfrm$Unemployment)] <- mean(dfrm$Unemployment,na.rm=TRUE)

#replace Type A/B/C by 1/2/3
dfrm$Type <- as.numeric(dfrm$Type)

#replace IsHoliday T/F by 1/2
dfrm$IsHoliday <- as.numeric(dfrm$IsHoliday) + 1

#Add Week Column based on Date
dfrm[,"Week"] <- as.numeric(dfrm$Date)

#add Seasonality 1-52
dfrm <- transform(dfrm, Seasonality = (Week %% 52) + 1 )

#scale parameters
dfrm$Size <- scale(dfrm$Size)
dfrm$Temperature <- scale(dfrm$Temperature)
dfrm$Fuel_Price <- scale(dfrm$Fuel_Price)
dfrm$MarkDown1 <- scale(dfrm$MarkDown1)
dfrm$MarkDown2 <- scale(dfrm$MarkDown2)
dfrm$MarkDown3 <- scale(dfrm$MarkDown3)
dfrm$MarkDown4 <- scale(dfrm$MarkDown4)
dfrm$MarkDown5 <- scale(dfrm$MarkDown5)
dfrm$CPI <- scale(dfrm$CPI)
dfrm$Unemployment <- scale(dfrm$Unemployment)

# fit a linear model
#MarkDown4 is not relevant
res <- lm( Weekly_Sales ~ Store 
		+ Dept 
		+ Week 
		+ IsHoliday 
		+ Type 
		+ Temperature 
		+ Fuel_Price 
		+ MarkDown1 
		+ MarkDown2 
		+ MarkDown3 
#		+ MarkDown4 
		+ MarkDown5 
		+ CPI 
		+ Unemployment 
		+ Seasonality
		, data = dfrm
		,subset = (Week < 144))
#		,subset = (is.na(Weekly_Sales)))

summary(res)
anova(res)

#predict weekly sales
#res <- predict (res, subset(dfrm, is.na(dfrm$Weekly_Sales)), interval='prediction', level=0.99)
dfrm$Weekly_Sales[is.na(dfrm$Weekly_Sales)] <- predict (res, subset(dfrm, is.na(dfrm$Weekly_Sales)))

#preparing the submission file
#Adding a column with store_department_date
dfrm[,"Id"] <- paste(dfrm$Store, dfrm$Dept, format(as.Date(dfrm$Date), "%Y_%m_%d" ), sep = "_") 

#save results in a csv file
write.csv(file = "Solution.csv", x = subset(dfrm, Week > 143,  select = c(Id, Weekly_Sales)), row.names=FALSE)
