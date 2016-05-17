
#========================================================================
#Data set load and prep

#working directory
setwd("C:/mauricio/Dropbox/vagrant/apps/Kaggle---Walmart-forecast-challenge")

#load data
dftrain <- read.csv("train.csv", header=TRUE)
dffeatures <- read.csv("features.csv", header=TRUE)
dfstores <- read.csv("stores.csv", header=TRUE)
dftmp2 <- merge(dftrain, dfstores, by = "Store")
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

#========================================================================
#basic linear model statistics
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
		+ MarkDown4 
		+ MarkDown5 
		+ CPI 
		+ Unemployment 
		+ Seasonality
		, data = dfrm)

summary(res)
anova(res)
#MarkDown4 not relevant
#High correlation of other variables (p-value < 0.05)
#Low quality of model (R-square VERY low)

#========================================================================
#selection of regression variables backward
resb <- lm( Weekly_Sales ~ Store 
		+ Dept 
		+ Week 
		+ IsHoliday 
		+ Type 
		+ Temperature 
		+ Fuel_Price 
		+ MarkDown1 
		+ MarkDown2 
		+ MarkDown3 
		+ MarkDown4 
		+ MarkDown5 
		+ CPI 
		+ Unemployment 
		+ Seasonality
		, data = dfrm)
reduced <- step(resb, direction = "backward")
summary(reduced)
anova(reduced)
#MarkDown4 not relevant

#========================================================================
#selection of regression variables forward
res1 <- lm( Weekly_Sales ~ 1, data = dfrm)
fwd <- step(res1, direction = "forward", scope = (~ Store 
		+ Dept 
		+ Week 
		+ IsHoliday 
		+ Type 
		+ Temperature 
		+ Fuel_Price 
		+ MarkDown1 
		+ MarkDown2 
		+ MarkDown3 
		+ MarkDown4 
		+ MarkDown5 
		+ CPI 
		+ Unemployment 
		+ Seasonality))
summary(fwd)
anova(fwd)
#MarkDown4, Week and Fuel_Price are not relevant

#========================================================================
library(MASS)
bc <- boxcox(res)

#========================================================================
#graphics analysis
#plot(dfrm$MarkDown1, dfrm$Weekly_Sales)
#abline (res)

#========================================================================
#predict weekly sales
#res <- predict (res, subset(dfrm, is.na(dfrm$Weekly_Sales)), interval='prediction', level=0.99)
#dfrm$Weekly_Sales[is.na(dfrm$Weekly_Sales)] <- predict (res, subset(dfrm, is.na(dfrm$Weekly_Sales)))

#save results in a csv file
#write.csv(file = "tmp.csv", x = dfrm)