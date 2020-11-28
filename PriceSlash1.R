train <- read.csv('Competitions/Amazon/Dataset/E comm - Train.csv')
test <- read.csv('Competitions/Amazon/Dataset/E comm -Test.csv')

any(is.na(train))

library(dplyr)
library(lubridate)

train$month <- month(as.Date(train$Date,"%Y-%m-%d"))
test$month <- month(as.Date(test$Date,"%Y-%m-%d"))

unique(train$Grade)
unique(train$Product_Category)

#plot X's vs. Y
plot(train$Grade,train$Low_Cap_Price)
plot(train$Product_Category,train$Low_Cap_Price)
plot(train$Demand,train$Low_Cap_Price)
plot(train$State_of_Country,train$Low_Cap_Price)
plot(train$Market_Category,train$Low_Cap_Price)
plot(train$Low_Cap_Price,train$High_Cap_Price) #Highly correlated as expected
plot(train$Date,train$Low_Cap_Price)
plot(train$month,train$Low_Cap_Price)

library(ggplot2)
pl <- print(ggplot(train,aes(Date,Low_Cap_Price))+geom_line(aes(color=Product_Category)))


mod5 <- lm(Low_Cap_Price ~ High_Cap_Price + Market_Category + State_of_Country +Grade * Product_Category + month,data=train)

summary(mod5)

#Assumption checks-
#1.multicollinearity check

library(rms)
vif(mod5)

#2.Normality of errors
hist(mod5$residuals)
qqnorm(mod5$residuals)

#3.Constant variance or homoscedacity check
plot(mod5$fitted.values,mod5$residuals) #Funnel Shape is present i.e. variance is non constant

#Applying remedies for constant variance assumption
mod6 <- lm(formula = log(Low_Cap_Price) ~ log(High_Cap_Price) + Market_Category + State_of_Country + 
             Grade * Product_Category + month, data = train)

summary(mod6)

#Again checking Linear regression assumptions
#1.multicollinearity check

vif(mod6)

#2.Normality of errors
hist(mod6$residuals)
qqnorm(mod6$residuals)

#3.Constant variance or homoscedacity check
plot(mod6$fitted.values,mod6$residuals) #Funnel Shape is not present

predicted.value <- mod6$fitted.values
trans.predicted.value <- exp(predicted.value)
trans.predicted.value <- round(trans.predicted.value,digits = 0)
train.result <- cbind(trans.predicted.value,train$Low_Cap_Price)
colnames(train.result) <- c('train.predicted','train.actual')

train.result <- as.data.frame(train.result)

#Calculating MSE

train.MSE <- mean((train.result$train.actual - train.result$train.predicted)^2)

#Making Predictions based upon mod6
test.predicted <- predict(mod6,test)
test.predicted <- round(exp(test.predicted),digits = 0)


outputDataset <- data.frame("Item_Id" = test$Item_Id,
                            "Low_Cap_Price" = test.predicted)

write.csv(outputDataset,"Competitions/submission.csv",quote = FALSE,row.names = FALSE)

###################################################################################

mod7 <- lm(formula = log(Low_Cap_Price) ~ log(High_Cap_Price) + Market_Category + State_of_Country + 
             Grade * Product_Category + month + Demand, data = train)

summary(mod7)

library(rms)
vif(mod7)

#2.Normality of errors
hist(mod7$residuals)
qqnorm(mod7$residuals)

#3.Constant variance or homoscedacity check
plot(mod7$fitted.values,mod7$residuals) #Funnel Shape is not present

predicted.value <- mod7$fitted.values
trans.predicted.value <- exp(predicted.value)

test.predicted <- predict(mod7,test)
test.predicted <- round(exp(test.predicted),digits = 0)

outputDataset <- data.frame("Item_Id" = test$Item_Id,
                            "Low_Cap_Price" = test.predicted)

write.csv(outputDataset,"Competitions/Result.csv",row.names = FALSE)
