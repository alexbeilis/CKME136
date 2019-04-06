########Analysis of model predictions.#######

#Disable scientifc notation.
options(scipen=999)

#Join predictions from random forest, XGBoost, base, and the original data.
mlData <- dplyr::left_join(xgResult, rfResult, 
by = c("Store" = "Store", "Date" = "Date")) %>%
left_join(dailyRFData, by = c("Store" = "Store", "Date" = "Date")) %>%
left_join(select(MeanPrediction,Store, Date,AvgSalesPrediction), by = c("Store" = "Store", "Date" = "Date"))


#Create data frame for plotting the actual sales and the predictions 
#from the two models.
plotData <- data.frame("XGBoost_Sales" = mlData$xgPredict,
"Random_Forest_Sales" = mlData$rfPredict, 
"Mean_Predict_Sales" = mlData$AvgSalesPrediction, 
"Actual_Sales" = mlData$Sales)

plotData %>% gather(Model, Sales, Actual_Sales, XGBoost_Sales, 
Random_Forest_Sales, Mean_Predict_Sales) %>%
ggplot(aes(x = Sales, colour = Model)) + geom_density() + 
ggtitle("Sales Distribution")

#Test the distributions for normality.
shapiro.test(sample(mlData$Sales,5000, replace = TRUE))
shapiro.test(sample(mlData$xgPredict,5000, replace = TRUE))
shapiro.test(sample(mlData$rfPredict, 5000,replace = TRUE))


#Statistical test of error in predictions between XGBoost and Random Forest.
wilcox.test(mlData$XGBoost_Error, mlData$Random_Forest_Error,
alternative = "less", paired = TRUE)

#Plot prediction error.
mlData <- mlData %>% mutate(XGBoost_Error = abs(xgPredict-Sales),
Random_Forest_Error = abs(rfPredict-Sales), xg_Error_Per = XGBoost_Error/Sales,
rf_Error_Per = Random_Forest_Error/Sales,
avg_Predict_Error = abs(AvgSalesPrediction-Sales),
avg_Error_Per = avg_Predict_Error/Sales)


mlData %>% 
gather(Model_Error, Error, XGBoost_Error, Random_Forest_Error) %>%
ggplot(aes(x = Error, colour = Model_Error)) + geom_density() + 
ggtitle("Prediction Error Distribution")

#Check which the highest errors
mlData %>% mutate(rfError = abs(as.numeric(rfPredict-Sales))) %>%
arrange(desc(rfError))

mlData %>% arrange(desc(abs(Random_Forest_Error))) %>% head() %>% View()

#Look at store 909
View(mlData %>% filter(Store==909) %>%
arrange(Date))

#Summarize prediction errors.
mlData %>% select(Random_Forest_Error, XGBoost_Error, avg_Predict_Error,
rf_Error_Per, xg_Error_Per, avg_Error_Per) %>%
sapply(summary)



#Plot a few specific stores as time series plots:
gTimePlot <- function(x) {
mlData %>% filter(Store==x) %>%
gather(Type, DailySales, Sales, rfPredict, xgPredict) %>%
ggplot(aes(x=Date, y = DailySales, colour = Type)) +geom_point() + 
xlab("Date") + ylab("Sales") + geom_line() +
ggtitle(paste("Actual Sales vs Predictions - Store#",x,sep = " "))
}

gTimePlot(259)

#Summarize results by week.
weeklyMLData <- mlData %>% group_by(Store, isoWeek) %>%
summarise(SalesWeek = sum(Sales),
rfPredictWeek = sum(rfPredict),
xgPredictWeek = sum(xgPredict),
BaseSalesPredictionWeek = sum(AvgSalesPrediction)) %>%
ungroup()%>%
mutate(Random_Forest_Error_Week = abs(rfPredictWeek - SalesWeek), 
XGBoost_Error_Week = abs(xgPredictWeek - SalesWeek), 
Base_Predict_Error_Week = abs(BaseSalesPredictionWeek - SalesWeek),
rf_Error_Percent_Week = Random_Forest_Error_Week/SalesWeek, 
xg_Error_Percent_Week = XGBoost_Error_Week/SalesWeek,
Base_Error_Percent_Week = Base_Predict_Error_Week/SalesWeek)

#Error measurements on a weekly basis.
RMSPE(weeklyMLData$rfPredictWeek, weeklyMLData$SalesWeek)
RMSPE(weeklyMLData$xgPredictWeek, weeklyMLData$SalesWeek)
RMSPE(weeklyMLData$BaseSalesPredictionWeek, weeklyMLData$SalesWeek)
Metrics::mape(weeklyMLData$SalesWeek, weeklyMLData$xgPredictWeek)
Metrics::mape(weeklyMLData$SalesWeek, weeklyMLData$rfPredictWeek)
Metrics::mape(weeklyMLData$SalesWeek, weeklyMLData$BaseSalesPredictionWeek)
View(weeklyMLData)
