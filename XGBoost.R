####XGBoost

#Below is the objective function for the XGboost.
RMPSEXGB<- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  lab <- exp(as.numeric(labels))-1
  preds <- exp(as.numeric(preds))-1
  err <- sum(((lab-preds)/lab)^2)/length(lab)
  err <- sqrt(err)
  return(list(metric = "RMPSEXGB", value = err))
}


#Prepare data for XGboost model.
XGData <- dailySales %>% filter(Open != 0)
XGData$Avg <- NULL



#Below is only used when modeling on data corresponding to the same time frame in years 2013 and 2014.
#testDateInfo <- XGData %>% filter(Date >= "2015-06-15") %>%
#select(isoWeek, DayOfWeek) %>%
#distinct()

#XGData <- semi_join(XGData, testDateInfo, 
#by = c("isoWeek" = "isoWeek", 
#"DayOfWeek" = "DayOfWeek"))



#Perform joins betwenn XGData and various tables with engineed features.
XGData <- dplyr::left_join(XGData, salesSchoolHoliday, 
by = c("Store"="Store", "SchoolHoliday" ="SchoolHoliday"))

XGData <- dplyr::left_join(XGData, salesStateHoliday, 
by = c("Store" = "Store", "StateHoliday" = "StateHoliday"))

XGData <- dplyr::left_join(XGData, salesSalesPromo, 
by = c("Store" = "Store","Promo" = "Promo"))

XGData <- dplyr::left_join(XGData, salesAvgDOW, 
by = c("Store" = "Store","DayOfWeek" = "DayOfWeek"))

XGData <- dplyr::left_join(XGData, salesAvgIsoWeek, 
by = c("Store" = "Store","isoWeek" = "isoWeek"))

XGData <- dplyr::left_join(XGData, salesAvgMonth, 
by = c("Store" = "Store", "Month"="Month"))

XGData <- dplyr::left_join(XGData, salesAvgCustomersDOW, 
by = c("Store"="Store","DayOfWeek" = "DayOfWeek"))

XGData <- dplyr::left_join(XGData, HolidayWeekIndicators, 
by=c("Store"="Store","isoWeek" = "isoWeek"))

XGData <- dplyr::left_join(XGData, PromoDayCount,
by = c("Store" = "Store", "Date"="Date"))


#Prepare date related features to be converted to factors and then one hot encoded.
XGData[,c("DayOfWeek",  "Month", "isoYear")] <- lapply(
XGData[,c("DayOfWeek", "Month", "isoYear")], function(x) as.factor(x))

ohe_features <- c("StateHoliday", "StoreType", "Assortment", 
"DayOfWeek", "Month", "isoYear")

#isoWeek removed from above two procedures, but can be added back.

dummies <- dummyVars(~ StateHoliday + StoreType + Assortment +
DayOfWeek +Month+isoYear , data = XGData)

#Date frame with OHE features.
XGDataOHE <- as.data.frame(predict(dummies, newdata = XGData))

#Combine the original data frame(less the columns used for dummy feature creation)
# and the new data frame that includes the dummy feature columns.
XGDataCombined <- cbind(XGData[,-c(which(colnames(XGData) %in% ohe_features))], 
XGDataOHE)

#Log transform Sales related features variable.
XGDataCombined$Sales <- log(XGDataCombined$Sales+1)
XGDataCombined$shopAvgSchoolHoliday <- log(XGDataCombined$shopAvgSchoolHoliday+1)
XGDataCombined$shopAvgStateHoliday <- log(XGDataCombined$shopAvgStateHoliday+1)
XGDataCombined$shopAvgPromo <- log(XGDataCombined$shopAvgPromo+1)
XGDataCombined$shopAvgDOW <- log(XGDataCombined$shopAvgDOW+1)
XGDataCombined$shopAvgISOWeek <- log(XGDataCombined$shopAvgISOWeek+1)
XGDataCombined$shopAvgMonth <- log(XGDataCombined$shopAvgMonth+1)
XGDataCombined$shopAvgDOWCustomer <- log(XGDataCombined$shopAvgDOWCustomer+1)


#Remove features that are no longer required and/or cannot be used in the XGBoost model.
XGDataCombined <- XGDataCombined %>% 
select(-c("Customers","Open","isoWeek","regWeek", "regYear",
"Promo2SinceDate", "CompetitionSinceDate"))

#Create test and training set.
XGDataTest <- XGDataCombined %>% filter(Date >= "2015-06-15")
XGDataTrain <- XGDataCombined %>% filter(Date < "2015-06-15")


#Create labels from test and training set, and remove Store and Date fields.
XGDataTrain.Labels <- XGDataTrain[,c(1,2)]
XGDataTrain <- XGDataTrain[,-c(1,2)]
XGDataTest.Labels <- XGDataTest[,c(1,2)]
XGDataTest <- XGDataTest[,-c(1,2)]

trainTaskXG <- makeRegrTask(data = XGDataTrain, target = "Sales")
testTaskXG <- makeRegrTask(data = XGDataTest, target = "Sales")

#Create a custom measure used by the MLR XGBoost implementation.
fRMPSE <- function(task, model, pred, feats, extra.args) {
labels <- pred$data$truth
lab <- exp(as.numeric(labels))-1
preds <- pred$data$response
preds <- exp(as.numeric(preds))-1
err <- sum(((lab-preds)/lab)^2)/length(lab)
err <- sqrt(err)
return (err)}

mlrRMPSE <- makeMeasure("mlrRMPSE", minimize = TRUE, 
properties = c("regr","req.pred","req.truth"),
fun = fRMPSE)

#Make XGB learner in mlr.
xgb_learner <- makeLearner("regr.xgboost", 
par.vals = list(objective = "reg:linear",
booster = "gbtree",
eval_metric = RMPSEXGB,
max_depth = 10,
eta = 0.01,
subsample = 0.9,
colsample_bytree = 0.7,
print_every_n = 250,
verbose = 1,
nrounds = 3000,
maximize = FALSE,
early_stopping_rounds = 50))



#Train model, predict, and evaluate.
xgModel <- mlr::train(xgb_learner, task = trainTaskXG)
resultXg <- predict(xgModel, task = testTaskXG)
RMSPE(exp(resultXg$data$response)-1, exp(resultXg$data$truth)-1)
Metrics::mape(exp(resultXg$data$truth)-1,exp(resultXg$data$response)-1)
Metrics::mae(exp(resultXg$data$truth)-1,exp(resultXg$data$response)-1)
Metrics::rmse(exp(resultXg$data$truth)-1,exp(resultXg$data$response)-1)



modelSalesPRedict <- data.frame("Sales" = exp(resultXg$data$truth)-1, 
"Predict" = exp(resultXg$data$response)-1)
head(modelSalesPRedict)

xgResult <- cbind(XGDataTest.Labels,
data.frame("xgPredict" = exp(resultXg$data$response)-1))
head(xgResult)
