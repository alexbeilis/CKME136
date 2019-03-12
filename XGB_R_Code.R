library(xgboost)
library(readr)
library(stringr)
library(caret)
library(dplyr)
library(tidyr)
library(Matrix)
library(methods)
library(MLmetrics)
library(gridExtra)
####XGBoost

XGData <- dailyRFData
str(XGData)
XGData[,c("DayOfWeek", "isoWeek", "Month", "isoYear")] <- lapply(XGData[,c("DayOfWeek", "isoWeek", "Month", "isoYear")], function(x) as.factor(x))
ohe_features <- c("StateHoliday", "StoreType", "Assortment", "DayOfWeek", "isoWeek", "Month", "isoYear")
dummies <- dummyVars(~ StateHoliday + StoreType + Assortment + DayOfWeek +isoWeek+Month+isoYear , data = XGData)
XGDataOHE <- as.data.frame(predict(dummies, newdata = XGData))
XGDataCombined <- cbind(XGData[,-c(which(colnames(XGData) %in% ohe_features))], XGDataOHE)

XGDataTest <- XGDataCombined %>% filter(Date >= "2015-07-01")
XGDataTrain <- XGDataCombined %>% filter(Date < "2015-07-01")
XGDataTrain.Labels <- XGDataTrain$Store
XGDataTrain <- XGDataTrain[,-(1:2)]
XGDataTest.Labels <- XGDataTest$Store
XGDataTest <- XGDataTest[,-(1:2)]

train <- as.matrix(XGDataTrain, rownames.force = NA)
test <- as.matrix(XGDataTest, rownames.force = NA)
train <- as(train, "sparseMatrix")
test <- as(test,"sparseMatrix")

train_Data <- xgb.DMatrix(data = train[,-1], label = train[,1])
test_Data <- xgb.DMatrix(data = test[,-1], label = test[,1])
cv.ctrl <-trainControl(method = "repeatedcv", repeats = 1, number = 3)

xgb.grid <- expand.grid(nrounds = 500,
                        max_depth = seq(6,10),
                        eta = c(0.01,0.3, 1),
                        gamma = c(0.0, 0.2, 1),
                        colsample_bytree = c(0.5,0.8, 1),
                        min_child_weight=seq(1,10)
)

xgb_tune <-train(Sales ~.,
                 data=XGDataTrain,
                 method="xgbTree",
                 metric = "RMSE",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid
)

param <- list(objective = "reg:linear",
              evalmetric = 'rmse', max_depth =6, eta = 0.3, subsample = 0.5, colsample_bytree = 1,
              verbose = 1)

watchlist <- list(eval = test_Data)

xgb.model <- xgb.train(param, data = train_Data, watchlist, nrounds = 2500, maximize = FALSE, earlystoppingrounds = 5, printevery_n = 100)

xgb.model1 <- xgb.model
XGPredict <- predict(xgb.model1, test_Data)
RMSPE(XGPredict,XGDataTest$Sales)
xgb.model1$callbacks
importance_matrix <- xgb.importance(names(XGDataTrain), model = xgb.model1)
