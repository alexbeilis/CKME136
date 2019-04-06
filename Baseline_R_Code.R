####Baseline######
#The previously created training and test tables for the Random Forest model will be re-used 
#in the baseline model based on mean for store for day of week.

StoreDailyAverageSales <- dailySales %>% filter(Open == 1 &
Date < "2015-06-15") %>% group_by(Store, DayOfWeek) %>%
summarise(AvgSalesPrediction = sum(Sales)/n())

StoreDailyAverageSales<- ungroup(StoreDailyAverageSales)

baseTest <- dailySales %>% filter(Open==1 & Date >= "2015-06-15")

MeanPrediction <- left_join(baseTest, StoreDailyAverageSales, 
                  by = c("Store" = "Store", "DayOfWeek" = "DayOfWeek" ))

RMSPE(MeanPrediction$AvgSalesPrediction,MeanPrediction$Sales)
Metrics::mape(MeanPrediction$Sales, MeanPrediction$AvgSalesPrediction)
Metrics::mae(MeanPrediction$Sales, MeanPrediction$AvgSalesPrediction)
Metrics::rmse(MeanPrediction$Sales, MeanPrediction$AvgSalesPrediction)
