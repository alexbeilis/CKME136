####Baseline######
#The previously created training and test tables for the Random Forest model will be re-used in the baseline
#model based on mean for store for day of week.

StoreDailyAverageSales <- RFTrain %>% group_by(Store, DayOfWeek) %>%
          summarise(AvgSales = sum(Sales)/n())

StoreDailyAverageSales<- ungroup(StoreDailyAverageSales)

MeanPrediction <- left_join(RFTest, StoreDailyAverageSales, 
                  by = c("Store" = "Store", "DayOfWeek" = "DayOfWeek" )) %>%
                  select(AvgSales)
RMSPE(MeanPrediction$AvgSales,RFTest$Sales)

