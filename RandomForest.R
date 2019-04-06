##############Random Forest Code#####################################

#Create a table for Random Forest Model from the dailySales table.
#Remove all rows flagged as closed from dailySales table.

dailyRFData <- dailySales %>% filter(Open !=0)


######Prepare the dailyRFData table for Machine Learning.######

#Remove the columns: Date, Customers, regWeek, regYear, Promo2SinceDate, CompetitionSinceDate, and Open.
dailyRFData <- dailyRFData %>% 
select(-c("Customers","regWeek", "regYear",
"Promo2SinceDate", "CompetitionSinceDate", "Open"))


#Join transfer engineered features from other tables to the dailyRFData table.

dailyRFData <- dplyr::left_join(dailyRFData, salesSchoolHoliday, 
by = c("Store"="Store", "SchoolHoliday" = "SchoolHoliday"))

dailyRFData <- dplyr::left_join(dailyRFData, salesStateHoliday, 
by = c("Store" = "Store","StateHoliday" = "StateHoliday"))

dailyRFData <- dplyr::left_join(dailyRFData, salesSalesPromo, 
by = c("Store" = "Store","Promo" = "Promo"))

dailyRFData <- dplyr::left_join(dailyRFData, salesAvgDOW, 
by = c("Store" = "Store","DayOfWeek" = "DayOfWeek"))

dailyRFData <- dplyr::left_join(dailyRFData, salesAvgIsoWeek, 
by = c("Store" = "Store","isoWeek" = "isoWeek"))

dailyRFData <- dplyr::left_join(dailyRFData, salesAvgMonth, 
by = c("Store" = "Store", "Month"="Month"))

dailyRFData <- dplyr::left_join(dailyRFData, salesAvgCustomersDOW, 
by = c("Store"="Store","DayOfWeek" = "DayOfWeek"))

dailyRFData <- dplyr::left_join(dailyRFData, HolidayWeekIndicators, 
by=c("Store"="Store","isoWeek" = "isoWeek"))

dailyRFData <- dplyr::left_join(dailyRFData, PromoDayCount,
by = c("Store" = "Store", "Date"="Date"))



#Prepare seperate data into training and testing sets.
dailyRFData <- dailyRFData %>% arrange(Store, Date)

RFTrain <- dailyRFData %>% filter(Date < as.Date("2015-06-15"))
RFTrainStoreDate <- RFTrain %>% select(c("Date", "Store"))
RFTrain <- RFTrain %>% select(-c("Date", "Store"))
sapply(RFTrain, function (x) sum(is.na(x)))


RFTest <- dailyRFData %>% filter(Date >= as.Date("2015-06-15"))
RFTestStoreDate <- RFTest %>% select(c("Date", "Store"))
RFTest <- RFTest %>% select(-c("Date", "Store"))
sapply(RFTest, function (x) sum(is.na(x)))

#Based on the above, the column shopAvgISOWeek has some missing values due to the 
#stores not having #sales for those specific weeks in prior years. The missing 
#values will be imputed using linear regression.

rfImp = impute(RFTrain, target = "Sales", 
cols = list(shopAvgISOWeek = imputeLearner("regr.lm", 
features = c("isoWeek", "CompetitionDistance",
"StoreType", "Assortment", "Competition"))))

rfImp2 <- reimpute(RFTest, rfImp$desc)
RFTest$shopAvgISOWeek <- rfImp2$shopAvgISOWeek


#Make training task for random forest in MLR package.
rfTrainTask <- makeRegrTask(data = RFTrain, target = "Sales")
rfTestTask <- makeRegrTask(data = RFTest, target = "Sales")


holdOutSample <- sample(x = 1:nrow(RFTrain),size=20000)
caseWeights <- rep(1, nrow(RFTrain))
caseWeights[holdOutSample] <- 0


colExclude <- c("Avg", "shopAvgMonth", "shopAvgISOWeek", "shopAvgDOWCustomer")
colUse <- names(RFTrain)[!names(RFTrain) %in% colExclude]
RFTest$DayOfWeek <- as.factor(RFTest$DayOfWeek)
RFTrain$DayOfWeek <- as.factor(RFTrain$DayOfWeek)
#RFTest$Month <- as.factor(RFTest$Month)
#RFTrain$Month <- as.factor(RFTrain$Month)

rangerSales <-ranger(Sales ~., data = RFTrain[,colUse], 
                     num.trees = 300, 
                     mtry = 4,
                     importance = 'impurity',
                     max.depth = 25,
                     verbose = TRUE,
                     num.threads = 4,
                     sample.fraction = 0.30,
                     holdout = TRUE,
                     case.weights = caseWeights,
                     oob.error = TRUE)

rangerPredict <- predict(rangerSales, RFTest)
RMSPE(rangerPredict$predictions, RFTest$Sales)
Metrics::mape(RFTest$Sales,rangerPredict$predictions)
Metrics::mae(RFTest$Sales,rangerPredict$predictions)
Metrics::rmse(RFTest$Sales,rangerPredict$predictions)


Variable = names(rangerSales$variable.importance)
Amount = unname(rangerSales$variable.importance)

rangerVI <- data.frame(Variable, Amount)
ranger::importance(rangerSales)

rfResult<- cbind(RFTestStoreDate, "rfPredict" = rangerPredict$predictions)


ggplot(rangerVI, aes(x=reorder(Variable,Amount), y=Amount,fill=Amount))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")


#Other used model:
rfLearner <- makeLearner("regr.ranger", 
par.vals = list(importance = 'impurity',
mtry = 5,
num.trees = 3000,
replace = TRUE,
sample.fraction = 0.2,
num.threads = 4))

startTime <- Sys.time()
rfTrainModel2  <- train(rfLearner, task = rfTrainTask)
endTime <- Sys.time()
rfPredict <- predict(rfTrainModel2, task = rfTestTask)
RMSPE(rfPredict$data$response*0.97, rfPredict$data$truth)
paste("Started at:", startTime, "Ended at:", endTime, sep = " ")