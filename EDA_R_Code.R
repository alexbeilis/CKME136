#Load necessary packages
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(ISOweek)
library(caret)
library(randomForest)
library(MLmetrics)
library(gridExtra)
set.seed(1)

#load the daily sales file
dailySales <- read.csv("C:/Users/Alex/Desktop/Ryerson/2019/CKME136/Data/Rossmann/all/train.csv")

str(dailySales)

#Load the store meta data file.
storeMeta <- read.csv("C:/Users/Alex/Desktop/Ryerson/2019/CKME136/Data/Rossmann/all/store.csv")

#Add date field to indicate date from which competition existed for a particular store based on
#provided CompetitionOpenSinceMonth and CompetitionOpenSinceYear fields. Then remove the 2 fields
# that were used to determine the date.
storeMeta <- storeMeta %>% mutate(CompetitionSinceDate = 
                                    as.Date.character(paste(CompetitionOpenSinceYear,
                                                            CompetitionOpenSinceMonth,
                                                            "01", sep = "-"))) %>%
                          select(-c("CompetitionOpenSinceYear","CompetitionOpenSinceMonth"))


#Create a seperate table with Promo 2 information.
promo2Info <- storeMeta %>%
              separate(PromoInterval,into = c("FirstMonth", "SecondMonth", "ThirdMonth",
                                              "FourthMonth")) %>%
              select(Store, Promo2, Promo2SinceWeek, Promo2SinceYear, FirstMonth,
                     SecondMonth, ThirdMonth, FourthMonth) %>%
              filter(Promo2 != 0) %>%
              gather(ApplicableMonth, Month, 5:8 )%>%
              select(-5)



#Obtain a list of unique month names from the promo2Info table. This list will be used
#in the next step to attach calendar month numbers to each row based on month name.
promo2Months <- unique(promo2Info$Month)

#Add two columns to promo2Info table. The first column will be an actual date of when
#promo 2 started based on Promo2SinceYear and Promo2SinceWeek. The 2nd column converts
#the calendar month names to month numbers to indicate the months in which Promo2 is active.
promo2Info <- promo2Info %>% mutate(Promo2SinceDate = ISOweek2date(
                            paste(Promo2SinceYear,"-W",
                              formatC(Promo2SinceWeek, width = 2, flag = "0"),
                              "-1", sep = "" )),
                          Promo2MonthNum = match(Month,promo2Months))




#Check the NA values in the storeMeta table.
lapply(storeMeta, function(x) sum(is.na(x)))


#preview the loaded data
head(dailySales)
head(storeMeta)


#Check the dimensions of the dataset.
dim(dailySales)

#Check the class format of each column in the table.
sapply(dailySales, class)



#Check columns for NAs.
sapply(dailySales, function(x) sum(is.na(x)))


#Convert the Date column from String to an actual date class field.
dailySales$Date <- ymd(dailySales$Date)


#Add ISO week column, month column, and ISO year column.
dailySales <- dailySales %>% mutate(isoWeek = isoweek(Date), 
                                    regWeek = week(Date),
                                    Month = month(Date), 
                                    isoYear = isoyear(Date),
                                    regYear = year(Date))


#Get the range of the dates captured in the sales table.
range(dailySales$Date)

#Check if there are any rows where store is indicated as 
#closed, but Sales amount or customer count exists.
dailySales[which(dailySales$Open == 0 & (dailySales$Sales >0 | 
                                     dailySales$Customers >0)),]

#Check if there are any rows where store is indicated as open, but
#Sales amount or Customers amount is 0.
noSalesOpen <- dailySales[which(dailySales$Open == 1 & (dailySales$Sales == 0 |
                                    dailySales$Customers==0 )),]

noSalesOpenRows <- which(dailySales$Open == 1 & (dailySales$Sales <= 0 | dailySales$Customers <=0))


length(noSalesOpenRows)

#Remove all rows flagged Open but no sales or customers from above.
dailySales <- dailySales[-noSalesOpenRows,]


#Add Promo2 info to dailySales table.
dailySales <-left_join(dailySales,select(promo2Info,Store,Promo2MonthNum,Promo2SinceDate),
               by=c("Store"="Store","Month"="Promo2MonthNum"))

promo2Fun <- function(tranDate, promo2Date) {
ifelse((tranDate>=promo2Date),1,0)}

#Create Promo2 indicator column in dailySales table.
dailySales <- dailySales %>% mutate(Promo2 = promo2Fun(Date,Promo2SinceDate))


#Add a 0 to any rows with NA under Promo2 in dailySales table.
dailySales$Promo2[which(is.na(dailySales$Promo2))] <- 0


#Check how many rows in storeMeta indicate competition distance but have no CompetitionSinceDate
nrow(storeMeta[is.na(storeMeta$CompetitionSinceDate) & storeMeta$CompetitionDistance >0,])

#Add the date "2013-01-01" under CompetitionSinceDate feature to all the rows identified above.
storeMeta$CompetitionSinceDate[which(is.na(storeMeta$CompetitionSinceDate) & 
                                       storeMeta$CompetitionDistance >0)] <- as.Date("2013-01-01")

sapply(storeMeta, function(x) sum(is.na(x)))



#Join the non-Promo2 info from storeMeta table to dailySales table.
dailySales <- left_join(dailySales, select(storeMeta,1:4,9), by = c("Store" = "Store"))

#Create a column to indicate if Competition exists for the store on the particular date of the transaction.
compIndicator <- function(tranDate,compDate) {
ifelse((tranDate>=compDate),1,0)  
}

dailySales <- dailySales %>% mutate(Competition = compIndicator(Date,CompetitionSinceDate))



#Create a Min Max scaling function.
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

#Create table with average daily for each store.
AvgDailySales <- dailySales %>% filter(Open != 0) %>%
            filter(Date < "2015-07-01") %>%
            group_by(Store) %>%
            summarise(Avg = sum(Sales)/n())

#Add stores daily average sales to dailySales table.
dailySales <- left_join(dailySales,AvgDailySales)



#For rows missing CompetitionDistance and Competition indicator due to no info in storeMeta table,
# replace the NA values with "0" for Competition and max value of CompetitionDistance feature.
dailySales$CompetitionDistance[which(is.na(dailySales$CompetitionDistance))] <- max(dailySales$CompetitionDistance, na.rm = TRUE)
dailySales$Competition[which(is.na(dailySales$Competition))] <-0

sapply(dailySales,function(x) sum(is.na(x)))
##############Random Forest Code#####################################
#Create a table for Random Forest Model from the dailySales table.
  #Remove all rows flagged as closed from dailySales table.


  dailyRFData <- dailySales %>% filter(Open !=0)

  #Remove the columns: Date, Customers, regWeek, regYear, Promo2SinceDate, CompetitionSinceDate.
  dailyRFData <- dailyRFData %>% select(-c("Customers","regWeek", "regYear",
                                           "Promo2SinceDate", "CompetitionSinceDate"))

  
  RFTest <- dailyRFData %>% filter(Date >= as.Date("2015-07-01")) %>% 
            select(-"Date")

  RFTrain <- dailyRFData %>% filter(Date < as.Date("2015-07-01")) %>%
            select(-"Date")
  
  
  startTime <- Sys.time()
  RFModel <- randomForest(y = RFTrain[,3], x = RFTrain[,c(-1,-3)], mtry = 16, 
              ntree = 40, max_depth = 30, sampsize = 50000, importance = TRUE)
  
  #na.action = na.roughfix
  #sampsize = 10000,
  
  endTime <- Sys.time()
  varImpPlot(RFModel, type = 2)
  
  p1 <- predict(RFModel,RFTest[,-3])
  paste("Started at:", startTime, "Ended at:", endTime, sep = " ")
  RMSPE(p1,RFTest$Sales)
  plot(RFModel)
  
###############################  
  
  

  
  
#Create a chart by store calculating the proportion of days the store was 
#open in a year.Stores with below normal proportion of days open will be eliminated
#from data analysis so that only comparable stores are used across the entire
#time period.

#Calculate the number of calendar days in each ISO year.
Days_2013 <- dailySales %>% filter(isoYear == 2013) %>%
  summarise(n_distinct(Date))


Days_2014 <- dailySales %>% filter(isoYear == 2014) %>%
  summarise(n_distinct(Date))

Days_2015 <- dailySales %>% filter(isoYear == 2015) %>%
  summarise(n_distinct(Date))

#Initially a table is created summarising by year and store, how many days they
# were open based on days where Sales are greater than 0.
PortionOfYearOpen <- dailySales %>% group_by(isoYear, Store) %>%
  summarise(OpenDays = sum(Sales>0)) %>%
  spread(isoYear,OpenDays) 

#The number of days calculated in previous step is converted to a portion of days 
#open in the specific ISO Year.
PortionOfYearOpen$`2013`= PortionOfYearOpen$`2013`/Days_2013$`n_distinct(Date)`
PortionOfYearOpen$`2014` = PortionOfYearOpen$`2014`/Days_2014$`n_distinct(Date)`
PortionOfYearOpen$`2015` = PortionOfYearOpen$`2015`/Days_2015$`n_distinct(Date)`


#Stores are filtered if they were open less than 0.75 of the possible days in any
#one of the operating years.
StoresMissingSales <- PortionOfYearOpen %>% 
  filter(PortionOfYearOpen$`2013` < 0.75 |
         PortionOfYearOpen$`2014` < 0.75 | 
         PortionOfYearOpen$`2015` < 0.75)

#Resulting table from above step.
head(StoresMissingSales)

#Count of stores that will be removed from data analysis due to having uncomprable
#years.

ungroup(StoresMissingSales)
length(StoresMissingSales$Store)


#Remove stores with incomplete sales from dailySales and store the revised 
#data frame in dailySales2.
dailySales2 <- dailySales[which(!dailySales$Store %in% StoresMissingSales$Store),]

#Filter stores with incomplete sales from dailySales and store in dailySales3.
dailySales3 <- dailySales[which(dailySales$Store %in% StoresMissingSales$Store),]






#########################################EDA###############################################

#Plot the time series of the stores with missing data to demonstrate issue.
tsSalesMissing <- dailySales3 %>% group_by(Date) %>%
            summarise(DailySales = sum(Sales))

tsSalesMissing$DailySales %>% ts(frequency = 365.25, start = 2013) %>%
  plot()


#Count the number of stores with sales every year.
dailySales2 %>% filter(Sales > 0) %>%
  group_by(isoYear) %>% 
  summarise(NumOfStores = n_distinct(Store))

#Remove all rows where store was closed.
dailySales2 <- dailySales2[which(dailySales$Open == 1),]

#Create a sales by date table.
salesByDate <- dailySales2 %>% group_by(Date) %>%
  select(Date, Sales) %>% summarise(DailySales = sum(Sales))


#Plot a time series of the daily sales.
dailySalesTS <- salesByDate$DailySales %>% 
  ts(frequency = 365.25, start = 2013)
plot(dailySalesTS)



#Summarize daily average store sales by month.
dailyAvgSalesMonthly <- dailySales2 %>% 
                        filter(Open == 1) %>%
                        group_by(regYear, Month) %>%
                        summarise(Mean_Daily_Sales = sum(Sales)/n())

dailyAvgSalesMonthly <- dailyAvgSalesMonthly %>% spread(Month, Mean_Daily_Sales)
View(dailyAvgSalesMonthly)

#Summarize monthly average store sales by month.
monthlyAvgSales <- dailySales2 %>% 
                    filter(Open == 1) %>%
                    group_by(Month) %>%
                    summarise(Mean_Monthly_Sales = sum(Sales) / n_distinct(Store)/
                                n_distinct(regYear))

View(monthlyAvgSales)
range(monthlyAvgSales$Mean_Monthly_Sales[1:11])

#Month with highest average monthly store sales.
monthlyAvgSales[which.max(monthlyAvgSales$Mean_Monthly_Sales),]

#Month with lowest average monthly store sales.
monthlyAvgSales[which.min(monthlyAvgSales$Mean_Monthly_Sales),]



#Find the average store sales and average spent by customer by day of week.
grid.table(dailySales2 %>% filter(Open == 1) %>%
                group_by(DayOfWeek) %>%
                summarise(Daily_Mean = sum(Sales)/n(),
                          AvgSpent = sum(Sales)/sum(Customers))
                )


#Look at daily average spent on a weekly basis over the entire time period.
YearWeekAvgCustomerSpent <- dailySales2 %>% filter(Open == 1) %>%
                group_by(isoYear, isoWeek) %>%
                summarise(Avg_Spent = (sum(Sales)/sum(Open))/(sum(Customers)
                                                              /sum(Open)))



ggplot(YearWeekAvgCustomerSpent, aes(x = isoWeek, y = Avg_Spent, color = as.factor(isoYear),
                                    group = isoYear )) + 
                                    geom_line() +
                                    geom_point(size = 2, shape=21, fill = "white")

#Create boxplots of store sales by year.

YearlyStoreSales <- dailySales2 %>% filter(Open == 1) %>%
                group_by(isoYear, Store) %>%
                summarise(StoreSales = sum(Sales))

ggplot(YearlyStoreSales[YearlyStoreSales$isoYear < 2015,], aes(x = as.factor(isoYear), y = StoreSales/10000, group = isoYear)) +
  geom_boxplot() + coord_flip()

View(YearlyStoreSales)

x <-YearlyStoreSales[YearlyStoreSales$isoYear == 2013,] 
x[which.max(x$StoreSales),]
YearlyStoreSales[YearlyStoreSales$Store == '262',]


#Complete table for EDA based on RFTable but with non-comparable stores removed.
combinedEDATable <- dailyRFData[which(!dailyRFData$Store %in% StoresMissingSales$Store),]


#Average daily sales by storeType
grid.table(combinedEDATable %>% group_by(StoreType, Store) %>%
                    summarise(AvgbyStore = sum(Sales)/n()) %>%
                    summarise(AvgByType = sum(AvgbyStore)/n(), NumberOfStores = n_distinct(Store)))

#Average daily sales by storeType and assortment type.
grid.table(combinedEDATable %>% group_by(StoreType, Assortment, Store) %>%
             summarise(AvgbyStore = sum(Sales)/n()) %>%
             summarise(AvgByType = sum(AvgbyStore)/n(), NumberOfStores = n_distinct(Store)))

#Check daily average sales based on day of the week with and without Promo. Exclude 2015 Data.
grid.table(combinedEDATable %>% filter(Date < as.Date("2015-01-01")) %>%
                    group_by(Promo, DayOfWeek) %>%
                    summarise(DailyAvgSales = sum(Sales)/n()) %>%
                    spread(Promo, DailyAvgSales))


#Calculate the percentage of operating days that have a Promo in effect.
grid.table(combinedEDATable %>% filter(Date < as.Date("2015-01-01")) %>%
                    count(Promo) %>%
                    mutate(Proportion = n/sum(n)) %>%
                    select(-2))
  
#Check daily average sales based on day of the week with and without Promo2. Exclude 2015 Data.
grid.table(combinedEDATable %>% filter(Date < as.Date("2015-01-01")) %>%
             group_by(Promo2, DayOfWeek) %>%
             summarise(DailyAvgSales = sum(Sales)/n()) %>%
             spread(Promo2, DailyAvgSales))



#Calculate the percentage of operating days that have a Promo2 in effect.
grid.table(combinedEDATable %>% filter(Date < as.Date("2015-01-01")) %>%
             count(Promo2) %>%
             mutate(Proportion = n/sum(n)) %>%
             select(-2))

#Calculate the percentage of operating that have Promo, and Promo2.

grid.table(combinedEDATable %>% filter(Date < as.Date("2015-01-01")) %>%
                    count(Promo,Promo2) %>%
                    mutate(Proportion = n/sum(n)) %>%
                    select(-3) %>%
                    spread(Promo2, Proportion))

grid.table(combinedEDATable %>% group_by(Competition) %>%
                      summarise( AvgDailySales =sum(Sales)/n()))
