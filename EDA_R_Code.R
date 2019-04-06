#########################################EDA###############################################

#Count of stores that will be removed from data analysis due to having uncomprable
#years.
length(StoresMissingSales$Store)

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


#Count the number of operating days in 2013 and 2014 based on ISO year.
combinedEDATable %>% group_by(isoYear) %>%
                      summarise(Num_Of_Days = n_distinct(Date))

#Create a table with matching days between 2013 and 2014 based on ISO year, ISO week, and day of week.
#Then use that table to filter out matching rows from the combinedEDATable.

dates2013 <- combinedEDATable %>% filter(isoYear == 2013) %>%
              group_by(isoWeek, DayOfWeek) %>%
              summarise()

dates2014 <- combinedEDATable %>% filter(isoYear == 2014) %>%
              group_by(isoWeek, DayOfWeek) %>%
              summarise()

dates20132014 <- dplyr::intersect(dates2013, dates2014) %>% ungroup()

comparable20132014 <- combinedEDATable %>% filter(isoYear == 2013 | isoYear == 2014) %>%
                    dplyr::semi_join(dates20132014, by = c("isoWeek", "DayOfWeek"))

#Create a table to show distribution of sales in 2013 and 2014.

sales20132014 <- comparable20132014 %>% group_by(isoYear, Store) %>%
                      summarise(Sales = sum(Sales)) %>%
                      ungroup() %>%
                      spread(isoYear,Sales)

summary(sales20132014)

#Calculate the IQR for 2013 and 2014 to see the precense of outliers.

IQR2013 <- IQR(sales20132014$`2013`)
Q12013 <- quantile(sales20132014$`2013`, 0.25)
Q32013<- quantile(sales20132014$`2013`, 0.75)

IQR2014 <- IQR(sales20132014$`2014`)
Q12014 <- quantile(sales20132014$`2014`, 0.25)
Q32014<- quantile(sales20132014$`2014`, 0.75)

#Check for outliers that are on the bottom end for 2013.
sum(sales20132014$`2013` < Q12013-(1.5*IQR2013))

#Check for outlers that are on the top end for 2013.
sum(sales20132014$`2013` > Q32013+(1.5*IQR2013))


#Check for outliers that are on the bottom end for 2014.
sum(sales20132014$`2014` < Q12014-(1.5*IQR2014))

#Check for outlers that are on the top end for 2014.
sum(sales20132014$`2014` > Q32014+(1.5*IQR2014))

#Create vectors for 2013 and 2014 that do not have the outliers on the top end.
sales2013NoOutlier <- subset(sales20132014$`2013`, 
                             !sales20132014$`2013` > Q32013+(1.5*IQR2013))


sales2014NoOutlier <- subset(sales20132014$`2014`, 
                      !sales20132014$`2014` > Q32014+(1.5*IQR2014))

t.test(sales2013NoOutlier,sales2014NoOutlier)


set.seed(1)
{salesMean2013 <- vector()
salesMean2014 <- vector()
for (i in 1:5000) {
salesMean2013[i] <- mean(sample(sales2013NoOutlier, 250, replace = FALSE))
salesMean2014[i] <- mean(sample(sales2014NoOutlier, 250, replace = FALSE))
}}

shapiro.test(salesMean2013)
shapiro.test(salesMean2014)


sd(salesMean2013)
sd(salesMean2014)
mean(salesMean2013)
mean(salesMean2014)


salesMean2013<- scale(salesMean2013, center = TRUE, scale = TRUE)
salesMean2014<- scale(salesMean2014, center = TRUE, scale = TRUE)

hist(salesMean2013, probability = TRUE)
hist(salesMean2014, probability = TRUE)


sdp <- function(x) sqrt(mean((x-mean(x))^2))

z.test(salesMean2013, 
       salesMean2014, 
       sigma.x = sdp(salesMean2013), 
       sigma.y = sdp(salesMean2014))

t.test(salesMean2013, 
       salesMean2014, var.equal = FALSE)

t.test(sales20132014$`2013`, sales20132014$`2014`, paired = TRUE)

z.test(salesMean2013[-c(which(salesMean2013 > 3 | salesMean2013 < -3))], 
       salesMean2014[-c(which(salesMean2014 > 3 | salesMean2014 < -3))], 
       sigma.x = sdp(salesMean2013[-c(which(salesMean2013 > 3 | salesMean2013 < -3))]), 
       sigma.y = sdp(salesMean2014[-c(which(salesMean2014 > 3 | salesMean2014 < -3))]),
       conf.level = 0.99)

hist(salesMean2013[-c(which(salesMean2013 > 3 | salesMean2013 < -3))], probability = TRUE)
salesMean2014[-c(which(salesMean2014 > 3 | salesMean2014 < -3))]
sum(salesMean2014 > 3 | salesMean2014 < -3)



#######EDA of Metastore Data#######
#Percentage breakdown of stores based on StoreType and Assortment.
storeBreakdown <- as.data.frame(storeMeta %>% group_by(StoreType, Assortment) %>%
                                  summarise(n = n()) %>%
                                  ungroup() %>%
                                  mutate(Relative_Frequency = n/sum(n)) %>%
                                  select(-3) %>%            
                                  spread(Assortment, Relative_Frequency))

storeBreakdown

rownames(storeBreakdown) <- storeBreakdown$StoreType
storeBreakdown <- storeBreakdown[-1]
storeBreakdown["Total"] <- rowSums(storeBreakdown, na.rm = TRUE)
storeBreakdown

#Percentage breakdown of stores that participate in Promo2.
storeMeta %>% group_by(Promo2) %>%
  summarise(Num_of_Stores = n()) %>%
  mutate(Relative_Frequency = Num_of_Stores/sum(Num_of_Stores))


#Percentage brekdown of stores based on store type that participate in Promo2.
storeMeta %>% group_by(StoreType,Promo2) %>%
  summarise(n = n()) %>%
  mutate(Relative_Frequency = n/sum(n)) %>%
  select(-3) %>%
  spread(StoreType,value = c(Relative_Frequency))

#Percentage breakdown of stores based on store type, and Assortment that participate in 
#Promo2.

storeMeta %>% group_by(StoreType, Assortment, Promo2) %>%
  summarise(n = n()) %>%
  mutate(Relative_Frquency = n/sum(n)) %>%
  select(-4) %>%
  spread(StoreType, Relative_Frquency)