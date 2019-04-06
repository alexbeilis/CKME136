#This file contains code for loading of the orginal data, preprocessing, and
#initial minor feature engineering such as adding binary indicators.

#load the daily sales file
dailySales <- read.csv("C:/Users/Alex/Desktop/Ryerson/2019/CKME136/Data/Rossmann/all/train.csv")

#Load the store meta data file.
storeMeta <- read.csv("C:/Users/Alex/Desktop/Ryerson/2019/CKME136/Data/Rossmann/all/store.csv")

#Add date field to indicate date from which competition existed for a particular store based on
#provided CompetitionOpenSinceMonth and CompetitionOpenSinceYear fields. Then remove the 2 fields
# that were used to determine the date.
storeMeta <- storeMeta %>% mutate(CompetitionSinceDate = 
as.Date.character(paste(CompetitionOpenSinceYear,CompetitionOpenSinceMonth,"01", sep = "-"))) %>%
select(-c("CompetitionOpenSinceYear","CompetitionOpenSinceMonth"))

#From the storeMeta table, reate a seperate table with Promo 2 information.
#The below turns a string field that indicates which months a store runs Promo2, into seperate rows
#broken down by Store and Month when Promo2 is active.
promo2Info <- storeMeta %>%
separate(PromoInterval,into = c("FirstMonth", "SecondMonth", "ThirdMonth","FourthMonth")) %>%
select(Store, Promo2, Promo2SinceWeek, Promo2SinceYear, FirstMonth,SecondMonth, ThirdMonth, FourthMonth) %>%
filter(Promo2 != 0) %>%
gather(ApplicableMonth, Month, 5:8 )%>%
select(-5)

#Obtain a list of unique month names from the promo2Info table. This list will be used
#in the next step to attach calendar month numbers to each row based on month name.
promo2Months <- unique(promo2Info$Month)


#Add two columns to promo2Info table. The first column will be an actual date of when
#promo 2 started based on Promo2SinceYear and Promo2SinceWeek. The 2nd column converts
#the calendar month names to month numbers to indicate the months in which Promo2 is active.
promo2Info <- promo2Info %>% 
mutate(Promo2SinceDate = ISOweek2date(paste(Promo2SinceYear,"-W",
formatC(Promo2SinceWeek, width = 2, flag = "0"),"-1", sep = "" )),
Promo2MonthNum = match(Month,promo2Months))

#Check storeMeta table for NA values.
lapply(storeMeta, function(x) sum(is.na(x)))

#Check dailySales table for NAs.
sapply(dailySales, function(x) sum(is.na(x)))

#In dailySales table, convert the Date column from String to an actual date class field.
dailySales$Date <- ymd(dailySales$Date)

#In dailySales, add ISO week column, month column, and ISO year column.
dailySales <- dailySales %>% 
mutate(isoWeek = isoweek(Date), regWeek = week(Date), Month = month(Date), 
isoYear = isoyear(Date),regYear = year(Date))

#In dailySales, check if there are any rows where store is indicated as 
#closed, but Sales amount or customer count exists.
dailySales[which(dailySales$Open == 0 & (dailySales$Sales >0 | 
dailySales$Customers >0)),]

#In dailySales, check if there are any rows where the store is indicated as open, but
#Sales amount or Customers amount is 0.
#Any rows that meet this criteria are removed from the dailySales table.
noSalesOpenRows <- which(dailySales$Open == 1 & (dailySales$Sales <= 0 | dailySales$Customers <=0))
length(noSalesOpenRows)
dailySales <- dailySales[-noSalesOpenRows,]

#Add Promo2 info to dailySales table.
dailySales <-left_join(dailySales,select(promo2Info,Store,Promo2MonthNum,Promo2SinceDate),
by=c("Store"="Store","Month"="Promo2MonthNum"))

#Create Promo2 binary indicator column in dailySales table.
promo2Fun <- function(tranDate, promo2Date) {ifelse((tranDate>=promo2Date),1,0)}

dailySales <- dailySales %>% 
mutate(Promo2 = promo2Fun(Date,Promo2SinceDate))

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

#Create a column to indicate if Competition exists for the store on the particular 
#date of the transaction.
compIndicator <- function(tranDate,compDate) {ifelse((tranDate>=compDate),1,0)}

dailySales <- dailySales %>% mutate(Competition = compIndicator(Date,CompetitionSinceDate))

#Create table with average daily for each store based on data only from the training set.
AvgDailySales <- dailySales %>% filter(Open != 0) %>%
filter(Date < "2015-06-15") %>%
group_by(Store) %>%
summarise(Avg = sum(Sales)/n())

#Add stores daily average sales to dailySales table.
dailySales <- left_join(dailySales,AvgDailySales)

#For rows missing CompetitionDistance and Competition indicator due to no info in storeMeta table,
#replace the NA values with "0" for Competition and max value of CompetitionDistance feature.
dailySales$CompetitionDistance[which(is.na(dailySales$CompetitionDistance))] <- 
max(dailySales$CompetitionDistance, na.rm = TRUE)

dailySales$Competition[which(is.na(dailySales$Competition))] <-0

#Check dailySales table for any NA values.
sapply(dailySales,function(x) sum(is.na(x)))



#####Determining stores that can be treated as comparable and seperating those stores into one table,
#and the stores not considered comparable seperated into a different table to allow for analysis
#on these two store groups.

#Create a table by store calculating the proportion of days the store was 
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
PortionOfYearOpen$`2015` < 0.75) %>%
ungroup()


#Remove stores with incomplete sales from dailySales and store the revised 
#data frame in dailySales2.
dailySales2 <- dailySales[which(!dailySales$Store %in% StoresMissingSales$Store),]

#Filter stores with incomplete sales from dailySales and store in dailySales3.
dailySales3 <- dailySales[which(dailySales$Store %in% StoresMissingSales$Store),]

