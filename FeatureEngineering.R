trainDataStats <- dailySales %>% filter(Date < "2015-06-15" & Open == 1)
nrow(trainDataStats)
range(trainDataStats$Date)


salesSchoolHoliday <- trainDataStats %>% group_by(Store, SchoolHoliday) %>%
                    summarise(shopAvgSchoolHoliday = mean(Sales)) %>%
                      ungroup()



salesStateHoliday <- trainDataStats %>% 
                    group_by(Store, StateHoliday) %>%
                    summarise(shopAvgStateHoliday = mean(Sales)) %>%
                    ungroup()
                                
                              
salesSalesPromo <- trainDataStats %>% 
                  group_by(Store, Promo) %>%
                    summarise(shopAvgPromo = mean(Sales))%>%
                    ungroup()
      

salesSaturday <- trainDataStats %>%
                  group_by(Store) %>%
                  filter(DayOfWeek == 6) %>%
                summarise(shopAvgSaturday = mean(Sales))%>%
                ungroup()


salesAvgDOW <- trainDataStats %>%
                group_by(Store, DayOfWeek) %>%
                summarise(shopAvgDOW = mean(Sales))%>%
                ungroup()
          

salesAvgIsoWeek <- trainDataStats %>% 
                    group_by(Store, isoWeek) %>%
                    summarise(shopAvgISOWeek = mean(Sales)) %>%
                    ungroup()


salesAvgMonth <- trainDataStats %>%
                group_by(Store, Month) %>%
                summarise(shopAvgMonth = mean(Sales)) %>%
              ungroup()

salesAvgCustomersDOW <- trainDataStats %>%
                        group_by(Store, DayOfWeek) %>%
                        summarise(shopAvgDOWCustomer = mean(Customers)) %>%
                        ungroup()


          
          
HolidayWeekIndicators <- dailySales %>% arrange(Store,Date) %>%
            mutate(Holiday = ifelse(StateHoliday != '0',1,0)) %>%
            group_by(Store, isoWeek) %>%
            summarise(HolidayThisWeek = ifelse(sum(Holiday)>0,1,0)) %>%
            mutate(HolidayNextWeek = ifelse(!is.na(lead(HolidayThisWeek,1)) & 
                                            lead(HolidayThisWeek,1)==1,1,0),
                   HolidayLastWeek = ifelse(!is.na(lag(HolidayThisWeek,1)) &
                                              lag(HolidayThisWeek,1)==1,1,0))
            



PromoDayCount<- dailySales %>% arrange(Store,Date) %>%
            group_by(Store, grp = cumsum(is.na(lag(Promo,1)) | Promo != lag(Promo, 1)))  %>%
            mutate(DaysInPromo = cumsum(Promo), DaysAfterPromo = cumsum(Promo == 0)) %>%
            ungroup() %>%
            select(c("Store", "Date", "DaysInPromo", "DaysAfterPromo"))



