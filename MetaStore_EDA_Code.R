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

