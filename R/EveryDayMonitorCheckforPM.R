pacman::p_load(tidyverse, ggtheme, gridExtra, grid, lubridate)

load('K:\\AirData\\OriginalData\\PM25_Data_20160120.RData') #PM25_AQS
load('K:\\AirData\\OriginalData\\PM10_Data_20160120.RData') #PM10_AQS


# Every day monitorign for PM2.5
temp1 = PM25_AQS %>% 
     mutate(YEAR = substr(Date, 1, 4), FIPSPOC_Year = paste0(FIPSPOC, '_',YEAR)) %>% 
     select(-PM25_Value, -FIPSPOC) %>% 
     mutate(n = sequence(rle(FIPSPOC_Year)$lengths)) %>%
     group_by(FIPSPOC_Year) %>%
     slice(c(1, n())) %>%
     ungroup

First = filter(temp1, n == 1) %>%
     select(-n, -YEAR)
Last = filter(temp1, n!=1) 

FirstLast2 = inner_join(First, Last, by = 'FIPSPOC_Year') %>%
     mutate(length = as.numeric(Date.y - Date.x), freq = length/n, StateFIPS = substr(FIPSPOC_Year, 1, 2)) %>% 
     filter(n > 345)

#Check by year
table(FirstLast2$YEAR)
# Year and State
table(FirstLast2$StateFIPS, FirstLast2$YEAR)
#CA Only
temp2 = FirstLast2 %>% 
     filter(StateFIPS == '06') %>% 
     mutate(CountyFIPS = substr(FIPSPOC_Year, 1, 5))
table(temp2$CountyFIPS, temp2$YEAR)

# Every day monitorign for PM10
temp3 = PM10_AQS %>% 
     mutate(YEAR = substr(Date, 1, 4), FIPSPOC_Year = paste0(FIPSPOC, '_',YEAR)) %>% 
     select(-PM10_Value, -FIPSPOC) %>% 
     mutate(n = sequence(rle(FIPSPOC_Year)$lengths)) %>%
     group_by(FIPSPOC_Year) %>%
     slice(c(1, n())) %>%
     ungroup

First = filter(temp3, n == 1) %>%
     select(-n, -YEAR)
Last = filter(temp3, n!=1) 

FirstLast4 = inner_join(First, Last, by = 'FIPSPOC_Year') %>%
     mutate(length = as.numeric(Date.y - Date.x), freq = length/n, StateFIPS = substr(FIPSPOC_Year, 1, 2)) %>% 
     filter(n > 345)

#Check by year
table(FirstLast4$YEAR)
# Year and State
table(FirstLast4$StateFIPS, FirstLast4$YEAR)
#CA Only
temp4 = FirstLast4 %>% 
     filter(StateFIPS == '06') %>% 
     mutate(CountyFIPS = substr(FIPSPOC_Year, 1, 5))
table(temp4$CountyFIPS, temp4$YEAR)

