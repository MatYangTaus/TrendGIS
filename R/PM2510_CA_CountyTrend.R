pacman::p_load(tidyverse, ggtheme, gridExtra, grid, lubridate)

load('K:\\AirData\\OriginalData\\PM25_Data_20160120.RData') #PM25_AQS
load('K:\\AirData\\OriginalData\\PM10_Data_20160120.RData') #PM10_AQS


##PN2.5
temp25 = filter(PM25_AQS,substr(FIPSPOC,1,2)=='06', PM25_Value >= 0) %>% 
     mutate(FIPS = substr(FIPSPOC, 1, 9)) %>% 
     group_by(FIPS, Date) %>% 
     summarize(PM25 = mean(PM25_Value)) %>% 
     ungroup()

summary(temp25$PM25)
quantile(temp25$PM25, c(0.005, 0.01, 0.025, 0.975, 0.99, 0.995))

# Determined to take out > 60 which is top 0.5%
temp2 = filter(temp25, PM25 <= 60)

# Filter infrequent Monitor
FirstLast = temp2 %>%
     mutate(n = sequence(rle(FIPS)$lengths)) %>%
     group_by(FIPS) %>%
     slice(c(1, n())) %>%
     select(-PM25) %>%
     ungroup()
First = filter(FirstLast, n == 1) %>%
     select(-n)
Last = filter(FirstLast, n!=1) 

FirstLast2 = inner_join(First, Last, by = 'FIPS') %>%
     mutate(length = as.numeric(Date.y - Date.x), freq = length/n) %>% 
     filter(length > 5110, freq < 6)
summary(FirstLast2)

### Pick up monitors satisgying above criteria
temp3 = temp2 %>% 
     filter(FIPS %in% unique(FirstLast2$FIPS)) %>% 
     mutate(FIPS_County = substr(FIPS, 1,5)) %>% 
     group_by(FIPS_County, Date) %>% 
     summarize(PM25 = mean(PM25)) %>% 
     ungroup()


## Plot Seasonal trend stratified by 5 years data
CountyList = unique(temp3$FIPS_County)
p2 = list()
for (jj in 1:length(CountyList)){
     p2[[jj]] = temp3 %>% 
          filter(FIPS_County == CountyList[jj], year(Date) > 1998) %>% 
          mutate(MonthDay.num = as.numeric(format(Date, '%j')),
                 CommonDate = as.Date(paste0("2000-", format(Date, '%j')), "%Y-%j"),
                 MonthName = factor(months(Date, abbreviate = TRUE)),
                 Period = cut(year(Date), breaks = c(seq(1999, 2018, 5), 2019), labels = c('1999-2003', '2004-2008', '2009-2013', '2014-2018'), right = FALSE)) %>% 
          ggplot(aes_string(x = "CommonDate", y = "PM25", col = "Period")) +
          #  geom_point() +
          geom_smooth(span=0.3, method='loess', size=1.5, se=FALSE) +
          scale_x_date('Month', date_breaks = "3 month", labels = function(x) format(x, "%b")) +
          coord_cartesian(ylim=c(0, 40)) +
          ggtitle(paste0('Monitor ', CountyList[jj])) +
          theme_minimal() +         
          theme(plot.title = element_text(hjust = 0.5)) 
}

test=do.call(arrangeGrob,c(p2, nrow = 6, ncol = 6))

pdf("C:\\Users\\kebisu\\Downloads\\plots_PM25_CACounty_YearCluster.pdf", onefile = TRUE, 20, 15)
grid.draw(test)
dev.off()


##PM10
temp10 = filter(PM10_AQS,substr(FIPSPOC,1,2)=='06', PM10_Value >= 0) %>% 
     mutate(FIPS = substr(FIPSPOC, 1, 9)) %>% 
     group_by(FIPS, Date) %>% 
     summarize(PM10 = mean(PM10_Value)) %>% 
     ungroup()

summary(temp10$PM10)
quantile(temp10$PM10, c(0.005, 0.01, 0.025, 0.975, 0.99, 0.995))

# Determined to take out > 150  which is top 0.5%
temp2 = filter(temp10, PM10 <= 150, year(Date) > 1998)

# Filter infrequent Monitor
FirstLast = temp2 %>%
     mutate(n = sequence(rle(FIPS)$lengths)) %>%
     group_by(FIPS) %>%
     slice(c(1, n())) %>%
     select(-PM10) %>%
     ungroup()
First = filter(FirstLast, n == 1) %>%
     select(-n)
Last = filter(FirstLast, n!=1) 

FirstLast2 = inner_join(First, Last, by = 'FIPS') %>%
     mutate(length = as.numeric(Date.y - Date.x), freq = length/n) %>% 
     filter(length > 5110, freq < 6)
summary(FirstLast2)

### Pick up monitors satisgying above criteria
temp3 = temp2 %>% 
     filter(FIPS %in% unique(FirstLast2$FIPS)) %>% 
     mutate(FIPS_County = substr(FIPS, 1,5)) %>% 
     group_by(FIPS_County, Date) %>% 
     summarize(PM10 = mean(PM10)) %>% 
     ungroup()

## Plot Seasonal trend stratified by 5 years data
CountyList = unique(temp3$FIPS_County)
p2 = list()
for (jj in 1:length(CountyList)){
     p2[[jj]] = temp3 %>% 
          filter(FIPS_County == CountyList[jj], year(Date) > 1998) %>% 
          mutate(MonthDay.num = as.numeric(format(Date, '%j')),
                 CommonDate = as.Date(paste0("2000-", format(Date, '%j')), "%Y-%j"),
                 MonthName = factor(months(Date, abbreviate = TRUE)),
                 Period = cut(year(Date), breaks = c(seq(1999, 2018, 5), 2019), labels = c('1999-2003', '2004-2008', '2009-2013', '2014-2018'), right = FALSE)) %>% 
          ggplot(aes_string(x = "CommonDate", y = "PM10", col = "Period")) +
               geom_smooth(span=0.3, method='loess', size=1.15, se=FALSE) +
               scale_x_date('Month', date_breaks = "3 month", labels = function(x) format(x, "%b")) +
               coord_cartesian(ylim=c(0, 100)) +
               ggtitle(paste0('Monitor ', CountyList[jj])) +
               theme_minimal() +         
               theme(plot.title = element_text(hjust = 0.5)) 
}

test=do.call(arrangeGrob,c(p2, nrow = 6, ncol = 5))

pdf("C:\\Users\\kebisu\\Downloads\\plots_PM10_CACounty_YearCluster.pdf", onefile = TRUE, 20, 15)
grid.draw(test)
dev.off()

rm(list=ls())
