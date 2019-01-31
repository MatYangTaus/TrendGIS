pacman::p_load(tidyverse, ggtheme, gridExtra, grid, lubridate)

load('K:\\AirData\\OriginalData\\PM25_Data_20160120.RData') #PM25_AQS
load('K:\\AirData\\OriginalData\\PM10_Data_20160120.RData') #PM10_AQS


temp25 = filter(PM25_AQS,substr(FIPSPOC,1,2)=='06') %>% 
     mutate(FIPS = substr(FIPSPOC, 1, 9)) %>% 
     group_by(FIPS, Date) %>% 
     summarize(PM25 = mean(PM25_Value)) %>% 
     ungroup()
temp10 = filter(PM10_AQS,substr(FIPSPOC,1,2)=='06') %>% 
     mutate(FIPS = substr(FIPSPOC, 1, 9)) %>% 
     group_by(FIPS, Date) %>% 
     summarize(PM10 = mean(PM10_Value)) %>% 
     ungroup()

temp = inner_join(temp10, temp25, by = c('FIPS', 'Date')) %>% 
     mutate(PMC = PM10 -PM25)

summary(temp$PMC)
quantile(temp$PMC, c(0.005, 0.01, 0.025, 0.975, 0.99, 0.995))

# Determined to take out <-10 and >100 value
temp2 = filter(temp, between(PMC, -10, 100))

# Filter infrequent Monitor
FirstLast = temp2 %>%
     mutate(n = sequence(rle(FIPS)$lengths)) %>%
     group_by(FIPS) %>%
     slice(c(1, n())) %>%
     select(-PM10, -PM25, -PMC) %>%
     ungroup()
First = filter(FirstLast, n == 1) %>%
     select(-n)
Last = filter(FirstLast, n!=1) 

FirstLast2 = inner_join(First, Last, by = 'FIPS') %>%
     mutate(length = as.numeric(Date.y - Date.x), freq = length/n) %>% 
     filter(length > 3650, freq < 8)
summary(FirstLast2)

### Pick up monitors satisgying above criteria
temp3 = temp2 %>% 
     filter(FIPS %in% unique(FirstLast2$FIPS)) %>% 
     mutate(FIPS_County = substr(FIPS, 1,5)) %>% 
     group_by(FIPS_County, Date) %>% 
     summarize(PM10 = mean(PM10), PM25 = mean(PM25), PMC = mean(PMC)) %>% 
     ungroup()

## Plot Each County Long Term Trend
CountyList = unique(temp3$FIPS_County)
#ii = 4
p=list()
for (ii in 1:length(CountyList)){
p[[ii]] = temp3 %>% 
     filter(FIPS_County == CountyList[ii]) %>% 
     ggplot(aes(x= Date, y = PMC)) +
          geom_point() +
          geom_smooth(span=0.1, method='loess', size=2.5, se=FALSE) +
          coord_cartesian(ylim=c(-10, 100)) +
          ggtitle(paste0('Monitor ', CountyList[ii])) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)) 
}

test=do.call(arrangeGrob,c(p, nrow = 6, ncol = 6))

pdf("C:\\Users\\kebisu\\Downloads\\plots_PMC_CACounty.pdf", onefile = TRUE, 20, 15)
grid.draw(test)
dev.off()

## Plot Seasonal trend stratified by 5 years data
#jj = 8
p2 = list()
for (jj in 1:length(CountyList)){
p2[[jj]] = temp3 %>% 
     filter(FIPS_County == CountyList[jj], year(Date) > 1998) %>% 
     mutate(MonthDay.num = as.numeric(format(Date, '%j')),
            CommonDate = as.Date(paste0("2000-", format(Date, '%j')), "%Y-%j"),
            MonthName = factor(months(Date, abbreviate = TRUE)),
            Period = cut(year(Date), breaks = c(seq(1999, 2018, 5), 2019), labels = c('1999-2003', '2004-2008', '2009-2013', '2014-2018'), right = FALSE)) %>% 
     ggplot(aes_string(x = "CommonDate", y = "PMC", col = "Period")) +
        #  geom_point() +
          geom_smooth(span=0.3, method='loess', size=2.5, se=FALSE) +
          scale_x_date('Month', date_breaks = "3 month", labels = function(x) format(x, "%b")) +
          ggtitle(paste0('Monitor ', CountyList[jj])) +
          theme_minimal() +         
          theme(plot.title = element_text(hjust = 0.5)) 
}

test=do.call(arrangeGrob,c(p2, nrow = 6, ncol = 6))

pdf("C:\\Users\\kebisu\\Downloads\\plots_PMC_CACounty_YearCluster.pdf", onefile = TRUE, 20, 15)
grid.draw(test)
dev.off()

rm(list=ls())
