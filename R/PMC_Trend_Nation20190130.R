pacman::p_load(tidyverse, ggtheme, gridExtra, grid, lubridate)

load('K:\\AirData\\OriginalData\\PM25_Data_20160120.RData') #PM25_AQS
load('K:\\AirData\\OriginalData\\PM10_Data_20160120.RData') #PM10_AQS

temp25 = PM25_AQS %>% 
     filter(PM25_Value > 0) %>%  
     mutate(FIPS = substr(FIPSPOC, 1, 9)) %>% 
     group_by(FIPS, Date) %>% 
     summarize(PM25 = mean(PM25_Value)) %>% 
     ungroup()
temp10 = PM10_AQS %>% 
     filter(PM10_Value > 0) %>%       
     mutate(FIPS = substr(FIPSPOC, 1, 9)) %>% 
     group_by(FIPS, Date) %>% 
     summarize(PM10 = mean(PM10_Value)) %>% 
     ungroup()

temp = inner_join(temp10, temp25, by = c('FIPS', 'Date')) %>% 
     mutate(PMC = PM10 -PM25)

summary(temp$PMC)
quantile(temp$PMC, c(0.005, 0.01, 0.025, 0.975, 0.99, 0.995))

# Determined to take out <-5 and >60 value (taking out last 1% tile)
temp2 = filter(temp, between(PMC, -5, 60))

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

### Plot all data 
temp3 = temp2 %>% 
     filter(FIPS %in% unique(FirstLast2$FIPS), year(Date) > 1998) %>% 
     group_by(Date) %>% 
     summarize(PM10 = mean(PM10), PM25 = mean(PM25), PMC = mean(PMC)) %>% 
     ungroup()

ggplot(temp3, aes(x = Date, y = PMC)) +
     geom_point() +
     geom_smooth(span=0.25, method='loess', size=2.5, se=FALSE) +
     ggtitle('PMC Trend using all US Monitors') +
     theme_minimal() +
     theme(plot.title = element_text(hjust = 0.5)) 


## Plot by EPA Region
RegionDef = read.csv('K:\\AirData\\OriginalData\\Fips_State.csv') %>%
     mutate(STATE_FIPS=sprintf("%02d",as.numeric(as.character(STATE_FIPS))))	

temp4 = temp2 %>% 
     filter(FIPS %in% unique(FirstLast2$FIPS), year(Date) > 1998) %>% 
     mutate(STATE_FIPS = substr(FIPS, 1, 2)) %>% 
     left_join(RegionDef, by = 'STATE_FIPS') %>% 
     group_by(Region, Date) %>% 
     summarize(PM10 = mean(PM10), PM25 = mean(PM25), PMC = mean(PMC)) %>% 
     ungroup()

## Plot Each County Long Term Trend
RegionList = unique(temp4$Region)

temp4 %>% 
     group_by(Region) %>% 
     summarize(PM10 = mean(PM10), PM25 = mean(PM25), PMC = mean(PMC))

#ii = 4
p=list()
for (ii in 1:length(RegionList)){
p[[ii]] = temp4 %>% 
     filter(Region == RegionList[ii]) %>% 
     ggplot(aes(x= Date, y = PMC)) +
          geom_point() +
          geom_smooth(span=0.1, method='loess', size=2.5, se=FALSE) +
          coord_cartesian(ylim=c(-10, 70)) +
          ggtitle(paste0('Region ', RegionList[ii])) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)) 
}

test=do.call(arrangeGrob,c(p, nrow = 3, ncol = 4))

pdf("C:\\Users\\kebisu\\Downloads\\plots_PMC_WholeUS.pdf", onefile = TRUE, 20, 15)
grid.draw(test)
dev.off()

## Plot Seasonal trend stratified by 5 years data
#jj = 8
p2 = list()
for (jj in 1:length(RegionList)){
p2[[jj]] = temp4 %>% 
     filter(Region == RegionList[jj]) %>% 
     mutate(MonthDay.num = as.numeric(format(Date, '%j')),
            CommonDate = as.Date(paste0("2000-", format(Date, '%j')), "%Y-%j"),
            MonthName = factor(months(Date, abbreviate = TRUE)),
            Period = cut(year(Date), breaks = c(seq(1999, 2018, 5), 2019), labels = c('1999-2003', '2004-2008', '2009-2013', '2014-2018'), right = FALSE)) %>% 
     ggplot(aes_string(x = "CommonDate", y = "PMC", col = "Period")) +
        #  geom_point() +
          geom_smooth(span=0.3, method='loess', size=2.5, se=FALSE) +
          scale_x_date('Month', date_breaks = "3 month", labels = function(x) format(x, "%b")) +
          coord_cartesian(ylim=c(0, 30)) +
          ggtitle(paste0('PMC Trend in EPA Region ', RegionList[jj])) +
          theme_minimal() +         
          theme(plot.title = element_text(hjust = 0.5)) 
}

test=do.call(arrangeGrob,c(p2, nrow = 3, ncol = 4))

pdf("C:\\Users\\kebisu\\Downloads\\plots_PMC_WholeUS_By5Yr.pdf", onefile = TRUE, 20, 15)
grid.draw(test)
dev.off()

rm(list=ls())
