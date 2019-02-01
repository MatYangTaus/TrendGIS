pacman::p_load(tidyverse, ggthemes, gridExtra, grid, lubridate, sf)

#load('F:\\Research\\AirTrend\\TrendGIS\\Data\\PM25_Data_20160120.RData') #PM25_AQS
#load('F:\\Research\\AirTrend\\TrendGIS\\Data\\PM25_Monitor_20160120.RData') #PM25_Monitor
#load('F:\\Research\\AirTrend\\TrendGIS\\Data\\PM10_Data_20160120.RData') #PM10_AQS

load('K:\\AirData\\OriginalData\\PM25_Data_20160120.RData') #PM25_AQS
load('K:\\AirData\\OriginalData\\PM25_Monitor_20160120.RData') #PM25_Monitor
load('K:\\AirData\\OriginalData\\PM10_Data_20160120.RData') #PM10_AQS


# Every day monitorign for PM2.5
temp1 = PM25_AQS %>% 
     arrange(FIPSPOC, Date) %>% 
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

# Monitor Location
Mon.temp1 = FirstLast2 %>% 
     mutate(FIPSPOC = substr(FIPSPOC_Year, 1, 11)) %>% 
     inner_join(PM25_Monitor, by = 'FIPSPOC') %>%
     filter(StateFIPS == '06') %>% 
     arrange(FIPSPOC_Year) 
Mon.temp2 = st_as_sf(Mon.temp1, coords = c("Longitude", "Latitude"), crs = 4326) %>% 
     st_transform(crs = st_crs(102003))

CountyGIS = st_read('K:\\GISData\\CalCountyFile\\gz_2010_us_050_00_5m.shp', stringsAsFactors = FALSE) %>% 
     st_transform(crs = st_crs(102003)) %>% 
     filter(!(STATE %in% c('02', '15', '72'))) %>% 
     filter(STATE == '06') %>% 
     arrange(STATE, COUNTY)

ggplot() +
     geom_sf(data = CountyGIS) +
     geom_sf(data = Mon.temp2, color = "red") +  
     theme_bw()

p = list()
YEARLIST = c(2002:2017)
for (year.i in 1:16){
     temp.year = filter(Mon.temp2, YEAR == YEARLIST[year.i]) 
     Plot.gg.PM25 = ggplot() +
               geom_sf(data = CountyGIS) +
               geom_sf(data = temp.year, size = 2.5, color = "red") +  
               ggtitle(paste0('Monitors in ', YEARLIST[year.i], '(n=', dim(temp.year)[1], ')')) +
               theme_bw() +
               theme(plot.title = element_text(hjust = 0.5)) 
     p[[year.i]] = Plot.gg.PM25
     rm(temp.year)
}     
test=do.call(arrangeGrob,c(p,nrow=4,ncol=4))

pdf("C:\\Users\\kebisu\\Downloads\\plots_pm25daily.pdf", onefile = TRUE, 15, 20)
grid.draw(test)
dev.off()

     
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

# Monitor Location
Mon.temp1 = FirstLast4 %>% 
     mutate(FIPSPOC = substr(FIPSPOC_Year, 1, 11)) %>% 
     inner_join(PM25_Monitor, by = 'FIPSPOC') %>%
     filter(StateFIPS == '06') %>% 
     arrange(FIPSPOC_Year) 
Mon.temp2 = st_as_sf(Mon.temp1, coords = c("Longitude", "Latitude"), crs = 4326) %>% 
     st_transform(crs = st_crs(102003))

CountyGIS = st_read('K:\\GISData\\CalCountyFile\\gz_2010_us_050_00_5m.shp', stringsAsFactors = FALSE) %>% 
     st_transform(crs = st_crs(102003)) %>% 
     filter(!(STATE %in% c('02', '15', '72'))) %>% 
     filter(STATE == '06') %>% 
     arrange(STATE, COUNTY)

ggplot() +
     geom_sf(data = CountyGIS) +
     geom_sf(data = Mon.temp2, color = "red") +  
     theme_bw()

p = list()
YEARLIST = c(1994:2017)
for (year.i in 1:24){
     temp.year = filter(Mon.temp2, YEAR == YEARLIST[year.i]) 
     Plot.gg.PM10 = ggplot() +
          geom_sf(data = CountyGIS) +
          geom_sf(data = temp.year, size = 2.5, color = "red") +  
          ggtitle(paste0('Monitors in ', YEARLIST[year.i], '(n=', dim(temp.year)[1], ')')) +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5)) 
     p[[year.i]] = Plot.gg.PM10
     rm(temp.year)
}     
test=do.call(arrangeGrob,c(p,nrow=5,ncol=5))

pdf("C:\\Users\\kebisu\\Downloads\\plots_pm10daily.pdf", onefile = TRUE, 15, 20)
grid.draw(test)
dev.off()
