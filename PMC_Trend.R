x<-c("tidyverse")
lapply(x, require, character.only=T)

drive=c("F:\\Research\\AirTrend\\TrendGIS\\Data")
setwd(drive)

load('PM10_Data_20160120.RData') # PM10_AQS
load('PM10_Monitor_20160120.RData') #PM10_Monitor
load('PM25_Data_20160120.RData') #PM25_AQS
load('PM25_Monitor_20160120.RData') #PM25_Monitor

PM10_AQS2 = PM10_AQS %>%
	mutate(FIPS = substr(FIPSPOC, 1, 9)) %>%
	group_by(FIPS, Date) %>%
	summarize(PM10_Value = mean(PM10_Value, na.rm = TRUE)) %>%
	ungroup()

PM10_Monitor2 = PM10_Monitor %>%
	mutate(FIPS = substr(FIPSPOC, 1, 9)) %>%
	group_by(FIPS) %>%
	summarize(PM10.Lat = mean(Latitude, na.rm = TRUE), PM10.Long = mean(Longitude, na.rm = TRUE)) %>%
	ungroup()	

PM25_AQS2 = PM25_AQS %>%
	mutate(FIPS = substr(FIPSPOC, 1, 9)) %>%
	group_by(FIPS, Date) %>%
	summarize(PM25_Value = mean(PM25_Value, na.rm = TRUE)) %>%
	ungroup()

PM25_Monitor2 = PM25_Monitor %>%
	mutate(FIPS = substr(FIPSPOC, 1, 9)) %>%
	group_by(FIPS) %>%
	summarize(PM25.Lat = mean(Latitude, na.rm = TRUE), PM25.Long = mean(Longitude, na.rm = TRUE)) %>%
	ungroup()	

temp = inner_join(PM10_AQS2, PM25_AQS2, by = c('FIPS', 'Date')) %>%
		inner_join(PM10_Monitor2, by = 'FIPS') %>%
		inner_join(PM25_Monitor2, by = 'FIPS') %>%
		mutate( PMC_Value = PM10_Value - PM25_Value) %>%
		filter(PM10_Value < 1000, PM10_Value >0 , PM25_Value < 250, PM25_Value > 0, PMC_Value > 0, PMC_Value < 750) 

First_Date = temp %>%
	arrange(FIPS, Date) %>%
	distinct(FIPS, .keep_all = TRUE) %>%
	select(FIPS, Date) %>%
	rename(First_Date = Date)

Last_Date = temp %>%
	arrange(FIPS, Date) %>%
	group_by(FIPS) %>%
	mutate(Numbering = row_number()) %>%
	ungroup() %>%
	arrange(FIPS, desc(Date)) %>%
	distinct(FIPS, .keep_all = TRUE) %>%	
	select(FIPS, Date, Numbering) %>%
	rename(Last_Date = Date)

FirstLast = inner_join(First_Date, Last_Date, by = c('FIPS')) %>%
	mutate(Length = as.numeric(Last_Date - First_Date)+1, Frequency = Length / Numbering) %>%
	filter(Length > 3650, Frequency < 8) 

df = temp %>%
	filter(FIPS %in% FirstLast$FIPS)

df2 = df %>%
	mutate(PMC_pct = PMC_Value*100 / PM10_Value, PM25_pct = PM25_Value*100 / PM10_Value, County = substr(FIPS, 1, 5), YM = substr(Date, 1, 7)) %>%
	group_by(County, YM) %>%
	summarize(PMC_pct = mean(PMC_pct, na.rm = TRUE), PM25_pct = mean(PM25_pct, na.rm = TRUE)) %>%
	ungroup() %>%
	mutate(Year = substr(YM, 1, 4)) %>%
	group_by(County, Year) %>%
	summarize(PMC_pct = mean(PMC_pct, na.rm = TRUE), PM25_pct = mean(PM25_pct, na.rm = TRUE)) %>%
	ungroup()

group_by(df2, Year) %>%
	summarize(PMC_pct = mean(PMC_pct, na.rm = TRUE), PM25_pct = mean(PM25_pct, na.rm = TRUE)) %>%
	print(n = Inf)

filter(df2, substr(County, 1, 2) == '06') %>%
	group_by(Year) %>%
	summarize(PMC_pct = mean(PMC_pct, na.rm = TRUE), PM25_pct = mean(PM25_pct, na.rm = TRUE)) %>%
	print(n = Inf)
