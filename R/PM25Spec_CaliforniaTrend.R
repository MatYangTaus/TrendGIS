x<-c("tidyverse", "reshape2")
lapply(x, require, character.only=T)

drive=c("F:\\Research\\AirTrend\\TrendGIS\\Data")
setwd(drive)

load("PM25_Species_Data_20160212.RData") #PM25_Spec_Data

PolName = PM25_Spec_Data %>%
	select(Parameter.Code, ParameterName) %>%
	distinct(Parameter.Code, ParameterName, .keep_all = TRUE)
ExcludeList = c(68108, 68105, 68101, 68102, 68112, 68113, 68111, 68115, 68116, 68114)

temp = PM25_Spec_Data %>%
	filter(Arithmetic.Mean!=0, !(Parameter.Code %in% ExcludeList)) %>%
	mutate(FIPS_Site = substr(FIPS_POC, 1, 9)) %>%
	group_by(FIPS_Site,  Parameter.Code, Date) %>%
	summarize(Value = mean(Arithmetic.Mean)) %>%
	ungroup() %>%
	arrange(FIPS_Site, Parameter.Code, Date) %>%
	group_by(FIPS_Site, Parameter.Code) %>%
	mutate(Numbering = row_number()) %>%
	ungroup() %>%
	arrange(FIPS_Site, Parameter.Code, Date) 

First_Date = temp %>%
	arrange(FIPS_Site, Parameter.Code, Date) %>%
	distinct(FIPS_Site, Parameter.Code, .keep_all = TRUE) %>%
	select(-c(Value, Numbering)) %>%
	rename(First_Date = Date)

Last_Date = temp %>%
	arrange(FIPS_Site, Parameter.Code, desc(Date)) %>%
	distinct(FIPS_Site, Parameter.Code, .keep_all = TRUE) %>%	
	select(-c(Value)) %>%
	rename(Last_Date = Date)

FirstLast = inner_join(First_Date, Last_Date, by = c('FIPS_Site', 'Parameter.Code')) %>%
	mutate(Length = as.numeric(Last_Date - First_Date)+1, Frequency = Length / Numbering) %>%
	filter(Length > 1, Frequency <8) 

filter(FirstLast, substr(FIPS_Site, 1, 2) == '06') %>%
	filter(Frequency < 3.9) %>%
	count(Parameter.Code) %>%
	print(n = Inf)


PolName2 = distinct(PolName, Parameter.Code, .keep_all = TRUE) %>%
	arrange(Parameter.Code) %>%
	mutate(PolName2 = substr(ParameterName, 1, 4)) %>%
	mutate(PolName2 = ifelse(ParameterName == 'Sulfur PM2.5 LC', 'Sulfur', 
						ifelse(ParameterName == 'Total Nitrate PM2.5 LC', 'Nitrate_Total',
						ifelse(ParameterName == 'EC CSN PM2.5 LC TOT', 'EC_CSN_LC',
						ifelse(ParameterName == 'Carbonate Carbon CSN PM2.5 LC TOT', 'CarbonateCarbon',	
						ifelse(ParameterName == 'Volatile Nitrate PM2.5 LC', 'Nitrate_Volatile',	
						ifelse(ParameterName == 'Non-volatile Nitrate PM2.5 LC', 'Nitrate_NonVolatile',	
						ifelse(ParameterName == 'Total Carbon PM2.5 LC TOT', 'TotalCarbon',	
						ifelse(ParameterName == 'Black Carbon PM2.5 at 880 nm', 'BlackCarbon',	
						ifelse(ParameterName == 'UV Carbon PM2.5 at 370 nm', 'UVCarbon',	
						ifelse(ParameterName == 'OC PM2.5 LC TOR', 'OC_LC_TOR',	
						ifelse(ParameterName == 'EC PM2.5 LC TOR', 'EC_LC_TOR',	
						ifelse(ParameterName == 'EC PM2.5 LC TOT', 'EC_LC_TOT',	
						ifelse(ParameterName == 'OC PM2.5 LC TOT', 'OC_LC_TOT',	
						ifelse(ParameterName == 'Sulfate PM2.5 LC', 'Sulfate',	
						ifelse(ParameterName == 'Tin PM2.5 LC', 'Tin',	
						ifelse(ParameterName == 'Potassium Ion PM2.5 LC', 'Potassium_Ion',	
						ifelse(ParameterName == 'Potassium PM2.5 LC', 'Potassium',
						ifelse(ParameterName == 'Sodium PM2.5 LC', 'Sodium',	
						ifelse(ParameterName == 'Sodium Ion Pm2.5 LC', 'Sodium_Ion',							
						ifelse(ParameterName == 'Chlorine PM2.5 LC', 'Chlorine',
						ifelse(ParameterName == 'Chloride PM2.5 LC', 'Chloride',	
						ifelse(ParameterName == 'Chromium PM2.5 LC', 'Chromium',						
								PolName2)))))))))))))))))))))))

## Select California Data
CA_temp = PM25_Spec_Data %>% 
	filter(!(Parameter.Code %in% ExcludeList), substr(FIPS_POC, 1, 2) == '06', 
		Arithmetic.Mean != 0, substr(Parameter.Code, 1, 2) != '68') %>%
	mutate(FIPS_Site = substr(FIPS_POC, 1, 9)) %>%
	group_by(FIPS_Site, Parameter.Code, Date) %>%
	summarize(Value = mean(Arithmetic.Mean)) %>%
	ungroup() %>%
	arrange(FIPS_Site, Parameter.Code, Date) %>%
#	group_by(FIPS_Site, Parameter.Code) %>%
#	mutate(Numbering = row_number()) %>%
#	ungroup() %>%
#	arrange(FIPS_Site, Parameter.Code) %>%
	inner_join(PolName2, by = 'Parameter.Code') %>%
	select(FIPS_Site, Date, PolName2, Value)

CA_temp2 = dcast(FIPS_Site + Date ~ PolName2, value.var = 'Value') %>%
	mutate(Year = year(Date))

table(CA_temp2$FIPS_Site, CA_temp2$Year)

MonLoc = PM25_Spec_Data %>%
	distinct(FIPS_POC, .keep_all = TRUE) %>%
	select(FIPS_POC, Latitude, Longitude) %>%
	mutate(FIPS_Site = substr(FIPS_POC, 1, 9)) %>%
	group_by(FIPS_Site) %>%
	summarize(Lat = mean(Latitude), Long = mean(Longitude)) %>%
	ungroup() %>%
	filter(substr(FIPS_Site, 1, 2) == '06')

CA_temp3 = CA_temp %>%
	mutate(Year = year(Date)) %>%
	filter(Year >= 2002) %>%
	group_by(FIPS_Site,PolName2, Year) %>%
	summarize(Value = mean(Value, na.rm = TRUE)) %>%
	ungroup() %>%
	inner_join(MonLoc, by = 'FIPS_Site')
