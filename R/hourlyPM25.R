pacman::p_load(tidyverse, data.table, skimr, gghighlight)

url = "https://aqs.epa.gov/aqsweb/airdata/hourly_88101_2017.zip"
download.file(url,'C:\\Users\\kebisu\\Downloads\\temp2.zip')
temp=read.csv(unz('C:\\Users\\kebisu\\Downloads\\temp2.zip',paste("hourly_88101_2017",".csv",sep='')),header=TRUE)

temp2 = temp %>%
	rename(Value = Sample.Measurement) %>%
	filter(Value >0, !is.na(Value)) %>%
	mutate(MDL.Index = ifelse(Value<MDL, 1, 0), FIPS = paste0(sprintf('%02d', as.numeric(as.character(State.Code))), sprintf('%03d', as.numeric(as.character(County.Code))), sprintf('%04d', as.numeric(as.character(Site.Num)))), FIPSPOC = paste0(FIPS, sprintf('%02d', POC))) %>%
	filter(State.Code == 6) %>%
	select(FIPS, FIPSPOC, Date.Local, Time.Local, Latitude, Longitude, Value, MDL, MDL.Index) %>%
	{.}


temp2 %>%
	group_by(MDL.Index) %>%
	count()

temp2 %>%
	filter(MDL.Index == 1,  MDL == 5) %>%
	mutate(Year = substr(Date.Local, 1, 4)) %>%
	group_by(FIPSPOC) %>%
	count() %>%
	print(n = Inf)

temp3 = temp2 %>%
	group_by(FIPSPOC, MDL.Index) %>%
	count() %>%
	ungroup() %>%
	spread(MDL.Index, n) %>%
	rename(Usual = '0', BelowMDL = '1') %>%
	mutate(BelowMDL = replace(BelowMDL, is.na(BelowMDL), 0), Percent = BelowMDL/(BelowMDL+Usual), Total = BelowMDL+Usual) %>% 
	filter(Percent<0.4, Total >7008) # This is 80% hours of 1 year

temp4 = temp2 %>%
	filter(FIPSPOC %in% temp3$FIPSPOC) %>%
	filter(Value >35)  %>%
#	count(substr(FIPSPOC, 3, 5)) %>%
	count(FIPSPOC) %>%
#	rename(CountyFIPS = 'substr(FIPSPOC, 3, 5)')
	filter(n > 60) %>%
	{.}

temp2 %>%
	filter(FIPSPOC %in% temp4$FIPSPOC) %>%
	group_by(FIPSPOC, Date.Local) %>%
	summarize(PM25mean = mean(Value)) %>%
	group_by(FIPSPOC) %>%
	summarize(PM25 = mean(PM25mean)) %>%
	#group_by(FIPS = substr(FIPSSite, 1, 5)) %>%
	#summarize(PM25 = mean(PM25)) %>%
	#mutate(CountyFIPS = substr(FIPS, 3, 5)) %>%
	left_join(temp4, by = 'FIPSPOC') %>% #filter(n>400)
	ggplot() +
		geom_point(aes(x= PM25, y = n)) +
		#gghighlight_point(n>1000, label_key = type, col = 'red') +
		ggtitle('PM2.5 Annual Mean and Exceeeding Hours') +
		xlab('Annual Mean of Daily PM2.5') +
		ylab('Total Hours Exceeeding 35ug/m3') +
        xlim(0, 20) +
        geom_vline(xintercept = 12, color = "red") +
		theme_minimal() +
		theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(size = 8), axis.text.y = element_text(size = 7))


## Day exceeding 35
url = "https://aqs.epa.gov/aqsweb/airdata/daily_88101_2018.zip"
download.file(url,'C:\\Users\\kebisu\\Downloads\\temp2.zip')
temp=read.csv(unz('C:\\Users\\kebisu\\Downloads\\temp2.zip',paste("daily_88101_2018",".csv",sep='')),header=TRUE)

temp5 = temp %>%
	rename(Value = Arithmetic.Mean) %>%
	filter(Value >0, !is.na(Value), Sample.Duration != '24-HR BLK AVG') %>%
	mutate(FIPS = paste0(sprintf('%02d', as.numeric(as.character(State.Code))), sprintf('%03d', as.numeric(as.character(County.Code))), sprintf('%04d', as.numeric(as.character(Site.Num)))), FIPSPOC = paste0(FIPS, sprintf('%02d', POC))) %>%
	filter(State.Code == 6) %>%
	select(FIPS, FIPSPOC, Date.Local, Latitude, Longitude, Value) %>%
	arrange(FIPSPOC, Date.Local) %>%
	{.}

temp6 = temp5 %>%
	group_by(FIPSPOC) %>%
	count() %>% 
	filter(n > 60)

temp7 = temp5 %>%
	filter(FIPSPOC %in% temp6$FIPSPOC) %>%
	filter(Value >35) %>%
	count(FIPSPOC) 

temp5 %>%
	filter(FIPSPOC %in% temp6$FIPSPOC) %>%
	group_by(FIPSPOC) %>%
	summarize(PM25 = mean(Value)) %>%
	#group_by(FIPSSite = substr(FIPSPOC, 1, 9)) %>%
	#summarize(PM25 = mean(PM25mean)) %>%
	left_join(temp7, by = 'FIPSPOC') %>%
	mutate(n = ifelse(is.na(n), 0, n)) %>%  #filter(n > 20)
	ggplot() +
		geom_point(aes(x= PM25, y = n)) +
		#gghighlight_point(n>1000, label_key = type, col = 'red') +
		ggtitle('PM2.5 Annual Mean and Exceeeding Days') +
		xlab('Annual Mean of Daily PM2.5') +
		ylab('Total Number of Days Exceeeding 35ug/m3') +
        xlim(0, 25) +
        geom_vline(xintercept = 12, color = "red") +
		theme_minimal() +
		theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(size = 8), axis.text.y = element_text(size = 7))

