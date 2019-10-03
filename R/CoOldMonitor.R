x<-c("dplyr","ggplot2","data.table", 'sf')
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)

######################
############Download AQS Site
######################
CO_AQS=data.frame(matrix(nrow=0,ncol=0))
CO_Monitor=data.frame(matrix(nrow=0,ncol=0))
test2=c(1980:2000)

ptm <- proc.time()
for (i in 1:length(test2)){  

	url=paste("https://aqs.epa.gov/aqsweb/airdata/daily_42101_",test2[i],".zip",sep='')
	download.file(url,'temp2.zip')
	temp=read.csv(unz('temp2.zip',paste("daily_42101_",test2[i],".csv",sep='')),header=TRUE)
	names(temp)=c('StateCode','CountyCode','SiteID','Parameter','POC','Latitude','Longitude','Datum','Name','SampleDuration',
		'PollutantStandard','Date','Unit','EventType','ObsCount','ObsPercent','Value','MaxValue','MaxHour','AQI','MethodCode',
		'MethodName','SiteName','Address','StateName','CountyName','CityName','CBSAName','DateChange')
	temp$Value[temp$Unit=='Parts per billion']=temp$Value/1000
	temp$Unit=as.character(temp$Unit)
	temp$Unit[temp$Unit=='Parts per billion']='Parts per million'
	temp=filter(temp,ObsCount>=16,Unit=='Parts per million')
	temp$Date=as.Date(as.character(temp$Date),format="%Y-%m-%d")
	temp2=temp[,c(1:7,12,13,15,17,20,25:27)]
	temp2=filter(temp2,StateCode!='CC')
	temp2$FIPS_C=paste(sprintf("%02d",as.numeric(as.character(temp2$StateCode))),sprintf("%03d",as.numeric(as.character(temp2$CountyCode))),sep='')
	temp2$FIPS_C[temp2$FIPS_C=='12086']='12025'
	temp2$FIPS=paste(temp2$FIPS_C,sprintf("%04d",temp2$SiteID),sep='')
	temp2$FIPSPOC=paste(temp2$FIPS,sprintf("%02d",temp2$POC),sep='')
	
	#Take average by Monitor, POC, and Date
	temp3=aggregate(Value~FIPSPOC+Date,temp2,mean,na.rm=TRUE)
	CO_AQS=rbind(CO_AQS,temp3)
	temp4=select(temp2,FIPSPOC,Latitude,Longitude) %>%
		distinct(FIPSPOC, .keep_all = TRUE)
	CO_Monitor=rbind(CO_Monitor,temp4)

	rm(url,temp,temp2,temp3,temp4)
}
proc.time() - ptm #This takes about 10min
CO_AQS=rename(CO_AQS,CO_Value=Value)

dim(CO_AQS)
CO_Monitor=distinct(CO_Monitor,FIPSPOC, .keep_all = TRUE)
dim(CO_Monitor)

CO_AQS.CA = filter(CO_AQS, substr(FIPSPOC, 1, 2) == '06')
CO_Monitor.CA = filter(CO_Monitor, substr(FIPSPOC, 1, 2) == '06') %>%
	distinct(FIPSPOC, .keep_all = TRUE)

temp = CO_AQS.CA %>%
	filter(CO_Value !=0) %>%
	arrange(FIPSPOC, Date) %>%
	mutate(FIPS = substr(FIPSPOC, 1, 9), YearMonth = substr(Date, 1, 7)) %>%
	group_by(FIPSPOC, YearMonth) %>%
	count() %>%
	ungroup() %>%
	filter(n > 5)

temp2 = temp %>%
	mutate(Year = substr(YearMonth, 1, 4)) %>%
	group_by(FIPSPOC, Year) %>%
	count() %>%
	ungroup() %>%
	filter(n == 12)

# Plot Monitosr which have more than 20 yrs observations
CO = CO_AQS.CA %>%
	filter(FIPSPOC %in% unique(temp2$FIPSPOC)) %>%
	group_by(FIPSPOC) %>%
	summarize(CO = mean(CO_Value, na.rm = TRUE))

allyear = temp2 %>%
	group_by(FIPSPOC) %>%
	count() %>%
	filter(n >= 20) %>%
	inner_join(CO, by = 'FIPSPOC') %>%
	left_join(CO_Monitor, by = 'FIPSPOC') %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)     

CountyGIS = st_read('M:\\Atesgis\\CMAQPM25_ER\\Data\\GISData\\gz_2010_us_050_00_5m.shp', stringsAsFactors = FALSE) %>% 
     st_transform(crs = st_crs(102003)) %>% 
     filter(STATE == '06') %>% 
     arrange(STATE, COUNTY)

ggplot() +
	geom_sf(data = CountyGIS, fill = 'white') +
	geom_sf(data = allyear, aes(color = CO, size = 18)) +
    scale_colour_viridis_c(option = "plasma", direction = -1) +
    theme_minimal() +
    ggtitle(paste0('CO Values in CA from 1980 - 2000 (n = ', dim(allyear)[1], ')')) +
    guides(size = FALSE) +
    theme(plot.title = element_text(hjust = 0.5, size = 24), 
    		axis.text.x = element_blank(), axis.text.y = element_blank(),
    		legend.title = element_text(size = 16), legend.text = element_text(size = 16)) 

# Plot by year
CO.year = CO_AQS.CA %>%
	filter(FIPSPOC %in% unique(temp2$FIPSPOC)) %>%
	mutate(Year = substr(Date, 1, 4)) %>%
	group_by(FIPSPOC, Year) %>%
	summarize(CO = mean(CO_Value, na.rm = TRUE)) %>%
	mutate(Year = as.integer(Year))

eachyear = CO.year %>%
	left_join(CO_Monitor, by = 'FIPSPOC') %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)    

pp = ggplot() +
	geom_sf(data = CountyGIS, fill = 'white') +
	geom_sf(data = eachyear, aes(color = CO, size = 18)) +
    scale_colour_viridis_c(option = "plasma", direction = -1) +
#    transition_time(Year ) +
#	transition_states(Year) +
    transition_states(Year , transition_length = 0, state_length = 6, wrap = FALSE ) +
    ease_aes('linear') +
    shadow_mark() +
    theme_minimal() +
    ggtitle(paste0('CO Values in CA from 1980 - 2000 (n = ', dim(allyear)[1], ')')) +
    #labs(title = 'Year: {frame_time}') +
    guides(size = FALSE) +
    theme(plot.title = element_text(hjust = 0.5, size = 24), 
    		axis.text.x = element_blank(), axis.text.y = element_blank(),
    		legend.title = element_text(size = 16), legend.text = element_text(size = 16)) +
    {}

animate(pp)

anim_save(pp, file = 'C:\\Users\\kebisu\\Downloads\\test33.gif')

Arguments
rm(list=ls())
