x<-c("dplyr","ggplot2","data.table","reshape2","foreign")
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)

load("PM25_Data_20160120.RData") #PM25_AQS  Units are in PPM 
load("PM25_Monitor_20160120.RData") #PM25_Monitor Units are in PPM

CA_Data=filter(PM25_AQS,substr(FIPSPOC,1,2)=='06',as.numeric(substr(Date,1,4))>=2001)


### Check Frequency of the monitor
CA_Data$n=sequence(rle(CA_Data$FIPSPOC)$lengths)
First_Date=data.frame(CA_Data[!duplicated(CA_Data$FIPSPOC),c('FIPSPOC','Date')])
names(First_Date)[2]='FirstObsDate'
Last_Date=data.frame(CA_Data[!duplicated(CA_Data$FIPSPOC,fromLast=TRUE),c('FIPSPOC','Date','n')])
names(Last_Date)[2]='LastObsDate'
Obs_List=merge(First_Date,Last_Date,by='FIPSPOC') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate),Freq=PeriodLength/n) %>%
		filter(Freq<=6.,PeriodLength>3650,as.numeric(substr(FirstObsDate,1,4))<=2003,as.numeric(substr(LastObsDate,1,4))>=2014)

CA_Data2=filter(CA_Data,FIPSPOC %in% Obs_List$FIPSPOC) %>%
	mutate(year=paste('Year',substr(Date,1,4),sep=''))

Trend=group_by(CA_Data2,FIPSPOC,year) %>%
	summarize(PM25=mean(PM25_Value)) %>%
	data.frame()
Trend2=dcast(Trend,FIPSPOC~year,value.var="PM25") %>%
	filter(!is.na(Year2011))
Trend2
Trend3=inner_join(Trend2,PM25_Monitor,by='FIPSPOC')
write.dbf(Trend3,file='K:\\AirData\\EPAFusedData\\CAPM25Trend.dbf')

CA_Data3=filter(CA_Data2,FIPSPOC %in% Trend2$FIPSPOC)  %>%
	select(-year)
CA_Data3$Date=as.POSIXct(CA_Data3$Date)

Mon_List=unique(CA_Data3$FIPSPOC)
i=27
CA_Data4=filter(CA_Data3,FIPSPOC==Mon_List[i])

ggplot(CA_Data4, aes(x=Date, y=PM25_Value)) + 
	geom_point()
