
x<-c("dplyr","ggplot2","data.table")
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)

######################
############Download AQS Site
######################

RH_AQS=data.frame()
RH_Monitor=data.frame()
Dew_AQS=data.frame()
Dew_Monitor=data.frame()

test2=c(1990:2015)

ptm <- proc.time()
for (i in 1:length(test2)){  
	url=paste("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_RH_DP_",test2[i],".zip",sep='')
	download.file(url,'temp2.zip')
	temp=read.csv(unz('temp2.zip',paste("daily_RH_DP_",test2[i],".csv",sep='')),header=TRUE)
	names(temp)=c('StateCode','CountyCode','SiteID','Parameter','POC','Latitude','Longitude','Datum','Name','SampleDuration',
		'PollutantStandard','Date','Unit','EventType','ObsCount','ObsPercent','Value','MaxValue','MaxHour','AQI','MethodCode',
		'MethodName','SiteName','Address','StateName','CountyName','CityName','CBSAName','DateChange')
	temp$Unit=as.character(temp$Unit)
	temp$Date=as.Date(as.character(temp$Date),format="%Y-%m-%d")
	temp2=temp[,c(1:7,12,13,15,17,20,25:27)]
	temp2=filter(temp2,StateCode!='CC')
	temp2$FIPS_C=paste(sprintf("%02d",as.numeric(as.character(temp2$StateCode))),sprintf("%03d",as.numeric(as.character(temp2$CountyCode))),sep='')
	temp2$FIPS_C[temp2$FIPS_C=='12086']='12025'
	temp2$FIPS=paste(temp2$FIPS_C,sprintf("%04d",temp2$SiteID),sep='')
	temp2$FIPSPOC=paste(temp2$FIPS,sprintf("%02d",temp2$POC),sep='')


	temp_RH=filter(temp2,Unit=='Percent relative humidity')
	temp2_RH=aggregate(Value~FIPSPOC+Date,temp_RH,mean,na.rm=TRUE)
	RH_AQS=rbind(RH_AQS,temp2_RH)
	temp4=select(temp_RH,FIPSPOC,Latitude,Longitude) %>%
		distinct(FIPSPOC)
	RH_Monitor=rbind(RH_Monitor,temp4)

	temp_Dew=filter(temp2,Unit=='Degrees Fahrenheit')
	temp2_Dew=aggregate(Value~FIPSPOC+Date,temp_Dew,mean,na.rm=TRUE)
	Dew_AQS=rbind(Dew_AQS,temp2_Dew)
	temp5=select(temp_Dew,FIPSPOC,Latitude,Longitude) %>%
		distinct(FIPSPOC)
	Dew_Monitor=rbind(Dew_Monitor,temp5)

	rm(url,temp,temp2,temp_RH,temp2_RH,temp4,temp_Dew,temp2_Dew,temp5)
}
proc.time() - ptm #This takes about 10min

RH_AQS=rename(RH_AQS,RH_Value=Value)
Dew_AQS=rename(Dew_AQS,Dew_Value=Value)

dim(RH_AQS)
RH_Monitor=distinct(RH_Monitor,FIPSPOC)
dim(RH_Monitor)

dim(Dew_AQS)
Dew_Monitor=distinct(Dew_Monitor,FIPSPOC)
dim(Dew_Monitor)

##Take Out Off-mainland
Outside_main=c('02','15','66','72','78','80')
RH_AQS=RH_AQS[!(substr(RH_AQS$FIPSPOC,1,2) %in% Outside_main),]
RH_Monitor=RH_Monitor[!(substr(RH_Monitor$FIPSPOC,1,2) %in% Outside_main),]
Dew_AQS=Dew_AQS[!(substr(Dew_AQS$FIPSPOC,1,2) %in% Outside_main),]
Dew_Monitor=Dew_Monitor[!(substr(Dew_Monitor$FIPSPOC,1,2) %in% Outside_main),]

RH_AQS=arrange(RH_AQS,FIPSPOC,Date)
RH_Monitor=arrange(RH_Monitor,FIPSPOC)
Dew_AQS=arrange(Dew_AQS,FIPSPOC,Date)
Dew_Monitor=arrange(Dew_Monitor,FIPSPOC)

test=substr(RH_AQS$Date,1,4)
table(test)
rm(test)

test=substr(Dew_AQS$Date,1,4)
table(test)
rm(test)

test=substr(RH_Monitor$FIPSPOC,1,2)
table(test)
rm(test)

test=substr(Dew_Monitor$FIPSPOC,1,2)
table(test)
rm(test)

test=substr(RH_Monitor$FIPSPOC,1,5)
table(test)
rm(test)

test=substr(Dew_Monitor$FIPSPOC,1,5)
table(test)
rm(test)

save(RH_AQS,file="RH_Data_20160120.RData") 
save(RH_Monitor,file="RH_Monitor_20160120.RData") 
save(Dew_AQS,file="Dew_Data_20160120.RData") 
save(Dew_Monitor,file="Dew_Monitor_20160120.RData") 
