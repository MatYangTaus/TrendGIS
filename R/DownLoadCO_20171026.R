x<-c("dplyr","ggplot2","data.table")
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)

######################
############Download AQS Site
######################
CO_AQS=data.frame(matrix(nrow=0,ncol=0))
CO_Monitor=data.frame(matrix(nrow=0,ncol=0))
test2=c(1990:2018)

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

##Take Out Off-mainland
Outside_main=c('02','15','66','72','78','80')
CO_AQS=CO_AQS[!(substr(CO_AQS$FIPSPOC,1,2) %in% Outside_main),]
CO_Monitor=CO_Monitor[!(substr(CO_Monitor$FIPSPOC,1,2) %in% Outside_main),]

CO_AQS=arrange(CO_AQS,FIPSPOC,Date)
CO_Monitor=arrange(CO_Monitor,FIPSPOC)

test=substr(CO_AQS$Date,1,4)
table(test)
rm(test)

test=substr(CO_Monitor$FIPSPOC,1,2)
table(test)
rm(test)

test=substr(CO_Monitor$FIPSPOC,1,5)
table(test)
rm(test)

save(CO_AQS,file="CO_Data_20171026.RData") #Units are in PPM
save(CO_Monitor,file="CO_Monitor_20171026.RData") #Units are in PPM

rm(list=ls())
