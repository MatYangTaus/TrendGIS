x<-c("dplyr","ggplot2","data.table")
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)

######################
############Download AQS Site
######################
SO2_AQS=data.frame(matrix(nrow=0,ncol=0))
SO2_Monitor=data.frame(matrix(nrow=0,ncol=0))
test2=c(1980:2019)

ptm <- proc.time()
for (i in 1:length(test2)){  
	url=paste("https://aqs.epa.gov/aqsweb/airdata/daily_42401_",test2[i],".zip",sep='')
	download.file(url,'temp2.zip')
	temp=read.csv(unz('temp2.zip',paste("daily_42401_",test2[i],".csv",sep='')),header=TRUE)
	names(temp)=c('StateCode','CountyCode','SiteID','Parameter','POC','Latitude','Longitude','Datum','Name','SampleDuration',
		'PollutantStandard','Date','Unit','EventType','ObsCount','ObsPercent','Value','MaxValue','MaxHour','AQI','MethodCode',
		'MethodName','SiteName','Address','StateName','CountyName','CityName','CBSAName','DateChange')
	temp$Value[temp$Unit=='Parts per million']=temp$Value*1000
	temp$Unit=as.character(temp$Unit)
	temp$Unit[temp$Unit=='Parts per million']='Parts per billion'
	temp=filter(temp,ObsCount>=16,Unit=='Parts per billion')
	temp$Date=as.Date(as.character(temp$Date),format="%Y-%m-%d")
	temp2=temp[,c(1:7,12,13,15,17,20,25:27)]
	temp2=filter(temp2,StateCode!='CC')
	temp2$FIPS_C=paste(sprintf("%02d",as.numeric(as.character(temp2$StateCode))),sprintf("%03d",as.numeric(as.character(temp2$CountyCode))),sep='')
	temp2$FIPS_C[temp2$FIPS_C=='12086']='12025'
	temp2$FIPS=paste(temp2$FIPS_C,sprintf("%04d",temp2$SiteID),sep='')
	temp2$FIPSPOC=paste(temp2$FIPS,sprintf("%02d",temp2$POC),sep='')
	
	#Take average by Monitor, POC, and Date
	temp3=aggregate(Value~FIPSPOC+Date,temp2,mean,na.rm=TRUE)
	SO2_AQS=rbind(SO2_AQS,temp3)
	temp4=select(temp2,FIPSPOC,Latitude,Longitude) %>%
		distinct(FIPSPOC, .keep_all = TRUE)
	SO2_Monitor=rbind(SO2_Monitor,temp4)

	rm(url,temp,temp2,temp3,temp4)
}
proc.time() - ptm #This takes about 13min

SO2_AQS=rename(SO2_AQS,SO2_Value=Value)

dim(SO2_AQS)
SO2_Monitor=distinct(SO2_Monitor,FIPSPOC, .keep_all = TRUE)
dim(SO2_Monitor)

##Take Out Off-mainland
Outside_main=c('02','15','66','72','78','80')
SO2_AQS=SO2_AQS[!(substr(SO2_AQS$FIPSPOC,1,2) %in% Outside_main),]
SO2_Monitor=SO2_Monitor[!(substr(SO2_Monitor$FIPSPOC,1,2) %in% Outside_main),]

SO2_AQS=arrange(SO2_AQS,FIPSPOC,Date)
SO2_Monitor=arrange(SO2_Monitor,FIPSPOC)

test=substr(SO2_AQS$Date,1,4)
table(test)
rm(test)

test=substr(SO2_Monitor$FIPSPOC,1,2)
table(test)
rm(test)

test=substr(SO2_Monitor$FIPSPOC,1,5)
table(test)
rm(test)

save(SO2_AQS,file="SO2_Data_20160120.RData") #Units are in PPM
save(SO2_Monitor,file="SO2_Monitor_20160120.RData") #Units are in PPM

rm(list=ls())
