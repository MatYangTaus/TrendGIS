x<-c("dplyr","ggplot2","data.table")
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)


############Download AQS Site
######################
PM25_AQS=data.frame(matrix(nrow=0,ncol=0))
PM25_Monitor=data.frame(matrix(nrow=0,ncol=0))
test2=c(1990:2018)

Outside_main=c('02','15','66','72','78','80')

PM10_Spec_Data=data.frame()
ptm <- proc.time()
for (i in 1:length(test2)){  
	tryCatch({

	url=paste("https://aqs.epa.gov/aqsweb/airdata/daily_PM10SPEC_",test2[i],".zip",sep='')
	download.file(url,'temp2.zip')
	temp=read.csv(unz('temp2.zip',paste("daily_PM10SPEC_",test2[i],".csv",sep='')),header=TRUE) %>%
		filter(State.Code!='CC') %>%
		mutate(State.Code=sprintf("%02d",as.numeric(as.character(State.Code)))) %>%
		mutate(FIPS=paste(sprintf("%02d",as.numeric(as.character(State.Code))),sprintf("%03d",as.numeric(as.character(County.Code))),sprintf("%04d",as.numeric(as.character(Site.Num))),sep=''),
		FIPS_POC=paste(FIPS,sprintf("%02d",as.numeric(as.character(POC))),sep='')) %>%
		filter(!State.Code %in% Outside_main) %>%
		mutate(Date=as.Date(as.character(Date.Local),format="%Y-%m-%d"),ParameterName=as.character(Parameter.Name)) %>%
		select(FIPS_POC,Date,Parameter.Code,ParameterName,Arithmetic.Mean, Longitude, Latitude)

	temp$FIPS_POC[substr(temp$FIPS_POC,1,5)=='12086']=paste('12025',substr(temp$FIPS_POC[substr(temp$FIPS_POC,1,5)=='12086'],6,11),sep='')
	temp=arrange(temp,ParameterName,FIPS_POC,Arithmetic.Mean) %>%
		mutate(willdelete=paste(ParameterName,FIPS_POC,sep=''))
	temp$Numbering=sequence(rle(c(temp$willdelete))$lengths)
	temp=select(temp,-willdelete)

	PM10_Spec_Data=rbind(PM10_Spec_Data, temp)
	rm(temp, url)
	}, error=function(e){})
}
proc.time() - ptm #This takes about 35min

save(PM10_Spec_Data,file="PM10_Species_Data_20180612.RData") #PM10_Spec_Data 

rm(list=ls())