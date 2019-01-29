x<-c("dplyr","ggplot2","gridExtra","grid")
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)

load("PM25_Data_20160120.RData") #Units are in PPM

CA_PM25=filter(PM25_AQS,substr(FIPSPOC,1,2)=='06') 
rm(PM25_AQS)

CA_PM25=CA_PM25 %>%
	mutate(Monitor=substr(FIPSPOC,1,9))
CA_PM25$PM25_Value=ifelse(CA_PM25$PM25_Value>750,CA_PM25$PM25_Value/1000,CA_PM25$PM25_Value)
CA_PM25_2=aggregate(PM25_Value~Date+Monitor,mean,data=CA_PM25)

CA_PM25_2$n=sequence(rle(CA_PM25_2$Monitor)$lengths)
First_Date=data.frame(CA_PM25_2[!duplicated(CA_PM25_2$Monitor),c('Monitor','Date')])
names(First_Date)[2]='FirstObsDate'
Last_Date=data.frame(CA_PM25_2[!duplicated(CA_PM25_2$Monitor,fromLast=TRUE),c('Monitor','Date','n')])
names(Last_Date)[2]='LastObsDate'
Obs_List=merge(First_Date,Last_Date,by='Monitor') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate),Freq=PeriodLength/n) %>%
		filter(n>1000,PeriodLength>1000,FirstObsDate<'1999-12-31',LastObsDate>'2011-01-01')
MonList=unique(Obs_List$Monitor)

p=list()
#i=2
for (i in 1:length(MonList)){
temp=filter(CA_PM25_2,Monitor==MonList[i])
temp$MonthDay_num=as.numeric(format(temp$Date,'%j'))
temp$MonthName=factor(months(temp$Date,abbreviate=TRUE))
temp$Period=cut(as.numeric(substr(temp$Date,1,4)),c(1990,seq(1995,2017,5),2019),
	labels=c('1990-1994','1995-1999','2000-2004','2005-2009','2010-2014', '2015-2018'),right=FALSE,include.highest=TRUE)
temp=filter(temp,Period!='1995-1999')

#title=substitute(paste(PM[2.5],' Trend in ',MonList[i]))
title=substitute(paste(PM[2.5],' Trend in Monitor ',nn1),list(nn1=MonList[i]))
Y_title=substitute(paste(PM[2.5],' Value'))
ylims=c(floor(min(temp$PM25_Value)), ceiling(max(temp$PM25_Value)))
Legend=data.frame(temp[!duplicated(temp$MonthName),c('MonthName','MonthDay_num')])
Legend$MonthDay_num=c(15,46,76,106,136,166,196,226,256,286,316,346)

Plot_gg_PM25=ggplot(data=temp,aes_string(x="MonthDay_num",y="PM25_Value",col="Period"))+
	geom_smooth(span=0.4,method='loess',size=2.5,se=FALSE)+
	scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
	scale_y_continuous(Y_title)+
	ggtitle(title)+
	geom_hline(aes(yintercept=12))+
	coord_cartesian(ylim=c(0,50))+
	theme_bw()

p[[i]]=Plot_gg_PM25
}

test=do.call(arrangeGrob,c(p,nrow=9,ncol=7))

pdf("C:\\Users\\kebisu\\Downloads\\plots_pm25.pdf", onefile = TRUE,20,15)
grid.draw(test)
dev.off()

rm(list=ls())