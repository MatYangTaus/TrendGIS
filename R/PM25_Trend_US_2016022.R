x<-c("dplyr","ggplot2","gridExtra","grid")
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)

load("PM25_Data_20160120.RData") #Units are in PPM

All_PM25=mutate(PM25_AQS,Monitor=substr(FIPSPOC,1,9))
All_PM25$PM25_Value=ifelse(All_PM25$PM25_Value>750,All_PM25$PM25_Value/1000,All_PM25$PM25_Value)
All_PM25_2=aggregate(PM25_Value~Date+Monitor,mean,data=All_PM25)
rm(PM25_AQS)


All_PM25_2$n=sequence(rle(All_PM25_2$Monitor)$lengths)
First_Date=data.frame(All_PM25_2[!duplicated(All_PM25_2$Monitor),c('Monitor','Date')])
names(First_Date)[2]='FirstObsDate'
Last_Date=data.frame(All_PM25_2[!duplicated(All_PM25_2$Monitor,fromLast=TRUE),c('Monitor','Date','n')])
names(Last_Date)[2]='LastObsDate'
Obs_List=merge(First_Date,Last_Date,by='Monitor') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate),Freq=PeriodLength/n) %>%
		filter(n>1000,PeriodLength>1000)

All_PM25_3=filter(All_PM25_2,Monitor %in% Obs_List$Monitor)
All_PM25_4=aggregate(PM25_Value~Date,mean,data=All_PM25_3)
All_PM25_4$MonthDay_num=as.numeric(format(All_PM25_4$Date,'%j'))
All_PM25_4$MonthName=factor(months(All_PM25_4$Date,abbreviate=TRUE))
All_PM25_4$Period=cut(as.numeric(substr(All_PM25_4$Date,1,4)),c(1990,seq(1995,2014,5),2016),
	labels=c('1990-1994','1995-1999','2000-2004','2005-2009','2010-2015'),right=FALSE,include.highest=TRUE)

title=substitute(paste(PM[2.5],' Trend from 1995 to 2015 in US'))
Y_title=substitute(paste(PM[2.5],' Value'))
ylims=c(floor(min(All_PM25_4$PM25_Value)), ceiling(max(All_PM25_4$PM25_Value)))
Legend=data.frame(All_PM25_4[!duplicated(All_PM25_4$MonthName),c('MonthName','MonthDay_num')])

Plot_gg_PM25=ggplot(data=All_PM25_4,aes_string(x="MonthDay_num",y="PM25_Value",col="Period"))+
	geom_smooth(span=0.4,method='loess',size=2.5,se=FALSE)+
	scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
	scale_y_continuous(Y_title)+
	ggtitle(title)+
	#coord_cartesian(ylim=c(0,2.5))+
	theme_bw()
#	theme(panel.background = element_rect(fill = "black"))
Plot_gg_PM25


######################
## Region Plot########
######################

FIPS_list=read.csv('Fips_State.csv') %>%
	mutate(STATE_FIPS=sprintf("%02d",as.numeric(as.character(STATE_FIPS))))	

All_PM25_region=mutate(All_PM25,STATE_FIPS=substr(FIPSPOC,1,2)) %>%
	inner_join(FIPS_list,by='STATE_FIPS')

p=list()

for (i in 1:10){

All_PM25_region2=filter(All_PM25_region,Region==i) 
All_PM25_region3=aggregate(PM25_Value~Date+Monitor,mean,data=All_PM25_region2)

All_PM25_region3$n=sequence(rle(All_PM25_region3$Monitor)$lengths)
First_Date=data.frame(All_PM25_region3[!duplicated(All_PM25_region3$Monitor),c('Monitor','Date')])
names(First_Date)[2]='FirstObsDate'
Last_Date=data.frame(All_PM25_region3[!duplicated(All_PM25_region3$Monitor,fromLast=TRUE),c('Monitor','Date','n')])
names(Last_Date)[2]='LastObsDate'
Obs_List=merge(First_Date,Last_Date,by='Monitor') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate),Freq=PeriodLength/n) %>%
		filter(n>500,PeriodLength>500)

All_PM25_region4=filter(All_PM25_region3,Monitor %in% Obs_List$Monitor)
All_PM25_region5=aggregate(PM25_Value~Date,mean,data=All_PM25_region4)
All_PM25_region5$MonthDay_num=as.numeric(format(All_PM25_region5$Date,'%j'))
All_PM25_region5$MonthName=factor(months(All_PM25_region5$Date,abbreviate=TRUE))
All_PM25_region5$Period=cut(as.numeric(substr(All_PM25_region5$Date,1,4)),c(1990,seq(1995,2014,5),2016),
	labels=c('1990-1994','1995-1999','2000-2004','2005-2009','2010-2015'),right=FALSE,include.highest=TRUE)

title=substitute(paste(PM[2.5],' Trend from 1990 to 2015 in US EPA Region ',nn1),list(nn1=i))
Y_title=substitute(paste(PM[2.5],' Value'))
ylims=c(floor(min(All_PM25_region5$PM25_Value)), ceiling(max(All_PM25_region5$PM25_Value)))
Legend=data.frame(All_PM25_region5[!duplicated(All_PM25_region5$MonthName),c('MonthName','MonthDay_num')])

Plot_gg_PM25=ggplot(data=All_PM25_region5,aes_string(x="MonthDay_num",y="PM25_Value",col="Period"))+
	geom_smooth(span=0.4,method='loess',size=2.5,se=FALSE)+
	scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
	scale_y_continuous(Y_title)+
	ggtitle(title)+
	coord_cartesian(ylim=c(0,30))+
	theme_bw()

p[[i]]=Plot_gg_PM25

}

test=do.call(arrangeGrob,c(p,nrow=3,ncol=4))

pdf("C:\\Users\\kebisu\\Downloads\\plots_pm25.pdf", onefile = TRUE,20,15)
grid.draw(test)
dev.off()


rm(list=ls())
