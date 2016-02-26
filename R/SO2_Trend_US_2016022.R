x<-c("dplyr","ggplot2")
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)

load("SO2_Data_20160120.RData") #Units are in PPM

All_SO2=mutate(SO2_AQS,Monitor=substr(FIPSPOC,1,9)) 
All_SO2_2=aggregate(SO2_Value~Date+Monitor,mean,data=All_SO2)
rm(SO2_AQS)

All_SO2_2$n=sequence(rle(All_SO2_2$Monitor)$lengths)
First_Date=data.frame(All_SO2_2[!duplicated(All_SO2_2$Monitor),c('Monitor','Date')])
names(First_Date)[2]='FirstObsDate'
Last_Date=data.frame(All_SO2_2[!duplicated(All_SO2_2$Monitor,fromLast=TRUE),c('Monitor','Date','n')])
names(Last_Date)[2]='LastObsDate'
Obs_List=merge(First_Date,Last_Date,by='Monitor') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate),Freq=PeriodLength/n) %>%
		filter(n>1000,PeriodLength>1000)

All_SO2_3=filter(All_SO2_2,Monitor %in% Obs_List$Monitor)
All_SO2_4=aggregate(SO2_Value~Date,mean,data=All_SO2_3)
All_SO2_4$MonthDay_num=as.numeric(format(All_SO2_4$Date,'%j'))
All_SO2_4$MonthName=factor(months(All_SO2_4$Date,abbreviate=TRUE))
All_SO2_4$Period=cut(as.numeric(substr(All_SO2_4$Date,1,4)),c(1990,seq(1995,2014,5),2016),
	labels=c('1990-1994','1995-1999','2000-2004','2005-2009','2010-2015'),right=FALSE,include.highest=TRUE)

title=substitute(paste(SO[2],' Trend from 1990 to 2015 in US'))
Y_title=substitute(paste(SO[2],' Value'))
ylims=c(floor(min(All_SO2_4$SO2_Value)), ceiling(max(All_SO2_4$SO2_Value)))
Legend=data.frame(All_SO2_4[!duplicated(All_SO2_4$MonthName),c('MonthName','MonthDay_num')])


Plot_gg_SO2=ggplot(data=All_SO2_4,aes_string(x="MonthDay_num",y="SO2_Value",col="Period"))+
	geom_smooth(span=0.4,method='loess',size=2.5,se=FALSE)+
	scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
	scale_y_continuous(Y_title)+
	ggtitle(title)+
	coord_cartesian(ylim=c(0,12.5))+
	theme_bw()
#	theme(panel.background = element_rect(fill = "black"))
Plot_gg_SO2



######################
## Region Plot########
######################

FIPS_list=read.csv('Fips_State.csv') %>%
	mutate(STATE_FIPS=sprintf("%02d",as.numeric(as.character(STATE_FIPS))))	

All_SO2_region=mutate(All_SO2,STATE_FIPS=substr(FIPSPOC,1,2)) %>%
	inner_join(FIPS_list,by='STATE_FIPS')

p=list()

for (i in 1:10){

All_SO2_region2=filter(All_SO2_region,Region==i) 
All_SO2_region3=aggregate(SO2_Value~Date+Monitor,mean,data=All_SO2_region2)

All_SO2_region3$n=sequence(rle(All_SO2_region3$Monitor)$lengths)
First_Date=data.frame(All_SO2_region3[!duplicated(All_SO2_region3$Monitor),c('Monitor','Date')])
names(First_Date)[2]='FirstObsDate'
Last_Date=data.frame(All_SO2_region3[!duplicated(All_SO2_region3$Monitor,fromLast=TRUE),c('Monitor','Date','n')])
names(Last_Date)[2]='LastObsDate'
Obs_List=merge(First_Date,Last_Date,by='Monitor') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate),Freq=PeriodLength/n) %>%
		filter(n>500,PeriodLength>500)

All_SO2_region4=filter(All_SO2_region3,Monitor %in% Obs_List$Monitor)
All_SO2_region5=aggregate(SO2_Value~Date,mean,data=All_SO2_region4)
All_SO2_region5$MonthDay_num=as.numeric(format(All_SO2_region5$Date,'%j'))
All_SO2_region5$MonthName=factor(months(All_SO2_region5$Date,abbreviate=TRUE))
All_SO2_region5$Period=cut(as.numeric(substr(All_SO2_region5$Date,1,4)),c(1990,seq(1995,2014,5),2016),
	labels=c('1990-1994','1995-1999','2000-2004','2005-2009','2010-2015'),right=FALSE,include.highest=TRUE)

title=substitute(paste(SO[2],' Trend from 1990 to 2015 in US EPA Region ',nn1),list(nn1=i))
Y_title=substitute(paste(SO[2],' Value'))
ylims=c(floor(min(All_SO2_region5$SO2_Value)), ceiling(max(All_SO2_region5$SO2_Value)))
Legend=data.frame(All_SO2_region5[!duplicated(All_SO2_region5$MonthName),c('MonthName','MonthDay_num')])

Plot_gg_SO2=ggplot(data=All_SO2_region5,aes_string(x="MonthDay_num",y="SO2_Value",col="Period"))+
	geom_smooth(span=0.4,method='loess',size=2.5,se=FALSE)+
	scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
	scale_y_continuous(Y_title)+
	ggtitle(title)+
	coord_cartesian(ylim=c(0,20))+
	theme_bw()

p[[i]]=Plot_gg_SO2

}

test=do.call(arrangeGrob,c(p,nrow=3,ncol=4))

pdf("C:\\Users\\kebisu\\Downloads\\plots_so2.pdf", onefile = TRUE,20,15)
grid.draw(test)
dev.off()



rm(list=ls())
