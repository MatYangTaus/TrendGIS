#Note this trend plot is made by PMCoarse data: PM10 and PM2.5 Trend might be a bit diffrent if whole data are used

x<-c('ggplot2','dplyr','gridExtra')
lapply(x, require, character.only=T)

drive=c("F:\\")
#drive=c("C:\\Users\\Keita\\Google Drive\\Research\\")
#setwd('/home/bst/other/kebisu/BW_PMCoarse/Data')
#setwd("C:\\Users\\Keita\\Google Drive\\Research\\BW_PMCoarse\\Data")
setwd(paste(drive,'BW_PMCoarse\\Data',sep='')) 
#output_location=c('C:\\Users\\ke52\\Desktop\\Download\\')
output_location=c('C:\\Users\\Keita\\Desktop\\Download\\')

State_FIPS=read.csv('state_geocodes.csv')
names(State_FIPS)[5:6]=c('RegionName','DivisionName')
State_FIPS$St_FIPS=sprintf("%02d",State_FIPS$St_FIPS)
State_FIPS=State_FIPS[order(State_FIPS$St_FIPS),]

load('PMCoarseData.RData')#PMCoarse_data2
PMCoarse_data2$St_FIPS=substr(PMCoarse_data2$FIPS,1,2)
PMCoarse_data22=merge(PMCoarse_data2,State_FIPS,by='St_FIPS')
Outside_main=c('02','15','72','78')
PMCoarse_data3=PMCoarse_data22[!(substr(PMCoarse_data22$FIPS,1,2) %in% Outside_main),]

PM10_DayAverage=aggregate(PM10_Value~Date.Local,mean,data=PMCoarse_data3)
PM10_DayAverage=mutate(PM10_DayAverage,
	Year_f=as.factor(substr(PM10_DayAverage$Date.Local,1,4)),
	MonthDay_num=as.numeric(format(PM10_DayAverage$Date.Local,'%j')),
	MonthName=factor(months(PM10_DayAverage$Date.Local,abbreviate=TRUE)))
PM10_DayAverage$Period=cut(as.numeric(substr(PM10_DayAverage$Date.Local,1,4)),c(1997,seq(2001,2014,5),2015),
	labels=c('1997-2000','2001-2005','2006-2010','2011-Present'),right=FALSE,include.highest=TRUE)

title=expression(PM[10]*' Trend by Period')
Y_title=expression(PM[10]*' Value')
ylims=with(PM10_DayAverage, c(floor(min(PM10_Value)), ceiling(max(PM10_Value))))
Legend=filter(PM10_DayAverage,substr(Date.Local,9,10)=='01') %>%
	mutate(.,daynon=as.numeric(MonthDay_num))
Legend$daynon2=ifelse(as.numeric(substr(Legend$Date.Local,1,4))%%4==0&(Legend$MonthName!='Jan'&Legend$MonthName!='Feb'),as.numeric(Legend$MonthDay_num)-1,as.numeric(Legend$MonthDay_num))

PM10_WholePoint=ggplot()+
	geom_point(data=PM10_DayAverage,aes(x=MonthDay_num,y=PM10_Value,col=Period))+
	scale_x_continuous('Month',breaks=Legend$daynon2,labels=Legend$MonthName,expand=c(0,0))+
	scale_y_continuous(Y_title)+
	coord_cartesian(ylim=ylims)+
	theme_bw()

PM10_Whole=ggplot(data=PM10_DayAverage,aes(x=MonthDay_num,y=PM10_Value,col=Period))+
	geom_smooth(span=0.4,method='loess',size=2.5,se=FALSE)+
	scale_x_continuous('Month',breaks=Legend$daynon2,labels=Legend$MonthName,expand=c(0,0))+
	scale_y_continuous(Y_title)+
	ggtitle(title)+
	coord_cartesian(ylim=c(0,35))+
	theme_bw()

##PM25 Trend
PM25_DayAverage=aggregate(PM25_Value~Date.Local,mean,data=PMCoarse_data3)
PM25_DayAverage=mutate(PM25_DayAverage,
	Year_f=as.factor(substr(PM25_DayAverage$Date.Local,1,4)),
	MonthDay_num=as.numeric(format(PM25_DayAverage$Date.Local,'%j')),
	MonthName=factor(months(PM25_DayAverage$Date.Local,abbreviate=TRUE)))
PM25_DayAverage$Period=cut(as.numeric(substr(PM25_DayAverage$Date.Local,1,4)),c(1997,seq(2001,2014,5),2015),
	labels=c('1997-2000','2001-2005','2006-2010','2011-Present'),right=FALSE,include.highest=TRUE)

title=expression(PM[2.5]*' Trend by Period')
Y_title=expression(PM[2.5]*' Value')
ylims = with(PM25_DayAverage, c(floor(min(PM25_Value)), ceiling(max(PM25_Value))))
Legend=filter(PM25_DayAverage,substr(Date.Local,9,10)=='01') %>%
	mutate(.,daynon=as.numeric(MonthDay_num))
Legend$daynon2=ifelse(as.numeric(substr(Legend$Date.Local,1,4))%%4==0&(Legend$MonthName!='Jan'&Legend$MonthName!='Feb'),as.numeric(Legend$MonthDay_num)-1,as.numeric(Legend$MonthDay_num))

PM25_WholePoint=ggplot()+
	geom_point(data=PM25_DayAverage,aes(x=MonthDay_num,y=PM25_Value,col=Period))+
	scale_x_continuous('Month',breaks=Legend$daynon2,labels=Legend$MonthName,expand=c(0,0))+
	scale_y_continuous(Y_title)+
	coord_cartesian(ylim=ylims)+
	theme_bw()

PM25_Whole=ggplot(data=PM25_DayAverage,aes(x=MonthDay_num,y=PM25_Value,col=Period))+
	geom_smooth(span=0.4,method='loess',size=2.5,se=FALSE)+
	scale_x_continuous('Month',breaks=Legend$daynon2,labels=Legend$MonthName,expand=c(0,0))+
	scale_y_continuous(Y_title)+
	ggtitle(title)+
	coord_cartesian(ylim=c(0,20))+
	theme_bw()



##PMCoarse
PMCoarse_DayAverage=aggregate(PMCoarse_Value~Date.Local,mean,data=PMCoarse_data3)
PMCoarse_DayAverage=mutate(PMCoarse_DayAverage,
	Year_f=as.factor(substr(PMCoarse_DayAverage$Date.Local,1,4)),
	MonthDay_num=as.numeric(format(PMCoarse_DayAverage$Date.Local,'%j')),
	MonthName=factor(months(PMCoarse_DayAverage$Date.Local,abbreviate=TRUE)))
PMCoarse_DayAverage$Period=cut(as.numeric(substr(PMCoarse_DayAverage$Date.Local,1,4)),c(1997,seq(2001,2014,5),2015),
	labels=c('1997-2000','2001-2005','2006-2010','2011-Present'),right=FALSE,include.highest=TRUE)

title=expression(PM[10-2.5]*' Trend by Period')
Y_title=expression(PM[10-2.5]*' Value')
ylims=with(PMCoarse_DayAverage,c(floor(min(PMCoarse_Value)),ceiling(max(PMCoarse_Value))))
Legend=filter(PMCoarse_DayAverage,substr(Date.Local,9,10)=='01') %>%
	mutate(.,daynon=as.numeric(MonthDay_num))
Legend$daynon2=ifelse(as.numeric(substr(Legend$Date.Local,1,4))%%4==0&(Legend$MonthName!='Jan'&Legend$MonthName!='Feb'),as.numeric(Legend$MonthDay_num)-1,as.numeric(Legend$MonthDay_num))

PMCoarse_WholePoint=ggplot()+
	geom_point(data=PMCoarse_DayAverage,aes(x=MonthDay_num,y=PMCoarse_Value,col=Period))+
	scale_x_continuous('Month',breaks=Legend$daynon2,labels=Legend$MonthName,expand=c(0,0))+
	scale_y_continuous(Y_title)+
	coord_cartesian(ylim=ylims)+
	theme_bw()

PMCoarse_Whole=ggplot(data=PMCoarse_DayAverage,aes(x=MonthDay_num,y=PMCoarse_Value,col=Period))+
	geom_smooth(span=0.4,method='loess',size=2.5,se=FALSE)+
	scale_x_continuous('Month',breaks=Legend$daynon2,labels=Legend$MonthName,expand=c(0,0))+
	scale_y_continuous(Y_title)+
	ggtitle(title)+
	coord_cartesian(ylim=c(0,20))+
	theme_bw()



##Stratified by Region

Trend_Line<-function(AirPol,Region,numb,yax_max)
{
	TargetRerion=Region
	Poll=AirPol
	DF=filter(PMCoarse_data3,RegionName==TargetRerion)
	DF$Pollution=DF[,Poll]
	DF2=aggregate(Pollution~Date.Local,mean,data=DF)
	DF3=mutate(DF2,
		Year_f=as.factor(substr(DF2$Date.Local,1,4)),
		MonthDay_num=as.numeric(format(DF2$Date.Local,'%j')),
		MonthName=factor(months(DF2$Date.Local,abbreviate=TRUE)))
	DF3$Period=cut(as.numeric(substr(DF3$Date.Local,1,4)),c(1997,seq(2001,2014,5),2015),
		labels=c('1997-2000','2001-2005','2006-2010','2011-Present'),right=FALSE,include.highest=TRUE)
	title=substitute(paste(PM[size],' Trend by Period in ',area),list(size=numb,area=Region))
	Y_title=substitute(paste(PM[size],' Value'),list(size=numb))
	ylims=with(DF3, c(floor(min(Pollution)), ceiling(max(Pollution))))
	Legend=filter(DF3,substr(Date.Local,9,10)=='01') %>%
		mutate(.,daynon=as.numeric(MonthDay_num))
	Legend$daynon2=ifelse(as.numeric(substr(Legend$Date.Local,1,4))%%4==0&(Legend$MonthName!='Jan'&Legend$MonthName!='Feb'),as.numeric(Legend$MonthDay_num)-1,as.numeric(Legend$MonthDay_num))

	output1=ggplot(data=DF3,aes(x=MonthDay_num,y=Pollution,col=Period))+
		geom_smooth(span=0.4,method='loess',size=2.5,se=FALSE)+
		scale_x_continuous('Month',breaks=Legend$daynon2,labels=Legend$MonthName,expand=c(0,0))+
		scale_y_continuous(Y_title)+
		ggtitle(title)+
		coord_cartesian(ylim=c(0,yax_max))+
		theme_bw()
	return(output1)
}

#Region map of PM10
PM10_List=list(
    Trend_Line("PM10_Value",'Midwest','10',35),
    Trend_Line("PM10_Value",'Northeast','10',35),
    Trend_Line("PM10_Value",'West','10',35),
    Trend_Line("PM10_Value",'South','10',35)
)
PM10_Region=do.call(arrangeGrob,c(PM10_List,nrow=2))

#Region map of PM2.5
PM25_List=list(
    Trend_Line("PM25_Value",'Midwest','2.5',25),
    Trend_Line("PM25_Value",'Northeast','2.5',25),
    Trend_Line("PM25_Value",'West','2.5',25),
    Trend_Line("PM25_Value",'South','2.5',25)
)
PM25_Region=do.call(arrangeGrob,c(PM25_List,nrow=2))

#Region map of PMCoarse
PMCoarse_List=list(
    Trend_Line("PMCoarse_Value",'Midwest','10-2.5',25),
    Trend_Line("PMCoarse_Value",'Northeast','10-2.5',25),
    Trend_Line("PMCoarse_Value",'West','10-2.5',25),
    Trend_Line("PMCoarse_Value",'South','10-2.5',25)
)
PMCoarse_Region=do.call(arrangeGrob,c(PMCoarse_List,nrow=2))


pdf(paste(output_location,'PMTrend.pdf',paste=''),15,10)
PM10_Whole
PM10_Region
PM25_Whole
PM25_Region
PMCoarse_Whole
PMCoarse_Region
dev.off()

rm(list=ls())
