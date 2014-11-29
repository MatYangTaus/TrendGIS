x<-c('ggplot2','XML')
lapply(x, require, character.only=T)

drive=c("E:\\")
#setwd('/home/bst/other/kebisu/BW_PMCoarse/Data')
setwd(paste(drive,'BW_PMCoarse\\Data',sep='')) 
output_location=c('C:\\Users\\ke52\\Desktop\\Download\\')
#output_location=c('C:\\Users\\Keita\\Desktop\\Download\\')


load('PM25Monitor_Correlation.RData')
load('PM10Monitor_Correlation.RData')
load('PMCoarseMonitor_Correlation.RData')

#ur='http://www.epa.gov/envirofw/html/codes/state.html'
#State_FIPS=data.frame(readHTMLTable(ur))
State_FIPS=read.csv('state_geocodes.csv')
names(State_FIPS)[5:6]=c('Region','Division')
State_FIPS$St_FIPS=sprintf("%02d",State_FIPS$St_FIPS)
State_FIPS=State_FIPS[order(State_FIPS$St_FIPS),]

PM25DF0=PM25Monitor_Correlation[PM25Monitor_Correlation$obs_days>180,]
PM25DF0$State1=substr(PM25DF0$FIPSPOC_1,1,2)
PM25DF0$State2=substr(PM25DF0$FIPSPOC_2,1,2) 
PM25DF00=merge(PM25DF0,State_FIPS,by.x='State1',by.y='St_FIPS')
names(PM25DF00)[12:16]=c('RegionNumber1','DivisionNumber1','StateName1','RegionName1','DivisionName1')
PM25DF=merge(PM25DF00,State_FIPS,by.x='State2',by.y='St_FIPS')
names(PM25DF)[17:21]=c('RegionNumber2','DivisionNumber2','StateName2','RegionName2','DivisionName2')
PM25DF$Type=paste(PM25DF$RegionName1,'_',PM25DF$RegionName2,sep='')
PM25DF$Type[PM25DF$Type=='South_Northeast']='Northeast_South'
PM25DF$Type[PM25DF$Type=='Midwest_South']='South_Midwest'
PM25DF$Type[PM25DF$Type=='West_South']='South_West'
PM25DF$Type[PM25DF$Type=='Midwest_Northeast']='Northeast_Midwest'
summary(PM25DF[,c('Correlation','obs_days')])
table(PM25DF$Type)
aggregate(Correlation~Type,PM25DF,mean)
#plot(PM25DF$Distance,PM25DF$Correlation,ylim=c(0,1))

PM10DF0=PM10Monitor_Correlation[PM10Monitor_Correlation$obs_days>180&!is.na(PM10Monitor_Correlation$Correlation),]
PM10DF0$State1=substr(PM10DF0$FIPSPOC_1,1,2)
PM10DF0$State2=substr(PM10DF0$FIPSPOC_2,1,2) 
PM10DF00=merge(PM10DF0,State_FIPS,by.x='State1',by.y='St_FIPS')
names(PM10DF00)[12:16]=c('RegionNumber1','DivisionNumber1','StateName1','RegionName1','DivisionName1')
PM10DF=merge(PM10DF00,State_FIPS,by.x='State2',by.y='St_FIPS')
names(PM10DF)[17:21]=c('RegionNumber2','DivisionNumber2','StateName2','RegionName2','DivisionName2')
PM10DF$Type=paste(PM10DF$RegionName1,'_',PM10DF$RegionName2,sep='')
PM10DF$Type[PM10DF$Type=='South_Northeast']='Northeast_South'
PM10DF$Type[PM10DF$Type=='Midwest_South']='South_Midwest'
PM10DF$Type[PM10DF$Type=='West_South']='South_West'
PM10DF$Type[PM10DF$Type=='Midwest_Northeast']='Northeast_Midwest'
summary(PM10DF[,c('Correlation','obs_days')])
table(PM10DF$Type)
aggregate(Correlation~Type,PM10DF,mean)
#plot(PM10DF$Distance,PM10DF$Correlation,ylim=c(0,1))

PMCoarseDF0=PMCoarseMonitor_Correlation[PMCoarseMonitor_Correlation$obs_days>180&!is.na(PMCoarseMonitor_Correlation$Correlation),]
PMCoarseDF0$State1=substr(PMCoarseDF0$FIPSPOC_1,1,2)
PMCoarseDF0$State2=substr(PMCoarseDF0$FIPSPOC_2,1,2) 
PMCoarseDF00=merge(PMCoarseDF0,State_FIPS,by.x='State1',by.y='St_FIPS')
names(PMCoarseDF00)[12:16]=c('RegionNumber1','DivisionNumber1','StateName1','RegionName1','DivisionName1')
PMCoarseDF=merge(PMCoarseDF00,State_FIPS,by.x='State2',by.y='St_FIPS')
names(PMCoarseDF)[17:21]=c('RegionNumber2','DivisionNumber2','StateName2','RegionName2','DivisionName2')
PMCoarseDF$Type=paste(PMCoarseDF$RegionName1,'_',PMCoarseDF$RegionName2,sep='')
PMCoarseDF$Type[PMCoarseDF$Type=='South_Northeast']='Northeast_South'
PMCoarseDF$Type[PMCoarseDF$Type=='Midwest_South']='South_Midwest'
PMCoarseDF$Type[PMCoarseDF$Type=='West_South']='South_West'
PMCoarseDF$Type[PMCoarseDF$Type=='Midwest_Northeast']='Northeast_Midwest'
summary(PMCoarseDF[,c('Correlation','obs_days')])
table(PMCoarseDF$Type)
aggregate(Correlation~Type,PMCoarseDF,mean)
#plot(PMCoarseDF$Distance,PMCoarseDF$Correlation,ylim=c(0,1))

pair_n=dim(PM25DF)[1]
title=substitute(paste(PM[2.5],' Correlation and Distaince (n=',nn,')'),list(nn=pair_n))
plot1=ggplot(PM25DF,aes(x=Distance,y=Correlation,size=obs_days),guide=FALSE)+
geom_point(colour="white", fill="blue", shape=21)+
#geom_point(aes(colour=PM25DF$Type))+
ylim(0,1)+
theme_bw()+
scale_size_area("Observation Days")+
xlab('Distance (km)')+
ylab('Correlation')+
ggtitle(title)

pair_n=dim(PM10DF)[1]
title=substitute(paste(PM[10],' Correlation and Distaince (n=',nn,')'),list(nn=pair_n))
plot2=ggplot(PM10DF,aes(x=Distance,y=Correlation,size=obs_days),guide=FALSE)+
geom_point(colour="white", fill="blue", shape=21)+
ylim(0,1)+
theme_bw()+
scale_size_area("Observation Days")+
xlab('Distance (km)')+
ylab('Correlation')+
ggtitle(title)

pair_n=dim(PMCoarseDF)[1]
title=substitute(paste(PM[10-2.5],' Correlation and Distaince (n=',nn,')'),list(nn=pair_n))
plot3=ggplot(PMCoarseDF,aes(x=Distance,y=Correlation,size=obs_days),guide=FALSE)+
geom_point(colour="white", fill="blue", shape=21)+
ylim(0,1)+
theme_bw()+
scale_size_area("Observation Days")+
xlab('Distance (km)')+
ylab('Correlation')+
ggtitle(title)

pdf(paste(output_location,'Correlation.pdf',paste=''),15,10)
print(plot1)
print(plot2)
print(plot3)
dev.off()

##Put Fitted Line
#PM25
fit_line=coef(lm(Correlation~Distance,data=PM25DF))
Intercept=1
fit_line2=lm(I(Correlation-Intercept)~0+Distance,data=PM25DF)
plot1_fitline=plot1+
geom_abline(intercept=fit_line[1],slope=fit_line[2],color='red',lwd=2)+
geom_abline(intercept=1,slope=fit_line2[[1]],color='black',lwd=2)

#PM10
fit_line=coef(lm(Correlation~Distance,data=PM10DF))
Intercept=1
fit_line2=lm(I(Correlation-Intercept)~0+Distance,data=PM10DF)
plot2_fitline=plot2+
geom_abline(intercept=fit_line[1],slope=fit_line[2],color='red',lwd=2)+
geom_abline(intercept=1,slope=fit_line2[[1]],color='black',lwd=2)

#PMCoarse
fit_line=coef(lm(Correlation~Distance,data=PMCoarseDF))
Intercept=1
fit_line2=lm(I(Correlation-Intercept)~0+Distance,data=PMCoarseDF)
plot3_fitline=plot3+
geom_abline(intercept=fit_line[1],slope=fit_line[2],color='red',lwd=2)+
geom_abline(intercept=1,slope=fit_line2[[1]],color='black',lwd=2)

pdf(paste(output_location,'Correlation_fitline.pdf',paste=''),15,10)
print(plot1_fitline)
print(plot2_fitline)
print(plot3_fitline)
dev.off()


##Stratified by Region + Fitted Line
plot_f<-function(dataframe,Region,numb)
{
	df=dataframe
	TargetRegion=Region
	pair_n=dim(df[df$Type==TargetRegion,])[1]
	title=substitute(paste(PM[kk],' Correlation and Distaince (n=',nn,') in ',ll),list(kk=numb,nn=pair_n,ll=TargetRegion))
	output=ggplot(df[df$Type==TargetRegion,],aes(x=Distance,y=Correlation,size=obs_days),guide=FALSE)+
	geom_point(colour="white", fill="blue", shape=21)+
	ylim(0,1)+
	theme_bw()+
	scale_size_area("Observation Days")+
	xlab('Distance (km)')+
	ylab('Correlation')+
	ggtitle(title)
	print(output)
}
pdf(paste(output_location,'CorrelationbyRegion.pdf',paste=''),15,10)
plot_f(PM25DF,'Northeast_Northeast','2.5')
plot_f(PM25DF,'Midwest_Midwest','2.5')
plot_f(PM25DF,'South_South','2.5')
plot_f(PM25DF,'West_West','2.5')
plot_f(PM10DF,'Northeast_Northeast','10')
plot_f(PM10DF,'Midwest_Midwest','10')
plot_f(PM10DF,'South_South','10')
plot_f(PM10DF,'West_West','10')
plot_f(PMCoarseDF,'Northeast_Northeast','10-2.5')
plot_f(PMCoarseDF,'Midwest_Midwest','10-2.5')
plot_f(PMCoarseDF,'South_South','10-2.5')
plot_f(PMCoarseDF,'West_West','10-2.5')
dev.off()


##Stratified by Region
plot_f_fitted<-function(dataframe,Region,numb)
{
	df_temp=dataframe
	TargetRegion=Region
	df=df_temp[df_temp$Type==TargetRegion,]
	pair_n=dim(df)[1]
	title=substitute(paste(PM[kk],' Correlation and Distaince (n=',nn,') in ',ll),list(kk=numb,nn=pair_n,ll=TargetRegion))
	fit_line=coef(lm(Correlation~Distance,data=df))
	Intercept=1
	fit_line2=lm(I(Correlation-Intercept)~0+Distance,data=df)
	output=ggplot(df[df$Type==TargetRegion,],aes(x=Distance,y=Correlation,size=obs_days),guide=FALSE)+
	geom_point(colour="white", fill="blue", shape=21)+
	ylim(0,1)+
	theme_bw()+
	scale_size_area("Observation Days")+
	xlab('Distance (km)')+
	ylab('Correlation')+
	ggtitle(title)+
	geom_abline(intercept=fit_line[1],slope=fit_line[2],color='red',lwd=2)+
	geom_abline(intercept=1,slope=fit_line2[[1]],color='black',lwd=2)
	print(output)
}

pdf(paste(output_location,'CorrelationbyRegion_fitted.pdf',paste=''),15,10)
plot_f_fitted(PM25DF,'Northeast_Northeast','2.5')
plot_f_fitted(PM25DF,'Midwest_Midwest','2.5')
plot_f_fitted(PM25DF,'South_South','2.5')
plot_f_fitted(PM25DF,'West_West','2.5')
plot_f_fitted(PM10DF,'Northeast_Northeast','10')
plot_f_fitted(PM10DF,'Midwest_Midwest','10')
plot_f_fitted(PM10DF,'South_South','10')
plot_f_fitted(PM10DF,'West_West','10')
plot_f_fitted(PMCoarseDF,'Northeast_Northeast','10-2.5')
plot_f_fitted(PMCoarseDF,'Midwest_Midwest','10-2.5')
plot_f_fitted(PMCoarseDF,'South_South','10-2.5')
plot_f_fitted(PMCoarseDF,'West_West','10-2.5')
dev.off()

rm(list=ls())
