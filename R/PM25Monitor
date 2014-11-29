x<-c('sp','rgdal','ggmap','RColorBrewer','classInt','maptools')
lapply(x, require, character.only=T)

drive=c("E:\\")
#setwd('/home/bst/other/kebisu/BW_PMCoarse/Data')
setwd(paste(drive,'BW_PMCoarse\\Data',sep='')) 
output_location=c('C:\\Users\\ke52\\Desktop\\Download\\')
#output_location=c('C:\\Users\\Keita\\Desktop\\Download\\')

load('PM25Data.RData')#PM25_data2
ExcVar1=names(PM25_data2) %in% c('Observation.Count','Observation.Percent','Datum','Parameter.Code ','X1st.Max.Value','X1st.Max.Hour','Date.of.Last.Change')
PM25_data3=PM25_data2[!ExcVar1]
PM25_data3$POC=sprintf("%02d",PM25_data3$POC)
PM25_data3$FIPS=paste(PM25_data3$State.Code,PM25_data3$County.Code,PM25_data3$Site.Num,sep='')
PM25_data3$FIPSPOC=paste(PM25_data3$State.Code,PM25_data3$County.Code,PM25_data3$Site.Num,PM25_data3$POC,sep='')
#PM25_data3$YR=format(PM25_data3$Date.Local,"%Y")
#PM25_data3$MON=format(PM25_data3$Date.Local,"%m")
#PM25_data3$YR_Mon=paste(PM25_data3$YR,PM25_data3$MON,sep='-')
Outside_main=c('02','15','72','78')
PM25_data3_temp=PM25_data3[!(substr(PM25_data3$FIPS,1,2) %in% Outside_main),]
PM25_data4=PM25_data3_temp[PM25_data3_temp$Event.Type=='None'&PM25_data3_temp$Sample.Duration=='24 HOUR',]
PM25_data5=aggregate(PM25_Value~FIPSPOC+Date.Local,data=PM25_data4,mean)

PM25_data5$Month=substr(PM25_data5$Date.Local,6,7)
PM25_data5$Season=c('Winter')
list_spring=c('03','04','05')
PM25_data5$Season[PM25_data5$Month %in% list_spring]=c('Spring')
list_summer=c('06','07','08')
PM25_data5$Season[PM25_data5$Month %in% list_summer]=c('Summer')
list_autumn=c('09','10','11')
PM25_data5$Season[PM25_data5$Month %in% list_autumn]=c('Autumn')
PM25_data5$FIPS=substr(PM25_data5$FIPSPOC,1,9)
PM25_data6=aggregate(PM25_Value~FIPS+Season,PM25_data5,mean)
PM25_data7=reshape(PM25_data6,timevar="Season",idvar=c("FIPS"),direction="wide")
PM25_data7$PeakSeason=colnames(PM25_data7[,2:5])[apply(PM25_data7[,2:5],1,which.max)]
table(PM25_data7$PeakSeason)
colnames(PM25_data7)[2:5]=c('Autumn','Spring','Summer','Winter')
Mon_Loc=PM25_data4[!duplicated(PM25_data4[,c('FIPS')]),c('FIPS','Latitude','Longitude')]
PM25_data8=merge(PM25_data7,Mon_Loc,by='FIPS',all.x=TRUE)

#GIS
colours=brewer.pal(6,"Reds")
brks=classIntervals(PM25_data4$PM25_Value,n=6)
plot(brks,pal=colours)#Make sure for color order
cuttingpoint=brks$brks
USBoundary=readOGR(paste(drive,"BW_PMCoarse\\GIS",sep=''), "USShape")

pdf(paste(output_location,'PM25Map.pdf',paste=''),24,12)
par(mfrow=c(2,2))
#Winter
plot(USBoundary)
points(PM25_data8$Longitude,PM25_data8$Latitude,pch=16,
	col=colours[findInterval(PM25_data8$Winter,cuttingpoint)],cex=1.5)
legend("bottomright",legend=leglabs(round(cuttingpoint,2.0)),bty='n',pch=16,col=colours,pt.cex=1.5)
title=substitute(paste(PM[2.5],' Levels in Winter (n=',nn,')'),list(nn=sum(!is.na(PM25_data8$Winter))))
title(main=list(title,cex=1.9))

#Spring
plot(USBoundary)
points(PM25_data8$Longitude,PM25_data8$Latitude,pch=16,
	col=colours[findInterval(PM25_data8$Spring,cuttingpoint)],cex=1.5)
legend("bottomright",legend=leglabs(round(cuttingpoint,2.0)),bty='n',pch=16,col=colours,pt.cex=1.5)
title=substitute(paste(PM[2.5],' Levels in Spring (n=',nn,')'),list(nn=sum(!is.na(PM25_data8$Spring))))
title(main=list(title,cex=1.9))

#Summer
plot(USBoundary)
points(PM25_data8$Longitude,PM25_data8$Latitude,pch=16,
	col=colours[findInterval(PM25_data8$Summer,cuttingpoint)],cex=1.5)
legend("bottomright",legend=leglabs(round(cuttingpoint,2.0)),bty='n',pch=16,col=colours,pt.cex=1.5)
title=substitute(paste(PM[2.5],' Levels in Summer (n=',nn,')'),list(nn=sum(!is.na(PM25_data8$Summer))))
title(main=list(title,cex=1.9))

#Autumn
plot(USBoundary)
points(PM25_data8$Longitude,PM25_data8$Latitude,pch=16,
	col=colours[findInterval(PM25_data8$Autumn,cuttingpoint)],cex=1.5)
legend("bottomright",legend=leglabs(round(cuttingpoint,2.0)),bty='n',pch=16,col=colours,pt.cex=1.5)
title=substitute(paste(PM[2.5],' Levels in Autumn (n=',nn,')'),list(nn=sum(!is.na(PM25_data8$Autumn))))
title(main=list(title,cex=1.9))

dev.off()

rm(list=ls())
