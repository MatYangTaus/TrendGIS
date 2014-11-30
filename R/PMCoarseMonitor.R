x<-c('sp','rgdal','ggmap','RColorBrewer','classInt','maptools','gridExtra')
lapply(x, require, character.only=T)

drive=c("F:\\")
#drive=c("C:\\Users\\Keita\\Google Drive\\Research\\")
#setwd('/home/bst/other/kebisu/BW_PMCoarse/Data')
#setwd("C:\\Users\\Keita\\Google Drive\\Research\\BW_PMCoarse\\Data")
setwd(paste(drive,'BW_PMCoarse\\Data',sep='')) 
#output_location=c('C:\\Users\\ke52\\Desktop\\Download\\')
output_location=c('C:\\Users\\Keita\\Desktop\\Download\\')

load('PMCoarseData.RData')#PMCoarse_data2

PMCoarse_data2$Month=substr(PMCoarse_data2$Date.Local,6,7)
PMCoarse_data2$Season=c('Winter')
list_spring=c('03','04','05')
PMCoarse_data2$Season[PMCoarse_data2$Month %in% list_spring]=c('Spring')
list_summer=c('06','07','08')
PMCoarse_data2$Season[PMCoarse_data2$Month %in% list_summer]=c('Summer')
list_autumn=c('09','10','11')
PMCoarse_data2$Season[PMCoarse_data2$Month %in% list_autumn]=c('Autumn')
Outside_main=c('02','15','72','78')
PMCoarse_data3=PMCoarse_data2[!(substr(PMCoarse_data2$FIPS,1,2) %in% Outside_main),]

PMCoarse_Season=aggregate(PMCoarse_Value~FIPS+Season,PMCoarse_data3,mean)
PM25_Season=aggregate(PM25_Value~FIPS+Season,PMCoarse_data3,mean)
PM10_Season=aggregate(PM10_Value~FIPS+Season,PMCoarse_data3,mean)

PMCoarse_Season2=reshape(PMCoarse_Season,timevar="Season",idvar=c("FIPS"),direction="wide")
PM25_Season2=reshape(PM25_Season,timevar="Season",idvar=c("FIPS"),direction="wide")
PM10_Season2=reshape(PM10_Season,timevar="Season",idvar=c("FIPS"),direction="wide")

nn=dim(PMCoarse_Season2)[1]
for (i in 1:nn){
	PMCoarse_Season2$Peak[i]=names(which.max(PMCoarse_Season2[i,c(2:5)]))
}
table(PMCoarse_Season2$Peak)

nn=dim(PM10_Season2)[1]
for (i in 1:nn){
	PM10_Season2$Peak[i]=names(which.max(PM10_Season2[i,c(2:5)]))
}
table(PM10_Season2$Peak)

nn=dim(PM25_Season2)[1]
for (i in 1:nn){
	PM25_Season2$Peak[i]=names(which.max(PM25_Season2[i,c(2:5)]))
}
table(PM25_Season2$Peak)

Mon_Loc=PMCoarse_data2[!duplicated(PMCoarse_data2[,c('Latitude','Longitude')]),c(1,6,7)]
PMCoarse_Season3=merge(PMCoarse_Season2,Mon_Loc,by='FIPS')
colnames(PMCoarse_Season3)[2:5]=c('Autumn','Spring','Summer','Winter')
Outside_main=c('02','15','72','78')
PMCoarse_Season4=PMCoarse_Season3[!(substr(PMCoarse_Season3$FIPS,1,2) %in% Outside_main),]

#Winter
colours=brewer.pal(6,"Reds")
brks=classIntervals(PMCoarse_Season$PMCoarse_Value,n=6)
plot(brks,pal=colours)#Make sure for color order
cuttingpoint=brks$brks
USBoundary=readOGR(paste(drive,"BW_PMCoarse\\GIS",sep=''),"USShape")

pdf(paste(output_location,'PMCoarseMap.pdf',paste=''),24,12)
par(mfrow=c(2,2))
plot(USBoundary)
points(PMCoarse_Season4$Longitude,PMCoarse_Season4$Latitude,pch=16,
	col=colours[findInterval(PMCoarse_Season4$Winter,cuttingpoint)],cex=1.5)
legend("bottomright",legend=leglabs(round(cuttingpoint,2.0)),bty='n',pch=16,col=colours,pt.cex=1.5)
title=substitute(paste('PM Coarse Levels in Winter (n=',nn,')'),list(nn=sum(!is.na(PMCoarse_Season4$Winter))))
title(main=list(title,cex=1.9))

#Spring
cuttingpoint=brks$brks
plot(USBoundary)
points(PMCoarse_Season4$Longitude,PMCoarse_Season4$Latitude,pch=16,
	col=colours[findInterval(PMCoarse_Season4$Spring,cuttingpoint)],cex=1.5)
legend("bottomright",legend=leglabs(round(cuttingpoint,2.0)),bty='n',pch=16,col=colours,pt.cex=1.5)
title=substitute(paste('PM Coarse Levels in Spring (n=',nn,')'),list(nn=sum(!is.na(PMCoarse_Season4$Spring))))
title(main=list(title,cex=1.9))

#Summer
cuttingpoint=brks$brks
plot(USBoundary)
points(PMCoarse_Season4$Longitude,PMCoarse_Season4$Latitude,pch=16,
	col=colours[findInterval(PMCoarse_Season4$Summer,cuttingpoint)],cex=1.5)
legend("bottomright",legend=leglabs(round(cuttingpoint,2.0)),bty='n',pch=16,col=colours,pt.cex=1.5)
title=substitute(paste('PM Coarse Levels in Summer (n=',nn,')'),list(nn=sum(!is.na(PMCoarse_Season4$Summer))))
title(main=list(title,cex=1.9))

#Autumn
cuttingpoint=brks$brks
plot(USBoundary)
points(PMCoarse_Season4$Longitude,PMCoarse_Season4$Latitude,pch=16,
	col=colours[findInterval(PMCoarse_Season4$Autumn,cuttingpoint)],cex=1.5)
legend("bottomright",legend=leglabs(round(cuttingpoint,2.0)),bty='n',pch=16,col=colours,pt.cex=1.5)
title=substitute(paste('PM Coarse Levels in Autumn (n=',nn,')'),list(nn=sum(!is.na(PMCoarse_Season4$Autumn))))
title(main=list(title,cex=1.9))

dev.off()


##GGMAP GIS##GGMAP GIS
brks=(classIntervals(PMCoarse_Season$PMCoarse_Value,n=6))
cuttingpoint=brks$brks

USContinental=get_googlemap(c(lon=-96.3,lat=39.75) ,zoom=4, xlim=c(-130,-50), ylim=c(25,50),maptype = "hybrid")

#Winter
PMCoarse_Season4$Value=cut(PMCoarse_Season4$Winter,cuttingpoint)
title=substitute(paste('PM Coarse Levels in Winter (n=',nn,')'),list(nn=sum(!is.na(PMCoarse_Season4$Winter))))
Type5=ggmap(USContinental)+
	geom_point(data=PMCoarse_Season4,aes(x=Longitude,y=Latitude,colour=Value),size=2)

plot1=Type5+
labs(x='Longitude',y='Latitude',title=title)+
scale_colour_manual(name='PM Coarse Value',values=(brewer.pal(6,"Reds")))+
theme(legend.title = element_text(colour="Black", size=16, face="bold"))#+
#guides(colour=FALSE)

#Spring
PMCoarse_Season4$Value=cut(PMCoarse_Season4$Spring,cuttingpoint)
title=substitute(paste('PM Coarse Levels in Spring (n=',nn,')'),list(nn=sum(!is.na(PMCoarse_Season4$Spring))))
Type5=ggmap(USContinental)+
	geom_point(data=PMCoarse_Season4,aes(x=Longitude,y=Latitude,colour=Value),size=2)

plot2=Type5+
labs(x='Longitude',y='Latitude',title=title)+
scale_colour_manual(name='PM Coarse Value',values=(brewer.pal(6,"Reds")))+
theme(legend.title = element_text(colour="Black", size=16, face="bold"))#+
#guides(colour=FALSE)

#Summer
PMCoarse_Season4$Value=cut(PMCoarse_Season4$Summer,cuttingpoint)
title=substitute(paste('PM Coarse Levels in Summer (n=',nn,')'),list(nn=sum(!is.na(PMCoarse_Season4$Summer))))
Type5=ggmap(USContinental)+
	geom_point(data=PMCoarse_Season4,aes(x=Longitude,y=Latitude,colour=Value),size=2)

plot3=Type5+
labs(x='Longitude',y='Latitude',title=title)+
scale_colour_manual(name='PM Coarse Value',values=(brewer.pal(6,"Reds")))+
theme(legend.title = element_text(colour="Black", size=16, face="bold"))#+
#guides(colour=FALSE)

#Autumn
PMCoarse_Season4$Value=cut(PMCoarse_Season4$Autumn,cuttingpoint)
title=substitute(paste('PM Coarse Levels in Autumn (n=',nn,')'),list(nn=sum(!is.na(PMCoarse_Season4$Autumn))))
Type5=ggmap(USContinental)+
	geom_point(data=PMCoarse_Season4,aes(x=Longitude,y=Latitude,colour=Value),size=2)

plot4=Type5+
labs(x='Longitude',y='Latitude',title=title)+
scale_colour_manual(name='PM Coarse Value',values=(brewer.pal(6,"Reds")))+
theme(legend.title = element_text(colour="Black", size=16, face="bold"))

pdf(paste(output_location,'PMCoarseMAP_GGMAP.pdf',paste=''),15,10)
grid.arrange(plot1,plot2,plot3,plot4,ncol=2)
dev.off()

rm(list=ls())
