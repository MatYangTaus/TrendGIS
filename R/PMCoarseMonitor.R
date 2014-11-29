x<-c('sp','rgdal','ggmap','RColorBrewer','classInt','maptools')
lapply(x, require, character.only=T)
 
drive=c("E:\\")
#setwd('/home/bst/other/kebisu/BW_PMCoarse/Data')
setwd("C:\\Users\\Keita\\Google Drive\\Research\\BW_PMCoarse\\Data")
#setwd(paste(drive,'BW_PMCoarse\\Data',sep='')) 
output_location=c('C:\\Users\\ke52\\Desktop\\Download\\')
#output_location=c('C:\\Users\\Keita\\Desktop\\Download\\')

load('PMCoarseData.RData')#PMCoarse_data2
PMCoarse_data2$Month=substr(PMCoarse_data2$Date.Local,6,7)
PMCoarse_data2$Season=c('Winter')
list_spring=c('03','04','05')
PMCoarse_data2$Season[PMCoarse_data2$Month %in% list_spring]=c('Spring')
list_summer=c('06','07','08')
PMCoarse_data2$Season[PMCoarse_data2$Month %in% list_summer]=c('Summer')
list_autumn=c('09','10','11')
PMCoarse_data2$Season[PMCoarse_data2$Month %in% list_autumn]=c('Autumn')

PMCoarse_Season=aggregate(PMCoarse_Value~FIPS+Season,PMCoarse_data2,mean)
PM25_Season=aggregate(PM25_Value~FIPS+Season,PMCoarse_data2,mean)
PM10_Season=aggregate(PM10_Value~FIPS+Season,PMCoarse_data2,mean)

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
USBoundary=readOGR(paste(drive,"BW_PMCoarse\\GIS",sep=''), "USShape")

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


##GGMAP GIS
colours=rev(brewer.pal(6,"Spectral"))
brks=(classIntervals(PMCoarse_Season$PMCoarse_Value,n=6))
cuttingpoint=brks$brks
plot(brks,pal=colours)
USContinental=get_googlemap(c(lon=-96.3,lat=39.75) ,zoom=4, xlim=c(-130,-50), ylim=c(25,50),maptype = "hybrid")
colScale <- scale_colour_manual(name = "PM Coarse Level",values = colours,labels=leglabs(round(cuttingpoint,2.0)))

test=cut(PMCoarse_Season$PMCoarse_Value, breaks=6)
test2=(classIntervals(PMCoarse_Season$PMCoarse_Value,n=6))
#Winter
title=substitute(paste('PM Coarse Levels in Winter (n=',nn,')'),list(nn=sum(!is.na(PMCoarse_Season4$Winter))))
ggmap(USContinental)+
#geom_point(aes(x=Longitude,y=Latitude),col=colours[findInterval(PMCoarse_Season4$Autumn,cuttingpoint)],data=PMCoarse_Season4,size=2.5)+
geom_point(data=PMCoarse_Season4,aes(x=Longitude,y=Latitude,col=colours[findInterval(PMCoarse_Season4$Autumn,cuttingpoint)]),size=2)+
#geom_point(data=PMCoarse_Season4,aes(x=Longitude,y=Latitude,col=PMCoarse_Season4$Autumn),size=2)+
labs(x='Longitude',y='Latitude',title=title)+
#scale_fill_brewer(palette="Spectral")
colScale
#scale_colour_brewer(palette = colours)
#scale_color_discrete("Drug Cases")
#scale_colour_continuous(low = "red", high = "blue", space = "Lab", guide = "colorbar")
#theme(axis.ticks = element_blank(),axis.text = element_blank())+
#scale_fill_manual(name="Pollution Levels",values=colours,labels=cuttingpoint)

rm(list=ls())
