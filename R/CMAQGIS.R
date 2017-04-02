x<-c("dplyr","rgdal",'reshape2')
lapply(x, require, character.only=T)

setwd('F:\\Research\\CMAQData\\Data')


load('GIS_Data.RData') #GIS_Data
GIS_Data=mutate(GIS_Data,Time=paste(Season,'_',Year,sep=''))
GIS_Data2=dcast(GIS_Data,FIPS~Time,value.var='mean')


CA.Tract=readOGR('F:\\Research\\CMAQData\\Data','tl_2010_06_tract10')
CA.Tract@data=merge(CA.Tract@data,GIS_Data2,by.x='GEOID10',by.y='FIPS')




CA.Tract@data$bin=cut(CA.Tract@data$Annual_2007, seq(0,25, by=2.5),include.lowest = TRUE, dig.lab = 2)
rbPal=colorRampPalette(c('green','red'))
CA.Tract@data$Col=rbPal(10)[as.numeric(cut(CA.Tract@data$Annual_2007,seq(0,25,by=2.5)))]
plot(CA.Tract,col=CA.Tract$Col,xlim=c(-118.5,-118),ylim=c(33.75,34.25))




