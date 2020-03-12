##########################
pacman::p_load(tidyverse, patchwork, gridExtra, grid)
load("CO_Data_20171026.RData") #Units are in PPM

All_CO_2 = CO_AQS %>%
       mutate(Monitor=substr(FIPSPOC,1,9)) %>%
       group_by(Date, Monitor) %>%
       summarize(CO_Value = mean(CO_Value)) %>%
       ungroup() %>%
       arrange(Monitor, Date) %>%
       group_by(Monitor) %>%
       mutate(n = row_number()) %>%
       ungroup()
#All_CO_2$n=sequence(rle(All_CO_2$Monitor)$lengths)

First_Date = All_CO_2 %>%
       arrange(Monitor, Date) %>%
       distinct(Monitor, .keep_all = TRUE) %>%
       rename(FirstObsDate = Date) %>%
       select(Monitor, FirstObsDate)

Last_Date = All_CO_2 %>%
       arrange(Monitor, desc(Date)) %>%
       distinct(Monitor, .keep_all = TRUE) %>%
       rename(LastObsDate = Date) %>%
       select(Monitor, LastObsDate, n)

Obs_List = inner_join(First_Date, Last_Date, by = 'Monitor') %>%
       mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate), Freq = PeriodLength/n) %>%
       filter(n > 1000, PeriodLength > 1000, Freq < 7)

All_CO_4 = All_CO_2 %>%
       filter(Monitor %in% Obs_List$Monitor) %>%
       group_by(Date) %>%
       summarize(CO_Value = mean(CO_Value, na.rm = TRUE)) %>%
       ungroup() %>%
       mutate(MonthDay_num = as.numeric(format(Date,'%j')),
              MonthName = factor(months(Date,abbreviate=TRUE)),
              Period = cut(as.numeric(substr(Date, 1, 4)), c(1990, seq(1995,2019,5), 2020), labels = c('1990-1994','1995-1999','2000-2004','2005-2009','2010-2014', '2015-2019'), right=FALSE, include.highest=TRUE))

title = substitute(paste(CO,' Trend from 1990 to 2019 in US'))
Y_title = substitute(paste(CO,' Value'))
ylims = c(floor(min(All_CO_4$CO_Value)), ceiling(max(All_CO_4$CO_Value)))
Legend = data.frame(All_CO_4[!duplicated(All_CO_4$MonthName),c('MonthName','MonthDay_num')])

Plot_gg_CO = All_CO_4 %>%
       filter(!is.na(Period)) %>%
       ggplot(aes_string(x = "MonthDay_num", y = "CO_Value", col = "Period"))+
       geom_smooth(span = 0.4, method = 'loess', size = 2.5, se = FALSE)+
       scale_x_continuous('Month', breaks = Legend$MonthDay_num, labels = Legend$MonthName, expand=c(0,0))+
       scale_y_continuous(Y_title)+
       ggtitle(title)+
       coord_cartesian(ylim = c(0, 2.0))+
       theme_bw()
#	theme(panel.background = element_rect(fill = "black"))
Plot_gg_CO



######################
## Region Plot########
######################

FIPS_list=read.csv('K:\\AirData\\OriginalData\\Fips_State.csv') %>%
       mutate(STATE_FIPS = sprintf("%02d", as.numeric(as.character(STATE_FIPS))))	

All_CO_region=mutate(CO_AQS, Monitor=substr(FIPSPOC,1,9), STATE_FIPS = substr(FIPSPOC,1,2)) %>%
       inner_join(FIPS_list,by='STATE_FIPS')

p=list()

for (i in 1:10){
       All_CO_region2 = All_CO_region %>%
              filter(Region == i) %>%
              arrange(Date, Monitor) %>%
              group_by(Date, Monitor) %>%
              summarize(CO_Value = mean(CO_Value, na.rm = TRUE)) %>%
              ungroup() %>%
              arrange(Monitor, Date) %>%
              group_by(Monitor) %>%
              mutate(n = row_number()) %>%
              ungroup()
       
       First_Date = All_CO_region2 %>%
              arrange(Monitor, Date) %>%
              distinct(Monitor, .keep_all = TRUE) %>%
              rename(FirstObsDate = Date) %>%
              select(Monitor, FirstObsDate)
       
       Last_Date = All_CO_region2 %>%
              arrange(Monitor, desc(Date)) %>%
              distinct(Monitor, .keep_all = TRUE) %>%
              rename(LastObsDate = Date) %>%
              select(Monitor, LastObsDate, n)
       
       Obs_List = inner_join(First_Date, Last_Date, by = 'Monitor') %>%
              mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate), Freq = PeriodLength/n) %>%
              filter(n > 1000, PeriodLength > 1000, Freq < 7)
       
       All_CO_region4 = All_CO_region2 %>%
              filter(Monitor %in% Obs_List$Monitor) %>%
              group_by(Date) %>%
              summarize(CO_Value = mean(CO_Value, na.rm = TRUE)) %>%
              ungroup() %>%
              mutate(MonthDay_num = as.numeric(format(Date,'%j')),
                     MonthName = factor(months(Date,abbreviate=TRUE)),
                     Period = cut(as.numeric(substr(Date, 1, 4)), c(1990, seq(1995,2019,5), 2020), labels = c('1990-1994','1995-1999','2000-2004','2005-2009','2010-2014', '2015-2019'), right=FALSE, include.highest=TRUE))
       
       title=substitute(paste(CO,' Trend from 1990 to 2019 in US EPA Region ', nn1), list(nn1 = i))
       Y_title=substitute(paste(CO,' Value'))
       ylims=c(floor(min(All_CO_region4$CO_Value)), ceiling(max(All_CO_region4$CO_Value)))
       Legend=data.frame(All_CO_region4[!duplicated(All_CO_region4$MonthName),c('MonthName','MonthDay_num')])
       
       Plot_gg_CO = All_CO_region4 %>%
              filter(!is.na(Period)) %>%
              ggplot(aes_string(x="MonthDay_num",y="CO_Value",col="Period"))+
              geom_smooth(span=0.4,method='loess',size=2.5,se=FALSE)+
              scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
              scale_y_continuous(Y_title)+
              ggtitle(title)+
              coord_cartesian(ylim=c(0,2.5))+
              theme_bw() +
              theme(plot.title = element_text(hjust = 0.5))
       
       p[[i]]=Plot_gg_CO
}

test=do.call(arrangeGrob,c(p,nrow=3,ncol=4))

pdf("C:\\Users\\kebisu\\Downloads\\plots_CO.pdf", onefile = TRUE,20,15)
grid.draw(test)
dev.off()
rm(list=ls())