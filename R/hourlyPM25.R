pacman::p_load(tidyverse, data.table, skimr)

url = "https://aqs.epa.gov/aqsweb/airdata/hourly_88101_2017.zip"
download.file(url,'C:\\Users\\kebisu\\Downloads\\temp2.zip')
temp=read.csv(unz('C:\\Users\\kebisu\\Downloads\\temp2.zip',paste("hourly_88101_2017",".csv",sep='')),header=TRUE)

temp %>%
	mutate(MDL.Index = ifelse(Sample.Measurement<MDL, 1, 0)) %>%
	group_by(MDL.Index) %>%
	count()

temp %>%
	mutate(MDL.Index = ifelse(Sample.Measurement<MDL, 1, 0)) %>%
	filter(MDL.Index == 1) %>%
	group_by(State.Code) %>%
	count() %>%
	print(n = Inf)
