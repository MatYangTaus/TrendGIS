pacman::p_load(tidyverse, data.table, skimr)

url = "https://aqs.epa.gov/aqsweb/airdata/hourly_88101_2017.zip"
download.file(url,'C:\\Users\\ebike\\Downloads\\temp2.zip')
temp=read.csv(unz('C:\\Users\\ebike\\Downloads\\temp2.zip',paste("hourly_88101_2017",".csv",sep='')),header=TRUE)

temp2 = temp %>%
	rename(Value = Sample.Measurement) %>%
	filter(Value >0) %>%
	mutate(MDL.Index = ifelse(Value<MDL, 1, 0)) %>%
	filter(State.Code == 6) %>%
	{.}

temp2 %>%
	group_by(MDL.Index) %>%
	count()

temp2 %>%
	filter(MDL.Index == 1,  MDL == 5) %>%
	mutate(Year = substr(Date.Local, 1, 4)) %>%
	group_by(County.Name) %>%
	count() %>%
	print(n = Inf)

temp3 = temp2 %>%
	filter(Value >35)  %>%

filter(temp2, County.Name =='Ventura', Date.Local == '2017-12-07')
