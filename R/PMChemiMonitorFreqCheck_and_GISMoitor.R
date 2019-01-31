pacman::p_load(tidyverse, ggthemes, gridExtra, grid, lubridate, reshape2, sf)

load('K:\\AirData\\OriginalData\\PM25_Species_Data_20160212.RData') #PM25_Spec_Data

list.constituents = c(88104, 88321, 88320, 88165, 88184, 88403, 88169, 88306)

temp1 = PM25_Spec_Data %>% 
     filter(Parameter.Code %in% list.constituents) %>% 
     arrange(FIPS_POC, Parameter.Code, Date) %>% 
     select(-Numbering) %>% 
     mutate(tempid = paste0(FIPS_POC, Parameter.Code)) %>% 
     mutate(n = sequence(rle(tempid)$lengths)) %>%
     select(-tempid)

table(temp1$Parameter.Code)
table(temp1$ParameterName )

temp2 = temp1 %>% 
     group_by(FIPS_POC, Parameter.Code) %>%
     slice(c(1, n())) %>%
     ungroup

First = filter(temp2, n == 1) %>%
     select(FIPS_POC, Date, Parameter.Code)
Last = filter(temp2, n!=1) %>% 
     select(-Latitude, -Longitude, -Arithmetic.Mean)

FirstLast2 = inner_join(First, Last, by = c('FIPS_POC', 'Parameter.Code')) %>%
     mutate(length = as.numeric(Date.y - Date.x), freq = length/n, StateFIPS = substr(FIPS_POC, 1, 2)) %>% 
     filter(length > 365, n > 500)

filter(FirstLast2, substr(FIPS_POC, 1, 2) == '06') %>% 
     mutate(FIPS = substr(FIPS_POC, 1, 9)) %>% 
     distinct(FIPS_POC, .keep_all = TRUE) %>% 
     arrange(Date.x) %>% 
     print(n = Inf)

temp3 = temp1 %>% 
     filter(FIPS_POC %in% unique(FirstLast2$FIPS_POC), substr(FIPS_POC, 1, 2) == '06') %>% 
     separate(ParameterName, c('Pollutant','After'), " PM2.5") %>% 
     select(-After, -n) %>% 
     mutate(Pollutant = str_replace(Pollutant, "Total Nitrate", "TotalNit"))

temp4 = temp3 %>% 
     select(FIPS_POC, Date, Pollutant, Arithmetic.Mean) %>% 
     dcast(FIPS_POC + Date ~ Pollutant, value.var = 'Arithmetic.Mean') %>% 
     filter(!is.na(EC), !is.na(Aluminum), !is.na(Sulfate)) 

## Plot Locations
Location = temp3 %>% 
     filter(FIPS_POC %in% unique(temp4$FIPS_POC)) %>% 
     distinct(FIPS_POC, .keep_all = TRUE) %>% 
     select(FIPS_POC, Longitude, Latitude) %>% 
     st_as_sf(coords = c("Longitude", "Latitude"), crs =4326) %>%  
     st_transform(crs = st_crs(102003))

CA_county = st_read('K:\\GISData\\CalCountyFile\\gz_2010_us_050_00_5m.shp', stringsAsFactors = FALSE) %>% 
     st_transform(crs = st_crs(102003)) %>% 
     filter(STATE == '06')

plot(st_geometry(CA_county))

ggplot() + 
     geom_sf(data = CA_county) +
     geom_sf(data = Location, size = 4, shape = 16, color = "red") + # theme_bw() 
     theme(axis.line=element_blank(),axis.text.x=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),legend.position="none",
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())