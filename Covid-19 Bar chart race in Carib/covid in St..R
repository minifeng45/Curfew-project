####Carib confirmed cases of COVID-19
library(utils)
library(dplyr)
library(lubridate)
library(devtools)
library(readr)

# obtain the target countries list, flag information and code
source("/Users/supermonk00/Desktop/programing/R/Curfew-project/Covid-19 Bar chart race in Carib/countryname.picture.sort.R")

#insert data from European Centre for Disease Prevention and Control(ECDC)
#https://www.ecdc.europa.eu/en
rawdata = read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
                   na.strings = "", fileEncoding = "UTF-8-BOM")

ECDC.mining = function(savename){
  
  # select the caribbean country
  carib= rawdata %>% 
    dplyr::filter(geoId %in% code)
  
  # arrange the order by month/day
  carib.by.timeseries = carib %>%
    group_by(month,day)%>%
    arrange(month,day) %>%
    filter(cases != 0)
  
  # required package: lubridate::dmy
  day = seq(dmy(carib.by.timeseries$dateRep[1]),
            dmy(carib.by.timeseries$dateRep[nrow(carib.by.timeseries)]), "days") %>% 
    format(., "%d/%m/%Y") 
  
  # establish the platform for filling the data
  case.fill.platform =  matrix(0, 
                               ncol = length(day), 
                               nrow = length(countryname)
  )
  Dataset = data.frame(countryname, code, flagaddress, case.fill.platform )
  colnames(Dataset) = c("Country","geoID","Flag",day)
  
  # every cases, fill in to their belonging country x day
  
  for (i in 1:nrow(carib.by.timeseries)){
    Dataset[Dataset$geoID == as.character(carib.by.timeseries$geoId[i]),
            which(colnames(Dataset) == carib.by.timeseries[i,]$dateRep)] = carib.by.timeseries$cases[i]
  }
  # culumative: today's cases = yesterday's + today's
  for (j in 5:(ncol(Dataset))) {
    Dataset[,j] = Dataset[,j]+ Dataset[,j-1]
  }
  write.csv(Dataset, savename, row.names = FALSE,)
}
  
ECDC.mining(savename = "covid19.2020.06.01")



 





