####Carib confirmed cases of COVID-19
library(readr)
library(dplyr)
library(lubridate)
library(devtools)

# obtain the target countries list, flag information and code
devtools::source_url("https://raw.githubusercontent.com/supermonk00/Curfew-project/master/Covid-19%20Bar%20chart%20race%20in%20Carib/countryname%26picturesort.R")
#insert data from European Centre for Disease Prevention and Control(ECDC)
#https://www.ecdc.europa.eu/en


ECDCdataclean = function(
  ECDCdata = "csv.dms",
  countrycode = Capcode,
  country_flag= flagaddress,
  filename= "filename.csv",
  form ="culumative"           #individual or cumulative
){
  #insert data
  
  rawdata <- read_csv(ECDCdata)
  #sort the specific, interested countries
  
  source_case = function(geoID){
    carib = rawdata %>% 
      dplyr::filter(geoId == geoID)
    return(carib)
  }
  countrycode_matrix = matrix(c(countrycode),nrow=1)
  #layout the raw data: (dateRep, day, month, cases, deaths, countriesAndTerritories, geoId, countryterritory)
  
  infectionbycountry_Rawdata = lapply(countrycode_matrix,source_case)
  
  ###bind into an dataframe
  #wholedata set in one frame
  
  whole_data = c()
  for ( i in (1:length(infectionbycountry_Rawdata))) {  #Note:length(country_Rawdata) = how many countries
    whole_data = rbind(whole_data,infectionbycountry_Rawdata[[i]]) #automatically erased the undefined countries
  }
  # chose the virus settled date
  # check the fist case in Caribean area
  orderday = data.frame()
  for (i in min(whole_data$month):max(whole_data$month)) {
    for (j in 1:31) {
      a = whole_data[which(whole_data$month == i & 
                             whole_data$day == j & 
                             whole_data$cases != 0),]
      if (nrow(a) != 0){
        orderday = rbind(orderday,a)
      }
    }
  }


  # required package: lubridate::dmy
  day = seq(dmy(orderday$dateRep[1]),
            dmy(orderday$dateRep[nrow(orderday)]), "days") %>% 
    format(., "%d/%m/%Y")
  
  # establish the platform for filling the data
  platform =  matrix(0, ncol = length(day), nrow = length(countryname))
  dataset = data.frame(countryname, countrycode, country_flag, platform)
  colnames(dataset) = c("Country","geoID","Flag",day)
  
  # every cases, fill in to their belonging country x day
  
    for (i in 1:nrow(orderday)){
      dataset[dataset$geoID == orderday$geoId[i],
              which(colnames(dataset) == orderday[i,]$dateRep)] = orderday$cases[i]
    }
  if (form == "culumative") {
    for (j in 5:(length(day)+3)) {
      dataset[,j] = dataset[,j]+ dataset[,j-1]
    }
    write.csv(dataset, filename, row.names = FALSE)
  }
}

  





 





