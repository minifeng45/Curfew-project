####Carib confirmed cases of COVID-19
library(utils)
library(tidyr)
library(lubridate)
library(devtools)
library(readr)



#insert data from European Centre for Disease Prevention and Control(ECDC)
#https://www.ecdc.europa.eu/en
rawdata = read.csv("C:/Users/User/Desktop/programing/R/Curfew-project/CO2 emission Bar chart race/methane-emissions-by-sector.csv", 
                   na.strings = "", fileEncoding = "UTF-8-BOM")

colnames(rawdata) = c("country","code","year","agriculture","fugitive","industry",
                      "landuse","otherfuel","waste")
rawdata[is.na(rawdata)] = 0

annual = rawdata %>% group_by(year) %>% summarise(Agriculture = sum(agriculture),
                                         Industry = sum(industry),
                                         Fugitive = sum(fugitive),
                                         Landuse = sum(landuse),
                                         Otherfuel = sum(otherfuel),
                                         Waste = sum(waste))

case.fill.platform =  matrix(0, 
                             ncol = length(annual$year), 
                             nrow = length(colnames(annual)[-1])
)
Dataset = data.frame(colnames(annual)[-1], case.fill.platform )
colnames(Dataset) = c("Sector",annual$year)

for (i in 1:nrow(Dataset)) {
  for (j in 2:ncol(Dataset)) {
    Dataset[i,j] = annual[j-1,i+1]
  }
}

for (j in 3:(ncol(Dataset))) {
  Dataset[,j] = Dataset[,j]+ Dataset[,j-1]
}


write.csv(Dataset,file = "Methane emission by sector.csv",row.names = FALSE)
