library(readr)
library(dplyr)
library(ggplot2)
Co2 = read_csv("C:/Users/User/Desktop/programing/R/Curfew-project/CO2 emission Bar chart race/C02 emission by sector.csv")

GHG = read_csv("C:/Users/User/Desktop/programing/R/Curfew-project/CO2 emission Bar chart race/carbon emission by sector.csv")

Methane = read_csv("C:/Users/User/Desktop/programing/R/Curfew-project/CO2 emission Bar chart race/Methane emission by sector.csv")

GHG_GHG = data.frame(GHG[,1],GHG[,ncol(GHG)],type = "GHG")

GHG_Methane = data.frame(Methane[,1],Methane[,ncol(Methane)]*25,type = "Methane")

GHG_Co2 = data.frame(Co2[,1],Co2[,ncol(Co2)],type = "CO2")

data = rbind(GHG_GHG,GHG_Methane,GHG_Co2) %>% filter(Sector %in% c("Agriculture","Electricity"))

data.adjr = rbind(GHG_GHG,GHG_Methane,GHG_Co2) %>% filter(Sector %in% c("Agriculture","Electricity"))

ggplot(data=data,aes(x = Sector,y=X2016,fill=type))+
  geom_bar(stat = "identity",position = "dodge") +
  labs(y = "emission(million tonnes)")




GHG_cal = rbind(GHG_Methane,GHG_Co2) %>% group_by(Sector) %>% summarize(X1990 = sum(X1990),
                                                                        X1991 = sum(X1991),
                                                                        X1992 = sum(X1992),
                                                                        X1993 = sum(X1993),
                                                                        X1994 = sum(X1994),
                                                                        X1995 = sum(X1995),
                                                                        X1996 = sum(X1996),
                                                                        X1997 = sum(X1997),
                                                                        X1998 = sum(X1998),
                                                                        X1999 = sum(X1999),
                                                                        X2000 = sum(X2000),
                                                                        X2001 = sum(X2001),
                                                                        X2002 = sum(X2002),
                                                                        X2003 = sum(X2003),
                                                                        X2004 = sum(X2004),
                                                                        X2005 = sum(X2005),
                                                                        X2006 = sum(X2006),
                                                                        X2007 = sum(X2007),
                                                                        X2008 = sum(X2008),
                                                                        X2009 = sum(X2009),
                                                                        X2010 = sum(X2010),
                                                                        X2011 = sum(X2011),
                                                                        X2012 = sum(X2012),
                                                                        X2013 = sum(X2013),
                                                                        X2014 = sum(X2014),
                                                                        X2015 = sum(X2015),
                                                                        X2016 = sum(X2016))

colnames(GHG_cal) = c("Sector","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000",
                      "2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011",
                      "2012","2013","2014","2015","2016")

write.csv(GHG_cal,file = "Methane+C02 emission by sector.csv",row.names = FALSE)
