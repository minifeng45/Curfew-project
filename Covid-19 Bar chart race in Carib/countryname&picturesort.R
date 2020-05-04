library(rvest)
library(git2r)
library(dplyr)
library(countrycode)
library(stringr)

# sort interested country list from wiki
countryname <- read_html("https://en.wikipedia.org/wiki/List_of_Caribbean_countries_by_population") %>% 
  html_nodes("table") %>% 
  { .[[1]] } %>% 
  html_table() %>% 
  dplyr::mutate(Country = sub("\\(.*\\)", "", gsub("\\[.*\\]", "", `Country(or dependent territory)`))) %>% 
  dplyr::select(Country) %>% 
  distinct()

countryname =gsub("^\\s|\\s$", "", countryname$Country) 

###
#### trim the undifined countries in the list ###
###

countryname = countryname[-which(countryname == "Saint Martin"|
                                   countryname == "Total")] 



# obtain ISO Alpha-2 Numeric Country Codes 
# package citation: https://cran.r-project.org/web/packages/countrycode/countrycode.pdf




Capcode = countrycode(c(countryname),
            origin = 'country.name', destination = 'iso2c') 
code = tolower(Capcode) #Captial letter to small letter

# obtain the url address by replacing the country code
flagaddress = c("https://www.countryflags.io/countrycode/flat/64.png") %>% 
  str_replace(., "countrycode", code)


         