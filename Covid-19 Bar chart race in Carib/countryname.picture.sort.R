library(rvest)
library(git2r)
library(dplyr)
library(stringr)

# sort interested country list from wiki
countryname = read_html("https://en.wikipedia.org/wiki/List_of_Caribbean_countries_by_population") %>% 
  html_nodes("table") %>% 
  { .[[1]] } %>% 
  html_table() %>% 
  #generate new column, based on Country(or dependent territory). Remove parentheses.
  dplyr::mutate(Country = sub("\\(.*\\)", "", gsub("\\[.*\\]", "", `Country(or dependent territory)`))) %>%
  # trim the undifined countries in the list ###
  dplyr::filter(Country != "Saint Martin ",Country!="Total")%>%
  dplyr::select(Country) %>%
  distinct() 

# tranform data.frame to character, remove space
countryname  = gsub("^\\s|\\s$", "", countryname$Country) 





# obtain ISO Alpha-2 Numeric Country Codes 
# package citation: https://cran.r-project.org/web/packages/countrycode/countrycode.pdf
library(countrycode)
code = countrycode(c(countryname),
            origin = 'country.name', destination = 'iso3c') %>%
  toupper() #Captial letter to small letter


# obtain the url address by replacing the country code
flagaddress = c("https://www.countryflags.io/countrycode/flat/64.png") %>% 
  str_replace(., "countrycode", code)


         