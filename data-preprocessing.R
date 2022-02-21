
#Loading libraries

library(utils)
library(httr)
library(dplyr)
library(tidyr)
library(rdrop2)
library(readr)
library(ISOweek)

#Helper function that mutates a dataframe rows that meet some criteria. It extends the functionality of the dplyr mutate functions.This function was obtained from StackOverflow
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
    condition <- eval(substitute(condition), .data, envir)
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
}

library(utils)

#read the Dataset sheet into “R”. The dataset will be called "data".
#data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")

#download the dataset from the ECDC website to a local  file called covid19.csv
#GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk("covid19.csv",overwrite = TRUE))


#Read csv file downloaded

data = data%>%mutate(country = as.character(country))%>%
  filter(!grepl("(total)",country))
#

#Read csv file with country names in Portuguese 
countries_names_pt =read.csv("./world-countries/data/pt/world.csv",encoding="UTF-8")
##Select only country name and 3 letters code
countries_names_pt =countries_names_pt %>% select(name,alpha3)%>%mutate(alpha3 = as.character(toupper(alpha3)))


#Read csv file with country names in Spanish
countries_names_es =read.csv("./world-countries/data/es/world.csv",encoding="UTF-8")
##Select only country name and 3 letters code
countries_names_es =countries_names_es %>% select(name,alpha3)%>%mutate(alpha3 = as.character(toupper(alpha3)))

#Join the dataset with the country names in Portuguese and Spanish
countries_names = left_join(countries_names_pt,countries_names_es,by=c("alpha3"="alpha3"))
countries_names = countries_names%>%rename(CountryName_pt = name.x, CountryName_es=name.y) #Rename country names columns

#join covid data with the dataframe containing the country's names
new_data = left_join(data,countries_names,by=c("country_code"="alpha3"))


#Try to use the name found in countriesAndTerritories for the countries for which the code did not match.
new_data = new_data%>%mutate_cond(is.na(CountryName_pt),CountryName_pt = country)
new_data = new_data%>%mutate_cond(is.na(CountryName_es),CountryName_es = country)
#Eliminate the observations whose names where not found. 
new_data = new_data%>%filter(!is.na(CountryName_pt)|!is.na(CountryName_es))
#new_data = new_data%>%rename(cases = cases_weekly,deaths=deaths_weekly)


library(tidyr)

new_data = new_data%>%
  pivot_wider(names_from=indicator,values_from=c(weekly_count, rate_14_day, cumulative_count))

new_data$year_week = str_replace(new_data$year_week,"-","-W") 
new_data$year_week = paste(new_data$year_week,"4",sep="-")
new_data$year_week = ISOweek2date(new_data$year_week)


new_data = new_data%>%
              mutate(rate_14_day_cases=round(rate_14_day_cases*population/100000/14),
                     rate_14_day_deaths=round(rate_14_day_deaths*population/1000000/14))
#Output the file
write_csv(new_data,"covid19.csv")

## Add the data to the dropbox file

#Send old file to an specific folder
drop_copy(from_path ="coronavirus/covid19.csv",to_path="coronavirus/old/covid19.csv",autorename = TRUE )
#Eliminate actual file
drop_delete(path="coronavirus/covid19.csv")
#Upload new file
drop_upload(file="covid19.csv",path="coronavirus/",mode="overwrite",autorename=FALSE)

new_data = drop_read_csv("coronavirus/covid19.csv",dtoken = token,encoding = "UTF-8",stringsAsFactors =FALSE)

########################

new_data$rate_14_day_cases


new_data$year_week
new_data$dateRep = as.Date(new_data$year_week)
tabdata = new_data%>%filter(dateRep==max(dateRep))%>%
  arrange(desc(cumulative_count_cases))%>%
  transmute("País"=CountryName_pt,
            "Número de Casos Totais"= cumulative_count_cases,
            "Número de Mortes Totais" = cumulative_count_deaths)
tabdata
new_data%>%filter(CountryName_pt == "Brasil")%>%select(country_code,year_week,population,weekly_count_cases,rate_14_day_cases)%>%arrange(desc(year_week))
qnew_data$dateRep
max(new_data$dateRep)
as.Date(new_data$year_week,"%G-%V-%u")
new_data$year_week
year_week
ISOweek2date(new_data$year_week)
library(stringr)
str_extract_all(new_data$year_week,"[0-9]+",TRUE)

new_data%>%sele
