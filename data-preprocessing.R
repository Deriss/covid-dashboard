
#Loading libraries

library(utils)
library(httr)
library(dplyr)
library(tidyr)
library(rdrop2)
library(readr)
library(ISOweek)
library(stringr)

#Helper function that mutates a dataframe rows that meet some criteria. It extends the functionality of the dplyr mutate functions.This function was obtained from StackOverflow
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
    condition <- eval(substitute(condition), .data, envir)
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
}



#read the Dataset sheet 
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")

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

# separate cases and deaths measures into their own columns
new_data = new_data%>%
  pivot_wider(names_from=indicator,values_from=c(weekly_count, rate_14_day, cumulative_count))
# Format the year_week variable for using Issoweek2date function
new_data$year_week = str_replace(new_data$year_week,"-","-W") 
new_data$year_week = paste(new_data$year_week,"4",sep="-")
# Data to date format
new_data$year_week = ISOweek2date(new_data$year_week)

# from rate, calculate the total number of cases/deaths per day per country
new_data = new_data%>%
              mutate(rate_14_day_cases=round(rate_14_day_cases*population/100000/14),
                     rate_14_day_deaths=round(rate_14_day_deaths*population/1000000/14))

## Add data to Dropbox
write.csv(new_data,'covid19.csv')
#Send old file to an specific folder
drop_copy(from_path ="coronavirus/covid19.csv",to_path="coronavirus/old/covid19.csv",autorename = TRUE )
#Eliminate actual file
drop_delete(path="coronavirus/covid19.csv")
#Upload new file
drop_upload(path="coronavirus",file="covid19.csv",mode="overwrite",autorename=FALSE)

