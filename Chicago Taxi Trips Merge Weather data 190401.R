#==================================================================#
# Applied Analytics: the Machine Learning Pipeline
# Creation date: April 01, 2019
# Last modification: April 02, 2019
# Created by: David Mitre Becerril                                 
# Objective: Retrieved and process the NOAA weather data for the City of Chicago in 2017
# Sources used: https://www.ncei.noaa.gov/data/global-hourly/doc/isd-format-document.pdf
#               https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/LCD_documentation.pdf"
# New variables:
# temp: the temperature of the air in celsius degrees
# rain: precipitation in millimeters per hour
#==================================================================#


# 1) Install libraries ------
packages<-c("rnoaa", "dplyr", "stringr")
lapply(packages, require, character.only=TRUE)
rm(packages)


# 2) Read Chicago taxi trips and merge it with weather data------
#Read Chicago dataset
df <- read.csv("data_2017.csv")
df$month <- as.integer(substr(df$TripStartTimestamp,1,2))
df$day <- as.integer(substr(df$TripStartTimestamp,4,5))
df$hour <- as.integer(substr(df$TripStartTimestamp,12,13))
df$morning <- substr(df$TripStartTimestamp,21,22)
df$hour <- ifelse(df$morning%in%"AM" & df$hour==12, 0, 
                  ifelse(df$morning%in%"PM" & df$hour!=12, df$hour+12, df$hour)) #transform 12-hour to 24 hours format
df$morning <- NULL

#Download the NOAA data
weather <- lcd("72530094846", year=2017) ##weather station id; only keep the first 20 columns
weather <- weather %>% select(date, tmp, aa1) %>% as.data.frame()

#Extract hour and month from date field
weather$month <- as.integer(substr(weather$date,6,7))
weather$day <- as.integer(substr(weather$date,9,10))
weather$hour <- as.integer(substr(weather$date,12,13))

#Extract temperature features (temperature of the air in celsius degrees)
weather$temp <- substr(weather$tmp,1,5) 
weather$temp <- ifelse(weather$temp=="+9999", NA, weather$temp)
weather$temp <- as.numeric(weather$temp)/10 #to remove the scaling factor of 10

#Extract precipitation features (precipitation in millimeters per hour)
weather$rain_hour <- substr(weather$aa1,1,2)
weather$rain_hour <- as.integer(ifelse(weather$rain_hour=="99", NA, weather$rain_hour))
weather$rain_mm <- substr(weather$aa1,4,7)
weather$rain_mm <- as.integer(ifelse(weather$rain_mm=="9999", NA, weather$rain_mm))/10 #to remove the scaling factor of 10
weather$rain <- with(weather, ifelse(rain_hour==0, rain_mm, rain_mm/rain_hour))
weather$rain_hour <- NULL
weather$rain_mm <- NULL

#Aggregate the average per station, month, day, and hour for each new feature
temp <- weather %>%
  select(-date, -tmp, -aa1) %>%
  group_by(month, day, hour) %>%
  summarise(temp_hour=mean(temp, na.rm = TRUE), rain_hour=mean(rain, na.rm = TRUE)) %>% as.data.frame()
temp2 <- weather %>%
  select(-date, -tmp, -aa1, -hour) %>%
  group_by(month, day) %>%
  summarise(temp_sdday=sd(temp, na.rm = TRUE), rain_sdday=sd(rain, na.rm = TRUE)) %>% as.data.frame()
temp[is.na(temp$rain_hour),] <- mean(temp$rain_hour, na.rm=TRUE) #impute a single NA

#Merge the weather stations with the Chicago dataset
df <- base::merge(df, temp, by=c("month", "day", "hour"))
df <- base::merge(df, temp2, by=c("month", "day"))

#Export file
write.csv(df, "data_2017_withweather.csv", row.names = FALSE)




