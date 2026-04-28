
#'[Investigation long-term South African Weather Service (SAWS) data.]

#Loading packages and preferences----

library(sp)
library(sf)
library(raster)
library(terra)

library(rnaturalearth)
library(rnaturalearthdata)
w_shp <- ne_countries(scale = "medium", returnclass = "sf")
w_shp <- st_make_valid(w_shp)

library(tidyverse)
library(magrittr)
library(readxl)
library(RColorBrewer)
library(patchwork)

library(httr)
library(httr2)
library(jsonlite)

options(scipen = 999)
set.seed(1972)

#Reading & Cleaning Station Key----

#Path to the folder with all the data:
path <- "C:\\Users\\Nietolaj\\OneDrive - Mondigroup\\Mondi\\Soil Water Deficit\\long-term data\\All weather data\\LT data\\CLEAN_DATA"
list.files(path)

df_key <- read_xls(paste0(path, "\\Matching_Old_New_Codes.xls"))

paste(colnames(df_key), collapse = ", ")
df_key %<>% select(statonidc, station_name_control, lngc, latc, altc)
colnames(df_key) <- c("ID", "NAME", "LON", "LAT", "ALT")

#Coordinates are in minutes, and need to be converted to decimal degrees.
df_key$LON <- df_key$LON / 60
df_key$LAT <- -df_key$LAT / 60

df_key %<>% filter(!is.na(ID))
df_key %<>% unique()

#Plotting from coordinates in the key.
key_shp <- st_as_sf(df_key, coords = c("LON", "LAT"), crs = 4326)
ggplot() +
  geom_sf(data = key_shp, color = "black", size = 1.5) +
  geom_sf(data = w_shp, color = "black", fill = NA) +
  theme_classic(base_size = 20) +
  coord_sf(xlim = c(16, 33),ylim = c(-35, -22))

#Plotting a shapefile of the stations.
saws_shp <- st_read("C:\\Users\\Nietolaj\\OneDrive - Mondigroup\\Mondi\\ArcGIS\\Ilaria_LongTerm_AWS\\saws_station_data.shp")
ggplot() +
  geom_sf(data = saws_shp, color = "black") +
  geom_sf(data = w_shp, color = "black", fill = NA) +
  theme_classic() +
  coord_sf(xlim = c(16, 33),ylim = c(-35, -22))

rm(saws_shp)

#Rainfall from 1950 to 2000----

list.files(path)
list.files(paste0(path, "\\1950_2000"))

#Getting a list of all weather station ID's in the folder.
id_vector <- list.files(paste0(path, "\\1950_2000\\rainfall"))
id_vector <- gsub(".csv", "", id_vector)
id_vector <- gsub("-", " ", id_vector)
id_vector <- gsub(" AW", "AW", id_vector)
id_vector <- gsub(" BW", "BW", id_vector)
id_vector

#Finding the folder weather station ID's that match with those in the Key.
#Necessary, otherwise there is no way of knowing where it is located in space.
df <- data.frame(ID_in_File = id_vector, ID_in_Key = df_key$ID[match(id_vector, df_key$ID)])
df %<>% filter(!is.na(ID_in_Key))
id_vector <- df$ID_in_File
id_vector <- gsub(" ", "-", id_vector)
id_vector <- gsub("AW", "-AW", id_vector)
id_vector <- gsub("BW", "-BW", id_vector)
rm(df)

#Now id_vector has all weather station ID's that have rainfall data...
#...and that could be matched with a weather station in the key.

#Now to set up the database to extract rainfall.
df_rain <- data.frame(Date = seq(from = as.Date("1950-01-01"), to = as.Date("2000-07-31"), by = "day"))
df_rain %<>% mutate(., Year = year(Date), Month = month(Date), Day = day(Date))

i <- 1198

#Loops through the whole folder, combining each csv into one dataframe.
for(i in 1:length(id_vector)){
  
  df_csv <- read.csv(paste0(path, "\\1950_2000\\rainfall\\", id_vector[i], ".csv"), header = TRUE, check.names = FALSE)
  
  #The start and end years and months are in the column headings, unfortunately.
  #Furthermore, the end date is the last day of the month indicated as the end month.
  start_year  <- as.numeric(colnames(df_csv)[2]) %>% round(., 0)
  start_month <- as.numeric(colnames(df_csv)[3]) %>% round(., 0)
  end_year    <- as.numeric(colnames(df_csv)[4]) %>% round(., 0)
  end_month   <- as.numeric(colnames(df_csv)[5]) %>% round(., 0)
  
  start_date <- as.Date(ISOdate(start_year, start_month, 1))
  #Plus one month minus one day to get to the last day of the month.
  end_date <- as.Date(ISOdate(end_year, end_month, 1)) %m+% months(1) - days(1)
  dates <- seq(from = start_date, to = end_date, by = "day")
  
  df_csv <- as.data.frame(df_csv[, 1])
  colnames(df_csv) <- id_vector[i]
  df_csv$Date <- dates
  
  df_rain <- left_join(df_rain, df_csv, by = "Date")
  
}

#The only other thing to consider now, is are there some strange values?

rm(df_csv, start_year, start_month, end_year, end_month, start_date, end_date, dates, i, path)

write.csv(df_rain, "Rain_1950to2000.csv")
rm(df_rain, id_vector, df_key)

#Temperature from 1950 to 2000----

list.files(path)
list.files(paste0(path, "\\1950_2000"))

#Getting a list of all weather station ID's in the folder.
id_vector <- list.files(paste0(path, "\\1950_2000\\temperature"))
id_vector <- gsub(".csv", "", id_vector)
id_vector <- gsub("-", " ", id_vector)
id_vector <- gsub(" AW", "AW", id_vector)
id_vector <- gsub(" BW", "BW", id_vector)
id_vector

#Finding the folder weather station ID's that match with those in the Key.
#Necessary, otherwise there is no way of knowing where it is located in space.
df <- data.frame(ID_in_File = id_vector, ID_in_Key = df_key$ID[match(id_vector, df_key$ID)])
df %<>% filter(!is.na(ID_in_Key))
id_vector <- df$ID_in_File
id_vector <- gsub(" ", "-", id_vector)
id_vector <- gsub("AW", "-AW", id_vector)
id_vector <- gsub("BW", "-BW", id_vector)
rm(df)

#Now id_vector has all weather station ID's that have temperature data...
#...and that could be matched with a weather station in the key.

#Now to set up the database to extract temperature.
df_tmax <- data.frame(Date = seq(from = as.Date("1950-01-01"), to = as.Date("2000-07-31"), by = "day"))
df_tmin <- data.frame(Date = seq(from = as.Date("1950-01-01"), to = as.Date("2000-07-31"), by = "day"))
df_tmax %<>% mutate(., Year = year(Date), Month = month(Date), Day = day(Date))
df_tmin %<>% mutate(., Year = year(Date), Month = month(Date), Day = day(Date))

i <- 1

#Loops through the whole folder, combining each csv into one dataframe.
for(i in 1:length(id_vector)){
  
  df_csv <- read.csv(paste0(path, "\\1950_2000\\temperature\\", id_vector[i], ".csv"), header = TRUE, check.names = FALSE)
  
  #The start and end years and months are in the column headings, unfortunately.
  #Furthermore, the end date is the last day of the month indicated as the end month.
  start_year  <- as.numeric(colnames(df_csv)[2]) %>% round(., 0)
  start_month <- as.numeric(colnames(df_csv)[3]) %>% round(., 0)
  end_year    <- as.numeric(colnames(df_csv)[4]) %>% round(., 0)
  end_month   <- as.numeric(colnames(df_csv)[5]) %>% round(., 0)
  
  start_date <- as.Date(ISOdate(start_year, start_month, 1))
  #Plus one month minus one day to get to the last day of the month.
  end_date <- as.Date(ISOdate(end_year, end_month, 1)) %m+% months(1) - days(1)
  dates <- seq(from = start_date, to = end_date, by = "day")
  
  df <- as.data.frame(df_csv[, c(1:2)])
  df$Date <- dates
  
  df_tmax <- left_join(df_tmax, df[c(1, 3)], by = "Date")
  colnames(df_tmax)[ncol(df_tmax)] <- id_vector[i]
  df_tmin <- left_join(df_tmin, df[c(2, 3)], by = "Date")
  colnames(df_tmin)[ncol(df_tmin)] <- id_vector[i]
  
}

#The only other thing to consider now, is are there some strange values?

rm(df_csv, df, start_year, start_month, end_year, end_month, start_date, end_date, dates, i, path)

write.csv(df_tmax, "TMax_1950to2000.csv")
write.csv(df_tmin, "TMin_1950to2000.csv")
rm(df_tmax, df_tmin, id_vector, df_key)






