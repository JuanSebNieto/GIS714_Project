
#'[Investigating long-term South African Weather Service (SAWS) data.]

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

#For fast modeling
library(fixest)
library(ranger)

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
  geom_sf(data = saws_shp, color = "black", size = 4) +
  geom_sf(data = w_shp, color = "black", fill = NA) +
  theme_classic() +
  theme(axis.text = element_text(size = 30),
        axis.title = element_blank()) +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(n.breaks = 6) +
  coord_sf(xlim = c(16, 33),ylim = c(-35, -22))

rm(saws_shp)
rm(df_key, path)

#Reading SAWS Data----

#'[SAWS long-term data.]

psum_df <- read.csv("SAWS\\Rain_1950to2000.csv", check.names = FALSE)
tmax_df <- read.csv("SAWS\\TMax_1950to2000.csv", check.names = FALSE)
tmin_df <- read.csv("SAWS\\TMin_1950to2000.csv", check.names = FALSE)
psum_df <- psum_df[, -1]
tmax_df <- tmax_df[, -1]
tmin_df <- tmin_df[, -1]

#Pivotting each variable into long-format.
psum_df %<>% pivot_longer(., cols = -c("Date", "Year", "Month", "Day"), names_to = "ID", values_to = "PSUM") 
tmax_df %<>% pivot_longer(., cols = -c("Date", "Year", "Month", "Day"), names_to = "ID", values_to = "TMAX") 
tmin_df %<>% pivot_longer(., cols = -c("Date", "Year", "Month", "Day"), names_to = "ID", values_to = "TMIN") 

#Joining the different variables into one dataframe.
saws_df <- full_join(psum_df, tmax_df, by = c("ID", "Date", "Year", "Month", "Day"))
saws_df <- full_join(saws_df, tmin_df, by = c("ID", "Date", "Year", "Month", "Day"))
rm(psum_df, tmax_df, tmin_df)

saws_df %<>% select(ID, Date, Year, Month, Day, PSUM, TMAX, TMIN)

#Getting station IDs.
id <- unique(saws_df$ID)
id <- gsub("-", " ", id)
id <- gsub(" AW", "AW", id)
id <- gsub(" BW", "BW", id)

#Only extracting weather stations found in the long-term data.
shp <- key_shp[match(id, key_shp$ID), ]
vect <- vect(shp)
rm(shp, id)

#Reading ERA5 Data----

#'[ERA5 long-term data.]

years <- c(1950:2025)

#PSUM
psum <- rast(paste0("netcdfs\\PSum\\era5_", years, ".nc"))
psum <- psum * 1000
names(psum) <- seq(as.Date("1950-01-01"), as.Date("2025-12-31"), by = "day")
time(psum) <- seq(as.Date("1950-01-01"), as.Date("2025-12-31"), by = "day")

#TMAX
tmax <- rast(paste0("netcdfs\\TMax\\era5_", years, ".nc"))
tmax <- tmax - 273.15
names(tmax) <- seq(as.Date("1950-01-01"), as.Date("2025-12-31"), by = "day")
time(tmax) <- seq(as.Date("1950-01-01"), as.Date("2025-12-31"), by = "day")

#TMIN
tmin <- rast(paste0("netcdfs\\TMin\\era5_", years, ".nc"))
tmin <- tmin - 273.15
names(tmin) <- seq(as.Date("1950-01-01"), as.Date("2025-12-31"), by = "day")
time(tmin) <- seq(as.Date("1950-01-01"), as.Date("2025-12-31"), by = "day")

# writeRaster(tmax, "rasters/tmax_long.tif", overwrite = TRUE)
# writeRaster(tmin, "rasters/tmin_long.tif", overwrite = TRUE)
# writeRaster(psum, "rasters/psum_long.tif", overwrite = TRUE)

rm(tmax, tmin, psum, years)

#Extracting ERA5 at SAWS----

psum <- rast("rasters/psum_long.tif")
tmax <- rast("rasters/tmax_long.tif")
tmin <- rast("rasters/tmin_long.tif")

#Summarising to a monthly time-step.
# psum_month <- tapp(psum, format(time(psum), "%Y-%m"), sum)
# tmax_month <- tapp(tmax, format(time(tmax), "%Y-%m"), mean)
# tmin_month <- tapp(tmin, format(time(tmin), "%Y-%m"), mean)
# names(psum_month) <- format(time(psum), "%Y-%m") %>% unique
# names(tmax_month) <- format(time(tmax), "%Y-%m") %>% unique
# names(tmin_month) <- format(time(tmin), "%Y-%m") %>% unique
# writeRaster(psum_month, "PSum_ERA5.tif", overwrite=TRUE)
# writeRaster(tmax_month, "TMax_ERA5.tif", overwrite=TRUE)
# writeRaster(tmin_month, "TMin_ERA5.tif", overwrite=TRUE)
# rm(psum_month, tmax_month, tmin_month)

era5 <- c(psum, tmax, tmin)
rm(psum, tmax, tmin)

#Extracting raster values at each station.
#This takes about a minute
era5_extract <- terra::extract(era5, vect)
era5_extract %<>% as.data.frame()
era5_extract$ID <- vect$ID
rm(vect, era5)

#Each variable can be extracted as below:
psum <- era5_extract[, c(1, 2:27760)]
tmax <- era5_extract[, c(1, 27761:55519)]
tmin <- era5_extract[, c(1, 55520:83278)]

psum %<>% pivot_longer(., cols = -c("ID"), names_to = "Date", values_to = "PSUM") 
tmax %<>% pivot_longer(., cols = -c("ID"), names_to = "Date", values_to = "TMAX") 
tmin %<>% pivot_longer(., cols = -c("ID"), names_to = "Date", values_to = "TMIN") 

#Joining the different variables into one dataframe.
era5_extract <- full_join(psum, tmax, by = c("ID", "Date"))
era5_extract <- full_join(era5_extract, tmin, by = c("ID", "Date"))
rm(psum, tmax, tmin)

#Adding altitude.
era5_extract$ALT <- key_shp$ALT[match(era5_extract$ID, key_shp$ID)] %>% st_drop_geometry()

#This takes about a minute.
era5_extract$ID <- gsub(" ", "-", era5_extract$ID)
era5_extract$ID <- gsub("AW", "-AW", era5_extract$ID)
era5_extract$ID <- gsub("BW", "-BW", era5_extract$ID)

#Joining the two datasets----

#'[Joining the ERA5 data with the SAWS data.]

matched_df <- left_join(era5_extract, saws_df, by = c("ID", "Date"))
colnames(matched_df) <- c("ID", "Date", "PSUMe", "TMAXe", "TMINe", "ALT", "Year", "Month", "Day", "PSUMs", "TMAXs", "TMINs")
matched_df %<>% select(ID, ALT, Date, Year, Month, Day, Year, PSUMe, PSUMs, TMAXe, TMAXs, TMINe, TMINs)
matched_df %<>% mutate(across(c(PSUMe, PSUMs, TMAXe, TMAXs, TMINe, TMINs), ~round(., 2)))
rm(era5_extract, saws_df)

matched_df %<>% filter(!is.na(Year))

head(matched_df)
str(matched_df)

#Percentage missing values per variable.
sum(is.na(matched_df$PSUMs))/nrow(matched_df)
sum(is.na(matched_df$TMAXs))/nrow(matched_df)
sum(is.na(matched_df$TMINs))/nrow(matched_df)

saws <- read.csv("SAWS\\saws.csv", sep = ",")
colnames(saws)[5] <- "ID"

matched_df %<>% left_join(., saws[, c("ID", "LATDD", "LNGDD", "Slope", "Aspect")], by = "ID")
rm(saws)

#Bringing in rainfall occurence.
# matched_df$PSUMs_Occ <- ifelse(matched_df$PSUMs != 0, 1, 0)
# matched_df$PSUMe_Occ <- ifelse(matched_df$PSUMe != 0, 1, 0)
# 
# tab <- as.data.frame(table(matched_df$PSUMs_Occ, matched_df$PSUMe_Occ))
# colnames(tab) <- c("AWS", "OS", "Freq")
# tab$AWS <- factor(tab$AWS)
# tab$OS  <- factor(tab$OS)
# tab$Perc <- tab$Freq / sum(tab$Freq) * 100
# 
# ggplot(tab, aes(x = OS, y = AWS, fill = Perc)) +
#   geom_tile() +
#   geom_text(aes(label = paste0(round(Perc, 1), "%")), col = "white", size = 20) +
#   labs(x = "Open-Source", y = "AWS", fill = "%") +
#   theme_classic() +
#   theme(legend.position = "none")
# rm(tab)

# plot(matched_df$TMAXe, matched_df$TMAXs)
# abline(a = 0, b = 1, col = "red", lwd = 5)

# plot(matched_df$TMINe, matched_df$TMINs)
# abline(a = 0, b = 1, col = "red", lwd = 5)

# plot(matched_df$PSUMe, matched_df$PSUMs)
# abline(a = 0, b = 1, col = "red", lwd = 5)

#Modelling SAWS by ERA5----

#This is a faster modeling method for huge datasets.
#Everything after the bar (|) is considered a fixed effect.
?feols

#'[Temperature.]

#Bias-correcting TMAX.
cor(matched_df$TMAXe, matched_df$TMAXs, use = "complete.obs")
model <- feols(TMAXs ~ TMAXe, data = matched_df)
summary(model)
model$coeftable
matched_df$TMAXbc <- predict(model, matched_df)

matched_df$diff <- matched_df$TMAXbc - matched_df$TMAXe
summary_df <- matched_df %>% group_by(Year, Month) %>% summarise(Diff = mean(diff, na.rm = TRUE))
summary_df$Date <- as.Date(paste0(summary_df$Year, "-", summary_df$Month, "-01"), format = "%Y-%m-%d")
ggplot(summary_df, aes(x = Date, y = Diff)) +
  geom_line()  +
  theme_classic(base_size = 48) +
  labs(x = "Date", y = "Mean Error (°C)") + 
  scale_x_date(date_breaks = "10 years", date_labels = "%Y-%m")

#Bias-correcting TMIN
cor(matched_df$TMINe, matched_df$TMINs, use = "complete.obs")
model <- feols(TMINs ~ TMINe, data = matched_df)
summary(model)
model$coeftable
matched_df$TMINbc <- predict(model, matched_df)

matched_df$diff <- matched_df$TMINbc - matched_df$TMINe
summary_df <- matched_df %>% group_by(Year, Month) %>% summarise(Diff = mean(diff, na.rm = TRUE))
summary_df$Date <- as.Date(paste0(summary_df$Year, "-", summary_df$Month, "-01"), format = "%Y-%m-%d")
ggplot(summary_df, aes(x = Date, y = Diff)) +
  geom_line()  +
  theme_classic(base_size = 48) +
  labs(x = "Date", y = "Mean Error (°C)") + 
  scale_x_date(date_breaks = "10 years", date_labels = "%Y")

#Notice here that ALT gets removed because it is perfectly colinear with ID.
#So we have to chose between either ALT or ID, not both.
#Using altitude will let us downscale to a DEM.
model <- feols(TMAXs ~ TMAXbc + ALT | ID , data = matched_df)
summary(model)

model <- feols(TMAXs ~ TMAXbc | ALT, data = matched_df)
model <- feols(TMAXs ~ TMAXbc + ALT, data = matched_df)
summary(model)
model$coefficients

#r.mapcalc "CoastTMAX = 5.5584955426 + 0.8239258717 * TMAX_08032022_resampled@Temperature + -0.0003534408 * SA_Coastal@DEMs"

model <- feols(TMINs ~ TMINe | ALT, data = matched_df)
model <- feols(TMINs ~ TMINe + ALT, data = matched_df)
summary(model)
model$coefficients


#'[Precipitation.]

#Rather than down-scaling a raster surface to a DEM, interpolate the weather station points....
#...to a new grid while taking into account the effect of elevation/aspect. 
#Options include: Spline with elevation, kridging with elevation.

#Linear model for precipitation.
cor(matched_df$PSUMe, matched_df$PSUMs, use = "complete.obs")
model <- feols(PSUMs ~ PSUMe + ALT + LATDD + LNGDD + Slope + Aspect, data = matched_df)
summary(model)
model$coeftable

#Bringing in rainfall occurence.
matched_df$PSUMs_Occ <- ifelse(matched_df$PSUMs != 0, 1, 0)
matched_df$PSUMe_Occ <- ifelse(matched_df$PSUMe != 0, 1, 0)

tab <- as.data.frame(table(matched_df$PSUMs_Occ, matched_df$PSUMe_Occ))
colnames(tab) <- c("AWS", "OS", "Freq")
tab$AWS <- factor(tab$AWS)
tab$OS  <- factor(tab$OS)
tab$Perc <- tab$Freq / sum(tab$Freq) * 100

cor(matched_df$PSUMs_Occ, matched_df$PSUMe_Occ, use = "complete.obs")
ggplot(tab, aes(x = OS, y = AWS, fill = Perc)) +
  geom_tile() +
  geom_text(aes(label = paste0(round(Perc, 1), "%")), col = "white", size = 30) +
  labs(x = "ERA5", y = "AWS", fill = "%") +
  theme_classic(base_size = 48) +
  theme(legend.position = "none")
rm(tab)

#Random forest for precipitation (takes about 10 minutes).
model <- ranger(PSUMs ~ PSUMe + ALT + LATDD + LNGDD + Slope + Aspect, data = na.omit(matched_df))
model$r.squared

#These models are clearly very weak. Hence, PSUM was summarized to a monthly time-step.

colnames(matched_df)
month_df <- matched_df %>% group_by(ID, ALT, LATDD, LNGDD, Slope, Aspect, Year, Month) %>% summarise(PSUMe = sum(PSUMe, na.rm = TRUE), PSUMs = sum(PSUMs, na.rm = TRUE))
head(month_df)

# plot(month_df$PSUMe, month_df$PSUMs)
# abline(a = 0, b = 1, col = "red", lwd = 5)

#Linear model for precipitation.
model <- feols(PSUMs ~ PSUMe + ALT + LATDD + LNGDD + Slope + Aspect, data = month_df)
summary(model)

#Random forest for precipitation (takes about 10 minutes).
model <- ranger(PSUMs ~ PSUMe + ALT + LATDD + LNGDD + Slope + Aspect, data = na.omit(month_df))
model$r.squared


#Copernicus----

sa_coast <- rast("C:/Users/Nietolaj/OneDrive - Mondigroup/NC State/Spring 2026/Geospatial Computation and Simulation/Project/Spatial Data/Copernicus/SA_Coastal.tif")
saws_shp <- st_read("C:\\Users\\Nietolaj\\OneDrive - Mondigroup\\Mondi\\ArcGIS\\Ilaria_LongTerm_AWS\\saws_station_data.shp")
saws_shp <- st_transform(saws_shp, crs(sa_coast))

# slope <- terrain(sa_coast, v = "slope",  unit = "degrees")
# aspect <- terrain(sa_coast, v = "aspect",  unit = "degrees")
# 
# pts <- vect(saws_shp)
# saws_shp$Slope <- extract(slope, pts)$slope
# saws_shp$Aspect <- extract(aspect, pts)$aspect
# 
# colnames(saws_shp)
# df <- as.data.frame(st_drop_geometry(saws_shp[, c("LATDD", "LNGDD", "STATION", "Oldcd_rain", "Oldcd_temp", "Slope", "Aspect")]))
# write.csv(df, "saws.csv")
# rm(pts, slope, aspect)

plot(sa_coast, plg = list(cex = 3))
plot(st_transform(w_shp, crs(sa_coast)), add = TRUE, colour = NA)
plot(saws_shp, add = TRUE, pch = 16)

ggplot() +
  geom_sf(data = aws_shp, color = "black", size = 4) +
  geom_sf(data = w_shp, color = "black", fill = NA) +
  theme_classic() +
  theme(axis.text = element_text(size = 30),
        axis.title = element_blank()) +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(n.breaks = 6) +
  coord_sf(xlim = c(16, 33),ylim = c(-35, -22))



#Plotting a shapefile of the stations.


