

library(ecmwfr)

wf_set_key(user ="a32056b1-8447-46f1-b490-c76be1230afa", key = "397f64e4-6f78-4a44-834c-ddf303abfb75")

#Will need to change variables according to which dataset you want to use.
#Daily: https://cds.climate.copernicus.eu/datasets/derived-era5-single-levels-daily-statistics?tab=download
#All others: https://cds.climate.copernicus.eu/datasets

# years <- c(1950:1960)
# years <- c(1961:1970)
# years <- c(1971:1980)
# years <- c(1981:1990)
# years <- c(1991:2000)
# years <- c(2001:2010)
# years <- c(2011:2020)
years <- c(2021:2025)

#Temperature
variable <- "2m_temperature"
stat <- "daily_minimum"
# stat <- "daily_maximum"
# stat <- "daily_mean"

#Rainfall
# variable <- "total_precipitation"
# stat <- "daily_sum"

for(i in 1:length(years)){
  
  print(Sys.time())
  print(years[i])
  
  request <- list(dataset_short_name = "derived-era5-single-levels-daily-statistics",
                  product_type   = "reanalysis",
                  variable = variable,
                  year = c(as.character(years[i])),
                  month = c("01","02","03","04","05","06",
                            "07","08","09","10","11","12"),
                  day = sprintf("%02d", 1:31),
                  time = c("00:00"),
                  daily_statistic  = stat,
                  time_zone = "utc+00:00",
                  frequency = "1_hourly",
                  format = "netcdf",
                  # area is specified as N, W, S, E
                  area = c(-22, 16, -35, 33),
                  target = paste0("era5_", years[i], ".nc"))
  
  #Follow request progress at: https://cds.climate.copernicus.eu/requests?tab=all
  file <- wf_request(user = "a32056b1-8447-46f1-b490-c76be1230afa",
                     request = request,
                     transfer = TRUE,
                     path = "C:\\Users\\Nietolaj\\OneDrive - Mondigroup\\NC State\\Spring 2026\\Geospatial Computation and Simulation\\Project\\R Scripts\\netcdfs\\TMin",
                     verbose = TRUE)
  
}

rm(i, years)
rm(file, request)






