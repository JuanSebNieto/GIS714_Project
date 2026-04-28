
#'[Investigating short-term Mondi Automatic Weather Stations data.]

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

library(caret)
library(nnet)
library(randomForest)
library(ranger)
library(viridis)

options(scipen = 999)
set.seed(1972)

#Reading Mondi Station Data----

#Plotting Mondi AWS locations.
aws_df <- read.csv("MAWS\\AWS.csv", sep = ";")
aws_df$X %<>% gsub(",", ".", .) %>% as.numeric()
aws_df$Y %<>% gsub(",", ".", .) %>% as.numeric()
colnames(aws_df)[5] <- "AWSID"
aws_shp <- st_as_sf(aws_df, coords = c("X", "Y"), crs = 4326)
vect <- vect(aws_shp)

plantations <- st_read("C:\\Users\\Nietolaj\\OneDrive - Mondigroup\\NC State\\Spring 2026\\Geospatial Computation and Simulation\\Project\\Spatial Data\\Shapefiles\\Plantations\\PLANTATION.shp")

ggplot() +
  geom_sf(data = aws_shp, color = "black", size = 4) +
  geom_sf(data = w_shp, color = "black", fill = NA) +
  theme_classic() +
  theme(axis.text = element_text(size = 30),
        axis.title = element_blank()) +
  scale_x_continuous(breaks = seq(29, 33, by = 1)) +
  scale_y_continuous(breaks = seq(-30, -26, by = 1)) +
  coord_sf(xlim = c(29.05, 32.7), ylim = c(-30.5, -26.5))

ggplot() +
  geom_sf(data = w_shp, color = "black", fill = NA) +
  geom_sf(data = plantations, color = "black", fill = "black", size = 1.5) +
  geom_sf(data = aws_shp, color = "red", size = 3.5) +
  theme_classic() +
  theme(axis.text = element_text(size = 30),
        axis.title = element_blank()) +
  scale_x_continuous(breaks = seq(29, 33, by = 1)) +
  scale_y_continuous(breaks = seq(-30, -26, by = 1)) +
  coord_sf(xlim = c(29.05, 32.7), ylim = c(-30.5, -26.5))

ggplot() +
  geom_sf(data = aws_shp, color = "black", size = 4) +
  geom_sf(data = w_shp, color = "black", fill = NA) +
  theme_classic() +
  theme(axis.text = element_text(size = 30),
        axis.title = element_blank()) +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(n.breaks = 6) +
  coord_sf(xlim = c(16, 33),ylim = c(-35, -22))

#coord_sf(xlim = c(16, 33),ylim = c(-35, -22))
#coord_sf(xlim = c(29, 33),ylim = c(-31, -26))
#coord_sf(xlim = c(29, 33),ylim = c(-31, -26))

#Ntonjaneni: coord_sf(xlim = c(31.52, 31.08),ylim = c(-28.45, -28.7))

rm(plantations)

#Reading in all Mondi AWS readings.
maws <- read.csv("MAWS\\clim_data.csv", sep = ",", row.names = 1)
colnames(maws)[1] <- "AWSID"
colnames(maws)[2] <- "AWSNAME"
colnames(maws)[3] <- "Date"
colnames(maws)[4] <- "PSUMm"
colnames(maws)[5] <- "TMAXm"
colnames(maws)[6] <- "TMINm"
colnames(maws)[7] <- "EVAPm"
colnames(maws)[8] <- "RHMAXm"
colnames(maws)[9] <- "RHMINm"
colnames(maws)[10] <- "SRADm"
colnames(maws)[11] <- "WINDm"

maws <- merge(maws, aws_df[, c("AWSID", "Altitude")], by = "AWSID")
colnames(maws)[12] <- "ALT"


maws$Date %<>% as.Date(., format = "%d-%m-%Y")
maws %<>% mutate(YEAR = year(Date), MONTH = month(Date), DAY = day(Date))
maws %<>% select(AWSID, AWSNAME, ALT, YEAR, MONTH, DAY, PSUMm, TMAXm, TMINm, EVAPm, RHMAXm, RHMINm, SRADm, WINDm)

maws %<>% filter(YEAR >= 2017 & YEAR <= 2025)
maws %<>% filter(AWSNAME != "Nqabeni - Ravenhill Estate")
maws %<>% arrange(AWSID, YEAR, MONTH, DAY)

maws %>% group_by(AWSID, YEAR) %>% summarise(DAYS = length(DAY)) %>% arrange(DAYS)

maws %<>% mutate(across(c(AWSID, AWSNAME), as.factor))
maws %<>% mutate(across(c(YEAR, MONTH, DAY), as.integer))

str(maws)

rm(aws_df, aws_shp)

#Reading ERA5 Climate Data----

years <- c(2017:2025)

#TMAX
tmax <- rast(paste0("netcdfs/TMax/era5_", years, ".nc"))
tmax <- tmax - 273.15
names(tmax) <- seq(as.Date("2017-01-01"), as.Date("2025-12-31"), by = "day")
time(tmax) <- seq(as.Date("2017-01-01"), as.Date("2025-12-31"), by = "day")

#TMIN
tmin <- rast(paste0("netcdfs/TMin/era5_", years, ".nc"))
tmin <- tmin - 273.15
names(tmin) <- seq(as.Date("2017-01-01"), as.Date("2025-12-31"), by = "day")
time(tmin) <- seq(as.Date("2017-01-01"), as.Date("2025-12-31"), by = "day")

#PSUM
psum <- rast(paste0("netcdfs/PSum/era5_", years, ".nc"))
psum <- psum*1000
names(psum) <- seq(as.Date("2017-01-01"), as.Date("2025-12-31"), by = "day")
time(psum) <- seq(as.Date("2017-01-01"), as.Date("2025-12-31"), by = "day")

#By using the raster generated above without first writing it to and then reading it from a .tif file...
#...terra:resample() in the next tab would cause R to instantly abort due to how NetCDFs are held in memory.

# writeRaster(tmax, "rasters/tmax_short.tif", overwrite = TRUE)
# writeRaster(tmin, "rasters/tmin_short.tif", overwrite = TRUE)
# writeRaster(psum, "rasters/psum_short.tif", overwrite = TRUE)

rm(tmax, tmin, psum, years)

#Interpolating to smooth boundaries----

tmax <- rast("rasters/tmax_short.tif")
tmin <- rast("rasters/tmin_short.tif")
psum <- rast("rasters/psum_short.tif")

#Cropping to the extent of Mondi South Africa.
#tmax <- crop(tmax, ext(29.05, 32.7, -30.5, -26.5))

#Template with four new cells for each original cell.
template <- rast(extent = ext(tmax), resolution = res(tmax)/4, crs = crs(tmax))
?terra::resample

#Resampling with interpolation, only aligns horizontal borders of cells.
tmax <- terra::resample(tmax, template, method = "bilinear")
tmin <- terra::resample(tmin, template, method = "bilinear")
psum <- terra::resample(psum, template, method = "bilinear")

?resample

rm(template)

#Joining the two datasets----

#'[Extracting raster values at each station.]

psum_extract <- terra::extract(psum, vect)
psum_extract %<>% as.data.frame()
psum_extract$ID <- vect$AWSID

tmax_extract <- terra::extract(tmax, vect)
tmax_extract %<>% as.data.frame()
tmax_extract$ID <- vect$AWSID

tmin_extract <- terra::extract(tmin, vect)
tmin_extract %<>% as.data.frame()
tmin_extract$ID <- vect$AWSID

psum_extract %<>% pivot_longer(., cols = -c("ID"), names_to = "Date", values_to = "PSUMe") 
tmax_extract %<>% pivot_longer(., cols = -c("ID"), names_to = "Date", values_to = "TMAXe") 
tmin_extract %<>% pivot_longer(., cols = -c("ID"), names_to = "Date", values_to = "TMINe") 

era5 <- merge(tmax_extract, tmin_extract, by = c("ID", "Date"))
era5 <- merge(era5, psum_extract, by = c("ID", "Date"))
colnames(era5)[1] <- "AWSID"

rm(vect, psum_extract, tmax_extract, tmin_extract)

era5 %<>% mutate(YEAR = year(Date), MONTH = month(Date), DAY = day(Date))
era5 %<>% mutate(across(c(YEAR, MONTH, DAY), as.integer))

era5$AWSID %<>% as.factor()
era5 %<>% select(AWSID, YEAR, MONTH, DAY, PSUMe, TMAXe, TMINe)
era5 %<>% filter(YEAR >= 2017 & YEAR <= 2025)

str(era5)

#'[Joining the ERA5 data with the Mondi AWS data.]

matched_df <- left_join(maws, era5, by = c("AWSID", "YEAR", "MONTH", "DAY"))
matched_df %<>% mutate(across(c(PSUMm:TMINe), ~round(., 2)))

#Modelling MAWS by ERA5----

#'[Daily TMAX]

#TMAX
tmax_model <- lm(TMAXm ~ TMAXe, matched_df)
summary(tmax_model)
tmax_model

cor(matched_df$TMAXm, matched_df$TMAXe)
ggplot(matched_df, aes(x = TMAXe, y = TMAXm)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 2) +
  theme_classic(base_size = 48) +
  labs(x = "ERA5 Maximum Temperature (°C)", y = "AWS Maximum Temperature (°C)")

#plot(matched_df$TMAXe, matched_df$TMAXm)
#abline(tmax_model, col = "red", lwd = 5)

#Applying model to each day.
matched_df$TMAXbc <- predict(tmax_model, matched_df)
pred_tmax <- app(tmax, function(x){as.numeric(tmax_model$coefficients[1]) + as.numeric(tmax_model$coefficients[2]) * x})
time(pred_tmax) <- as.Date(names(pred_tmax))

diff <- pred_tmax - tmax
time_bias <- global(diff, mean, na.rm = TRUE)
time_bias$Date <- as.Date(rownames(time_bias), format = "%Y-%m-%d")
ggplot(time_bias, aes(x = Date, y = mean)) +
  geom_line()  +
  theme_classic(base_size = 48) +
  labs(x = "Date", y = "Mean Error (°C)")

#Summarizing to a monthly level.
pred_tmax_month <- tapp(pred_tmax, format(time(pred_tmax), "%Y-%m"), mean)
names(pred_tmax_month) <- format(time(pred_tmax), "%Y-%m") %>% unique
#writeRaster(pred_tmax_month, "TMAX_BC.tif", overwrite=TRUE)
rm(tmax_model, pred_tmax, pred_tmax_month)

tmax_month <- tapp(tmax, format(time(tmax), "%Y-%m"), mean)
names(tmax_month) <- format(time(tmax), "%Y-%m") %>% unique
#writeRaster(tmax_month, "TMAX_OS.tif", overwrite=TRUE)
rm(tmax_month)

#'[Daily TMIN]

#TMIN
tmin_model <- lm(TMINm ~ TMINe, matched_df)
summary(tmin_model)
tmin_model

cor(matched_df$TMINm, matched_df$TMINe)
ggplot(matched_df, aes(x = TMINe, y = TMINm)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 2) +
  theme_classic(base_size = 48) +
  labs(x = "ERA5 Minimum Temperature (°C)", y = "AWS Minimum Temperature (°C)")
#plot(matched_df$TMINe, matched_df$TMINm)
#abline(tmin_model, col = "red", lwd = 5)

#Applying model to each day.
matched_df$TMINbc <- predict(tmin_model, matched_df)
pred_tmin <- app(tmin, function(x){as.numeric(tmin_model$coefficients[1]) + as.numeric(tmin_model$coefficients[2]) * x})
time(pred_tmin) <- as.Date(names(pred_tmin))

diff <- pred_tmin - tmin
time_bias <- global(diff, mean, na.rm = TRUE)
time_bias$Date <- as.Date(rownames(time_bias), format = "%Y-%m-%d")
ggplot(time_bias, aes(x = Date, y = mean)) +
  geom_line()  +
  theme_classic(base_size = 48) +
  labs(x = "Date", y = "Mean Error (°C)")

#Summarizing to a monthly level.
pred_tmin_month <- tapp(pred_tmin, format(time(pred_tmin), "%Y-%m"), mean)
names(pred_tmin_month) <- format(time(pred_tmin), "%Y-%m") %>% unique
#writeRaster(pred_tmin_month, "TMIN_BC.tif", overwrite=TRUE)
rm(tmin_model, pred_tmin, pred_tmin_month)

tmin_month <- tapp(tmin, format(time(tmin), "%Y-%m"), mean)
names(tmin_month) <- format(time(tmin), "%Y-%m") %>% unique
#writeRaster(tmin_month, "TMIN_OS.tif", overwrite=TRUE)
rm(tmin_month)

rm(diff, time_bias)

#Temperature Lapse-Rate----

# writeRaster(tmax[[1893]], "TMAX_08032022_resampled_again.tif", overwrite=TRUE)
# writeRaster(tmin[[1893]], "TMIN_08032022.tif", overwrite=TRUE)

#'[Temperature lapse-rate by altitude.]

#Daily lapse-rate
lapse_df <- matched_df %>% 
  group_by(AWSID, AWSNAME, ALT, YEAR, MONTH, DAY) %>% 
  summarise(TMAXm = mean(TMAXm, na.rm = TRUE),
            TMINm = mean(TMINm, na.rm = TRUE),
            TMAXe = mean(TMAXe, na.rm = TRUE),
            TMINe = mean(TMINe, na.rm = TRUE),
            TMAXbc = mean(TMAXbc, na.rm = TRUE),
            TMINbc = mean(TMINbc, na.rm = TRUE)) 

#'[TMAX Lapse Rate.]    

#Modelling by altitude.                                                   
model <- lm(TMAXm ~ ALT + TMAXbc, lapse_df)
summary(model)
model$coefficients

#Reorganizing for a rules file.
coefs <- model$coefficients
aws_coefs <- coefs[grep("^AWSID", names(coefs))]
aws_coefs[is.na(aws_coefs)] <- 0
ids <- sub("AWSID", "", names(aws_coefs))
output <- paste0(ids, ":", ids, ":", sprintf("%.10f", aws_coefs))
cat(output, sep = "\n")

#'[TMIN Lapse Rate.]                                                            

#Modelling by altitude. 
model <- lm(TMINm ~ ALT + TMINbc, lapse_df)
summary(model)
model$coefficients

#Reorganizing for a rules file.
coefs <- model$coefficients
aws_coefs <- coefs[grep("^AWSID", names(coefs))]
aws_coefs[is.na(aws_coefs)] <- 0
ids <- sub("AWSID", "", names(aws_coefs))
output <- paste0(ids, ":", ids, ":", sprintf("%.10f", aws_coefs))
cat(output, sep = "\n")

rm(coefs, aws_coefs, ids, output)
rm(lapse_df, model)


#Precipitation Modelling----

#Rather than down-scaling a raster surface to a DEM, interpolate the weather station points....
#...to a new grid while taking into account the effect of elevation/aspect. 
#Options include: Spline with elevation, kridging with elevation.

#Summarizing to a monthly level.
psum_month <- tapp(psum, format(time(psum), "%Y-%m"), sum)
names(psum_month) <- format(time(psum), "%Y-%m") %>% unique
#writeRaster(psum_month[[63]], "PSUM_032022_resampled_again.tif", overwrite=TRUE)
rm(psum_month)

#writeRaster(psum[[1893]], "PSUM_08032022_resampled.tif", overwrite=TRUE)

#'[Based on the work of Jian et al., 2025]
#In Science Direct use keywords: downscale AND rainfall

#Daily lapse-rate
lapse_df <- matched_df %>% 
  group_by(AWSID, AWSNAME, ALT, YEAR, MONTH) %>% 
  summarise(PSUMm = sum(PSUMm, na.rm = TRUE),
            PSUMe = sum(PSUMe, na.rm = TRUE),
            RHMAXm = mean(RHMAXm, na.rm = TRUE),
            RHMINm = mean(RHMINm, na.rm = TRUE),
            WINDm = mean(WINDm, na.rm = TRUE)) 

cor(lapse_df$PSUMm, lapse_df$PSUMe)
ggplot(lapse_df, aes(x = PSUMe, y = PSUMm)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 2) +
  theme_classic(base_size = 48) +
  labs(x = "ERA5 Precipitation (mm)", y = "AWS Precipitation (mm)")

#Bringing in rainfall occurence.
lapse_df$PSUMm_Occ <- ifelse(lapse_df$PSUMm != 0, 1, 0)
lapse_df$PSUMe_Occ <- ifelse(lapse_df$PSUMe != 0, 1, 0)
table(lapse_df$PSUMm_Occ, lapse_df$PSUMe_Occ)

tab <- as.data.frame(table(lapse_df$PSUMm_Occ, lapse_df$PSUMe_Occ))
colnames(tab) <- c("AWS", "OS", "Freq")
tab$AWS <- factor(tab$AWS)
tab$OS  <- factor(tab$OS)
tab$Perc <- tab$Freq / sum(tab$Freq) * 100

cor(lapse_df$PSUMm_Occ, lapse_df$PSUMe_Occ)
ggplot(tab, aes(x = OS, y = AWS, fill = Perc)) +
  geom_tile() +
  geom_text(aes(label = paste0(round(Perc, 1), "%")), col = "white", size = 30) +
  labs(x = "ERA5", y = "AWS", fill = "%") +
  theme_classic(base_size = 48) +
  theme(legend.position = "none")
rm(tab)

#Reading in slope and aspect.
aws <- read.csv("C:\\Users\\Nietolaj\\OneDrive - Mondigroup\\NC State\\Spring 2026\\Geospatial Computation and Simulation\\Project\\Spatial Data\\AWS.csv")
colnames(aws)[6] <- "AWSID"
aws$AWSID %<>% as.factor()

lapse_df <- left_join(lapse_df, aws[, c("AWSID", "SLOPE", "ASPECT", "X", "Y")], by = "AWSID")
colnames(lapse_df)
rm(aws)

#'[PSUM Lapse Rate.]    

#Modelling rainfall amount by altitude.                                                   
model <- lm(PSUMm ~ ALT + SLOPE + ASPECT + PSUMe + RHMAXm + RHMINm + WINDm + X + Y, lapse_df)
model <- lm(PSUMm ~ ALT + SLOPE + ASPECT + PSUMe, lapse_df)
summary(model)
model$coefficients

lapse_df$PSUMbc <- predict(model, lapse_df)
model <- lm(PSUMm ~ ALT + SLOPE + ASPECT + PSUMbc, lapse_df)
summary(model)
model$coefficients

#r.mapcalc "Prec = 22.445866598 + 0.954723222*PSUM_032022_resampled_again@Precipitation + -0.013906041*Zululand_DEM@DEMs + 0.009682644*Aspect@Precipitation + -1.077087136*Slope@Precipitation"

pred <- predict(model, newdata = lapse_df)
sqrt(mean((lapse_df$PSUMm - pred)^2, na.rm = TRUE))
rm(model, pred)

#Modelling rainfall occurence.                                                   
model <- glm(PSUMm_Occ ~ PSUMe_Occ, lapse_df, family = binomial)
prob <- predict(model, newdata = lapse_df, type = "response")
prob <- ifelse(prob > 0.5, 1, 0)
summary(predict(model, type = "response"))
table(Predicted = prob, Actual = lapse_df$PSUMm_Occ)
rm(model)

#Clearly, there is a very poor correlation.
#This won't be as simple as for temperature.
plot(lapse_df$PSUMe, lapse_df$PSUMm)
abline(a = 0, b = 1, col = "red", lwd = 5)

#Summarizing to a monthly level gives a much better r-squared.

#'[Machine Learning.]

lapse_df %<>% na.omit
set.seed(1972)

#'[Random Forest.]

model <- ranger(PSUMm ~ ALT + SLOPE + ASPECT + PSUMe + RHMAXm + RHMINm + WINDm + X + Y, data = lapse_df, num.trees = 500, importance = "impurity")
#model <- ranger(PSUMm ~ PSUMe + AWSID + ALT + SLOPE + ASPECT, data = lapse_df, num.trees = 500, importance = "impurity")
pred <- predict(model, data = lapse_df)
sqrt(mean((lapse_df$PSUMm - pred$predictions)^2, na.rm = TRUE))

vals <- sort(model$variable.importance, decreasing = TRUE)
names(vals) <- c("ERA5 Rainfall", "RH Min", "RH Max", "Wind Run", "Elevation", "Longitude", "Latitude", "Slope", "Aspect")

df <- data.frame(var = factor(names(vals), levels = names(vals)), value = vals)
ggplot(df, aes(x = var, y = value, fill = value)) +
  geom_col() +
  scale_fill_viridis(direction = 1, guide = "none") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 40, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 30),
        axis.title = element_text(size = 40)) + 
  labs(x = "Variables", y = "Importance")

model$r.squared
rm(model, pred)

#'[Artificial Neural Networks.]

train_df <- lapse_df %>% slice_sample(prop = 0.8)
test_df <- lapse_df %>% anti_join(train_df)

#Establishing n-folds (subsamples) from the training dataset.
folds <- createFolds(train_df$PSUMm, k = 5, returnTrain = TRUE)
#Dictating cross-validation as the model validation method.
ctrl <- trainControl(method = "cv", number = 5, search = "grid", savePredictions = TRUE, index = folds)

#Initial run to get an idea of the range of hyperparameters.
ann_model <- train(PSUMm ~ ALT + SLOPE + ASPECT + PSUMe + RHMAXm + RHMINm + WINDm + X + Y, data = train_df, 
                   trControl = ctrl, method = "nnet")

ann_model

#Different parameters to tune.
getModelInfo("nnet")$nnet$parameters

#Ranges of different hyperparameters.
data.frame(Hyperparameter = c("size", "decay"),
           LowerLimit = c(range(ann_model$results[, "size"])[1], range(ann_model$results[, "decay"])[1]),
           UpperLimit = c(range(ann_model$results[, "size"])[2], range(ann_model$results[, "decay"])[2]))

rm(ann_model)

#Manually establishing hyperparameters to test.
hyperparams <- expand.grid(size = c(1, 2, 3, 4, 5, 6, 7, 8),   
                           decay = c(0, 0.001, 0.01, 0.1, 1))

#Re-running the ANN model with manually-defined hyperparameters.
#This takes about 10 minutes.
ann_model <- train(PSUMm ~ ALT + AWSID + PSUMe, data = train_df, 
                   trControl = ctrl, method = "nnet",
                   tuneGrid = hyperparams)

#Extracting relevant hyperparameters.
params <- data.frame(size = ann_model$results[, "size"],
                     decay = ann_model$results[, "decay"], 
                     accuracy = ann_model$results[, "Rsquared"])

#Plotting internal model accuracy as a function of hyperparameters: size and decay
smoothScatter(params$size, params$accuracy, xlab = "Size (Number of hidden units)", ylab = "Accuracy", colramp = viridis)
smoothScatter(params$decay, params$accuracy, xlab = "Weight Decay", ylab = "Accuracy", colramp = viridis)
#In both cases, neither hyperparameter by itself has a clear effect on accuracy.
#Accuracy varies significantly within each level of the hyperparameters.

#Re-ordering each combination of hyperparameters.
params <- params[order(params$size, params$decay), ]
params %<>% unique
params$size %<>% as.factor()
params$decay %<>% as.factor()

#Plotting average internal acurracy as a function of both size and decay
ggplot(params, aes(x = size, y = accuracy, group = decay, colour = decay)) +
  geom_point() +
  geom_line() +
  theme_classic(base_size = 20) +
  ggtitle("Average Internal Accuracy as a function of size and decay.") +
  ylab("Accuracy")
#The most accurate model is achieved using size = 2 and decay = 0.1.

#These R-Squared values are even worse than for the multiple linear regression.

#All combinations of hyperparameters tried by the train() function.
ann_model$results

#Combination of hyperparameters that produces the best model.
print("The combination of hyperparameters that produces the best model are:")
print(ann_model$results[which.max(ann_model$results[, "Rsquared"]), c("size", "decay")])

#Accuracy of the model using the best combination of hyperparameters.
paste0("The internal accuracy of the best model is: ", 
       round(ann_model$results[which.max(ann_model$results[, "Rsquared"]), "Rsquared"], 4))

#Predicting on the test dataset using the best model.
test_df$ann_predictions <- predict(ann_model, newdata = test_df, type = "raw")

rm(params, hyperparams, ann_model, folds, ctrl, train_df, test_df)

#Precipitation Kridging----

ntonj_dem <- rast("C:\\Users\\Nietolaj\\OneDrive - Mondigroup\\NC State\\Spring 2026\\Geospatial Computation and Simulation\\Project\\Spatial Data\\DEM\\Ntonj.tif")
ntonj_dem <- project(ntonj_dem, crs(psum))

psum_pts <- as.points(psum[[1]], na.rm = TRUE)

plot(ntonj_dem)
plot(psum_pts, add = TRUE)

psum_hi <- resample(psum[[1]], ntonj_dem, method = "bilinear")
psum_pts <- as.points(psum_hi[[1]], na.rm = TRUE)

psum_pts$elev <- terra::extract(ntonj_dem, psum_pts)[, 2]
psum_pts <- psum_pts[!is.na(psum_pts$elev), ]
names(psum_pts) <- c("Prec", "Elev")

plot(psum_pts)

psum_shp <- as(psum_pts, "Spatial")
proj4string(psum_shp)
psum_shp <- spTransform(psum_shp, CRS("EPSG:32736"))
proj4string(psum_shp)

psum_shp <- psum_shp[!is.na(psum_shp$Prec), ]

library(gstat)

g <- gstat(NULL, id = "Prec", formula = Prec ~ 1, data = psum_shp)
g <- gstat(g, id = "Elev", formula = Elev ~ 1, data = psum_shp)
g

v <- variogram(g)
fit <- fit.lmc(v, g, model = vgm(1, "Sph", 50000, 1))





