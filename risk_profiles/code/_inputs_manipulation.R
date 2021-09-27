options(warn = -1, scipen = 999)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, terra, raster, sp))

root <- '//catalogue/Workspace14/WFP_ClimateRiskPr/Harold/WFP_ClimateRiskPr'

country <- 'Burundi'
iso <- 'BDI'

# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
# FROM RASTER TO TABLE
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #

shp <- terra::vect(paste0(root,'/1.Data/shps/',tolower(country),'/',tolower(iso),'_gadm/',country,'_GADM2.shp'))
rst <- terra::rast(paste0(root,'/1.Data/auxiliars/Temperature-Air-2m-Max-24h_C3S-glob-agric_AgERA5_daily_19810101-19810131_final-v1.0.nc'))
rst # General overview of the data
plot(rst[[1]])

rst <- rst - 273.15 # Transformation from Kelvin to Celsius degrees
plot(rst[[1]])

# Resample information to ~5 km resolution
prc <- terra::rast(paste0(root,'/1.Data/chirps-v2.0.2020.01.01.tif'))
prc <- prc %>% terra::crop(terra::ext(shp))

# Crop information to Burundi boundaries
bdi <- rst %>% terra::crop(terra::ext(prc))
plot(bdi[[1]])

# Resampling original resolution (25 km) to 5 km
bdi <- bdi %>% terra::resample(prc)
plot(bdi[[1]])

# Rasterize shp
shr <- terra::rasterize(x = shp, y = prc, field = 'NAME_0')
plot(shr)

# Crop and mask to country limits
bdi <- bdi %>% terra::crop(terra::ext(shr))
bdi <- bdi %>% terra::mask(mask = shr)

# Transform raster stack to data frame
df <- bdi %>% terra::as.data.frame(cell = T, xy = T, na.rm = T)
names(df)

# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #
# FROM TABLE TO RASTER
# -------------------------------------------------------------------------- #
# -------------------------------------------------------------------------- #

# Calculate daily average maximum temperature for Jan 1981
df$Average <- rowMeans(x = df %>% dplyr::select(Temperature_Air_2m_Max_24h_1:Temperature_Air_2m_Max_24h_31), na.rm = T)
hist(df$Average)

# Rasterize the daily average maximum temperature
r <- df %>%
  dplyr::select(x, y, Average) %>%
  terra::rast(type = 'xyz')
plot(r)
