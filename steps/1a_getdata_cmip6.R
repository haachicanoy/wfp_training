source("risk_profiles/code/_CMIP6_funs.R")

vars <- c("pr","tas","tasmax","tasmin")
models <- c("ACCESS-ESM1-5","EC-Earth3-Veg","INM-CM5-0","MPI-ESM1-2-HR","MRI-ESM2-0")

varmod <- expand.grid(vars, models)
names(varmod) <- c("vars", "models")

i <- 1
var <- as.character(varmod$vars[i])
model <- as.character(varmod$models[i])

# historical simulations
dhist <- try(getMetaCMIP6(offset = 0,
                      limit = 10000,
                      activity_id="CMIP",
                      experiment_id = "historical",
                      frequency = "day",
                      member_id = "r1i1p1f1",
                      variable_id = var,
                      source_id = model,
                      mip_era = "CMIP6"))

head(dhist)
dim(dhist)


# future simulations
dfut <- try(getMetaCMIP6(offset = 0,
                         limit = 10000,
                         activity_id="ScenarioMIP",
                         experiment_id = "ssp585",
                         member_id = "r1i1p1f1",
                         frequency = "day",
                         variable_id = var,
                         source_id = model,
                         mip_era = "CMIP6"))
head(dfut)
dim(dfut)
            
# combine both results
dd <- rbind(dhist, dfut)

# now download files
options(timeout=3600)
downdir <- "..."

# one file
getDataCMIP6(1, dd, downdir, silent=FALSE)
# all file
lapply(1:nrow(dd), getDataCMIP6, dd, downdir, silent=FALSE)


# order by file size
dfut$file_size <- as.numeric(dfut$file_size)
View(dfut[order(dfut$file_size),])


################################################################################
# try processing a smaller file
library(terra)
library(geodata)
library(data.table)

dfut <- try(getMetaCMIP6(offset = 0,
                         limit = 10000,
                         activity_id="ScenarioMIP",
                         experiment_id = "ssp585",
                         member_id = "r1i1p1f1",
                         frequency = "day",
                         variable_id = "tas",
                         source_id = "MPI-ESM1-2-HR",
                         mip_era = "CMIP6"))
getDataCMIP6(1, dfut, downdir, silent=FALSE)

f <- list.files(downdir, ".nc$", full.names = TRUE)
r <- rast(f)
# time from layer names
tm <- as.POSIXct(r@ptr$time, origin = "1970-01-01")
# let's say we want the month of may
k <- which(tm >= "2100-05-01" & tm <= "2100-05-31")
r <- subset(r, k)
rt <- rotate(r)

# what is the effect of rotate?
dev.new(width=6, height=4, noRStudioGD = TRUE)
par(mfrow=c(1,2))
plot(r[[1]], range=c(273,330), legend=FALSE)
plot(rt[[1]], range=c(273,330))

# mask by country boundary
v <- geodata::gadm(country="TZA", level=0, path=downdir)
rs <- crop(rt, v)
rs <- mask(rs, v)
plot(rs[[1]])
plot(v, add = T)

# resample/disaggregate to smaller cell size
tres <- 0.05 # CHIRPS resolution
rs1 <- disaggregate(rs, fact = round(res(rs)/tres))

chr <- rain_chirps(region, res, interval, "2020-01-01", "2020-01-01", 
                   path, asRaster = TRUE)
ciso <- crop(chr, v)
ciso <- mask(ciso, v)
rs2 <- resample(rs, ciso) 

dev.new(width=6, height=4, noRStudioGD = TRUE)
par(mfrow=c(1,2))
plot(rs1[[1]], range=c(273,330), legend=FALSE)
plot(v, add = T)
plot(rs2[[1]], range=c(273,330))
plot(v, add = T)

# save raster as dataframe
dd <- terra::as.data.frame(rs1, xy = TRUE)
names(dd) <- c("x", "y", as.character(as.Date(tm[k])))

# convert from wide to long with dataframe, safer way?
ddl <- melt(setDT(dd), id.vars = c("x","y"), value.name = "tas", variable = "date")

# add cellnumbers for using with join later
xy <- as.matrix(ddl[,c("x","y")])
ddl <- data.frame(id = cellFromXY(rs1, xy), ddl, stringsAsFactors = FALSE)
