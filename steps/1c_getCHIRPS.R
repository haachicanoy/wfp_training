library(terra)
library(RColorBrewer)
region <- "global"
res <- 0.05
interval <- "daily"
startdate <- "2020-01-01"
enddate <- "2020-01-02"
path <- datadir

r <- rain_chirps(region, res, interval, startdate, enddate, 
            path, asRaster = TRUE)
r[r<0] <- NA
plot(r[[1]], col = brewer.pal(5, "BuPu"))  
