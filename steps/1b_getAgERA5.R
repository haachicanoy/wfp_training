# CDS dataset description
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/sis-agrometeorological-indicators?tab=overview

# install.packages("ecmwfr")
library("ecmwfr")

wf_set_key(user = uid,
           key = key,
           service = "cds")

# combinations to download
qq <- data.frame(variable = c("precipitation_flux","solar_radiation_flux",rep("2m_temperature",3),
                              "10m_wind_speed", "2m_relative_humidity"),
                 statistics = c(NA, NA, "24_hour_maximum", "24_hour_mean", "24_hour_minimum",
                                "24_hour_mean", NA),
                 time = c(NA,NA,NA,NA,NA,NA, "12_00"))


# download one variable for a day
tryCatch(getERA5(1, qq, year, month, day, uid, datadir), error = function(e)NULL)

# temporal range
years <- as.character(1979:2020)
months <- c(paste0("0", 1:9), 10:12)
