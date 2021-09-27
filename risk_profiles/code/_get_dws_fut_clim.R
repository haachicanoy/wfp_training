# -------------------------------------------------- #
# Climate Risk Profiles -- Get future downscaled data
# A. Esquivel, C. Saavedra, H. Achicanoy, A. Ghosh, J. Ramirez-Villegas
# Alliance Bioversity-CIAT, 2021
# -------------------------------------------------- #

# Input parameters:
#   iso: ISO 3 country code
#   outfile: output file path
# Output:
#   Data frame with soil capacity and soil saturation values per pixel
get_dws_fut_clim <- function(iso = iso,
                             gcm = 'ACCESS-ESM1-5',
                             period = '2021-2040',
                             ncores = 1,
                             outfile = paste0('./',iso,'.fst')){
  if(!file.exists(outfile)){
    
    suppressMessages(library(pacman))
    suppressMessages(pacman::p_load(fuzzyjoin,terra,tidyverse,raster,sf,data.table,fst))
    if (packageVersion("terra") < "1.1.0"){warning("terra version should be at least 1.1.0, \ninstall the the most updated version remotes::install_github('rspatial/terra')")}
    
    
    
  } else {
    cat('Future downscaled climate data already calculated.\n')
  }
  return(cat('Get future downscaled climate data: finished successfully!\n'))
}