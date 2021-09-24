# -------------------------------------------------- #
# Climate Risk Profiles -- Get historical climate data
# A. Esquivel, C. Saavedra, H. Achicanoy, A. Ghosh, J. Ramirez-Villegas
# Alliance Bioversity-CIAT, 2021
# -------------------------------------------------- #

# Input parameters:
#   iso: ISO 3 country code
#   outfile: output file path
# Output:
#   Data frame with daily values for: tmax, tmin, prec, srad, rh, and wind per pixel
get_his_clim <- function(iso = iso, outfile = paste0('./',iso,'.fst')){
  
  if(!file.exists(outfile)){
    
  } else {
    cat('Historical climate data already calculated.\n')
  }
  return(cat('Get historical climate data: finished successfully!\n'))
}
