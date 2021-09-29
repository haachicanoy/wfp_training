options(warn = -1, scipen = 999)

# Sourcing functions and loading packages
source('https://raw.githubusercontent.com/haachicanoy/wfp_training/main/risk_profiles/code/_main_functions.R')
source('https://raw.githubusercontent.com/haachicanoy/wfp_training/main/risk_profiles/code/_get_soil_data.R')
source('https://raw.githubusercontent.com/haachicanoy/wfp_training/main/risk_profiles/code/_get_his_clim.R')
source('https://raw.githubusercontent.com/haachicanoy/wfp_training/main/risk_profiles/code/_get_dws_fut_clim.R')
source('https://raw.githubusercontent.com/haachicanoy/wfp_training/main/risk_profiles/code/_get_bc_fut_clim.R')
source('https://raw.githubusercontent.com/haachicanoy/wfp_training/main/risk_profiles/code/_calc_indices.R')
source('https://raw.githubusercontent.com/haachicanoy/wfp_training/main/risk_profiles/code/_calc_spi_drought.R')
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(caTools,SPEI,tidyverse,raster,ncdf4,sf,future,future.apply,furrr,lubridate,glue,vroom,sp,fst,compiler))

# Working directory
OSys    <- Sys.info()[1]
root    <- switch(OSys,
                  'Linux'   = '/home/jovyan/work',
                  'Windows' = '//catalogue/Workspace14/WFP_ClimateRiskPr/Harold/WFP_ClimateRiskPr')

# Country parameters
country <- 'Burundi'
iso     <- 'BDI'
seasons <- list(s1 = c(2:7), s2 = c(9:12,1:2))

#------------------------------------------------------------------#
# Soil data
#------------------------------------------------------------------#

# Load soil data
soil <- paste0(root,"/1.Data/soil/",iso,"/soilcp_data.fst")
Soil <- soil %>% tidyft::parse_fst(path = .) %>% base::as.data.frame(); rm(soil)

#------------------------------------------------------------------#
# Climate data
#------------------------------------------------------------------#

# Load historical climate data
climate <- paste0(root,'/1.Data/observed_data/',iso,'/',iso,'.fst')
clim_data <- climate %>% tidyfst::parse_fst(path = .) %>% base::as.data.frame(); rm(climate)
clim_data$year <- NULL
clim_data <- clim_data %>%
  dplyr::select(id,x,y,date,prec,tmax,tmean,tmin,srad,rh) %>%
  dplyr::mutate(id1 = id) %>%
  tidyr::nest(Climate = c('id','date','prec','tmax','tmean','tmin','srad','rh')) %>% # 'wind'
  dplyr::rename(id = 'id1') %>%
  dplyr::select(id, dplyr::everything(.))

# Match soil and climate pixels
px <- intersect(clim_data$id, Soil$id)
clim_data <- clim_data[clim_data$id %in% px,]

#------------------------------------------------------------------#
# Calculate hazard indices
#------------------------------------------------------------------#

id <- 7650603
cat(' --- Obtain complete time series per pixel\n')
tbl <- clim_data$Climate[[which(clim_data$id == id)]]
tbl <- tbl %>%
  dplyr::mutate(year  = lubridate::year(as.Date(date)),
                month = lubridate::month(as.Date(date)))
years <- tbl$year %>% unique

cat(' --- Calculate water balance for complete time series\n')
soilcp <- Soil$scp[Soil$id == id]
soilst <- Soil$ssat[Soil$id == id]
watbal_loc <- watbal_wrapper(tbl, soilcp, soilst)
watbal_loc$IRR <- watbal_loc$ETMAX - watbal_loc$prec

tbl <- tbl %>%
  dplyr::mutate(ERATIO  = watbal_loc$ERATIO,
                TAV     = (watbal_loc$tmin + watbal_loc$tmax)/2,
                IRR     = watbal_loc$IRR,
                LOGGING = watbal_loc$LOGGING,
                GDAY    = ifelse(TAV >= 6 & ERATIO >= 0.35, yes = 1, no = 0))

cat(' --- Estimate growing seasons from water balance\n')

### CONDITIONS TO HAVE IN ACCOUNT
# Length of growing season per year
# Start: 5-consecutive growing days.
# End: 12-consecutive non-growing days.

# Run process by year
lgp_year_pixel <- lapply(1:length(years), function(k){
  
  # Subsetting by year
  watbal_year <- tbl[tbl$year==years[k],]
  
  # Calculate sequences of growing and non-growing days within year
  runsDF <- rle(watbal_year$GDAY)
  runsDF <- data.frame(Lengths=runsDF$lengths, Condition=runsDF$values)
  runsDF$Condition <- runsDF$Condition %>% tidyr::replace_na(replace = 0)
  
  # Identify start and extension of each growing season during year
  if(!sum(runsDF$Lengths[runsDF$Condition==1] < 5) == length(runsDF$Lengths[runsDF$Condition==1])){
    
    LGP <- 0; LGP_seq <- 0
    for(i in 1:nrow(runsDF)){
      if(runsDF$Lengths[i] >= 5 & runsDF$Condition[i] == 1){
        LGP <- LGP + 1
        LGP_seq <- c(LGP_seq, LGP)
        LGP <- 0
      } else {
        if(LGP_seq[length(LGP_seq)]==1){
          if(runsDF$Lengths[i] >= 12 & runsDF$Condition[i] == 0){
            LGP <- 0
            LGP_seq <- c(LGP_seq, LGP)
          } else {
            LGP <- LGP + 1
            LGP_seq <- c(LGP_seq, LGP)
            LGP <- 0
          }
        } else {
          LGP <- 0
          LGP_seq <- c(LGP_seq, LGP)
        }
      }
    }
    LGP_seq <- c(LGP_seq, LGP)
    LGP_seq <- LGP_seq[-c(1, length(LGP_seq))]
    runsDF$gSeason <- LGP_seq; rm(i, LGP, LGP_seq)
    LGP_seq <- as.list(split(which(runsDF$gSeason==1), cumsum(c(TRUE, diff(which(runsDF$gSeason==1))!=1))))
    
    # Calculate start date and extension of each growing season by year and pixel
    growingSeason <- lapply(1:length(LGP_seq), function(g){
      
      LGP_ini <- sum(runsDF$Lengths[1:(min(LGP_seq[[g]])-1)]) + 1
      LGP     <- sum(runsDF$Lengths[LGP_seq[[g]]])
      results <- data.frame(id=tbl$id %>% unique, year=years[k], gSeason=g, SLGP=LGP_ini, LGP=LGP)
      return(results)
      
    })
    growingSeason <- do.call(rbind, growingSeason)
    if(nrow(growingSeason)>2){
      growingSeason <- growingSeason[rank(-growingSeason$LGP) %in% 1:2,]
      growingSeason$gSeason <- rank(growingSeason$SLGP)
      growingSeason <- growingSeason[order(growingSeason$gSeason),]
    }
    
  } else {
    
    growingSeason <- data.frame(id=tbl$id %>% unique, year=years[k], gSeason = 1:2, SLGP = NA, LGP = NA)
    
  }
  
  print(k)
  return(growingSeason)
  
})
lgp_year_pixel <- do.call(rbind, lgp_year_pixel); rownames(lgp_year_pixel) <- 1:nrow(lgp_year_pixel)

cat(' --- Calculate agro-climatic indices for an specific season\n')
if(!is.null(seasons)){
  indices <- 1:length(seasons) %>%
    purrr::map(.f = function(i){
      # i = 1; # 's1'
      season <- seasons[[i]]
      # Season across two years
      if(sum(diff(season) < 0) > 0){
        pairs     <- NA; for(j in 1:length(years)-1){pairs[j] <- paste0(years[j:(j+1)], collapse = '-')}
        tbl_list  <- lapply(1:(length(years)-1), function(k){
          df <- tbl %>%
            dplyr::filter(year %in% years[k:(k+1)])
          df$pairs <- paste0(years[k:(k+1)], collapse = '-')
          df1 <- df %>%
            dplyr::filter(year == years[k] & month %in% season[1]:12)
          df2 <- df %>%
            dplyr::filter(year == years[k+1] & month %in% 1:season[length(season)])
          df <- rbind(df1, df2); rm(df1, df2)
          return(df)
        })
        tbl_list <- dplyr::bind_rows(tbl_list)
      } else {
        # Season in one year
        tbl_list  <- lapply(1:length(years), function(k){
          # k = 1; # year 1: 1981
          df <- tbl %>%
            dplyr::filter(year %in% years[k])
          df$pairs <- years[k]
          df <- df %>%
            dplyr::filter(year == years[k] & month %in% season)
          return(df)
        })
        tbl_list <- dplyr::bind_rows(tbl_list)
      }
      
      idx <- tbl_list %>%
        dplyr::group_split(pairs) %>%
        purrr::map(.f = function(df){
          idx <- tibble::tibble(ATR  = calc_atrMP(PREC = df$prec), 
                                AMT  = calc_amtMP(TMEAN = df$tmean),
                                NDD  = calc_nddCMP(PREC = df$prec),
                                P5D  = calc_p5dCMP(PREC = df$prec),
                                P95  = calc_p95CMP(PREC = df$prec),
                                NT_X = calc_htsCMP(tmax = df$tmax, t_thresh = 35),
                                NDWS = calc_wsdays(df$ERATIO, season_ini = 1, season_end = length(df$ERATIO), e_thresh=0.5),
                                NWLD = calc_NWLDMP(LOGG = df$LOGGING),
                                NWLD50 = calc_NWLD50MP(LOGG = df$LOGGING, sat = soilst),
                                NWLD90 = calc_NWLD90MP(LOGG = df$LOGGING, sat = soilst),
                                IRR  = sum(df$IRR, na.rm = T),
                                SHI  = calc_SHIMP(tmax = df$tmax, RH = df$rh),
                                calc_HSIMP(tmax = df$tmax, RH = df$rh),
                                calc_THIMP(tmax = df$tmax, RH = df$rh),
                                CSDI = calc_csdiMP(TMIN = df$tmin))
          return(idx)
        })
      idx <- dplyr::bind_rows(idx)
      idx$year <- tbl_list$pairs %>% unique
      if(sum(diff(season) < 0) > 0){
        idx$year <- substr(x = idx$year, start = 6, stop = 10) %>% as.numeric
      } else {
        idx$year <- idx$year %>% as.numeric
      }
      idx$season <- names(seasons)[i]
      idx$id <- id
      
      return(idx)
      
    })
  indices <- dplyr::bind_rows(indices)
}
all <- dplyr::full_join(x = indices, y = lgp_year_pixel, by = c('id','year')) %>% unique()

#------------------------------------------------------------------#
# Visualize hazard indices
#------------------------------------------------------------------#

fl <- paste0(root,"/7.Results/",country,"/past/",iso,"_indices.fst")
indices <- tidyfst::parse_fst(fl) %>% base::as.data.frame()

# Selected pixel
#      id      x         y
# 7650603 30.125 -3.125001
library(DT)
# Explore the output
DT::datatable(indices[1:10,])
# Visualize the output for one index-one year:
indices %>%
  dplyr::filter(year == 2019) %>%
  ggplot2::ggplot(aes(x, y, fill = ATR)) +
  ggplot2::geom_tile() +
  ggplot2::coord_equal() +
  ggplot2::theme_bw() +
  ggplot2::scale_fill_continuous(type = 'viridis') +
  ggplot2::facet_wrap(~season) +
  ggplot2::geom_point(data = indices %>%
                        dplyr::filter(id == 7650603),
                      aes(x, y), colour = 'red', size = 2.5)
# Visualize the output for time series
indices %>%
  dplyr::filter(id == 7650603) %>%
  ggplot2::ggplot(aes(x = year, y = ATR, colour = factor(season))) +
  ggplot2::geom_line() +
  ggplot2::theme_bw()

# save.image(file = './hazard_indices.RData')
