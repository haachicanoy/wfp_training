options(warn = -1, scipen = 999)
suppressMessages(library(pacman))
suppressMessages(pacman::p_load(tidyverse, tidyfst, qmap))

root <- '/home/jovyan/work'
iso <- 'BDI'
gcm <- 'MRI-ESM2-0'
prd <- '2021-2040'
his_obs <- paste0(root,'/1.Data/observed_data/',iso,'/',iso,'.fst')
his_gcm <- paste0(root,'/1.Data/future_data/',gcm,'/',iso,'/downscale/1995-2014/',iso,'.fst')
fut_gcm <- paste0(root,'/1.Data/future_data/',gcm,'/',iso,'/downscale/',prd,'/',iso,'.fst')

load(paste0(root,'/bias_correction.RData'))

his_obs <- tidyfst::parse_fst(path = his_obs) %>%
  tidyfst::filter_fst(id == 7650603) %>%
  base::as.data.frame()
his_gcm <- tidyfst::parse_fst(path = his_gcm) %>%
  tidyfst::filter_fst(id == 7650603) %>%
  base::as.data.frame()
fut_gcm <- tidyfst::parse_fst(path = fut_gcm) %>%
  tidyfst::filter_fst(id == 7650603) %>%
  base::as.data.frame()

# prec_fit <- qmap::fitQmap(obs     = his_obs$prec,
#                           mod     = his_gcm$prec,
#                           method  = "RQUANT",
#                           qstep   = 0.01,
#                           wet.day = TRUE,
#                           na.rm   = TRUE)
tmax_fit <- qmap::fitQmap(obs     = his_obs$tmax,
                          mod     = his_gcm$tmax,
                          method  = "RQUANT",
                          qstep   = 0.01,
                          wet.day = FALSE,
                          na.rm   = TRUE)

bc_fut_gcm <- fut_gcm
# bc_fut_gcm$prec <- qmap::doQmap(x = fut_gcm$prec, prec_fit, type = "linear")
bc_fut_gcm$tmax <- qmap::doQmap(x = fut_gcm$tmax, tmax_fit, type = "linear")

# Daily time series
plot(his_obs$tmax[1:365], ty = 'l', ylim = c(20,35), xlab = 'Day of the year', ylab = 'Tmax daily-one year') # Observed
lines(fut_gcm$tmax[1:365], col = 2)    # Downscaled
lines(bc_fut_gcm$tmax[1:365], col = 4) # Bias-corrected
legend(10, 27, legend=c("observed", "downscaled", "bias-corrected"), lty = 1, col=c(1,2,4), cex=0.8, box.lty=0)

# Time series
mx_obs <- ts(data = his_obs$tmax, start = c(1981, 1), frequency = 365)
mx_dws <- ts(data = fut_gcm$tmax, start = c(2021, 1), frequency = 365)
mx_bcd <- ts(data = bc_fut_gcm$tmax, start = c(2021, 1), frequency = 365)

# Time series decomposition
d_mx_obs <- decompose(mx_obs); # plot(d_mx_obs)
d_mx_dws <- decompose(mx_dws); # plot(d_mx_dws)
d_mx_bcd <- decompose(mx_bcd); # plot(d_mx_bcd)

# Trend
plot(d_mx_obs$trend, xlim = c(1981,2041), ylim = c(24,27.5), ylab = 'Tmax trend') # Observed
lines(d_mx_dws$trend, col = 2) # Downscaled
lines(d_mx_bcd$trend, col = 4) # Bias-corrected
legend(1982, 27, legend=c("observed", "downscaled", "bias-corrected"), lty = 1, col=c(1,2,4), cex=0.8, box.lty=0)

# save.image(file = './bias_correction.RData')
