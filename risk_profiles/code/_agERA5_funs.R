getERA5 <- function(i, qq, year, month, day, uid, datadir){
  q <- qq[i,]
  format <- "zip"

  if(missing(day)){
    ndays <- lubridate::days_in_month(as.Date(paste0(year, "-" ,month, "-01")))
    ndays <- 1:ndays
    ndays <- sapply(ndays, function(x) ifelse(length(x) == 1, sprintf("%02d", x), x))
    ndays <- dput(as.character(ndays))
  } else {
    ndays <- day
  }
  
  ofile <- paste0(paste(q$variable, q$statistics, year, month, day, sep = "-"), ".",format)
  
  if(!file.exists(file.path(datadir,ofile))){
    
    cat("Downloading", q[!is.na(q)], "for", year, month, day, "\n"); flush.console();
    
    request <- list("dataset_short_name" = "sis-agrometeorological-indicators",
                    "variable" = q$variable,
                    "statistic" = q$statistics,
                    "year" = year,
                    "month" = month,
                    "day" = ndays,
                    "area" = "90/-180/-90/179.9", # download global #c(ymax,xmin,ymin,xmax)? 
                    "time" = q$time,
                    "format" = format,
                    "target" = ofile)
    
    request <- Filter(Negate(anyNA), request)
    
    file <- wf_request(user     = uid,   # user ID (for authentification)
                       request  = request,  # the request
                       transfer = TRUE,     # download the file
                       path     = datadir)  
  } else {
    cat("Already exists", q[!is.na(q)], "for", year, month, day, "\n"); flush.console();
  }
  return(NULL)
}

