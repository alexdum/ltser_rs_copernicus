write_netcdf <- function(x, filename, timestamp = NA, compression = 7, nvars = 1, timestep = "hours", resolution = "month",
                         var1_short = NA, var1_long = NA, units1 = NA, precision = "float", title = NA, copyright = NA,
                         rnd = 1) {
  
  require(ncdf4)
  
  if (resolution == "month") {
    year.month <- format(timestamp, "%Y%m")
    time_start <- as.POSIXct(paste(year.month,"010000"), "%Y%m%d%H%M", tz = "UTC")
    time_end <-  as.POSIXct( paste(lubridate::ceiling_date(as.Date(time_start), "month") - 1, "2330"), "%Y-%m-%d %H%M", tz = "UTC")
  } else if (resolution == "increment") {
    time_start <- as.POSIXlt(timestamp,  tz = "UTC") |> min()
    time_end <- as.POSIXlt(timestamp,  tz = "UTC") |> max()
  } else {
    year <- format(timestamp, "%Y")
    time_start <- as.POSIXct(paste(year,"01010000"), "%Y%m%d%H%M", tz = "UTC" )
    time_end <- as.POSIXct(paste0(format(time_start, "%Y"),"-12-31 2330"), "%Y-%m-%d %H%M", tz = "UTC")
  }
  
  if (timestep == "hours") {
    timelabel = "time"
    sec_time <- seq(time_start, time_end, by = "30 min")
    timedim <- ncdim_def("time", "seconds since 1970-1-1 00:00:00", as.numeric(sec_time), unlim = TRUE)
    time_index <- which(sec_time %in% timestamp)
  } else {
    timelabel = "days"
    sec_date <- seq(as.Date(time_start), as.Date(time_end), by = "days")
    timedim <- ncdim_def("days", "days since 1970-1-1 00:00:00", as.numeric(sec_date), unlim = TRUE)
    time_index <- which(sec_date %in% timestamp)
  }
  
  y <- x[[1]]
  fillvalue <- NA
  
  if (!file.exists(filename)) {
    
    # define dimensions
    londim <- ncdim_def("lon", "degrees_east", xFromCol(y, 1:ncol(y)))
    latdim <- ncdim_def("lat", "degrees_north", yFromRow(y, 1:nrow(y)))
    
    # creeaza dimensiunea temporala cu numarul de zile/ore ramase dintr-un an
    var1 <- ncvar_def( var1_short, units1 , list(londim, latdim, timedim), fillvalue, 
                       var1_long, prec = precision, shuffle = F, compression = compression)
    if (nvars == 2) {
      var2 <- ncvar_def("quality_flag", "degree Celsius", list(londim, latdim, timedim), fillvalue, 
                        "Q_FLAGS", prec = "float", shuffle = F, compression = compression)
      ncout <- nc_create(filename, list(var1, var2), force_v4 = T)
      ncvar_put(ncout, var1, round(values(x[[1]], mat = F),rnd),start = c(1,1, time_index), count = c(-1,-1,1))
      ncvar_put(ncout, var2, round(values(x[[2]], mat = F),rnd),start = c(1,1, time_index), count = c(-1,-1,1))
    } else {
      ncout <- nc_create(filename, var1, force_v4 = T)
      # create netCDF file and put arrays
      ncvar_put(ncout, var1, round(values(x, mat = F),rnd),start = c(1,1, time_index), count = c(-1,-1,1))
    }
    
    
    # put variables
    
    #   # put additional attributes into dimension and data variables
    #   ncatt_put(ncout, "lon", "axis", "X")  #,verbose=FALSE) #,definemode=FALSE)
    #   ncatt_put(ncout, "lat", "axis", "Y")
    #   ncatt_put(ncout, "time", "axis", "T")
    
    # add global attributes
    ncatt_put(ncout, 0, "title", title)
    ncatt_put(ncout, 0, "institution", "National Meterological Administration, Department of Climatology")
    ncatt_put(ncout, 0, "source", copyright)
    #ncatt_put(ncout, 0, "references", references$value)
    history <- paste("Alexandru Dumitrescu (alexandru.dumitrescu@gmail.com)", date(), sep = ", ")
    ncatt_put(ncout, 0, "history", history)
    #ncatt_put( ncout, 0, "description", "cantitatea de precipitatii din intervalului 06 UTC ziua precedenta - 06 UTC ziua curenta")
    #ncatt_put(ncout, 0, "Conventions", Conventions$value)
    
    # close the file, writing data to disk
    nc_close(ncout)
    
  } else {
    
    ncid_old <- nc_open(filename, write = TRUE, verbose = F)
    
    londim <- ncid_old$dim[['lon']]
    latdim <- ncid_old$dim[['lat']]
    timedim <- ncid_old$dim[[timelabel]]
    
    # increment date pentru fisiere ncs
    if (resolution == "increment" &  timestep == "days") {
      dats <- append(timedim$vals, as.numeric(as.Date(timestamp)))
      timedim <- ncdim_def("days", "days since 1970-1-1", dats, unlim = TRUE)
      time_index <- timedim$len
    }
    
    if (nvars == 2) {
      
      ncvar_put(ncid_old, varid = var1_short, vals = round(values(x[[1]], mat = F),rnd),start = c(1,1, time_index), count = c(-1,-1,1))
      ncvar_put(ncid_old, varid = var2_short, vals = round(values(x[[2]], mat = F),rnd),start = c(1,1, time_index), count = c(-1,-1,1))
      ncvar_put(ncid_old, varid = timelabel, vals =  timedim$vals, start = c(1), count = length(timedim$vals))
    } else {
      # put variables
      #dimensiunea temporala este data de data de inceput a fisierului nc minus data actuala
      ncvar_put(ncid_old, varid = var1_short, vals = round(terra::values(x[[1]], mat = F),rnd),start = c(1,1, time_index), count = c(-1,-1,1))
      ncvar_put(ncid_old, varid = timelabel, vals =  timedim$vals, start = c(1), count = length(timedim$vals))
    }
    
    history <- paste("Alexandru Dumitrescu (alexandru.dumitrescu@gmail.com)", date(), sep = ", ")
    ncatt_put(ncid_old, 0, "history", history)
    
    #print(ncid_old)
    
    nc_close(ncid_old)
  }
} 

