library_check<- function(libraries) {
  ## Details
  # This function will check for and then either load or install any libraries needed to run subsequent functions
  
  # Args:
  # libraries = Vector of required library names
  
  # Returns: NA, just downloads/loads libraries into current work space.
  
  ## Start function
  lapply(libraries, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
  ## End function
}

make360 <- function(lon) {
  ## Details
  # This is a simple function to translate negative longitudes (measured on -180:180 scale) into 0-360, which is coordinate system used by some environmental datasets.
  
  # Args:
  # lon = Longitude in -180:180 degrees
  
  # Returns: 0 to 360 longitude
  
  ## Start function
  
  ind <- which(lon < 0)
  lon[ind] <- lon[ind] + 360
  return(lon)
  
  ## End function
}

fix_raster<- function(x, lons.use, lats.use, x.min.use, x.max.use, y.min.use, y.max.use) {
  ## Details
  # This function helps orient the OISST rasters, which originally come in flipped/rotated.
  
  # Args:
  # x = 
  # lons.use = 
  # lats.use = 
  # x.min.use = 
  # x.max.use = 
  # y.min.use = 
  # y.max.use = 
  
  # Returns: Correctly oriented raster 
  
  ## Start function
  
  r.temp<- t(x)[ncol(x):1,]
  rast.out<- raster(r.temp, xmn = lons.use[x.min.use], xmx = lons.use[x.max.use], ymn = lats.use[y.min.use], ymx = lats.use[y.max.use])
  return(rast.out)
  
  ## End function
}

convert_kelvin_to_celsius<- function(temp) {
  celsius<- temp - 273.15
  return(celsius)
}

make_climate_threddsURL<- function(project, model.short, experiment, frequency, model.realm1, model.realm2, ensemble, version, variable){
  if(FALSE){
    project = "cmip5"
    project = "cmip6"
    model.short = "GFDL-ESM2G"
    model.short = "GFDL-CM4"
    experiment = "rcp85"
    experiment = "piControl"
    frequency = "mon"
    model.realm1 = "ocean"
    model.realm2 = "Omon"
    ensemble = "r1i1p1"
    ensemble = "r1i1p1f1"
    version = "v20110601"
    version = "v20190201"
    variable = "tos"
  }
  
  # Each of the projects are set up a bit differently -- so going to make two different pieces, one URL creation for cmip5 and the other for cmip6
  if(project == "cmip5"){
    thredds.base<- "http://esgdata.gfdl.noaa.gov/thredds/dodsC/gfdl_dataroot/NOAA-GFDL/"
    
    # Getting list of datasets
    # Getting time period chunks of the different nc files
    date.starts<- as.Date(seq(ymd('2006-01-01'), ymd('2100-12-31'), by='5 years'))
    starts<- gsub("-", "", format(date.starts, "%Y-%m"))
    ends<- gsub("-", "", format(as.Date(seq(ymd('2010-12-01'), ymd('2100-12-31'),by='5 years')), "%Y-%m"))
    dates<- paste(paste(starts, ends, sep = "-"), ".nc", sep = "")
    
    # Creating list of nc files
    thredds.part1<- paste(model.short, experiment, frequency, model.realm1, model.realm2, ensemble, version, variable, sep = "/")
    thredds.part2<- paste("/", paste(variable, model.realm2, model.short, experiment, ensemble, dates, sep = "_"), sep = "")
    thredds.urls<- paste(thredds.base, thredds.part1, thredds.part2, sep = "")
    
    return(thredds.urls)
  }
  
  # Now, cmip6 URL
  if(project == "cmip6"){
    thredds.base<- "http://esgdata.gfdl.noaa.gov/thredds/dodsC/gfdl_dataroot4/CMIP/NOAA-GFDL/"
    
    # Getting list of datasets
    # Getting time period chunks of the different nc files
    duration<- ymd('0170-12-31') - ymd('0151-01-01') + 1
    date.starts<- as.Date(seq(ymd('0151-01-01'), ymd('0350-12-31'), by = duration))
    starts<- gsub("-", "", format(date.starts, "%Y-%m"))
    ends<- gsub("-", "", format(as.Date(seq(ymd('0170-12-31'), ymd('0350-12-31'), by = duration)), "%Y-%m"))
    dates<- paste(paste(starts, ends, sep = "-"), ".nc", sep = "")
    
    # Creating list of nc files
    thredds.part1<- paste(model.short, experiment, ensemble, model.realm2, variable, "gn", version, sep = "/")
    thredds.part2<- paste("/", paste(variable, model.realm2, model.short, experiment, ensemble, "gn", dates, sep = "_"), sep = "")
    thredds.urls<- paste(thredds.base, thredds.part1, thredds.part2, sep = "")
    
    return(thredds.urls)
  }
}

clim_data_extract<- function(thredds.urls, box, project, out.path){
  
  library_check(c("tidyverse", "raster", "ncdf4"))
  
  if(FALSE){
    thredds.urls<- thredds.use$Thredds.URLS[[1]]
    thredds.urls<- thredds.urls
    box<- c(-77, -60, 35, 46)
    project<- "cmip5"
    out.path<- "./Output/"
  }
  
  # Looping over thredds.urls -- project dependent!
  if(project == "cmip5") {
    for(i in seq_along(thredds.urls)){
      
      # Connecting and extracting lat/lon/time variables from netcdf file
      my.nc<- nc_open(thredds.urls[i])
      lats<- ncvar_get(my.nc, var = "rlat")
      lons<- ncvar_get(my.nc, var = "rlon")
      times<- ncvar_get(my.nc, var = "time")
      
      # Make times a little bit easier to handle
      dates.full<- as.Date(times, origin = "2006-01-01")
      
      # Find indices and windows corresponding to spatial box of interest, which are then used in the "start" and "count" arguments to the ncvar_get call for the sst variable
      b.box<- c(box[1], box[2], box[3], box[4])
      
      x.window<- which(lons > b.box[1] & lons < b.box[2])
      x.min<- min(x.window)
      x.max<- max(x.window)
      x.count<- ifelse(x.max - x.min > 0, x.max-x.min, 1)
      
      y.window<- which(lats > b.box[3] & lats < b.box[4])
      y.min<- min(y.window)
      y.max<- max(y.window)
      y.count<- ifelse(y.max - y.min > 0, y.max - y.min, 1) 
      
      time.min<- which.min(dates.full)
      time.max<- which.max(dates.full)
      time.count<- ifelse(time.max - time.min > 0, (time.max - time.min)+1, 1)
      
      # Now we have the lon,lat,time indices and windows, but need to match up their order with how they are handled in the ncvar_get call 
      dim.order <- sapply(my.nc$var$tos$dim, function(x) x$name)
      
      # Set start and counts
      start.use<- c("rlon" = x.min, "rlat" = y.min, "time" = time.min)
      count.use<- c("rlon" = x.count, "rlat" = y.count, "time" = time.count)
      
      # Run ncvar_get, adjusting order of start and count as needed
      temp<- ncvar_get(my.nc, varid = "tos", start = start.use[dim.order], count = count.use[dim.order])
      
      # Moving from the array format of temp to a raster stack
      temp.list<- lapply(seq(dim(temp)[3]), function(x) fix_raster(temp[,,x], lons.use = lons, lats.use = lats, x.min.use = x.min, x.max.use = x.max, y.min.use = y.min, y.max.use = y.max))
      rast.temp<- raster::stack(temp.list)
      
      # Convert to Celsius for CMIP5 output
      rast.temp<- calc(rast.temp, fun = convert_kelvin_to_celsius)
      
      # Add names?
      names(rast.temp)<- dates.full
      
      if(i == 1) {
        stack.out<- rast.temp
      } else {
        stack.out<- stack(stack.out, rast.temp)
      }
      print(paste(thredds.urls[i], "is done", sep = " "))
    }
    # Set projection information
    proj4string(stack.out)<- CRS("+init=epsg:4326")
    
    # Save it?
    if(!is.null(out.path)){
      file.name.temp1<- gsub("http://esgdata.gfdl.noaa.gov/thredds/dodsC/gfdl_dataroot/NOAA-GFDL/", "", thredds.urls[1])
      file.name.temp2<- strsplit(file.name.temp1, "/")
      file.name<- paste(file.name.temp2[[1]][1:length(file.name.temp2[[1]])-1], collapse = ".")
      writeRaster(stack.out, filename = paste(out.path, project, file.name, ".grd", sep = ""))
    }
  }
  
  # Now, CMIP6
  if(project == "cmip6"){
    
    for(i in seq_along(thredds.urls)){
      
      # Connecting and extracting lat/lon/time variables from netcdf file
      my.nc<- nc_open(thredds.urls[i])
      lats<- ncvar_get(my.nc, var = "y")
      lons<- ncvar_get(my.nc, var = "x")
      times<- ncvar_get(my.nc, var = "time")
      
      # Make times a little bit easier to handle -- these seem weird...
      dates.full<- as.Date(times, origin = "1700-01-01")
      
      # Find indices and windows corresponding to spatial box of interest, which are then used in the "start" and "count" arguments to the ncvar_get call for the sst variable
      b.box<- c(box[1], box[2], box[3], box[4])
      
      x.window<- which(lons > b.box[1] & lons < b.box[2])
      x.min<- min(x.window)
      x.max<- max(x.window)
      x.count<- ifelse(x.max - x.min > 0, x.max-x.min, 1)
      
      y.window<- which(lats > b.box[3] & lats < b.box[4])
      y.min<- min(y.window)
      y.max<- max(y.window)
      y.count<- ifelse(y.max - y.min > 0, y.max - y.min, 1) 
      
      time.min<- which.min(dates.full)
      time.max<- which.max(dates.full)
      time.count<- ifelse(time.max - time.min > 0, (time.max - time.min)+1, 1)
      
      # Now we have the lon,lat,time indices and windows, but need to match up their order with how they are handled in the ncvar_get call 
      dim.order <- sapply(my.nc$var$tos$dim, function(x) x$name)
      
      # Set start and counts
      start.use<- c("x" = x.min, "y" = y.min, "time" = time.min)
      count.use<- c("x" = x.count, "y" = y.count, "time" = time.count)
      
      # Run ncvar_get, adjusting order of start and count as needed
      temp<- ncvar_get(my.nc, varid = "tos", start = start.use[dim.order], count = count.use[dim.order])
      
      # Moving from the array format of temp to a raster stack
      temp.list<- lapply(seq(dim(temp)[3]), function(x) fix_raster(temp[,,x], lons.use = lons, lats.use = lats, x.min.use = x.min, x.max.use = x.max, y.min.use = y.min, y.max.use = y.max))
      rast.temp<- raster::stack(temp.list)
      
      # Add names?
      names(rast.temp)<- dates.full
      
      if(i == 1) {
        stack.out<- rast.temp
      } else {
        stack.out<- stack(stack.out, rast.temp)
      }
      print(paste(thredds.urls[i], "is done", sep = " "))
    }
    
    # Set projection information
    proj4string(stack.out)<- CRS("+init=epsg:4326")
    
    # Save it
    if(!is.null(out.path)){
      file.name.temp1<- gsub("http://esgdata.gfdl.noaa.gov/thredds/dodsC/gfdl_dataroot4/CMIP/NOAA-GFDL/", "", thredds.urls[1])
      file.name.temp2<- strsplit(file.name.temp1, "/")
      file.name<- paste(file.name.temp2[[1]][1:length(file.name.temp2[[1]])-1], collapse = ".")
      writeRaster(stack.out, filename = paste(out.path, project, file.name, ".grd", sep = ""))
    }
  }
  
  # Return it 
  return(stack.out)
  
}

