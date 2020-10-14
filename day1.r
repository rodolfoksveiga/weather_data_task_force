# first of all it's necessary to install netcdf packages in your os
# on linux (ubuntu 20.04), just run the following command in the terminal
  # sudo apt-get install libnetcdf-dev libnetcdff-dev
# if everything goes well, you can install the ncdf4 package as follows
  # install.packages('ncdf4)

# setup environment ####
library(dplyr)
library(lubridate)
library(ncdf4)
library(purrr)
library(raster)
library(stringr)

# declare variables ####
lat = -23.6512
long = 360 - 46.6224

# load data
files_dir = '~/Downloads/esgf/'
file_paths = dir(files_dir, pattern = '\\.nc$',
                 full.names = TRUE, recursive = TRUE)

GetVarName = function(file_path) {
  pattern = c('hurs', 'huss', 'ps', 'rsds', 'sfcWind', 'tas') %>%
    str_flatten('|')
  var_name = file_path %>%
    basename() %>%
    str_extract(pattern)
  return(var_name)
}

GetTimes = function(file_path) {
  file = nc_open(file_path)
  times = file %>%
    ncvar_get('time') %>%
    as.numeric()
  times = times*24*60*60
  base = file %>%
    ncatt_get('time', 'units') %>%
    pluck('value') %>%
    str_remove('days since ') %>%
    as_datetime()
  times = base + seconds(times)
  return(times)
}

var_names = file_paths %>%
  sapply(GetVarName) %>%
  as.character()
times = lapply(file_paths, GetTimes)

GetData = function(file_path, var_name, time, coords) {
  file_path = file_paths[1]
  var_name = var_names[1]
  time = times[[1]]
  coords = c(lat, long)
  
  file = nc_open(file_path)
  
  PickLocals = function(orient, file) {
    locals = file %>%
      ncvar_get(orient) %>%
      as.vector()
    return(locals)
  }
  
  locals = c('lat', 'lon') %>%
    sapply(PickLocals, file)
  
  dists = locals %>% apply(1, function(x, y) dist(rbind(x, y)), coords)
  index = which.min(dists)
  
  min_local = locals[index, ]
  mtx_lat = ncvar_get(file, 'lat')
  
  cols = which(mtx_lat == min_local[1]) %/% nrow(mtx_lat)[1]
  rows = which(mtx_lat == min_local[1]) %% nrow(mtx_lat)[1]
  
  print(paste('File path:', file_path))
  print(paste('Variable:', var_name))
  print(paste('Length of latitudes:', length(locals[, 1])))
  print(paste('Length of longitudes:', length(locals[, 2])))
  print(paste('Smallest distance D:', dists[index]))
  print(paste('Lat:', min_local[1]))
  print(paste('Lon:', min_local[2]))
  print(paste('IJ index:', cols[1], rows[1]))
  
  var = ncvar_get(file, var_name, start = c(cols[1], rows[1], 1),
                  count = c(1, 1, length(time)))
  ncatt_get(file, var_name, 'dims')
}





