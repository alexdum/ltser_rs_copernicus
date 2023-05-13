library(terra)
#library(sf)
library(dplyr)
setwd("~/ltser_rs_copernicus")
source("R/fun/write_netcdf.R")
library(rnaturalearthdata)
#tab <- read.table("https://land.copernicus.vgt.vito.be/manifest/swi_v1_1km/manifest_cgls_swi_v1_1km_20230504.txt")
# write without auxiliary file

files <- list.files("/data/MTDA/BIOPAR/BioPar_SSM1km_V1_Global/", recursive = T, full.names = T, pattern = ".nc$")
dats_all <- strsplit(files, "1km_|_CEURO") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")

# elimina duplicatele si alege ultima versiunea superioara a fisierelor
versions <- 
  strsplit(files, "_V|/c_") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(4) |>
  unlist() %>% gsub("\\.", "",.) |> as.numeric() |> bind_cols(dats_all) |> 
  rename(version = 1, date = 2) |>
  mutate(cod = paste(date, version))

versions_max <- 
  versions |>
  group_by(date) |> summarise(max = max(version)) |>
  mutate(cod = paste(date, max)) |>
  filter(date >= as.Date("2015-01-01")) # selecteaza fisierele dupa anul 2015

files_sub <- files[versions$cod %in% versions_max$cod]
dats_sub <- strsplit(files_sub, "1km_|_CEURO") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")
length(unique(dats_all))

# verificare selectie
files[dats_all %in% as.Date("2017-09-17")]
files_sub[dats_sub %in% as.Date("2017-09-17")]

# fisier vector
ltser <- vect("shp/ltser.topojson")
ltser$name_stand <- c("brailaislands", "bucegipiatracraiului", "danubedelta", "neajlov", "retezat", "rodneicalimani")
ltser_buff <- buffer(ltser, 0.05)

# pentru climp raster
rs_wgs84 <- "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"4326\"]]"
rou <- vect("shp/rou_border.shp") 
rou_buff <- vect("shp/rou_buff_5km.shp") 
crs(ltser_buff) <- rs_wgs84 


# # reluare de unde s-a oprit
# dats_all <- strsplit(files_final, "1km_|_CEURO") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
#   unlist() |> substr(1,8) |> as.Date("%Y%m%d")
# tifs <- list.files("grids/SWI/ro", pattern = "tif$", recursive = T) |> substr(13, 20) |> as.Date("%Y%m%d")
# 
# files_final <- files_final[!dats_all %in% tifs]

for (i in 1:length(files_sub)) {
  # fromateaza data
  day <- strsplit(files_sub[i], "1km_|_CEURO")[[1]][5] %>% substr(1,12) %>% as.POSIXct("%Y%m%d%H%M", tz = "UTC")
  print(day)
  # incepe procesarea
  r <- rast(files_sub[i])
  
  # # pentru fiecare zona
  # for (e in 1:length(ltser$natcode)) {
  
  r.sub <- crop(r[[1]], ext(rou_buff)) * 0.5
  r.sub <- terra::mask(r.sub, rou_buff)
  ver.max <- global(r.sub, max, na.rm = T)
  if (is.na(ver.max)) next # daca nu ai valori in susbset, mergi mai departe
  r.sub[r.sub %in% c(241,242, 255, 251, 252, 253,255,200)] <- NA
  
  # ctry_utm <-  vect(paste0("shp/",names(exts[e]),"_L1_roi.shp"))
  # print(show( ctry_utm ))
  # ctry_geo <- project(ctry_utm, "EPSG:4326")
  # #plot(mean(r.sub, na.rm = T))
  # #plot(ctry_geo, add = T)
  
  write_netcdf(x = r.sub, filename = "ncs/SSM_ltser_day.nc", timestamp = day, compression = 9,nvars = 1,  timestep = "days",
               resolution = "increment", var1_short = "SSM", var1_long = "Surface Soil Moisture - LTSER Rou", precision = "integer",
               units1 = "%", title = "Surface Soil Moisture", copyright = "copyright: Copernicus Service information 2019 - https://land.copernicus.eu/"
  )
}


# # upload to ftp
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/fr/  /mnt/Z//MAGDA/input/France/Dynamic/RS/SWI')  
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/ro/  /home/rstudio/fw_ftp/MAGDA/input/Romania/Dynamic/RS/SWI')
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/it/  /home/rstudio/fw_ftp/MAGDA/input/Italia/Dynamic/RS/SWI')


