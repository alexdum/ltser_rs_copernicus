library(terra)
#library(sf)
library(dplyr)
setwd("~/ltser_rs_copernicus")
source("R/fun/write_netcdf.R")
#tab <- read.table("https://land.copernicus.vgt.vito.be/manifest/swi_v1_1km/manifest_cgls_swi_v1_1km_20230504.txt")
# write without auxiliary file

files1 <- list.files("/data/MTDA/BIOPAR/BioPar_NDVI300_V1_Global/", recursive = T, full.names = T, pattern = ".nc$")
dats1_all <- strsplit(files1, "300_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")
files1 <- files1[dats1_all >= as.Date("2015-01-01") & dats1_all <= as.Date("2020-06-30")]

files2 <- list.files("/data/MTDA/BIOPAR/BioPar_NDVI300_V2_Global/", recursive = T, full.names = T, pattern = ".nc$")
dats2_all <- strsplit(files2, "300_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")

files <- c(files1)
dats_all <- strsplit(files, "300_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")
dats_all[duplicated(dats1_all)] 
# selecteaza doar pe cele din ziua 1, care reprezinta ndvi pentru ultimele 30 de zile




files_sub <- files[dats_all < as.Date("2020-07-01")]
dats_sub <- strsplit(files_sub, "NDVI300_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
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
  day <- strsplit(files_sub[i], "NDVI300_|_GLOBE")[[1]][5] %>% substr(1,12) %>% as.POSIXct("%Y%m%d%H%M", tz = "UTC")
  print(day)
  # incepe procesarea
  r <- rast(files_sub[i])
  
  # # pentru fiecare zona
  # for (e in 1:length(ltser$natcode)) {
  
  r.sub <- crop(r[[1]], ext(rou_buff)) 
  r.sub <- terra::mask(r.sub, rou_buff)
  
 
 
  # r.sub[r.sub %in% c(241,242, 255, 251, 252, 253,255,200)] <- NA
  # elimina factor scale,face rast automat
  # r.sub <- r.sub * 0.5
  ver.max <- global(r.sub, max, na.rm = T)
  if (is.na(ver.max)) next # daca nu ai valori in susbset, mergi mai departe
  
  # ctry_utm <-  vect(paste0("shp/",names(exts[e]),"_L1_roi.shp"))
  # print(show( ctry_utm ))
  # ctry_geo <- project(ctry_utm, "EPSG:4326")
  # #plot(mean(r.sub, na.rm = T))
  # #plot(ctry_geo, add = T)
  
  write_netcdf(x = r.sub, filename = "ncs/ndvip1_ltser_10day.nc", timestamp = day, compression = 9,nvars = 1,  timestep = "days",
               resolution = "increment", var1_short = "ndvi", var1_long = "NORMALIZED DIFFERENCE VEGETATION INDEX - LTSER Rou", precision = "float",
               units1 = "", title = "Surface Soil Moisture", copyright = "copyright: Copernicus Service information 2019 - https://land.copernicus.eu/",
               rnd = 2
  )
}


nc <- rast("ncs/ndvi_ltser_10day.nc")
dats <- names(nc) %>% gsub("ndvi_days=", "",.) |> as.numeric() |> as.Date(origin = "1970-1-1")
timesteps <- which(format(dats, "%d") %in% "21")
system(paste0("cdo select,timestep=",gsub(" " , "", toString(timesteps))," ncs/ndvi_ltser_10day.nc ncs/ndvi_ltser_mon.nc"))


# # upload to ftp
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/fr/  /mnt/Z//MAGDA/input/France/Dynamic/RS/SWI')  
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/ro/  /home/rstudio/fw_ftp/MAGDA/input/Romania/Dynamic/RS/SWI')
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/it/  /home/rstudio/fw_ftp/MAGDA/input/Italia/Dynamic/RS/SWI')


