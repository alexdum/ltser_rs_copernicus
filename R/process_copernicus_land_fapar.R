library(terra)
#library(sf)
library(dplyr)
setwd("~/ltser_rs_copernicus")
source("R/fun/write_netcdf.R")
#tab <- read.table("https://land.copernicus.vgt.vito.be/manifest/swi_v1_1km/manifest_cgls_swi_v1_1km_20230504.txt")
# write without auxiliary file

files1 <- 
  list.files("/data/MTDA/BIOPAR/BioPar_FAPAR300_V1_Global/", recursive = T, full.names = T, pattern = ".nc$") %>%
  grep("GLOBE_PROBAV_",., value = T) %>% grep("RT",., value = T, invert = T)
dats1_all <- strsplit(files1, "300_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")
files1 <- files1[dats1_all >= as.Date("2015-01-01") & dats1_all < as.Date("2016-07-31")]
dats1_all <- strsplit(files1, "300_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")

files2 <- 
  list.files("/data/MTDA/BIOPAR/BioPar_FAPAR300_V1_Global/", recursive = T, full.names = T, pattern = ".nc$") %>%
  grep("GLOBE_PROBAV_",., value = T) %>% grep("RT2",., value = T)
dats2_all <- strsplit(files2, "RT2_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(4) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")
files2 <- files2[dats2_all <  as.Date("2020-08-20")]
dats2_all <- strsplit(files2, "RT2_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(4) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")

files3 <- 
  list.files("/data/MTDA/BIOPAR/BioPar_FAPAR300_V1_Global/", recursive = T, full.names = T, pattern = ".nc$") %>%
  grep("GLOBE_OLCI_",., value = T)  %>% grep("RT2",., value = T) 
dats3_all <- strsplit(files3, "RT2_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(4) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")


files <- c(files1, files2,files3)
dats_all <-c(dats1_all, dats2_all, dats3_all)
dats_all[duplicated(dats_all)] 

# citeste datele raster pentru completare

fapar <- rast("ncs/fapar_ltser_10day.nc")
rdats <- as.Date(names(fapar) %>% gsub("fapar_days=", "",.) %>% as.integer(), origin = "1970-1-1 00:00:00")

files_sub <- files[!dats_all %in% rdats]
dats_sub <- strsplit(files_sub, "RT2_|_GLOBE") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(4) |>
  unlist() |> substr(1,8) |> as.Date("%Y%m%d")



# pentru climp raster
rs_wgs84 <- "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.0174532925199433,AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"4326\"]]"
rou <- vect("shp/rou_border.shp") 
rou_buff <- vect("shp/rou_buff_5km.shp") 


# # reluare de unde s-a oprit
# dats_all <- strsplit(files_final, "1km_|_CEURO") %>% do.call(rbind,.) |> as_tibble() |> dplyr::select(5) |>
#   unlist() |> substr(1,8) |> as.Date("%Y%m%d")
# tifs <- list.files("grids/SWI/ro", pattern = "tif$", recursive = T) |> substr(13, 20) |> as.Date("%Y%m%d")
# 
# files_final <- files_final[!dats_all %in% tifs]

for (i in 1:length(files_sub)) {
  # fromateaza data
  day <- dats_all[i]
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
  
  write_netcdf(x = r.sub, filename = "ncs/fapar_ltser_10day.nc", timestamp = day, compression = 9,nvars = 1,  timestep = "days",
               resolution = "increment", var1_short = "fapar", var1_long = "FRACTION OF PHOTOSYNTHETICALLY ACTIVE RADIATION - LTSER Rou", precision = "float",
               units1 = "", title = "FRACTION OF PHOTOSYNTHETICALLY ACTIVE RADIATION", copyright = "copyright: Copernicus Service information 2019 - https://land.copernicus.eu/",
               rnd = 2
  )
}




system(paste0("cdo monmean ncs/fapar_ltser_10day.nc ncs/fapar_ltser_mon.nc"))




# # upload to ftp
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/fr/  /mnt/Z//MAGDA/input/France/Dynamic/RS/SWI')  
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/ro/  /home/rstudio/fw_ftp/MAGDA/input/Romania/Dynamic/RS/SWI')
# system('rsync -avu --delete /home/rstudio/magda_rs_copernicus/grids/SWI/it/  /home/rstudio/fw_ftp/MAGDA/input/Italia/Dynamic/RS/SWI')


