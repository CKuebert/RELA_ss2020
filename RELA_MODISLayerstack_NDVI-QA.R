# Dr. Carina Kuebert-Flock
# RELA_MODISLayerstack_NDVI-QA.R
# 2020-05-25
# last run: 2020-05-25
# R version 3.6.2 (2019-12-12) - "Dark and Stormy Night"
# This program masks MODIS NDVI layerstacked data using pixReli layerstacked data

### packages ####
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }
loadandinstall("stringr")
loadandinstall("raster")
loadandinstall("rgeos")
loadandinstall("rgdal")
loadandinstall("sf")

#### input parameters / user settings ####
indir <- "F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/MODIS_Ufr_2001-2019/MODIS_layerstacks"
infilespixReli <- list.files(indir, pattern = glob2rx("*_pixReli_*.tif$"), full.names = T) 
infilesNDVI <- list.files(indir, pattern = glob2rx("*_NDVI_*.tif$"), full.names = T) 

#### mask and save for each year ####
for (i in 1:length(infiles)){
  
  pixReli <- brick(infilespixReli[i])
  NDVI <- brick(infilesNDVI[i])
  
  # chance values (reclass) according to MODIS Pixel Reliability
  # TABLE 1: MOD13Q1 Pixel Reliability
  #-1   Fill/No Data   Not Processed
  # 0   Good Data   Use with confidence
  # 1   Marginal data   Useful, but look at other QA information
  # 2 	Snow/Ice 	Target covered with snow/ice
  # 3 	Cloudy 	Target not visible, covered with cloud
  m <- c(0, 1, 0,  1, 3, 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  # reclassify the values into two groups 
  # all values > 0 and <= 1 become 0, etc.
  # > rclmat
  # [,1] [,2] [,3]
  # [1,]    0    1    0       values from 0 to 1 will be 0; because 1 is appearing in this line first, 1 -> 0
  # [2,]    1    3    1       values from 1 to 3 will be 0
  
  # variable that contains 0 and 1
  pixReliBinary <- raster::reclassify(pixReli, rclmat)
  
  # masked NDVI
  dataOut <- mask(NDVI, pixReliBinary, maskvalue = 1)
  
  # save file to ~/MODIS_DOA_input/
  writeRaster(dataOut, filename=paste0(indir, "/", basename(gsub("pixReli", "NDVI-QA", file.path(infilespixReli[i])))), datatype=dtsetR)
  
}

