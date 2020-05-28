# Dr. Carina Kuebert-Flock
# 04_MODIS_interpolateNDVI.R
# 2020-05-25
# last run: 2020-05-25
# R version 3.6.2 (2019-12-12) - "Dark and Stormy Night"
# This program interpolates NDVI values which were masked out due to bad quality
# run this code after you ran 03_MODIS_maskNDVIwithQAsummary.R

### packages ####
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }
loadandinstall("stringr")
loadandinstall("raster")
loadandinstall("rgeos")
loadandinstall("rgdal")

#### input parameters / user settings ####
indir <- "F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/MODIS_Ufr_2001-2019/MODIS_layerstacks"
infiles <- list.files(indir, pattern = glob2rx("*_NDVI-QA_*.tif$"), full.names = T) 

#### mask and save for each year ####
for (i in 1:length(infiles)){
  
  data <- brick(infiles[i])
  dataInterpol <- raster::approxNA(data, method="linear", rule=1)
  
  # save file to ~/MODIS_DOA_input/
  writeRaster(dataInterpol, filename=paste0(indir, "/", basename(gsub("NDVI-QA", "NDVI-QA_interpol", file.path(infiles[i])))), datatype="INT2S")
  
}
