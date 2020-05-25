# Dr. Carina Kuebert-Flock
# RELA_MODISLayerstack_NDVI.R
# 2020-05-15
# last run: 2020-05-25
# R version 3.6.2 (2019-12-12) - "Dark and Stormy Night"
# This program layerstacks MODIS data downloaded from appEEARS. 

### packages ####
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }
loadandinstall("stringr")
loadandinstall("raster")
loadandinstall("rgeos")
loadandinstall("rgdal")
loadandinstall("sf")
loadandinstall("gdalUtils")
gdal_setInstallation()
gdalPath <- getOption("gdalUtils_gdalPath")[[1]]$path # find gdal installation

#### functions ####
myStackInteger <- function(files2stack, dtsetR){
  
  # define the output vrt-file
  vrtFile <- file.path(indir, paste0("MOD13Q1.006__250m_16_days_NDVI_", years[i],'.vrt'))
  # built a vrt from all infiles
  gdalbuildvrt(infiles, vrtFile, separate=T)
  
  # read data into variable datafile
  datafile <- stack(vrtFile)
  # setMinMax
  datafile <- setMinMax(datafile)
  # set NAvalue
  NAvalue(datafile) <- -3000
  
  # save file to ~/MODIS_DOA_input/
  writeRaster(datafile, filename=paste0(odir, "/", basename(gsub(".vrt", ".tif", file.path(vrtFile)))), datatype=dtsetR)
}


#### input parameters / user settings ####
indir <- "F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/MODIS_Ufr_2001-2019/MOD13Q1_NDVI_Unterfranken_2001-2019"
odir <-  "F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/MODIS_Ufr_2001-2019/MODIS_layerstacks"
# create odir, if it does't exist
if (file.exists(odir) != T) {dir.create(odir)}
# define year range
years <- seq(2001,2019) # years <- 2019
dtsetR <- "INT2S" # for NDVI

# for each year = each folder
for (i in 1:length(years)){
  idir <- paste0(indir, "/", years[i])
  # list all files to stack
  infiles <- list.files(idir, pattern = "*.tif$", full.names = T)
  # stack all files, mask them and save output using function myStackInteger
  myStackInteger(files2stack = infiles, dtsetR)
}



