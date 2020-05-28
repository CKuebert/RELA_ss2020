# Dr. Carina Kuebert-Flock
# 02_RELA_MODIS_layerstack_pixReli.R
# 2020-05-25
# last run: 2020-05-25
# R version 3.6.2 (2019-12-12) - "Dark and Stormy Night"
# This program layerstacks MODIS data downloaded from appEEARS. 
# Before running the code, you need to copy all AppEEARS data into a separate folder. 

### packages ####
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }
loadandinstall("stringr")
loadandinstall("gdalUtils")
loadandinstall("raster")
loadandinstall("rgeos")
loadandinstall("rgdal")
loadandinstall("sf")

#### input parameters / user settings ####
indir <- "F:/.../MOD13Q1_PixelReliability_Unterfranken_2001-2019"
odir <-  "F:/.../MODIS_layerstacks"
# create odir, if it does't exist
if (file.exists(odir) != T) {dir.create(odir)}
# define year range
years <- seq(2001,2019) # years <- 2019
dtsetR <- "INT2S" # for pixReli

# read shapefile of study area, here Lower Franconia, for masking the study area
shp <- st_read("F:/.../Unterfranken.shp")

# define year range
years <- seq(2001,2019) # years <- 2019
dtsetR <- "INT2S" # for NDVI

#### for each year = each folder ####
for (i in 1:length(years)){
  idir <- paste0(indir, "/", years[i])
  # list all files to stack
  infiles <- list.files(idir, pattern = "*.tif$", full.names = T)
  
  #### stack all files, mask them and save output using function myStackInteger ####
  # define the output vrt-file
  vrtFile <- file.path(indir, paste0("MOD13Q1.006__250m_16_days_pixReli_", years[i],'.vrt'))
  # built a vrt from all infiles
  gdalbuildvrt(infiles, vrtFile, separate=T)
  
  # read data into variable datafile
  datafile <- stack(vrtFile)
  # setMinMax
  datafile <- setMinMax(datafile)
  # set NAvalue
  NAvalue(datafile) <- -3000
  
  # # check, how datafile looks by plotting the first layer datafile[[1]]
  # plot(datafile[[1]])
  
  # mask study area
  datafile2 <- mask(datafile,shp)
  
  # # check, how datafile2 looks by plotting the first layer datafile2[[1]]
  # plot(datafile2[[1]])
  
  # save file to ~/MODIS_DOA_input/
  writeRaster(datafile2, filename=paste0(odir, "/", basename(gsub(".vrt", ".tif", file.path(vrtFile)))), datatype="INT2S")
}