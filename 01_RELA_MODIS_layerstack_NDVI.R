# Dr. Carina Kuebert-Flock
# 01_RELA_MODIS_layerstack_NDVI.R
# 2020-05-15
# last run: 2020-05-28
# R version 3.6.2 (2019-12-12) - "Dark and Stormy Night"
# This program layerstacks MOD13Q1 NDVI data downloaded from appEEARS as GeoTiff.
# Before running the code, you need to structure all AppEEARS data using 00_structureMODISdata.R

### packages ####
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }
loadandinstall("stringr")
loadandinstall("raster")
loadandinstall("rgeos")
loadandinstall("rgdal")
loadandinstall("sf")
loadandinstall("gdalUtils")


#### input parameters / user settings ####
# path to NDVI dir
indir <- "F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/MODIS_Ufr_2001-2019/MOD13Q1_NDVI_Unterfranken_2001-2019"
# path to output where layserstacks are saved
odir <-  "F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/MOD13A3_NDVI_1km_monthly/MODIS_layerstacks"
# create odir, if it does't exist
if (file.exists(odir) != T) {dir.create(odir)}

# read shapefile of study area, here Lower Franconia, for masking the study area
shp <- st_read("F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/Vektordaten_Unterfranken/Unterfranken_CLC10.shp")

# define year range
years <- seq(2001,2019) # years <- 2019

#### for each year = each folder ####
for (i in 1:length(years)){
  idir <- paste0(indir, "/", years[i])
  # list all files to stack
  infiles <- list.files(idir, pattern = "*.tif$", full.names = T)
  
  #### stack all files, mask them and save output using function myStackInteger ####
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
  
  # # check, how datafile looks by plotting the first layer datafile[[1]]
  # plot(datafile[[1]])
  
  # mask study area
  datafile2 <- mask(datafile, shp)
  
  # # check, how datafile2 looks by plotting the first layer datafile2[[1]]
  # plot(datafile2[[1]])
  
  # save file to ~/MODIS_DOA_input/
  writeRaster(datafile2, filename=paste0(odir, "/", basename(gsub(".vrt", ".tif", file.path(vrtFile)))), datatype="INT2S")
}



