# Dr. Carina Kuebert-Flock
# 06_MOD13A3_maskLF_QA.R
# 2020-05-31
# last run: 2020-05-31
# R version 3.6.2 (2019-12-12) - "Dark and Stormy Night"
# This program 
# - masks MODIS NDVI downloaded from appEEARS for the study area and
# - performs Quality Assessment using the pixel reliability

### packages ####
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }
loadandinstall("stringr")
loadandinstall("raster")
loadandinstall("rgeos")
loadandinstall("rgdal")
loadandinstall("sf")


#### input parameters / user settings ####
# path to NDVI dir
indirNDVI <- "F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/MOD13A3_NDVI_1km_monthly"
# path to pixReli dir
indirQA <- "F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/MOD13A3_pixReli_1km_monthly"
# path to output where layserstacks are saved
odir <-  "F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/MOD13A3_NDVImaskLF_QA_1km_monthly"
# create odir, if it does't exist
if (file.exists(odir) != T) {dir.create(odir)}

# read shapefile of study area, here Lower Franconia, for masking the study area
shp <- st_read("F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/Vektordaten_Unterfranken/Unterfranken_CLC10.shp")

# create output file string
mon <- seq(1,12)
mon1 <- paste0(str_pad(seq(1, 12), 2, pad = "0") , "_", month.abb[mon])

# define year range
years <- seq(2001,2019) # years <- 2019

for (i in 1:length(years)){
  idirNDVI <- paste0(indirNDVI, "/", years[i])
  idirQA <- paste0(indirQA, "/", years[i])
  
  # list all files to stack
  infilesNDVI <- list.files(idirNDVI, pattern = "*.tif$", full.names = T)
  infilesQA <- list.files(idirQA, pattern = "*.tif$", full.names = T)
  
  for (j in 1:length(infilesNDVI)){ # here: each month
    # read dataset
    data <- raster(infilesNDVI[j])
    
    # mask study area
    data2 <- mask(data, shp)
    
    # perform Quality Assessment
    pixReli <- raster(infilesQA[j])
    
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
    dataOut <- mask(data2, pixReliBinary, maskvalue = 1)
    
    # save file to ~/MODIS_DOA_input/
    writeRaster(dataOut, filename=paste0(odir, "/", paste0("MOD13A3_1km_monthly_NDVI_", years[i], "_", mon1[j], "_maskLF_QA.tif")), datatype="INT2S")
  } # end j
  
} # end i