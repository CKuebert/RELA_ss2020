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











#ToDo (data preparation): 
#1. download MODQ1 data at https://lpdaac.usgs.gov/products/modis_products_table/mod13q1
#2. use the MODIS reprojection tool (https://lpdaac.usgs.gov/tools/modis_reprojection_tool) to convert MODIS hdf data into Geotif
#3. create one folder, where you will store all the data, e.g. C://Data
#4. For each day of the year (DOY) create one subfolder. The name of each folder must start with "DOY_", e.g. DOY_033
#5. Rename the files by addding a prefix following the pattern DOY_YYYY_, e.g. 033_2001 or 001_2005. 
#Total Commander is a useful tool to rename multiple files (using Total Commander)http://www.ghisler.com/index.htm).
#Renaming the files is important to automatize the filenames and the titles of the resulting maps.
#6. Create another subfolder within the main data folder called "shape". Store the shapefile with the country boarder here.


# installing relevant packages
install.packages("raster") #you only have to do this once
install.packages("rgdal") #you only have to do this once

library(raster)
library(rgdal)

# load borders 
border<-shapefile("C:/Data/MOD13Q1_EVI/shape/GTM_adm0.shp") #ToDo: insert link to the shapefile with the country borders
# download country borders as shapefiles http://www.gadm.org/download

path <- "C:/Data/MOD13Q1_EVI" #ToDo: enter link to the folder where you have stored the MODIS data
dlist <- dir(path,pattern="DOY") 

pb <- txtProgressBar (min=0, max=length(dlist), style=1) # this creates a progress bar in the Console, 
#which ends at the end of the loop. The proegress bar looks like this: =========
setTxtProgressBar (pb, 0)
for (i in 1:length(dlist)) {            # start of the outer for-loop
  fold <- paste(path,dlist[i],sep="/")  # the respective DOY-folder
  fls <- dir(fold,pattern=".tif")       # all files that are available in the respective DOY-folder
  flsp <-paste(fold,fls,sep="/")        # all files that are available in the respective DOY-folder with complete path name
  
  ndvistack <- stack(flsp) #creates a layer stack of all files within the DOY folder
  ndviresize<- crop(ndvistack,border) #resizes the layer stack to the rectangular extent of the border shapefile
  ndvimask<-mask(ndviresize,border) # masks the layer stack using the border shapefile
  ndvi<-ndvimask*0.0001 #rescaling of MODIS data
  ndvi[ndvi==-0.3]<-NA #Fill value(-0,3) in NA
  ndvi[ndvi<(-0.2)]<-NA # as valid range is -0.2 -1 , all values smaller than -0,2 are masked out
  
  # extracting max and min value for each pixel
  ndvimax <- stackApply (ndvi, rep (1, nlayers (ndvi)),max, na.rm=F) #calculating the maximum value for the layer stack for each indivisual pixel
  ndvimin <- stackApply (ndvi, rep (1, nlayers (ndvi)), min, na.rm=F) #calculating the minimum value for the layer stack for each indivisual pixel
  
  # If na.rm is FALSE an NA value in any of the arguments will cause a value of NA to be returned, otherwise NA values are ignored.
  # https://stat.ethz.ch/R-manual/R-devel/library/base/html/Extremes.html
  
  z<-ndvimax - ndvimin # aggregation of the determinator
  
  VCI_all <- ((ndvi-ndvimin)/z)*100 #calculating VCI
  
  my_palette <- colorRampPalette(c("red", "yellow", "lightgreen")) #definition of the color scheme of the resulting maps
  
  
  
  for (k in 1:nlayers(VCI_all)) {     # start of the inner for-loop
    
    year <- substr(fls[k],5,8) #extracting the fifth to eigths letter of the filename, which is the year (cf. data preparation above)
    doy <- substr(fls[k],1,3) #extracting the first to third letter of the filename, which is the DOY (cf. data preparation above)
    
    
    #writeRaster(ndvi[[k]], filename=paste(fold,"/",doy,"_",year,sep=""), format="ENVI", datatype='FLT4S', overwrite=TRUE)        # in case you would like to have Envi files (Attention: note the datatype)
    jpeg(filename=paste(fold,"/",doy,"_",year,".jpg",sep=""), quality = 100) #writes the jpg maps and names the files autmatically accoring to the pattern DOY_YYYY
    
    plot(VCI_all[[k]],zlim=c(0,100), col=my_palette(101),main=paste(doy," VCI "," (NDVI) ",year,sep=""))#automizes the title of the plot. ToDo: Adjust the file naming according to the data you are processing! E.g. if you base your VCI on EVI data, write (EVI) instead of (NDVI)
    
    dev.off()
    
    
    writeRaster(VCI_all[[k]], filename=paste(fold,"/",doy,"_",year,".tif",sep=""), format="GTiff", overwrite=TRUE) #writes the geotiff and automizes the file naming according to the pattern DOY_YYYY
  }       # end of the inner for-loop
  
  
  setTxtProgressBar (pb, i)
}                         # end of the outer for-loop

