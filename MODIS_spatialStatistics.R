# Dr. Carina Kuebert-Flock
# MODIS_spatialStatistics.R
# 2020-05-26
# last run: 2020-05-26
# R version 3.6.2 (2019-12-12) - "Dark and Stormy Night"
# This program performs the task defined in "Übung 3" as of 2020-05-26

### packages ####
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }
loadandinstall("stringr")
loadandinstall("raster")

#### user input ####
# MODIS data (already masked by pixelSummary and interpolated)
indir <- "F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/MODIS_Ufr_2001-2019/MODIS_layerstacks/"
infiles <- list.files(indir, pattern = glob2rx("*_NDVI-QA_interpol_*.tif$"), full.names = T) 
# read CLC shapefile 
shp <- st_read("F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/Vektordaten_Unterfranken/Unterfranken_CLC10.shp")

# the tasks need to performed for:
# two CLC classes (pastures (231) and broad-leaved forests (311))
clc <- c("231", "311")
# all years
years <- seq(2001,2019)
# and three timesteps within the layerstacks: 
# beginning of April: timestep 7; end of July: timestep 14; mid of october: timestep 19
tstep <- c(7,14,19)

# we need a variable to save the results, here an empty data frame with 6 columns and 114 rows
allStats <- data.frame(matrix(ncol = 6, nrow = length(years)*length(clc)*length(tstep)))
colnames(allStats) <- c("year", "clc", "min", "max", "mean", "sd")
# and an indexing variable
count <- 0

# we need three loops: 
for (i in 1:length(years)){ # loop for CLC
  # read the layerstack
  data <- brick(infiles[i])
  
  for (j in 1:length(clc)){ # loop for years
    # select only clc[i]
    loc <-  shp[shp$clc == clc[j],]    
    # mask with loc
    dataCLC <- mask(data, loc) # takes some time
    # # check result: 
    # plot(dataCLC)
    
    for (k in 1:(length(tstep))){ # loop for timesteps
      count <- count+1
      # extract layer for timestep k
      tsdata <- dataCLC[[tstep[k]]]
      
      # compute statistics; there are various ways how to do that...
      stats <- c(cellStats(tsdata, 'min'), 
                 cellStats(tsdata, 'max'), 
                 cellStats(tsdata, 'mean'), 
                 cellStats(tsdata, 'sd'))
      
      # save results into a variable
      res <- c(years[i], clc[j], unlist(stats))
      allStats[count, ] <- res
      
    } # end k
    
  } # end j
  
} # end i


# save the output
colnames(allStats) <- c("year", "clc", "min", "max", "mean", "sd")
write.csv2(allStats, file="spatialAnalysis.csv", quote=F, row.names=F)


# here some plots
