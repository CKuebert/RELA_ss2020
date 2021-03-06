# Dr. Carina Kübert-Flock
# 00_structureMODISdata.R
# 2020-05-17
# last run: 2020-05-25
# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"
# small program to structure MODIS data downloaded with appEEARS into subfolders by year
# to run the code, you need to first copy all downloaded MODIS SDS into one dir (NDVI into one, Quality into another), which then is specified in line 17


#### install packages ####
# not run, because only {base} functions needed
# loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE) }
# print("Loading necessary Libraries")


#### input parameters / user settings ####
# specify download folder 
indir <- "F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/MOD13A3_pixReli_1km_monthly" 
# list all tiff-files
files <- list.files(indir, patter = "*.tif$", full.names = T)
# infiles <- basename(files)

# vector of years downloaded
years <- seq(2001, 2019) # alternative: years <- c(2001,2005,2019)

for (i in 1:length(years)){
  # define subdir name
  odir <- paste0(indir, "/", years[i])
  # create the subdir year[i]
  if (file.exists(odir) != T) {dir.create(odir)}
  
  # select all files for year[i]
  cpfiles <- files[grep(paste0("doy", years[i]), files)]
  
  # move files
  file.rename(cpfiles, paste0(odir, "/",basename(cpfiles)))
  
}