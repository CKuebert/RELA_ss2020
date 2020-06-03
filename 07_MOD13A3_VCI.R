# Dr. Carina Kuebert-Flock
# 07_MOD13A3_VCI.R
# 2020-06-03
# last run: 2020-06-03
# R version 4.0.0 (2020-04-24) -- "Arbor Day"
# This program 
# - calculates monthly VCI, 
# - saves monthly VCI as tiff and
# - plots maps

### packages ####
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }
# install.packages("extrafont")
# library(extrafont)
# loadfonts(device = "win")
loadandinstall("stringr")
loadandinstall("raster")
loadandinstall("rgeos")
loadandinstall("rgdal")
loadandinstall("dplyr")
loadandinstall("sf")
loadandinstall("gdalUtils")
loadandinstall("ggplot2")
loadandinstall("stars")
loadandinstall("RColorBrewer")
loadandinstall("reshape2")

#### input parameters / user settings ####
# path to NDVI dir
indir <- "D:/Lehre_SS20/MOD13A3/MOD13A3_NDVImaskLF_QA_1km_monthly"
# path to output where layserstacks are saved
odir <-  "D:/Lehre_SS20/MOD13A3/MOD13A3_VCI"
# create odir, if it does't exist
if (file.exists(odir) != T) {dir.create(odir)}
# read shapefile of study area, here Lower Franconia, for masking the study area
shp <- st_read("D:/Lehre_SS20/Vektordaten_Unterfranken/Unterfranken.shp")


# create output file string
mon <- seq(1,12)
mon1 <- paste0(str_pad(seq(1, 12), 2, pad = "0") , "_", month.abb[mon])

# define year range
years <- seq(2001,2019) # years <- 2019


#### derive 19-years min and max for each pixel for each month ####
for (i in 1:length(mon)){
  # create subdir for month
  outdir <-  paste0("D:/Lehre_SS20/MOD13A3/MOD13A3_VCI/", mon1[i])
  # create odir, if it does't exist
  if (file.exists(outdir) != T) {dir.create(outdir)}
  
  # part 1: create a vrt-file with layerstacked data
  # list all files
  infiles <- list.files(indir, pattern = mon1[i], full.names = T)
  # define the output vrt-file
  vrtFile <- file.path(indir, paste0("MOD13A3_1km_monthly_NDVI_ls_", mon1[i], ".vrt"))
  # built a vrt from all infiles
  gdalbuildvrt(infiles, vrtFile, separate = T)
  # read vrtfile to variable
  NDVIts <- brick(vrtFile)
  
  # part 2: derive min, max and mean and save the files
  # derive min
  NDVI_min <- calc(NDVIts, fun=function(x){min(x, na.rm = T)})
  writeRaster(NDVI_min, filename=paste0(outdir, "/", paste0("MOD13A3_1km_monthly_NDVI_min_", mon1[i], ".tif")), datatype="INT2S")
  # derive max
  NDVI_max <- calc(NDVIts, fun=function(x){max(x, na.rm = T)})
  writeRaster(NDVI_max, filename=paste0(outdir, "/", paste0("MOD13A3_1km_monthly_NDVI_max_", mon1[i], ".tif")), datatype="INT2S")
  # derive mean
  NDVI_mean <- calc(NDVIts, fun=function(x){mean(x, na.rm = T)})
  writeRaster(NDVI_mean, filename=paste0(outdir, "/", paste0("MOD13A3_1km_monthly_NDVI_mean_", mon1[i], ".tif")), datatype="INT2S")
  
  #### part 3: compute VCI for each layer in NDVIts ####
  # VCI: Vegetation Condition Index as defined by 
  # Kogan, F. N. F. Remote sensing of weather impacts on vegetation in non-homogeneous areas. International Journal of Remote Sensing 1990, 11, 1405–1419.
  VCI <- (NDVIts - NDVI_min) / (NDVI_max - NDVI_min) * 100
  # rename layers
  names(VCI) <- paste0("VCI_", mon1[i], "_", years)
  # save as file
  writeRaster(VCI, filename=paste0(outdir, "/", paste0("MOD13A3_1km_monthly_VCI_ls_", mon1[i], ".tif")), datatype="INT2S")
  
  #### part 4: make a plot with all layers #### 
  # define outfile
  outnamepng <- paste0(outdir, "/VCI_", month.name[i], ".png")
  
  # # create plot version 1: labelling isn't tidy --> see lines 106ff for better plots
  # # read data
  # paste0(outdir, "/", paste0("MOD13A3_1km_monthly_VCI_ls_", mon1[i], ".tif")) %>% read_stars() -> dataC
  # # create plot
  # g <- ggplot() + 
  #   geom_stars(data = dataC) +
  #   facet_wrap(~ band, scales = "free_y", labeller = "label_both") +
  #   scale_x_discrete(expand=c(0,0)) +
  #   scale_y_discrete(expand=c(0,0)) + 
  #   scale_fill_gradient2(midpoint = 50, low = "#A50026", mid = "#EDE68B", high = "#006837", 
  #                        name = "VCI", na.value = "white") +
  #   theme(legend.position="bottom") +
  #   theme_light() +
  #   xlab("") + 
  #   ylab("") +
  #   ggtitle(paste0("Monthly VCI in ", month.name[i], " for Lower Franconia "))
  # g
  
  # create plot version 2
  # fortify raster for plotting with ggplot
  VCI.df <- fortify(VCI)
  # melt df 
  meltVCI <- melt(VCI.df, measure.vars = names(VCI.df)[3:21], id.vars=c("x", "y"))
  # not run: change levels of variable so labelling displays only years
  # levels(meltVCI$variable) <- as.character(years)
  # change levels of variable so labelling displays "Month Years"
  levels(meltVCI$variable)<- as.character(paste(month.name[i], years))
  
  g <- ggplot(meltVCI, aes(x, y)) + 
    geom_raster(aes(fill = value)) +
    facet_wrap(~ variable) +
    scale_fill_gradient2(midpoint = 50, low = "#A50026", mid = "#EDE68B", high = "#006837", 
                         name = "VCI", na.value = "white") +
    theme(legend.position="bottom") +
    theme_light() +
    xlab("") + 
    ylab("") +
    ggtitle(paste0("Monthly VCI in ", month.name[i], " for Lower Franconia "))
  g
  
  ggsave(filename = outnamepng, plot = g, dpi = "print")
  
} # endfor i (each month)




