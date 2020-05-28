# Dr. Carina Kuebert-Flock
# 05_MODIS_NDVIstatistics.R
# 2020-05-26
# last run: 2020-05-26
# R version 3.6.2 (2019-12-12) - "Dark and Stormy Night"
# This program performs the task defined in "Übung 3" as of 2020-05-26:
# - load NDVI layerstacks
# - load CORINE Land Cover classsification (CLC) shapefile
# - mask according to CLC
# - derive NDVI statistics for selected timesteps (here: beginning of April: timestep 7; end of July: timestep 14; mid of october: timestep 19)
# - make some plots and run statsitcal tests (Shapiro-Wilk-test for normal distribution and t-tests)
# Note: This code is intended to be readable and executable for students on a R beginner level. It can be re-written to perform more efficient (e.g. apply-functions instead of for-loops).

### packages ####
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE)  }
loadandinstall("stringr")
loadandinstall("sf")
loadandinstall("raster")
loadandinstall("ggplot2")
loadandinstall("pastecs")
loadandinstall("reshape2")

#### user input ####
# MODIS data (already masked by pixelSummary and interpolated)
indir <- "F:/2019_LehreCK_Sicherung/03_MSc/02_RELA-2/RELA-Daten/MODIS_Ufr_2001-2019/MODIS_layerstacks/04_NDVI_QA_maskiert_interpoliert/"
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
tstepmonth <- c("April", "July", "October")

#### part 1: derive statistics (mon, max, mean, sd) ####
# we need a variable to save the results, here an empty data frame with 8 columns and 114 rows
allStats <- data.frame(matrix(ncol = 8, nrow = length(years)*length(clc)*length(tstep)))
colnames(allStats) <- c("year", "month", "clc", "n", "min", "max", "mean", "sd")
# and an indexing variable
count <- 0

# we need three loops: 
for (i in 1:length(years)){ # loop for years
  # read the layerstack
  data <- brick(infiles[i])
  
  for (j in 1:length(clc)){ # loop for CLC
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
      stats <- c(cellStats(tsdata, function(x, na.rm=T) sum(!is.na(x))), 
                 cellStats(tsdata, 'min', na.rm = T), 
                 cellStats(tsdata, 'max', na.rm = T), 
                 cellStats(tsdata, 'mean', na.rm = T), 
                 cellStats(tsdata, 'sd', na.rm = T))
      
      # save results into a variable
      res <- c(years[i], tstepmonth[k], clc[j], unlist(stats))
      allStats[count, ] <- res
      
    } # end k
    
  } # end j
  
} # end i

# save the output
colnames(allStats) <- c("year", "month", "clc", "n", "min", "max", "mean", "sd")
# conversion of dataType 
allStats$year <- as.factor(allStats$year)
allStats$month <- as.factor(allStats$month)
allStats$clc <- as.factor(allStats$clc)
allStats$n <- as.numeric(allStats$n)
allStats$min <- as.numeric(allStats$min)
allStats$max <- as.numeric(allStats$max)
allStats$mean <- round(as.numeric(allStats$mean), digits=0)
allStats$sd <- round(as.numeric(allStats$sd), digits=0)
allStats$ID <- paste0(allStats$year, "_", allStats$month)

write.csv2(allStats, file="spatialAnalysis.csv", quote=F, row.names=F)


#### part 2: plots and statistics as shown in presentation during course ####
#### normal distribution? ####
dataVals <- raster::values(tsdata)
# remove NA
dataVals <- dataVals[!is.na(dataVals)]
# testing wheter the distribution is normal
# since shapiro.test() only accepts up to 5000 elements, we first need to sample 5000 elements from dataVals
samp <- sample(dataVals, 5000)
shapiro.test(samp)

# plot qqplot
qplot(sample=samp, stat="qq") +
 xlab("theoretisch") +
 ylab("Sample") +
 ggtitle("QQ-Plot des NDVI (Zeitschnitt 19 Jahr 2019, CLC 311)") +
 theme_bw()
# save the plot
ggsave(filename = "QQPlotNDVI_2019_ts19_CLC311.png", dpi=300)

# plot histogram
p <- ggplot() +
 # geom_histogram(aes(dataVals)) +
 geom_density(aes(dataVals)) +
 theme_bw() +
 ggtitle("Dichteverteilung des NDVI (Zeitschnitt 19 Jahr 2019, CLC 311)") +
 xlab("NDVI * 10000") +
 ylab("Verteilungsdichte")
p
# save the plot
ggsave(filename = "densityPlotNDVI_2019_ts19_CLC311.png", dpi=300)


#### some t-tests ####
# t-Test für 2019 April both CLC
alph <- .05 # Irrtumswahrscheinlichkeit
clc231 <- 109 # rownumber for 231
clc311 <- 112 # rownumber for 311
  
n231 <- allStats$n[clc231]
n311 <- allStats$n[clc311]
m231 <- allStats$mean[clc231] 
m311 <- allStats$mean[clc311] 
st231 <- allStats$sd [clc231]
st311 <- allStats$sd[clc311] 

# Add On: Erstellen von Datensätzen, die so verteilt sind, wie gefordert und Test mit implementierter R-Funktion t.test():
CLC231 <- round(rnorm(n231, m231, st231), digits=0) # round(), da nur ganzzahlige Bienen gezählt werden 
CLC311  <- round(rnorm(n311, m311, st311), digits=0) # round(), da nur ganzzahlige Bienen gezählt werden 
# einfachste Lösung mit verwenden der Funktion t.test(), aber nur wenn Rohdaten vorhanden
t.test(CLC231, CLC311, var.equal = T, alternative = "two.sided")


# t-Test für 2019 Juli both CLC
alph <- .05 # Irrtumswahrscheinlichkeit
clc231 <- 110 # rownumber for 231
clc311 <- 113 # rownumber for 311

n231 <- allStats$n[clc231]
n311 <- allStats$n[clc311]
m231 <- allStats$mean[clc231] 
m311 <- allStats$mean[clc311] 
st231 <- allStats$sd [clc231]
st311 <- allStats$sd[clc311] 

# Add On: Erstellen von Datensätzen, die so verteilt sind, wie gefordert und Test mit implementierter R-Funktion t.test():
CLC231 <- round(rnorm(n231, m231, st231), digits=0) # round(), da nur ganzzahlige Bienen gezählt werden 
CLC311  <- round(rnorm(n311, m311, st311), digits=0) # round(), da nur ganzzahlige Bienen gezählt werden 
# einfachste Lösung mit verwenden der Funktion t.test(), aber nur wenn Rohdaten vorhanden
t.test(CLC231, CLC311, var.equal = T, alternative = "two.sided")

# effect size
res <- t.test(sama, samb, var.equal = T, alternative = "two.sided")
t <- res$statistic[[1]]
degfree <- res$parameter[[1]]
es <- sqrt(t^2/(t^2+degfree))



# t-Test für CLC231
alph <- .05 # Irrtumswahrscheinlichkeit
a <- 110 # rownumber for July
b <- 111 # rownumber for October

n_a <- allStats$n[a]
nb <- allStats$n[b]
ma <- allStats$mean[a] 
mb <- allStats$mean[b] 
sta <- allStats$sd [a]
stb <- allStats$sd[b] 

# Add On: Erstellen von Datensätzen, die so verteilt sind, wie gefordert und Test mit implementierter R-Funktion t.test():
sama <- round(rnorm(n_a, ma, sta), digits=0) # round(), da nur ganzzahlige Bienen gezählt werden 
samb  <- round(rnorm(nb, mb, stb), digits=0) # round(), da nur ganzzahlige Bienen gezählt werden 
# einfachste Lösung mit verwenden der Funktion t.test(), aber nur wenn Rohdaten vorhanden
t.test(sama, samb, var.equal = T, alternative = "two.sided")


# t-Test für CLC311
alph <- .05 # Irrtumswahrscheinlichkeit
a <- 113 # rownumber for July
b <- 114 # rownumber for October

n_a <- allStats$n[a]
nb <- allStats$n[b]
ma <- allStats$mean[a] 
mb <- allStats$mean[b] 
sta <- allStats$sd [a]
stb <- allStats$sd[b] 

# Add On: Erstellen von Datensätzen, die so verteilt sind, wie gefordert und Test mit implementierter R-Funktion t.test():
sama <- round(rnorm(n_a, ma, sta), digits=0) # round(), da nur ganzzahlige Bienen gezählt werden 
samb  <- round(rnorm(nb, mb, stb), digits=0) # round(), da nur ganzzahlige Bienen gezählt werden 
# einfachste Lösung mit verwenden der Funktion t.test(), aber nur wenn Rohdaten vorhanden
t.test(sama, samb, var.equal = T, alternative = "two.sided")

# effect size
res <- t.test(sama, samb, var.equal = T, alternative = "two.sided")
t <- res$statistic[[1]]
degfree <- res$parameter[[1]]
es <- sqrt(t^2/(t^2+degfree))


#### some more graphics ####
# melt year and month
df2 <- melt(data = allStats, id.vars = c('year', "month", "clc", "ID"), measure.vars = c('min', "max", "mean", "sd"))
p <- ggplot(subset(df2, variable == "mean"), aes(ID, value, colour = clc)) +
  geom_point(size=3) +
  geom_line(aes(group = clc)) +
  facet_grid(rows = vars(clc)) +
  scale_colour_discrete(name = "Landnutzungsklasse") +
  theme_bw() + 
  ggtitle("Mittlerer NDVI für zwei Landnutzungsklassen für ausgewählte Monate von 2001-2019") +
  xlab("Datum") +
  ylab("mittlerer NDVI * 10000") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        legend.position = "bottom") 
p
ggsave(filename = "MittlererNDVI_timeseriesMonths_beideCLC.png", dpi=300)

# only year 2019
df3 <- subset(df2, year == 2019 & variable == "mean") 
p <- ggplot(df3, aes(ID, value, colour = clc)) +
  geom_point(size = 4) +
  geom_line(aes(group = clc)) +
  scale_colour_discrete(name = "Landnutzungsklasse") +
  theme_bw() + 
  ggtitle("Mittlerer NDVI für zwei Landnutzungsklassen im Jahr 2019") +
  xlab("Datum") +
  ylab("mittlerer NDVI * 10000") 
p
ggsave(filename = "MittlererNDVI_2019_beideCLC.png", dpi=300)

# only July for CLC 311
df4 <- subset(df2, month == "July" & variable == "mean" & clc == "311") 
p <- ggplot(df4, aes(ID, value, colour = clc)) +
  geom_point(size = 4) +
  geom_line(aes(group = clc)) +
  geom_hline(yintercept =  mean(df4$value)) +
  scale_colour_discrete(name = "Landnutzungsklasse") +
  theme_bw() + 
  ggtitle("Mittlerer NDVI der Landnutzungsklasse Laubwald im August von 2001-2019") +
  xlab("Datum") +
  ylab("mittlerer NDVI * 10000") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        legend.position = "bottom") 
p
ggsave(filename = "MittlererNDVI_July_CLC311.png", dpi=300)


