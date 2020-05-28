# Dr. Carina Kübert-Flock
# 00_readPlotcsv_appEEARS.R
# 2020-05-15
# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"
# small program to plot mean NDVI time series from the csv-file downloaded with appEEARS.

#### install packages ####
loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE) }
print("Loading necessary Libraries")
loadandinstall("ggplot2")
loadandinstall("utils")
loadandinstall("RCurl")

#### read data ####
dat <- read.csv("https://raw.github.com/CKuebert/RELA_ss2020/master/MOD13Q1-006-Statistics_2001-2019.csv")

#### add information ####
dat$x <- seq(1:dim(dat)[1])
dat$Date <- as.Date(dat$Date)

#### plot data ####
p <- ggplot(dat, aes(x, Mean))
p + geom_point() +
  geom_line() + 
  xlab("Year") +
  ylab("Mean NDVI") + 
  scale_x_continuous(breaks=dat$x[seq(1, dim(dat)[1], 23)], labels = seq(2001,2019)) + # this line only works for the *.csv-file specified in line 15
  theme_bw()
# save the plot
ggsave(paste0(getwd(), "/MODISmeanNDVI_2001-2019.png"), plot = last_plot(), width = 210, height = 100, units = "mm")





