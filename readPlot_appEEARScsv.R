# Dr. Carina Kübert-Flock
# readPlot_appEEARScsv.R
# 2020-05-15
# R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night"
# small program to plot mean NDVI time series 


loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1])){install.packages(mypkg)}; library(mypkg, character.only=TRUE) }
print("Loading necessary Libraries")
loadandinstall("ggplot2")
loadandinstall("utils")
loadandinstall("RCurl")


dat <- read.csv("https://raw.github.com/CKuebert/RELA_ss2020/master/MOD13Q1-006-Statistics_2001-2019.csv")
# dat <- read.csv2(file, header = T, sep = ",", dec = ".")
dat$x <- seq(1:dim(dat)[1])
dat$Date <- as.Date(dat$Date)


p <- ggplot(dat, aes(x, Mean))
p + geom_point() +
  geom_line() + 
  xlab("Jahr") +
  ylab("Mean NDVI") + 
  scale_x_continuous(breaks=dat$x[seq(2, dim(dat)[1], 23)], labels = seq(2001,2019)) +
  theme_bw()




