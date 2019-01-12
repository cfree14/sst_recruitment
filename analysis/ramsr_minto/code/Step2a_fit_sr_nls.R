
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(freeR)
library(TMB)
library(TMBhelper)

# Directories
basedir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/sst_recruitment"
inputdir <- file.path(basedir, "input")
outputdir <- file.path(basedir, "output")
codedir <- file.path(basedir, "code")

# Read data
load(file.path(inputdir, "ramsr_minto_data_use.Rdata"))

# Read function
source(file.path(codedir, "fitSRnls.R"))


# Fit model
################################################################################

# Stocks
stocks$bh_a <-stocks$bh_b <- stocks$ricker_a <- stocks$ricker_b <- NA

# Loop through stocks
for(i in 1:nrow(stocks)){
  
  # Subset data
  stock <- stocks$stockid[i]
  sdata <- filter(data, stockid==stock)
  
  # Plot data
  plot(r_sd ~ ssb_sd, sdata, bty="n",  main=stock, 
       xlim=c(0,max(ssb_sd)), ylim=c(0,(max(r_sd))), xlab="SSB", ylab="Recruits")
  
  # Fit BevHolt
  srfit <- fitSRnls(r=sdata$r_sd, ssb=sdata$ssb_sd, type="BH")
  if(!inherits(srfit, "try-error")){
    a <- coef(srfit)["a"]; b <- coef(srfit)["b"]
    curve(a*x/(1+b*x), from=0, to=max(sdata$ssb_sd), add=T, lwd=0.9, col="blue")
    stocks$bh_a[i] <- a
    stocks$bh_b[i] <- b
  }
  
  # Fit Ricker
  srfit <- fitSRnls(r=sdata$r_sd, ssb=sdata$ssb_sd, type="Ricker")
  if(!inherits(srfit, "try-error")){
    a <- coef(srfit)["a"]; b <- coef(srfit)["b"]
    curve(a*x*exp(-b*x), from=0, to=max(sdata$ssb_sd), add=T, lwd=0.9, col="red")
    stocks$ricker_a[i] <- a
    stocks$ricker_b[i] <- b
  }

}

