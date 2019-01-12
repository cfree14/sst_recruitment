
# Setup
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
codedir <- "code"
datadir <- "data/ramldb/data"
plotdir <- "data/ramldb/figures"
sstdir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/sst/data/averages"
bndrydir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/stock_boundaries/data"

# Read potential RAM data
load(file.path(datadir, "ramldb_stock_recruit_data_trimmed_sst.Rdata"))

# Source code
source(file.path(codedir, "fit_sr_nls.R"))

# Plot data
################################################################################

# Add columns
stocks$a_r <- stocks$b_r <- stocks$a_bh <- stocks$b_bh <- NA

# Setup figure
figname <- "AppendixB_RAMLDB_sr_data.pdf"
pdf(file.path(plotdir, figname), width=8.5, height=11)
par(mfrow=c(6,4), mar=c(3,3,1,0.5), oma=rep(3,4))

# Loop through stocks and fit
for(i in 1:nrow(stocks)){
  
  # Subset
  stock <- stocks$stockid[i]
  sdata <- filter(data, stockid==stock & r!=0)
  
  # Plot data
  xmax <- ceiling(max(sdata$ssb_sd))
  ymax <- ceiling(max(sdata$r_sd))
  plot(r_sd ~ ssb_sd, sdata, bty="n", las=1, pch=21, bg="grey70", cex=1.2,
       xlim=c(0,xmax), ylim=c(0,ymax), xlab="", ylab="", main=stock)
  
  # Fit and plot: Ricker
  srfit1 <- fit_sr_nls(r=sdata$r_sd, ssb=sdata$ssb_sd, type="Ricker")
  if(!inherits(srfit1, "try-error")){
    a <- coef(srfit1)[1]
    b <- coef(srfit1)[2]
    stocks$a_r[i] <- a
    stocks$b_r[i] <- b
    curve(a*x*exp(-b*x), from=0, to=xmax, add=T, lwd=1.2, col="black")
  }
  
  # Fit and plot: Beverton-Holt
  srfit2 <- fit_sr_nls(r=sdata$r_sd, ssb=sdata$ssb_sd, type="BH")
  if(!inherits(srfit2, "try-error")){
    a <- coef(srfit2)[1]
    b <- coef(srfit2)[2]
    stocks$a_bh[i] <- a
    stocks$b_bh[i] <- b
    curve(a*x/(1+b*x), from=0, to=xmax, add=T, lwd=1.2, col="red", lty=3)
  }
  
}

# Off
dev.off()

# Inspect distribution of a and b parameters
par(mfrow=c(1,2))
hist(stocks$a_r, main="", xlab="alpha", las=1, col="grey70", border=F)
hist(stocks$b_r, main="", xlab="beta", las=1, col="grey70", border=F)

# Beverton-Holt
hist(stocks$a_bh[stocks$a_bh<30], main="", xlab="alpha", las=1, col="grey70", border=F)
hist(stocks$b_bh[stocks$b_bh<10], main="", xlab="beta", las=1, col="grey70", border=F)

