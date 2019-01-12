
# Setup
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(plyr)
library(dplyr)
library(reshape2)

# Directories
datadir <- "data/ramldb/data"
plotdir <- "data/ramldb/figures"

# Read potential RAM data
load(file.path(datadir, "ramldb_stock_recruit_data.Rdata"))
data_orig <- data
stocks_orig <- stocks

# Add surplus production
################################################################################

# Function to calculate surplus production: 
# SP(t) = TB(t+1) - TB(t) + C(t)
# tb <- subset(bdata, assessid==unique(bdata$assessid)[1])$tb
# catch <- subset(bdata, assessid==unique(bdata$assessid)[1])$catch
calc_sp <- function(tb, catch){
  sp <- c(tb[2:length(tb)] - tb[1:(length(tb)-1)] + catch[1:(length(catch)-1)], NA)
  return(sp)
}

# Function to fit surplus production model
# sp <- subset(data, assessid==unique(data$assessid)[1])$sp
# tb <- subset(data, assessid==unique(data$assessid)[1])$tb
fit.sp.model <- function(sp, tb){
  r_start <- log(0.4)
  k_start <- log(max(tb, na.rm=T) * 1.5)
  spfit <- try(nls(sp ~ exp(r)*tb*(1-tb/exp(k)),
                   start=list(r=r_start, k=k_start)))
  return(spfit)
}

# Add surplus production
data1 <- data_orig %>%
  group_by(stockid) %>%
  mutate(sp=calc_sp(tb, catch))

# Identify stocks and years to trim
################################################################################

# Stocks to cut
stocks_cut <- c("BKCDLFENI", "BLACKOREOPR", "BLACKOREOWECR", "BLACKROCKORECOAST", 
                "BNSNZ", "LNOSESKAPCOAST", "OROUGHYNZMEC", "OROUGHYSE", "OROUGHYCASCADE",
                "SAABALONESA", "SMOOTHOREOBP", "SMOOTHOREOEPR",
                "SMOOTHOREOSLD", "SMOOTHOREOWECR", "YNOSESKACSCH", "YNOSESKASCH")

# Identify trim years
# Columns: stock id, biomass year, recruitment year, catch year, 
trim.years <- matrix(data=c("ACADREDGOMGB", 1940, 1952, 1935,
                            # "ALPLAICBSAI", 1987, NA, NA,
                            "ALSKABSAI", 1983, 1983, NA,
                            "ARFLOUNDGA", NA, 1968, NA,
                            "ARFLOUNDPCOAST", 1939, 1965, NA,
                            "AUROCKPCOAST", NA, 1958, NA,
                            "AUSSALMONNZ", NA, 1983, NA,
                            # "ATHAL5YZ", 1898, NA, NA,
                            "BGRDRNSWWA", 1977, 1973, 1978,
                            "BIGEYEIO", 1968, 1962, NA,
                            # "BKINGCRABPI", 1982, NA, NA,
                            "BLACKROCKCAL", NA, 1955, NA,
                            # "BLACKROCKNPCOAST", 1943, 1968, 1965,
                            # "BLACKROCKSPCOAST", 1943, 1968, NA,
                            "BLACKROCKWASH", NA, 1958, NA,
                            # "BLUEROCKCAL", 1940, 1960, 1940,
                            # "BOCACCSPCOAST", NA, 1955, NA,
                            "CABEZORECOAST", NA, 1979, NA,
                            "CABEZNCAL", NA, 1969, NA,
                            "CABEZSCAL", NA, 1969, NA,
                            "CALSCORPSCAL", NA, 1964, NA,
                            "CHAKESA", NA, 1984, NA,
                            # "CHILISPCOAST", NA, 1963, NA,
                            # "COWCODSCAL", NA, NA, 1918,
                            # "CRLOBSTERSA12", NA, 1980, NA,
                            # "CRLOBSTERSA34", NA, 1980, NA,
                            # "CRLOBSTERSA56", NA, 1980, NA,
                            # "CRLOBSTERSA7", NA, 1980, NA,
                            # "CRLOBSTERSA8", NA, 1980, NA,
                            # "CROCKPCOAST", 1943, 1961, NA,
                            "COBGM", 1958, 1982, 1948,
                            "CTRACSA", NA, 1985, NA,
                            "DEEPCHAKESA", 1955, 1984, 1955,
                            # "DKROCKPCOAST", NA, 1975, NA,
                            "DSOLEPCOAST", NA, 1960, NA,
                            "ESOLEPCOAST", 1918, 1938, 1918,
                            "GEMFISHNZ", NA, 1979, NA,
                            "GEMFISHSE", 1968, NA, NA,
                            # "GOPHERSPCOAST", NA, 1980, NA,
                            "GRAMBERGM", 1980, 1980, NA,
                            "GRAMBERSATLC", NA, 1978, NA,
                            # "GRNSTROCKPCOAST", 1942, 1969, NA,
                            # "GRSPROCKNCAL", 1942, NA, NA,
                            "GSTRGZRSTA7", NA, 1976, NA,
                            "HOKIWNZ", NA, 1974, NA,
                            "KELPGREENLINGORECOAST", 1981, 1980, NA,
                            "LINGCODNPCOAST", 1940, 1960, NA,
                            "LINGCODSPCOAST", NA, 1972, NA,
                            "LNOSESKAPCOAST", 1950, NA, 1950,
                            "LSTHORNHPCOAST", NA, 1981, NA,
                            # "MORWONGSE", NA, 1942, NA,
                            "MORWONGESE", NA, 1945, NA,
                            "NSHRIMPCSCH", NA, 1964, NA,
                            "NZLINGLIN3−4", 1974, 1972, NA,
                            "NZLINGLIN5−6", NA, 1973, NA,
                            "NZLINGLIN72", 1972, 1983, NA,
                            "NZLINGLIN7WC", 1974, 1974, NA,
                            "NZLINGESE", 1977, 1983, 1977,
                            "NZLINGWSE", 1982, 1982, NA,
                            "NZSNAPNZ1BOP-HAGU", NA, 1970, NA,
                            "NZSNAPNZ1ENLD", NA, 1966, NA,
                            "NZSNAPNZ7", NA, 1950, NA,
                            "PAUANPAU5A", NA, 1986, NA,
                            "PAUAPAU5B", NA, 1980, NA,
                            "PAUAPAU5D", NA, 1980, 1975,
                            "PAUAPAU7", NA, 1980, 1976, 
                            "PAUASPAU5A", NA, 1986, NA, 
                            # "PGEODWA", 1995, NA, NA,
                            "PSOLEPCOAST", NA, 1960, 1920,
                            "PTOOTHFISHPEI", NA, 1992, NA,
                            # "RSNAPGM", NA, 1971, NA,
                            # "RSNAPSATLC", NA, 1973, NA,
                            "SABLEFPCOAST", NA, 1965, NA,
                            "SBELLYROCKPCOAST", NA, 1960, NA,
                            # "SNROCKPCOAST", 1950, 1960, NA,
                            "SPHAKECH", 1965, NA, NA,
                            "SPANMACKGM", NA, 1984, NA,
                            # "SPSDOGPCOAST", 1938, 1942, 1938,
                            "SSTHORNHPCOAST", NA, 1984, NA,
                            "STFLOUNNPCOAST", NA, 1985, NA,
                            "STFLOUNSPCOAST", NA, 1978, NA,
                            "TARAKNZ", NA, 1975, NA,
                            "TIGERFLATSE", NA, 1940, NA,
                            "TREVALLYTRE7", NA, 1970, NA,
                            "SWHITSE", NA, 1981, NA,
                            # "VSNAPSATLC", NA, 1975, 1960,
                            "WMARLINATL", NA, 1978, NA,
                            "WROCKPCOAST", 1970, 1970, NA), ncol=4, byrow=T)

# Format trim years data frame
trim.years.df <- as.data.frame(trim.years, stringsAsFactors=F) %>% 
  setNames(c("stockid", "year_b", "year_r", "year_c")) %>% 
  mutate(year_b=as.numeric(year_b),
         year_r=as.numeric(year_r),
         year_c=as.numeric(year_c),
         year_trim=pmax(year_b, year_r, year_c, na.rm=T))
trim.years.df$stockid[!(trim.years.df$stockid%in%stocks_orig$stockid)] # must report 0 = confirms stock IDs spelled correctly

# Mark usable years
data2 <- data1 %>% 
  # Add trim year (year before which data is not usable)
  left_join(select(trim.years.df, stockid, year_trim), by="stockid") %>% 
  mutate(year_trim=ifelse(is.na(year_trim), 0, year_trim)) %>% 
  # Mark usable
  mutate(use=ifelse(year>=year_trim, T, F),
         use=ifelse(stockid %in% stocks_cut, F, use))


# Stocks where BAD years come at the END of the time series
# bdata$use[bdata$stockid=="BIGHTREDSE" & bdata$year>1990] <- "no"
data2$use[data2$stockid=="ARFLOUNDPCOAST" & data$year>2007] <- F
data2$use[data2$stockid=="ARGANCHOSARG" & data$year>2006] <- F
data2$use[data2$stockid=="CALSCORPSCAL" & data$year>2000] <- F
data2$use[data2$stockid=="NZLINGLIN5−6" & data$year>1999] <- F
data2$use[data2$stockid=="NZLINGLIN6b" & data$year>1998] <- F
data2$use[data2$stockid=="PTOOTHFISHMI" & data$year>2002] <- F
data2$use[data2$stockid=="PTOOTHFISHPEI" & data$year>2008] <- F
data2$use[data2$stockid=="SMOOTHOREOCR" & data$year>2001] <- F
data2$use[data2$stockid=="SOUTHHAKESA" & data$year>1994] <- F
data2$use[data2$stockid=="YEGROUPGM" & data$year>2001] <- F


# Plot trimming
################################################################################

# For headers
top.i <- seq(1, nrow(stocks_orig), 6)

# Plot data and trimming decisions
figname <- "AppendixA_ramldb_data_and_trimming.pdf"
pdf(paste(plotdir, figname, sep="/"), width=8.5, height=11)
par(mfrow=c(6, 5), mar=c(1.5, 1.0, 2.5, 0.5), mgp=c(2.5,0.5,0), oma=c(3,3,3,3), lwd=0.8)

# Loop through stocks
for(i in 1:nrow(stocks_orig)){
  
  # Subset data
  stock <- stocks_orig$stockid[i]
  sdata <- subset(data2, stockid==stock)
  print(paste(i, stock))
  
  # Format data
  sdata <- sdata %>% 
    mutate(ssb=ssb/1000,
           r=r/1E6,
           tb=tb/1000,
           catch=catch/1000,
           sp=sp/1000)

  # Year limits
  tmin <- floor(min(sdata$year)/10)*10
  tmax <- ceiling(max(sdata$year)/10)*10 
  
  # Biology limits
  ssbmin <- floor(min(sdata$ssb, na.rm=T))
  ssbmax <- ceiling(max(sdata$ssb, na.rm=T))
  tbmin <- floor(min(sdata$tb, na.rm=T))
  tbmax <- ceiling(max(sdata$tb, na.rm=T))
  smin <- floor(min(sdata$sp, na.rm=T))
  smax <- ceiling(max(sdata$sp, na.rm=T))
  cmin <- floor(min(sdata$catch, na.rm=T))
  cmax <- ceiling(max(sdata$catch, na.rm=T))
  rmin <- floor(min(sdata$r, na.rm=T))
  rmax <- ceiling(max(sdata$r, na.rm=T))
  if(rmax==1){rmax <- freeR::ceiling1(max(sdata$r, na.rm=T), 0.1)}
  if(rmax==0.1){rmax <- freeR::ceiling1(max(sdata$r, na.rm=T), 0.01)}
  
  # A. Plot biomass
  #######################################
  
  # Plot data
  plot(ssb ~ year, sdata, type="l", bty="n", 
       xlim=c(tmin, tmax), ylim=c(ssbmin, ssbmax), yaxt="n",
       xlab="", ylab="", las=2, col="blue")
  axis(2, at=c(ssbmin, ssbmax))
  title(stock, line=0.1, xpd=NA, col.main=ifelse(stock%in%stocks_cut, "red", "black"))
  if(i %in% top.i){title("Biomass (1000s mt)\ntime series", col.main="blue", line=2, xpd=NA)}
  
  # Add trim year
  trim.yr <- trim.years.df$year_b[trim.years.df$stockid==stock]
  if(length(trim.yr)>0){
    lines(x=c(trim.yr,trim.yr), y=c(ssbmin,ssbmax), col="black", lty=3, lwd=1.5)
    text(x=trim.yr, y=ssbmin+(ssbmax-ssbmin)*0.10, pos=2, labels=trim.yr, col="black", cex=0.9, xpd=NA)
  }
  
  # B. Plot recruitment
  #######################################
  
  # Plot data
  if(sum(sdata$r, na.rm=T)>0){
    plot(r ~ year, sdata, type="l", bty="n", 
         xlim=c(tmin, tmax), ylim=c(rmin, rmax), yaxt="n",
         xlab="", ylab="", las=2, col="purple")
    axis(2, at=c(rmin, rmax))
  }else{
    plot.new()
  }
  if(i %in% top.i){title("Recruitment (millions)\ntime series", col.main="purple", line=2, xpd=NA)}
  
  # Add trim year
  trim.yr <- trim.years.df$year_r[trim.years.df$stockid==stock]
  if(length(trim.yr)>0){
    lines(x=c(trim.yr,trim.yr), y=c(rmin,rmax), col="black", lty=3, lwd=1.5)
    text(x=trim.yr, y=rmin+(rmax-rmin)*0.10, pos=2, labels=trim.yr, col="black", cex=0.9, xpd=NA)
  }
  
  # C. Plot catch
  #######################################
  
  # Plot data
  if(sum(sdata$catch, na.rm=T)>0){
    plot(catch ~ year, sdata, type="l", bty="n", 
         xlim=c(tmin, tmax), ylim=c(cmin, cmax), yaxt="n",
         xlab="", ylab="", las=2, col="darkgreen")
    axis(2, at=c(cmin, cmax))
  }else{
    plot.new()
  }
  if(i %in% top.i){title("Catch (1000s mt)\ntime series", col.main="darkgreen", line=2, xpd=NA)}
  
  # Add trim year
  trim.yr <- trim.years.df$year_c[trim.years.df$stockid==stock]
  if(length(trim.yr)>0){
    lines(x=c(trim.yr,trim.yr), y=c(cmin,cmax), col="black", lty=3, lwd=1.5)
    text(x=trim.yr, y=cmin+(cmax-cmin)*0.10, pos=2, labels=trim.yr, col="black", cex=0.9, xpd=NA)
  }
  
  # D. Plot stock-recruit relationship
  #######################################
  
  # Plot stock-recruit relationship
  if(sum(sdata$r, na.rm=T)>0){
    plot(r ~ ssb, sdata, type="p", bty="n", 
         xlim=c(0, ssbmax), ylim=c(0, rmax), xaxt="n", yaxt="n", col=c("red", "grey60")[factor(sdata$use, levels=c(F, T))])
    axis(1, at=c(0, ssbmax))
    axis(2, at=c(0, rmax))
  }else{
    plot.new()
  }
  if(stock%in%stocks_cut){text(x=ssbmin, y=rmax, pos=4, labels="*", cex=3, col="red", xpd=NA)}
  if(i %in% top.i){title("Stock-recruit\nrelationship", col.main="black", line=2, xpd=NA)}
  
  # E. Plot surplus production
  #######################################
  
  # Plot data
  if(sum(sdata$sp, na.rm=T)>0 & sum(sdata$tb, na.rm=T)>0){
    plot(sp ~ tb, sdata, type="p", bty="n",
         xlim=c(0, tbmax), ylim=c(pmin(0, smin), smax), xaxt="n", yaxt="n",
         xlab="", ylab="", col=c("red", "grey60")[factor(sdata$use, levels=c(F, T))])
    axis(1, at=c(0, tbmax))
    axis(2, at=c(pmin(0, smin), smax))

    # Fit and plot SP model
    # sdata1 <- subset(sdata, use=="yes")
    # spfit <- fit.sp.model(sp=sdata1$sp, tb=sdata1$tb)
    # if(!(inherits(spfit, "try-error"))){
    #   r <- exp(coef(spfit)["r"])
    #   k <- exp(coef(spfit)["k"])
    #   curve(r*x*(1-x/k), from=tbmin, to=tbmax, add=T, lty=1, lwd=0.9, col="black")
    # }
    
  }else{
    plot.new()
  }
  if(i %in% top.i){title("Surplus production\n(1000s mt) curve", col.main="black", line=2, xpd=NA)}
  
  
}

# Off
dev.off()
graphics.off()

# Export data
################################################################################

# Check # of yrs
check <- data2 %>%
  filter(use==T) %>% 
  group_by(stockid) %>% 
  summarize(nyr=n()) %>% 
  filter(nyr>=20) %>% 
  arrange(nyr)

# Final data
stocks <- filter(stocks_orig, stockid%in%check$stockid)
data <- filter(data2, stockid %in% stocks$stockid & use==T)

# Export
save(data, stocks, file=file.path(datadir, "ramldb_stock_recruit_data_trimmed.Rdata"))

