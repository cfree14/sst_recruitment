

# Function to plot stock-recruit curves to PDF
plot_sr_curves <- function(data, results, plotdir=getwd(), figname="srfits.pdf"){
  
  # Setup figure
  pdf(file.path(plotdir, figname), width=8.5, height=11)
  par(mfrow=c(6,4), mar=c(3,3,1,0.5), oma=rep(3,4))
  
  # Loop through stocks and fit
  for(i in 1:nrow(results$stock)){
    
    # Subset data
    stock <- results$stock$stockid[i]
    results1 <- results$stock
    sdata <- filter(data, stockid==stock)
    
    # Add colors
    sst_diff <- freeR::ceiling1(max(abs(range(sdata$sst_sd))), 0.25)
    sst_bins <- seq(-1*sst_diff, sst_diff, 0.25)
    sst_cols <- rev(freeR::colorpal(RColorBrewer::brewer.pal(11, "RdBu"), length(sst_bins)-1)) # one less bin then break
    sst_cols <- freeR::tcolor(sst_cols, 0.5)
    sdata <- sdata %>% 
      mutate(sst_sd_bin=cut(sst_sd, breaks=sst_bins),
             sst_sd_col=sst_cols[sst_sd_bin])
    
    # Plot data
    xmax <- ceiling(max(sdata$ssb_sd))
    ymax <- ceiling(max(sdata$r_sd))
    plot(r_sd ~ ssb_sd, sdata, bty="n", las=1, pch=21, bg=sst_sd_col, cex=1.5,
         xlim=c(0,xmax), ylim=c(0,ymax), xlab="", ylab="", main=stock)
    
    # Plot Ricker curve
    a <- results1$alpha[results1$stockid==stock]
    b <- results1$beta[results1$stockid==stock]
    curve(a*x*exp(-b*x), from=0, to=xmax, add=T, lwd=1.2, col="black")
    
    # Plot SST-Ricker curves
    if("theta" %in% colnames(results1)){
      theta <- results1$theta[results1$stockid==stock]
      theta_sig <- results1$theta_sig[results1$stockid==stock]
      temps <- c(-2, -1, -0.5, 0.5, 1, 2)
      temp_cols <- rev(RColorBrewer::brewer.pal(length(temps), "RdBu"))
      for(j in 1:length(temps)){
        curve(a*x*exp(-b*x)*exp(theta*temps[j]), from=0, to=xmax, add=T, lwd=1.2, col=temp_cols[j])
      }
      theta_font <- ifelse(theta_sig=="none", 1, 2)
      theta_col <- ifelse(theta<0, "red", "blue")
      theta_trans <- ifelse(theta_sig=="none", 0.6, 1)
      text(x=0, y=ymax-ymax*0.05, pos=4, labels=freeR::roundf(theta, 2), cex=1.2, 
           font=theta_font, col=freeR::tcolor(theta_col, theta_trans))
    }
    
  }
  
  # Off
  dev.off()
  
}
