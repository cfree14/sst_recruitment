
# Plot SR parameter estimates
################################################################################

# Plot parameter estimates
# plot_params(results)
plot_params <- function(results){
  
  # Plotting parameters
  par(mfrow=c(2,3), mar=c(4,4,0.5,0.5), mgp=c(2,0.8,0))
 
  # Parameters to plot
  if("theta" %in% colnames(results$stock)){
    par2plot <- c("alpha", "beta", "sigmaR", "theta", "rho")
  }else{
    par2plot <- c("alpha", "beta", "sigmaR", "rho")
  }
  
  # Plot parameters
  for(i in 1:length(par2plot)){
    
    # Subset data
    param <- par2plot[i]
    p_stock <- results$stock
    vals <- data.frame(est=p_stock[,param],
                       est_lo=p_stock[,paste0(param, "_lo")],
                       est_hi=p_stock[,paste0(param, "_hi")])
    vals <- arrange(vals, desc(est))
    
    # Axis limits
    xmin <- freeR::floor1(min(vals$est_lo), 0.5)
    xmax <- freeR::ceiling1(max(vals$est_hi), 0.5)
    
    # Plot data
    plot(vals$est, 1:nrow(vals), type="n", bty="n", yaxt="n", cex.axis=0.85,
         xlim=c(xmin, xmax), ylim=c(1,nrow(vals)),
         xlab=param, ylab="", cex.main=0.8)
    
    # Add estimates
    sapply(1:nrow(vals), function(x) lines(x=c(vals$est_lo[x], vals$est_hi[x]), y=c(x,x), col="grey60", lwd=0.6))
    points(vals$est, 1:nrow(vals), pch=16, cex=0.5, col="black")
    
    # Add vertical line
    if(param=="theta"){lines(x=c(0,0), y=c(1, nrow(vals)), lty=3)}
    
  }
  
}