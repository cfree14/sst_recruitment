
# Plot SR parameter estimates
################################################################################

# Plot parameter estimates
plot_param_hists <- function(results){
  
  # Plotting parameters
  par(mfrow=c(2,2), mar=c(4,4,0.5,0.5))
 
  # Parameters to plot
  if("theta" %in% colnames(results$stock)){
    par2plot <- c("alpha", "beta", "theta", "sigmaR")
  }else{
    par2plot <- c("alpha", "beta", "sigmaR")
  }
  
  # Plot parameters
  for(i in 1:length(par2plot)){
    vals <- results$stock[,par2plot[i]]
    xmin <- freeR::floor1(pmin(min(vals), 0), 0.5)
    xmax <- freeR::ceiling1(max(vals), 0.5)
    hist(vals, main="", xlab=par2plot[i], las=1, col="grey60", border=F,
         xlim=c(xmin, xmax))
  }
  
}