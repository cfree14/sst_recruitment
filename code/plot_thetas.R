
# Plot theta estimates
plot_thetas <- function(results){
  
  # Individual rheta estimates
  vals <- results$stock %>% 
    arrange(desc(theta))
  
  # Plotting parameters
  par(mfrow=c(1,1))
  
  # Setup empty plot
  plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
       xlim=c(-1.5, 1.5), ylim=c(1,nrow(vals)),
       xlab="SST influence", ylab="", main="", cex.main=1)
  
  # Add theta mean
  mu_est <- results$global$est[results$global$param=="mu_T"]
  mu_min <- results$global$est_lo[results$global$param=="mu_T"]
  mu_max <- results$global$est_hi[results$global$param=="mu_T"]
  rect(xleft=mu_min, xright=mu_max, ytop=nrow(vals), ybottom=1, border=NA, col="grey80")
  text(x=0, y=nrow(vals), labels=paste0("μ = ", format(round(mu_est,2), nsmall=2)), pos=4, cex=0.9, col="grey30")
  
  # Add theta estimates (points) and errors (lines)
  sapply(1:nrow(vals), function(x) lines(x=c(vals$theta_lo[x], vals$theta_hi[x]), y=c(x,x), col=vals$lcolor[x], lwd=0.6))
  points(vals$theta, 1:nrow(vals), pch=16, cex=0.6, col=vals$pcolor)
  
  # Add theta=0 line
  lines(x=c(0,0), y=c(1, nrow(vals)), lty=3, col="black", lwd=0.8)
  
  # Add positive/negative influence labels
  n_pos <- sum(vals$theta_sig=="positive")
  n_neg <- sum(vals$theta_sig=="negative")
  text_pos <- paste0(n_pos, " stocks\n", "positive")
  text_neg <- paste0(n_neg, " stocks\n", "negative")
  text(x=-1.65, y=6, labels=text_pos, pos=4, adj=1, cex=0.9, col="blue")
  text(x=1.65, y=nrow(vals)-12, labels=text_neg, pos=2, adj=0, cex=0.9, col="red")
  
}