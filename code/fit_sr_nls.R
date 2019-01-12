
# Package
library(FSA)

# Fit SR relationship
# r <- sdata$r_sd; ssb <- sdata$ssb_sd; type <- "Ricker"
fit_sr_nls <- function(r, ssb, type){
  
  # Start values
  starts_r <- FSA::srStarts(r~ssb, type="Ricker")
  starts_bh <- FSA::srStarts(r~ssb, type="BevertonHolt")
  
  # Beverton-Holt
  if(type=="BH"){
    srfit <- try(nls(log(r) ~ log(a*ssb / (1+b*ssb)),
                     algorithm="port", lower=c(a=0,b=0), 
                     start=list(a=starts_bh$a, b=starts_bh$b)))}
  
  # Ricker
  if(type=="Ricker"){
    srfit <- try(nls(log(r) ~ log(a*ssb * exp(-b*ssb)),
                     algorithm="port", lower=c(a=0,b=0), 
                     start=list(a=starts_r$a, b=starts_r$b)))}
  
  
  # Return
  return(srfit)
  
}