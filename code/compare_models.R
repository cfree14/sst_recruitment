
# Packages
library(TMB)
library(TMBhelper)

# Compare models
compare_models <- function(models, names){
  
  # Data frame
  aic_df <- data.frame(model=names, k=NA, lik=NA, aic=NA, stringsAsFactors=F)
  
  # Loop through models and calculate/record AIC value
  for(i in 1:length(models)){
    
    # Calculate/record AIC value
    output <- models[[i]]$fit
    k <- length(output[["par"]])
    lik <- output[["objective"]]
    aic_val <- TMBhelper::TMBAIC(output)
    aic_df$k[i] <- k
    aic_df$lik[i] <- lik
    aic_df$aic[i] <- aic_val
    
  }
  
  # Format table
  aic_final <- aic_df %>% 
    arrange(aic) %>% 
    mutate(daic=aic-min(aic))
  
  # Return
  return(aic_final)
  
}