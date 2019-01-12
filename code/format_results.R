
# Format SR model results
################################################################################

# Format results
format_results <- function(srfit){
  
  # Extract stuff
  model <- srfit[[1]]
  fit <- srfit[[2]]
  sd <- srfit[[3]]
  data <- srfit[[4]]
  stockids <- unique(data$StockID)
  groupids <- unique(data$GroupID)
  
  # Format parameter estimates
  results.mat <- summary.sdreport(sd)
  results.df <- data.frame(param=rownames(results.mat),
                           est=results.mat[,1],
                           se=results.mat[,2], stringsAsFactors=F)
  
  # Add confidence intervals
  results_long <- results.df %>% 
    mutate(est_lo=est-se*1.96,
           est_hi=est+se*1.96)
  
  # Format global parameters
  # sort(unique(results_long$param))
  p_global <- results_long %>% 
    filter(param %in% c("mu_T", "sd_T", "sd_group"))
  
  # Format group-level parameters
  if("mu_group" %in% results_long$param){
    p_group <- results_long %>% 
      filter(param %in% c("mu_group")) %>% 
      rename(group=param) %>% 
      mutate(group=groupids) %>% 
      arrange(group)
  }else{
    p_group <- NULL
  }
  
  # Format stock-level parameters
  stock_long <- results_long %>% 
    filter(param %in% c("alpha", "beta", "theta", "sigmaR", "rho")) %>% 
    mutate(stockid=rep(stockids, n_distinct(param)))

  # Create wide versions
  stock_est <- dcast(stock_long, stockid ~ param, value.var="est")
  stock_se <- dcast(stock_long, stockid ~ param, value.var="se")
  colnames(stock_se)[2:ncol(stock_se)] <- paste0(colnames(stock_se)[2:ncol(stock_se)], "_se")
  stock_lo <- dcast(stock_long, stockid ~ param, value.var="est_lo")
  colnames(stock_lo)[2:ncol(stock_lo)] <- paste0(colnames(stock_lo)[2:ncol(stock_lo)], "_lo")
  stock_hi <- dcast(stock_long, stockid ~ param, value.var="est_hi")
  colnames(stock_hi)[2:ncol(stock_hi)] <- paste0(colnames(stock_hi)[2:ncol(stock_hi)], "_hi")
  
  # Merge wide versions
  p_stock <- stock_est %>% 
    left_join(stock_se, by="stockid") %>% 
    left_join(stock_lo, by="stockid") %>% 
    left_join(stock_hi, by="stockid")
  
  # If SST-linked
  if("theta" %in% colnames(p_stock)){
    p_stock <- p_stock %>% 
      # Add theta significance and colors
      mutate(theta_sig="none",
             theta_sig=ifelse(theta_lo>0, "positive", theta_sig),
             theta_sig=ifelse(theta_hi<0, "negative", theta_sig),
             lcolor=revalue(theta_sig, c("none"="grey60", "positive"="blue", "negative"="red")),
             pcolor=revalue(theta_sig, c("none"="black", "positive"="blue", "negative"="red")))
  }
    
  # Combine results
  results <- NULL
  results$stock <- p_stock
  results$group <- p_group
  results$global <- p_global
  
  # Return
  return(results)
  
}