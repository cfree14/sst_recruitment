
# Packages
library(TMB)
library(TMBhelper)

# Fit stock-recruit model w/ SST link
# type <- "Ricker"; group <- "lme_name"
fit_sr_ar1_sst_group <- function(data, type="Ricker", group){
  
  # Directories
  basedir <- getwd()
  setwd(file.path(basedir, tmbdir))
  
  # Parameters
  stockids <- unique(data$stockid)
  nstocks <- length(stockids)
  group_data <- as.character(unlist(data[,group]))
  groups <- unique(group_data)
  ngroups <- length(groups)
  
  # Compile TMB code
  # Only run once to compile code
  if(FALSE){
    dyn.unload(paste(tmbdir, dynlib("srmodel_ar1_sst_group.cpp"),sep="/") )
    file.remove(paste(tmbdir, c("srmodel_ar1_sst_group.o","srmodel_ar1_sst_group.dll"), sep="/") )
    compile("srmodel_ar1_sst_group.cpp")
  }
  
  # Load TMB code
  dyn.load(dynlib("srmodel_ar1_sst_group"))
  
  # Input data
  if(type=="Ricker"){SR_type <- 0}
  if(type=="BevHolt"){SR_type <- 1}
  input.data <- list(Nstocks=nstocks,
                     Nobs=nrow(data),
                     Ngroups=ngroups,
                     StockID=as.factor(data$stockid),
                     GroupID=as.factor(group_data),
                     SB_t=data$ssb_sd,
                     R_t=data$r_sd,
                     SST_t=data$sst_sd,
                     SR_type=SR_type) # 0=Ricker, 1=Beverton-Holt
  
  # Parameter starting values
  if(type=="Ricker"){
    params <- list(ln_alpha=rep(log(2), nstocks),
                   ln_beta=rep(log(0.3), nstocks),
                   ln_sigmaR=rep(0.1, nstocks),
                   rho=rep(0.2, nstocks),
                   theta=rep(0, nstocks),
                   mu_T=0.0,
                   ln_sd_T=log(0.2),
                   mu_group=rep(0, ngroups),
                   ln_sd_group=log(0.2))
  }
  if(type=="BevHolt"){
    params <- list(ln_alpha=rep(log(3), nstocks),
                   ln_beta=rep(log(2), nstocks),
                   ln_sigmaR=rep(0.1, nstocks),
                   rho=rep(0.2, nstocks),
                   theta=rep(0, nstocks),
                   mu_T=0.0,
                   ln_sd_T=log(0.2),
                   mu_group=rep(0, ngroups),
                   ln_sd_group=log(0.2))
  }
  
  # Initialize model
  model <- MakeADFun(data=input.data, parameters=params, 
                     random=c("theta", "mu_group"), DLL="srmodel_ar1_sst_group")
  model$control <- list(trace=1, parscale=rep(1,13), REPORT=1, reltol=1e-12, maxit=100)
  model$hessian <- F
  newtonOption(model, smartsearch=TRUE)
  
  # Fit model
  fit <- TMBhelper::Optimize(obj=model, lower=-Inf, upper=Inf, 
                             loopnum=3, newtonsteps=3, bias.correct=FALSE, getsd=FALSE)
  
  # Calculate SD report
  # This computes the standard errors for your parameter estimates
  sd <- try(sdreport(model))
  
  # Confirm convergence
  g_max <- fit$max_gradient # if converged, the gradient should be near zero
  message(fit$Convergence_check)

  # Return
  out <- list(model=model, fit=fit, sd=sd, data=input.data)
  setwd(basedir)
  return(out)
  
}