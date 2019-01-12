
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(plyr)
library(dplyr)
library(freeR)
library(reshape2)
library(TMB)
library(TMBhelper)

# Directories
basedir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/sst_recruitment"
inputdir <- file.path(basedir, "input")
outputdir <- file.path(basedir, "output")
plotdir <- file.path(basedir, "figures")
tmbdir <- file.path(basedir, "code/tmbcode")
codedir <- file.path(basedir, "code")
setwd(tmbdir)

# Read data
load(file.path(inputdir, "ramsr_minto_data_use.Rdata"))

# Read helper functions
source(file.path(codedir, "helper_functions.R"))


# Fit model
################################################################################

# Problem stocks
prob_stocks <- c("BDUCK", "COD3M3", "HAD5Z")
data <- filter(data, !stockid%in%prob_stocks)

# Parameters
stockids <- unique(data$stockid)
nstocks <- length(stockids)

# Compile TMB code
# Only run once to compile code
if(FALSE){
  dyn.unload(paste(tmbdir, dynlib("srmodel.cpp"),sep="/") )
  file.remove(paste(tmbdir, c("srmodel.o","srmodel.dll"), sep="/") )
  compile("srmodel.cpp")
}

# Load TMB code
dyn.load(dynlib("srmodel"))

# Input data
input.data <- list(Nstocks=nstocks,
                   Nobs=nrow(data),
                   StockID=as.factor(data$stockid),
                   SB_t=data$ssb_sd,
                   R_t=data$r_sd,
                   SR_type=1) # 0=Ricker, 1=Beverton-Holt

# Parameter starting values
params <- list(ln_alpha=rep(log(0.1), nstocks),
               ln_beta=rep(log(0.5), nstocks),
               ln_sigmaR=rep(0.1, nstocks))

# Initialize model
model <- MakeADFun(data=input.data, parameters=params)
model$control <- list(trace=1, parscale=rep(1,13), REPORT=1, reltol=1e-12, maxit=100)
model$hessian <- F
newtonOption(model, smartsearch=TRUE)

# Fit model
output <- TMBhelper::Optimize(obj=model, lower=-Inf, upper=Inf, 
                              loopnum=3, newtonsteps=3, bias.correct=FALSE, getsd=FALSE)

# Confirm convergence
output$max_gradient # if converged, the gradient should be near zero
output$Convergence_check

# Calculate SD report
# This computes the standard errors for your parameter estimates
sd <- try(sdreport(model))

# Format results
################################################################################

# Format results
results <- format_results(sd, stockids)

# Plot parameters
plot_parameters(results)

# Plot SR fits
plot_sr_fits(indata=input.data, results, plotdir, figname="RAMSR_minto_bevholt_fits.pdf")












