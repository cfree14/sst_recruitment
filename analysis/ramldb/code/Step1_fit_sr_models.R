
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
outputdir <- "analysis/ramldb/output"
codedir <- "code"
tmbdir <- "code/tmbcode"
plotdir <- "analysis/ramldb/figures"

# Read data
load(file.path(datadir, "ramldb_stock_recruit_data_trimmed_sst.Rdata"))
data_orig <- data
stocks_orig <- stocks

# Load helper functions
sapply(list.files(codedir, ".R"), function(x) source(file.path(codedir, x)))


# Remove problem stocks
################################################################################

# Remove problem stocks
rec0 <- c("COD3M")
no_peak <- c("ANCHMEDGSA17-18", "HERR30", "LOBSTERSNE", "MEG8c9a", "NZLINGLIN72",
             "WHITMEDGSA29", "WINFLOUNSNEMATL")
# "COD5Zjm", "CODGB", "RBRMECS", "SARDMEDGSA17-18")
messy <- c("ALPLAICBSAI", "DSOLEGA", "GEMFISHSE", "PATGRENADIERSARG", 
           "SABLEFPCOAST", "SMOOTHOREOCR", "SSTHORNHPCOAST")
preventing_conv <- c("MUTSNAPSATLCGM", "PANCHPERUNC", "PAUANPAU5A", "STFLOUNNPCOAST")
prob_stocks <- c(rec0, no_peak, messy)

# Remove problem stocks
stocks <- filter(stocks, !stockid %in% prob_stocks)
# stocks <- filter(stocks, stockid %in% stocks$stockid[1:100])
data <- filter(data_orig, stockid %in% stocks$stockid)
nrow(stocks)


################################################################################
# AUTO-CORRELLATED RESIDUALS
#################################################################################
   
# SR-AR1 model
################################################################################

# Fit SR model
srfit <- fit_sr_ar1(data, type="Ricker")

# Format SR results
results <- format_results(srfit)

# Plot SR parameter results
plot_params(results)
plot_param_hists(results)


# SR-AR1-SST model
################################################################################

# Fit SR model
srfit <- fit_sr_ar1_sst(data, type="Ricker")

# Format SR results
results <- format_results(srfit)

# Plot SR parameter results
plot_params(results)
plot_param_hists(results)

png(file.path(plotdir, "Fig1_thetas.png"), height=4, width=4, units="in", res=600)
par(mar=c(4,0.5, 0.5, 0.5))
plot_thetas(results)
dev.off()



# SR-AR1-SST-Group model
################################################################################

# Fit SR model
srfit <- fit_sr_ar1_sst_group(data, type="Ricker", group="lme_name")

# Format SR results
results <- format_results(srfit)

# Plot SR parameter results
plot_params(results)
plot_param_hists(results)

plot_thetas(results)


################################################################################
# INDEPENDENT RESIDUALS
#################################################################################

# SR model
################################################################################

# Fit SR model
srfit <- fit_sr(data, type="Ricker")

# Format SR results
results <- format_results(srfit)

# Plot SR parameter results
plot_params(results)
plot_param_hists(results)
plot_thetas(results)

# Plot SR model fits
# plot_sr_curves(data, results, plotdir, figname="RAMLDB_ricker_fits.pdf")

# Export SR results
save(srfit, results, prob_stocks, data, stocks,
     file=file.path(outputdir, "RAMLDB_ricker.Rdata"))


# SR-SST model
################################################################################

# Fit SR model
srfit <- fit_sr_sst(data, type="Ricker")

# Format SR results
results <- format_results(srfit)

# Plot SR parameter results
plot_params(results)
plot_param_hists(results)
plot_thetas(results)

# Plot SR model fits
plot_sr_curves(data, results, plotdir, figname="RAMLDB_ricker_sst_fits.pdf")

# Export SR results
save(srfit, results, prob_stocks, data, stocks,
     file=file.path(outputdir, "RAMLDB_ricker_sst.Rdata"))


# SR-SST-Group model
################################################################################

# Fit SR model
srfit <- fit_sr_sst_group(data, type="Ricker", group="lme_name")

# Format SR results
results <- format_results(srfit)

# Plot SR parameter results
plot_params(results)
plot_thetas(results)
plot_thetas_group(results)

# Plot SR model fits
plot_sr_curves(data, results, plotdir, figname="RAMLDB_ricker_sst_lme_fits.pdf")

# Export SR results
save(srfit, results, prob_stocks, data, stocks,
     file=file.path(outputdir, "RAMLDB_ricker_sst_lme.Rdata"))

