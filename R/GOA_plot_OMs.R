################################################
# Set-up
################################################
fit_all <- FALSE
source("R/GOA_condition_models_1977.R")
source("R/GOA_condition_ricker_models_1977.R")
library(gmRi)
library(Rceattle)
library(tidyr)

# Update SB0
gc()
ms_run$quantities$depletionSSB <- ms_run$quantities$biomassSSB/ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)]
ms_run_ricker$quantities$depletionSSB <- ms_run_ricker$quantities$biomassSSB/ms_run_ricker$quantities$biomassSSB[,ncol(ms_run_ricker$quantities$biomassSSB)]



################################################
# Plots ----
################################################
### OMS
# 1. Single-species fix M
# 2. Single-species estimate M
# 3. Multi-species type II
projected_OM_no_F <- list(ss_run, ss_run_M, ms_run, ss_run_ricker, ss_run_ricker_M, ms_run_ricker)
om_names = c("SS_OM", "SSM_OM", "MS_OM", "SS_Ricker_OM", "SSM_Ricker_OM", "MS_Ricker_OM")
om_names_print = c("SS fix M", "SS est M", "MS", "SS fix M Ricker", "SS est M Ricker", "MS Ricker")
MPcols <- gmri_pal("main")(8)

MPcols <- rev(oce::oce.colorsViridis(6))
# plot_biomass(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,2,4,6)])

plot_recruitment(projected_OM_no_F, file = "Results/Figures/GOA_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,1,3,5)], lty = c(1,1,1,5,5,5), species = c(1,3,2))
plot_ssb(projected_OM_no_F, file = "Results/Figures/GOA_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,1,3,5)], lty = c(1,1,1,5,6,6), species = c(1,3,2))
plot_biomass(projected_OM_no_F, file = "Results/Figures/GOA_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,1,3,5)], lty = c(1,1,1,5,6,6), species = c(1,3,2))
plot_stock_recruit(projected_OM_no_F[4:6], file = "Results/Figures/GOA_OM_", model_names = om_names_print[4:6], width = 6, height = 4.5, line_col = MPcols[c(1,3,5)], species = c(1,3,2))
plot_b_eaten_prop(projected_OM_no_F[c(3,6)], file = "Results/Figures/GOA_OM_", model_names = om_names_print[c(3,6)], width = 6, height = 4.5, line_col = MPcols[c(2,6)], species = c(1,3,2))


################################################
# Project ssb w/ rec devs ----
################################################
library(parallel)
cl <- makeCluster(detectCores())

source("R/Functions/Project recruitment stochasticity.R")
nsim = 300

ss_run_ssb <- lapply(667:(666+nsim), function(x) project_ssb(ss_run, seed = x, sample_rec = TRUE))
ss_run_ssb <- apply(array(unlist(ss_run_ssb), c(dim(ss_run_ssb[[1]]), length(ss_run_ssb))), 2, rowMeans) # Mean across projections
ss_run$quantities$biomassSSB[] <- ss_run_ssb

ss_run_M_ssb <- lapply(667:(666+nsim), function(x) project_ssb(ss_run_M, seed = x, sample_rec = TRUE))
ss_run_M_ssb <- apply(array(unlist(ss_run_M_ssb), c(dim(ss_run_M_ssb[[1]]), length(ss_run_M_ssb))), 2, rowMeans) # Mean across projections
ss_run_M$quantities$biomassSSB[] <- ss_run_M_ssb

ms_run_ssb <- lapply(667:(666+nsim), function(x) project_ssb(ms_run, seed = x, sample_rec = TRUE))
ms_run_ssb <- apply(array(unlist(ms_run_ssb), c(dim(ms_run_ssb[[1]]), length(ms_run_ssb))), 2, rowMeans) # Mean across projections
ms_run$quantities$biomassSSB[] <- ms_run_ssb

ss_run_ricker_ssb <- lapply(667:(666+nsim), function(x) project_ssb(ss_run_ricker, seed = x, sample_rec = TRUE))
ss_run_ricker_ssb <- apply(array(unlist(ss_run_ricker_ssb), c(dim(ss_run_ricker_ssb[[1]]), length(ss_run_ricker_ssb))), 2, rowMeans) # Mean across projections
ss_run_ricker$quantities$biomassSSB[] <- ss_run_ricker_ssb

ss_run_ricker_M_ssb <- lapply(667:(666+nsim), function(x) project_ssb(ss_run_ricker_M, seed = x, sample_rec = TRUE))
ss_run_ricker_M_ssb <- apply(array(unlist(ss_run_ricker_M_ssb), c(dim(ss_run_ricker_M_ssb[[1]]), length(ss_run_ricker_M_ssb))), 2, rowMeans) # Mean across projections
ss_run_ricker_M$quantities$biomassSSB[] <- ss_run_ricker_M_ssb

ms_run_ricker_ssb <- lapply(667:(666+nsim), function(x) project_ssb(ms_run_ricker, seed = x, sample_rec = TRUE))
ms_run_ricker_ssb <- apply(array(unlist(ms_run_ricker_ssb), c(dim(ms_run_ricker_ssb[[1]]), length(ms_run_ricker_ssb))), 2, rowMeans) # Mean across projections
ms_run_ricker$quantities$biomassSSB[] <- ms_run_ricker_ssb

stopCluster(cl)

# - Plot
MPcols <- gmri_pal("main")(3)
ss_run$data_list$spnames <- paste("GOA", ss_run$data_list$spnames)

projected_OM_no_F <- list(ss_run, ss_run_M, ms_run, ss_run_ricker, ss_run_ricker_M, ms_run_ricker)
plot_ssb(projected_OM_no_F, file = "Results/Figures/GOA_OM_projection", , model_names = om_names_print[1:3], incl_proj = TRUE, width = 7, height = 6, line_col = MPcols[c(3:1, 3:1)], lty = c(1,1,1,5,6,6), maxyr = 2060)

