################################################
# Set-up
################################################
source("R/BSAI_condition_models.R")
source("R/BSAI_condition_ricker_models.R")
library(gmRi)
library(Rceattle)
library(tidyr)

gc()
ms_run$quantities$depletionSSB <- ms_run$quantities$biomassSSB/ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)]
ms_run_ricker$quantities$depletionSSB <- ms_run_ricker$quantities$biomassSSB/ms_run_ricker$quantities$biomassSSB[,ncol(ms_run_ricker$quantities$biomassSSB)]


################################################
# Management strategy evaluation
################################################
### OMS
# 1. Single-species fix M
# 2. Single-species estimate M
# 3. Multi-species type II
om_list <- list(ss_run_Tier3, ss_run_M_Tier3, ms_run_f25, ss_run_ricker_Tier3, ss_run_ricker_M_Tier3, ms_run_ricker_f25)
projected_OM_no_F <- list(ss_run, ss_run_M, ms_run, ss_run_ricker, ss_run_ricker_M, ms_run_ricker)
om_names = c("SS_OM", "SSM_OM", "MS_OM", "SS_Ricker_OM", "SSM_Ricker_OM", "MS_Ricker_OM")
om_names_print = c("SS fix M", "SS est M", "MS", "SS fix M Ricker", "SS est M Ricker", "MS Ricker")


# Lists to update reference points in OM (can remove later)
# - No ricker
om_hcr_list_fixM <- list(ss_run_Tier3, ss_run_dynamicTier3, ss_run_Cat1, ss_run_dynamicCat1, ss_run_Tier1, ss_run_dynamicTier1, ss_run_Fspr, ss_run_AvgF) # Fixed M
om_hcr_list_fixM = c(om_hcr_list_fixM, om_hcr_list_fixM)

om_hcr_list_estM <- list(ss_run_M_Tier3, ss_run_M_dynamicTier3, ss_run_M_Cat1, ss_run_M_dynamicCat1, ss_run_M_Tier1, ss_run_M_dynamicTier1, ss_run_M_Fspr, ss_run_M_AvgF) # Estimate M
om_hcr_list_estM = c(om_hcr_list_estM, om_hcr_list_estM)

# - Ricker
om_hcr_list_ricker_fixM <- list(ss_run_ricker_Tier3, ss_run_ricker_dynamicTier3, ss_run_ricker_Cat1, ss_run_ricker_dynamicCat1, ss_run_ricker_Tier1, ss_run_ricker_dynamicTier1, ss_run_ricker_Fspr, ss_run_ricker_AvgF) # Fixed M
om_hcr_list_ricker_fixM = c(om_hcr_list_ricker_fixM, om_hcr_list_ricker_fixM)

om_hcr_list_ricker_estM <- list(ss_run_ricker_M_Tier3, ss_run_ricker_M_dynamicTier3, ss_run_ricker_M_Cat1, ss_run_ricker_M_dynamicCat1, ss_run_ricker_M_Tier1, ss_run_ricker_M_dynamicTier1, ss_run_ricker_M_Fspr, ss_run_ricker_M_AvgF) # Estimate M
om_hcr_list_ricker_estM = c(om_hcr_list_ricker_estM, om_hcr_list_ricker_estM)


### Management strategies
## EM
# 1. Single-species fix M
# 2. Single-species estimate M

# HCR
# 1, NPFMC Tier 3 HCR
# 1b. NPFMC Tier 3 dynamic HCR 
# 2. PFMC Category 1 40-10 HCR
# 2b. PFMC Category 1 40-10 dynamic HCR
# 3. SESSF Tier 1 HCR
# 3b. SESSF Tier 1 dynamic HCR
# 4. Average F
# 5. Equilibrium F_(40%)
em_hcr_list <- list(ss_run_Tier3, ss_run_dynamicTier3, ss_run_Cat1, ss_run_dynamicCat1, ss_run_Tier1, ss_run_dynamicTier1, ss_run_Fspr, ss_run_AvgF, # Fixed M
                    ss_run_M_Tier3, ss_run_M_dynamicTier3, ss_run_M_Cat1, ss_run_M_dynamicCat1, ss_run_M_Tier1, ss_run_M_dynamicTier1, ss_run_M_Fspr, ss_run_M_AvgF # Estimate M
)


em_hcr_names <- c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", "SS_fixM_AvgF_EM", # Fixed M
                  "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM", "SS_estM_AvgF_EM")


# Plots ----
# MPcols <- gmri_pal("main")(8)
MPcols <- rev(oce::oce.colorsViridis(6))
# plot_biomass(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,2,4,6)])
plot_recruitment(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,1,3,5)], lty = c(1,1,1,5,5,5))
plot_ssb(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,1,3,5)], lty = c(1,1,1,5,6,6))
plot_biomass(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,1,3,5)], lty = c(1,1,1,5,6,6))
plot_stock_recruit(projected_OM_no_F[4:6], file = "Results/Figures/EBS_OM_", model_names = om_names_print[4:6], width = 6, height = 4.5, line_col = MPcols[c(1,3,5)])
plot_b_eaten_prop(projected_OM_no_F[c(3,6)], file = "Results/Figures/EBS_OM_", model_names = om_names_print[c(3,6)], width = 6, height = 4.5, line_col = MPcols[c(2,6)])



################################################
# Plots ----
################################################
### OMS
# 1. Single-species fix M
# 2. Single-species estimate M
# 3. Multi-species type II
om_list <- list(ss_run_Tier3, ss_run_M_Tier3, ms_run_f25, ss_run_ricker_Tier3, ss_run_ricker_M_Tier3, ms_run_ricker_f25)
projected_OM_no_F <- list(ss_run, ss_run_M, ms_run, ss_run_ricker, ss_run_ricker_M, ms_run_ricker)
om_names = c("SS_OM", "SSM_OM", "MS_OM", "SS_Ricker_OM", "SSM_Ricker_OM", "MS_Ricker_OM")
om_names_print = c("SS fix M", "SS est M", "MS", "SS fix M Ricker", "SS est M Ricker", "MS Ricker")

# Plots ----
MPcols <- rev(oce::oce.colorsViridis(6))
# plot_biomass(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,2,4,6)])
plot_recruitment(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,1,3,5)], lty = c(1,1,1,5,5,5))
plot_ssb(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,1,3,5)], lty = c(1,1,1,5,6,6))
plot_biomass(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,1,3,5)], lty = c(1,1,1,5,6,6))
plot_stock_recruit(projected_OM_no_F[4:6], file = "Results/Figures/EBS_OM_", model_names = om_names_print[4:6], width = 6, height = 4.5, line_col = MPcols[c(1,3,5)])
plot_b_eaten_prop(projected_OM_no_F[c(3,6)], file = "Results/Figures/EBS_OM_", model_names = om_names_print[c(3,6)], width = 6, height = 4.5, line_col = MPcols[c(2,6)])


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
ss_run$data_list$spnames <- paste("EBS", ss_run$data_list$spnames)
projected_OM_no_F <- list(ss_run, ss_run_M, ms_run, ss_run_ricker, ss_run_ricker_M, ms_run_ricker)
plot_ssb(projected_OM_no_F, file = "Results/Figures/EBS_OM_projection", model_names = om_names_print[1:3], incl_proj = TRUE, width = 7, height = 6, line_col = MPcols[c(3:1, 3:1)], lty = c(1,1,1,5,6,6), maxyr = 2060)


################################################
# Load and run summary
################################################
# Do summary ----
source("R/Functions/MSE_summary.R", encoding = 'UTF-8', echo=TRUE)
source("R/Functions/MSE_summary_function.R")


# SAFS 313-12
# - No SRR
summary_fun(system = "EBS", recname = "ConstantR", om_list_no_F = projected_OM_no_F[1:2], om_names = om_names[1:2], om_hcr_list_fixM = om_hcr_list_fixM, om_hcr_list_estM = om_hcr_list_estM, em_hcr_names = em_hcr_names, species = 1:3)
summary_fun(system = "EBS", recname = "TRUE regen", om_list_no_F = projected_OM_no_F[3], om_names = om_names[3], om_hcr_list_fixM = om_hcr_list_fixM, om_hcr_list_estM = om_hcr_list_estM, em_hcr_names = em_hcr_names, species = 1:3)
gc()

# - Ricker SRR
summary_fun(system = "EBS", recname = "ConstantR", om_list_no_F = projected_OM_no_F[4:5], om_names = om_names[4:5], om_hcr_list_fixM = om_hcr_list_ricker_fixM, om_hcr_list_estM = om_hcr_list_ricker_estM, em_hcr_names = em_hcr_names, species = 1:3)
summary_fun(system = "EBS", recname = "TRUE regen", om_list_no_F = projected_OM_no_F[6], om_names = om_names[6], om_hcr_list_fixM = om_hcr_list_ricker_fixM, om_hcr_list_estM = om_hcr_list_ricker_estM, em_hcr_names = em_hcr_names, species = 1:3)
gc()



rm(list = ls())
gc()

