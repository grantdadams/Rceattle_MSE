################################################
# Set-up
################################################
library(Rceattle)
library(tidyr)
library(gmRi)
library(dplyr)
# source("R/Climate_MSE_condition_GOA_OMs.R")
# projected_OM_no_F <- om_list
# rm(mod_list_all);gc()
# 
# 
# ms_mod$quantities$depletionSSB <- ms_mod$quantities$biomassSSB/ms_mod$quantities$biomassSSB[,ncol(ms_mod$quantities$biomassSSB)]
# ms_mod_ricker$quantities$depletionSSB <- ms_mod_ricker$quantities$biomassSSB/ms_mod_ricker$quantities$biomassSSB[,ncol(ms_mod_ricker$quantities$biomassSSB)]


################################################
# Management strategy evaluation
################################################
### OMS
om_names_print <- c(
  "MS", "MS SSP126", "MS SSP245", "MS SSP585",
  "MS-R", "MS-R SSP126", "MS-R SSP245", "MS-R SSP585"
)
om_names <- paste0(c(
  # "ss_mod", "ss_mod_ssp126", "ss_mod_ssp245", "ss_mod_ssp585",
  # "ss_mod_ricker", "ss_mod_ricker_ssp126", "ss_mod_ricker_ssp245", "ss_mod_ricker_ssp585",
  # "ss_mod_M", "ss_mod_M_ssp126", "ss_mod_M_ssp245", "ss_mod_M_ssp585",
  # "ss_mod_M_ricker", "ss_mod_M_ricker_ssp126", "ss_mod_M_ricker_ssp245", "ss_mod_M_ricker_ssp585",
  "ms_mod", "ms_mod_ssp126", "ms_mod_ssp245", "ms_mod_ssp585",
  "ms_mod_ricker", "ms_mod_ricker_ssp126", "ms_mod_ricker_ssp245", "ms_mod_ricker_ssp585"
), "_OM")

### Management strategies
## EM-HCR names
em_names <- paste0(c("ss_run_Tier3", "ss_run_dynamicTier3", "ss_run_M_Tier3", "ss_run_M_dynamicTier3", "ms_run_fb40", "ms_run_fb40iter", "ms_run_cmsy", "ms_run_concmsy"), "_EM")


# Plots ----
MPcols <- gmri_pal("main")(8)

MPcols <- rev(oce::oce.colorsViridis(6))
# plot_biomass(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,2,4,6)])

# plot_recruitment(projected_OM_no_F, file = "Results/Climate MSE/Figures/Climate_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,1,3,5)], lty = c(1,1,1,5,5,5), species = c(1,3,2))
# plot_ssb(projected_OM_no_F, file = "Results/Climate MSE/Figures/Climate_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,1,3,5)], lty = c(1,1,1,5,6,6), species = c(1,3,2))
# plot_biomass(projected_OM_no_F, file = "Results/Climate MSE/Figures/Climate_OM_", model_names = om_names_print[1:3], width = 6, height = 4.5, line_col = MPcols[c(1,3,5,1,3,5)], lty = c(1,1,1,5,6,6), species = c(1,3,2))
# plot_stock_recruit(projected_OM_no_F[5:8], file = "Results/Climate MSE/Figures/Climate_OM_", model_names = om_names_print[4:6], width = 6, height = 4.5, line_col = MPcols[c(1,3,5)], species = c(1,3,2))
# plot_b_eaten_prop(projected_OM_no_F[c(3,6)], file = "Results/Climate MSE/Figures/Climate_OM_", model_names = om_names_print[c(3,6)], width = 6, height = 4.5, line_col = MPcols[c(2,6)], species = c(1,3,2))

# Do summary ----
source("D:/GitHub/Rceattle/R/11b-mse_summary.R", encoding = 'UTF-8', echo=TRUE)
source("D:/GitHub/Rceattle/R/7c-plot_diagnostics.R", encoding = 'UTF-8', echo=TRUE)
source("R/Functions/Summarize_climate_MSE_function.R")
source("R/Functions/Plot SSB MSE function.R", echo=TRUE)

# # * Check convergence ----
# no_cap_con <- check_mse_convergence(system = "GOA_Climate_2", cap = "FALSE",
#                       om_names = om_names,
#                       em_hcr_names = em_names)
# 
# cap_con <- check_mse_convergence(system = "GOA_Climate_2", cap = "TRUE",
#                                     om_names = om_names,
#                                     em_hcr_names = em_names)
# gc()
# write.csv(rbind(no_cap_con, cap_con), file = "MSE convergence check.csv")
convergence_sims <- read.csv(file = "MSE convergence check.csv")

discard <- convergence_sims %>%
  dplyr::filter(Use == FALSE) %>%
  dplyr::distinct(Ind) %>%
  arrange(Ind)


# * Summarize ----
# - No cap
summary_fun(system = "GOA_Climate_2", cap = "FALSE", 
            # om_list_no_F = projected_OM_no_F, 
            mse_om_names = om_names, # OM-6 & 7 (EM 7 & 8)
            em_hcr_names = em_names[1:4],
            exclude = NULL
)
gc()

# - Cap
summary_fun(system = "GOA_Climate_2", cap = "TRUE", 
            # om_list_no_F = projected_OM_no_F, 
            mse_om_names = om_names, 
            em_hcr_names = em_names[1:4], # OM 5 EM 7, OM 6 EM 6
            exclude = NULL
)
gc()



# rm(list = ls())
gc()
