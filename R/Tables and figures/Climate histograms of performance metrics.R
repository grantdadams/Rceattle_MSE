# Histogram and table of performance metrics (only NPFMC is used) rest is coommented out

## Libraries ----
pacman::p_load(fmsb, gmRi, scales, tidyr, dplyr, cowplot)
# devtools::install_github("https://github.com/gulfofmaine/gmRi")
source("R/Functions/Climate_PM_histogram_function.R")
source("R/Functions/Climate_PM_table_functions.R")

# Histograms ----
# - Combined
climate_mse_histogram(species = "Pollock", file = "Results/Climate MSE/Figures/Histograms by PM/Pollock/", height = 6, width = 12)
climate_mse_histogram(species = "Pollock", file = "Results/Climate MSE/Figures/Histograms by PM/Pollock/", height = 6, width = 12, cap_leg = 2)
# climate_mse_histogram(species = "Cod", file = "Results/Climate MSE/Figures/Histograms by PM/Cod/", height = 6, width = 12)
# climate_mse_histogram(species = "Arrowtooth flounder", file = "Results/Climate MSE/Figures/Histograms by PM/ATF/", height = 6, width = 12)

climate_mse_histogram(species = "Pollock", file = "Results/Climate MSE/Figures/Histograms by PM/Pollock/", height = 6, width = 12, legend.pos = "bottomleft")
climate_mse_histogram(species = "Cod", file = "Results/Climate MSE/Figures/Histograms by PM/Cod/", height = 6, width = 12, legend.pos = NA)
climate_mse_histogram(species = "Arrowtooth flounder", file = "Results/Climate MSE/Figures/Histograms by PM/ATF/", height = 6, width = 12, legend.pos = NA)

climate_mse_histogram(species = "Pollock", file = "Results/Climate MSE/Figures/Histograms by PM/Pollock/", height = 6, width = 12, legend.pos = "topright")
# climate_mse_histogram(species = "Cod", file = "Results/Climate MSE/Figures/Histograms by PM/Cod/", height = 6, width = 12, legend.pos = "topright")
# climate_mse_histogram(species = "Arrowtooth flounder", file = "Results/Climate MSE/Figures/Histograms by PM/ATF/", height = 6, width = 12, legend.pos = "topright")

climate_mse_histogram(species = "Pollock", file = "Results/Climate MSE/Figures/Histograms by PM/Pollock/", height = 6, width = 12, legend.pos = "bottomright")
# climate_mse_histogram(species = "Cod", file = "Results/Climate MSE/Figures/Histograms by PM/Cod/", height = 6, width = 12, legend.pos = "bottomright")
# climate_mse_histogram(species = "Arrowtooth flounder", file = "Results/Climate MSE/Figures/Histograms by PM/ATF/", height = 6, width = 12, legend.pos = "bottomright")



# * Look at convergence ----
# OM and EM names
om_names <- paste0(c(
  "ms_mod", "ms_mod_ricker",
  "ms_mod_ssp126", "ms_mod_ricker_ssp126",
  "ms_mod_ssp245", "ms_mod_ricker_ssp245",
  "ms_mod_ssp585", "ms_mod_ricker_ssp585" 
), "_OM")

em_hcr_names <- paste0(c("ss_run_Tier3", "ss_run_dynamicTier3", "ss_run_M_Tier3", "ss_run_M_dynamicTier3", "ms_run_fb40iter", "ms_run_fb40", "ms_run_cmsy", "ms_run_concmsy"), "_EM")


# Summary tables ----
# * Performance metrics
output_table = climate_pm_summary_table(om_names, em_hcr_names, cap = c(TRUE, FALSE), format = FALSE, reverse = FALSE)
output_table %>% group_by(OM, EM, Cap) %>% slice(n()) %>%
  filter(Nsim < 190) %>%
  arrange(EM, Cap, OM, Nsim) %>%
  as.data.frame()

pm_names <- c("Average Catch", "Catch IAV", "OM: P(SSB < SSBlimit)" , "OM: Terminal SSB", "OM: Terminal SSB Depletion (Dynamic)") # Names in table

output_table = climate_pm_summary_table(om_names, em_hcr_names, cap = c(TRUE, FALSE), format = TRUE, reverse = FALSE)
table_save <- output_table %>%
  filter(Performance.metric %in% pm_names & OM %in% c("ms_mod_OM") & Cap == FALSE) %>%
  select(-Cap, -Nsim) %>%
  pivot_wider(names_from = c(OM, EM), values_from = Value)
write.csv(table_save, file = paste0("Results/Climate MSE/Tables/Table 7 - EM summary climate naive no srr.csv"))


output_table = climate_pm_summary_table(om_names, em_hcr_names, cap = c(TRUE, FALSE), format = TRUE, reverse = FALSE)
table_save <- output_table %>%
  filter(Performance.metric %in% pm_names & OM %in% c("ms_mod_ricker_OM") & Cap == FALSE) %>%
  select(-Cap, -Nsim) %>%
  pivot_wider(names_from = c(OM, EM), values_from = Value)
write.csv(table_save, file = paste0("Results/Climate MSE/Tables/Table 8 - EM summary climate naive ricker.csv"))



# Model parameters ----
pacman::p_load(Rceattle, readxl, dplyr, tidyr, writexl, ggplot2)
load("Models/GOA_20_1_1_mod_list.RData")
combined_data <- read_data(file = "Data/GOA_23_1_1_data_1977_2023_edited.xlsx")
combined_data$projyr <- 2100
combined_data$endyr <- 2020
alpha = exp(c(3.143, 1.975, 1.44))


# * Ajust inits ----
turn_offs <- c("logH_1", "logH_1a", "logH_1b", "logH_2", "logH_3", "H_4", "log_gam_a", "log_gam_b", "log_phi", "ln_pop_scalar", "sel_coff_dev")
for(i in 1:length(mod_list_all)){
  mod_list_all[[i]]$estimated_params[turn_offs] <- NULL
  
  mod_list_all[[i]]$estimated_params$rec_dev <- cbind(
    mod_list_all[[i]]$estimated_params$rec_dev, matrix(0, nrow = 3, ncol = 50))
  
  mod_list_all[[i]]$estimated_params$beta_rec_pars <- matrix(0, 3, 1)
}



# * Load and scale ----
summer_bt_data <- read.csv("Data/goa_temp_610_to_630_summer_300M.csv") %>%
  filter(depthclass == "Bottom", hind == "yes")  %>%
  mutate(varname = "summer bt") %>%
  rename(value = mean_value_dc_610_to_630)

winter_sst_data <- read.csv("Data/goa_temp_610_to_630_winter_300M.csv") %>%
  filter(depthclass == "Surface", hind == "yes")  %>%
  mutate(varname = "winter sst") %>%
  rename(value = mean_value_dc_610_to_630)

zoo_data <- read.csv("Data/goa_large_zoo_610_to_630_fall_300M.csv") %>%
  filter(depthclass == "Surface", hind == "yes") %>%
  mutate(varname = "mzl") %>%
  rename(value = mean_value_dc_610_to_630)

nyrs <- length(1980:2020) # Years of hindcast for scaling

climate_data <- rbind(summer_bt_data, winter_sst_data, zoo_data) %>%
  dplyr::select(-X) %>%
  mutate(value_squared = value^2) %>%
  pivot_wider(names_from = c(simulation), values_from = c(value, value_squared)) %>%
  dplyr::select(-depthclass, -hind) %>%
  rename(Year = year) %>%
  group_by(varname) %>%
  mutate(value_ssp126z = (value_ssp126 - mean(value_ssp126[1:nyrs])) / sqrt(var(value_ssp126[1:nyrs])),
         value_ssp245z = (value_ssp245 - mean(value_ssp245[1:nyrs])) / sqrt(var(value_ssp245[1:nyrs])), 
         value_ssp585z = (value_ssp585 - mean(value_ssp585[1:nyrs])) / sqrt(var(value_ssp585[1:nyrs])),  
         value_squared_ssp126z = (value_squared_ssp126 - mean(value_squared_ssp126[1:nyrs])) / sqrt(var(value_squared_ssp126[1:nyrs])),
         value_squared_ssp245z = (value_squared_ssp245 - mean(value_squared_ssp245[1:nyrs])) / sqrt(var(value_squared_ssp245[1:nyrs])),
         value_squared_ssp585z = (value_squared_ssp585 - mean(value_squared_ssp585[1:nyrs])) / sqrt(var(value_squared_ssp585[1:nyrs]))
  ) %>%
  ungroup() %>%
  as.data.frame()

# * Add missing years ----
# -- summer bottom temp
summer_bt_data <- climate_data %>%
  filter(varname == "summer bt") %>%
  dplyr::select(-varname)

temp_sub <- data.frame(Year = 1977:1979, 
                       value_ssp126 = mean(summer_bt_data$value_ssp126[1:10]), 
                       value_ssp245 = mean(summer_bt_data$value_ssp245[1:10]),
                       value_ssp585 = mean(summer_bt_data$value_ssp585[1:10]), 
                       value_squared_ssp126 = mean(summer_bt_data$value_squared_ssp126[1:10]), 
                       value_squared_ssp245 = mean(summer_bt_data$value_squared_ssp245[1:10]),
                       value_squared_ssp585 = mean(summer_bt_data$value_squared_ssp585[1:10]), 
                       value_ssp126z = 0,
                       value_ssp245z = 0,
                       value_ssp585z = 0,
                       value_squared_ssp126z = 0,
                       value_squared_ssp245z = 0,
                       value_squared_ssp585z = 0)

summer_bt_data <- rbind(temp_sub, summer_bt_data) 
colnames(summer_bt_data) <- c("Year", paste0("BT_", colnames(summer_bt_data)[2:ncol(summer_bt_data)]))


# -- winter sst
winter_sst_data <- climate_data %>%
  filter(varname == "winter sst") %>%
  dplyr::select(-varname)

temp_sub <- data.frame(Year = 1977:1979, 
                       value_ssp126 = mean(winter_sst_data$value_ssp126[1:10]), 
                       value_ssp245 = mean(winter_sst_data$value_ssp245[1:10]),
                       value_ssp585 = mean(winter_sst_data$value_ssp585[1:10]), 
                       value_squared_ssp126 = mean(winter_sst_data$value_squared_ssp126[1:10]), 
                       value_squared_ssp245 = mean(winter_sst_data$value_squared_ssp245[1:10]),
                       value_squared_ssp585 = mean(winter_sst_data$value_squared_ssp585[1:10]), 
                       value_ssp126z = 0,
                       value_ssp245z = 0,
                       value_ssp585z = 0,
                       value_squared_ssp126z = 0,
                       value_squared_ssp245z = 0,
                       value_squared_ssp585z = 0)

winter_sst_data <- rbind(temp_sub, winter_sst_data) 
colnames(winter_sst_data) <- c("Year", paste0("SST_", colnames(winter_sst_data)[2:ncol(winter_sst_data)]))


# -- Zooplankton
zoo_data <- climate_data %>%
  filter(varname == "mzl") %>%
  dplyr::select(-varname)

mzl_sub <- data.frame(Year = 1977:1979, 
                      value_ssp126 = mean(zoo_data$value_ssp126[1:10]), 
                      value_ssp245 = mean(zoo_data$value_ssp245[1:10]),
                      value_ssp585 = mean(zoo_data$value_ssp585[1:10]), 
                      value_squared_ssp126 = mean(zoo_data$value_squared_ssp126[1:10]), 
                      value_squared_ssp245 = mean(zoo_data$value_squared_ssp245[1:10]),
                      value_squared_ssp585 = mean(zoo_data$value_squared_ssp585[1:10]), 
                      value_ssp126z = 0,
                      value_ssp245z = 0,
                      value_ssp585z = 0,
                      value_squared_ssp126z = 0,
                      value_squared_ssp245z = 0,
                      value_squared_ssp585z = 0)
zoo_data <- rbind(mzl_sub, zoo_data) 
colnames(zoo_data) <- c("Year", paste0("MZL_", colnames(zoo_data)[2:ncol(zoo_data)]))

# - Combine
climate_data <- winter_sst_data %>% 
  inner_join(zoo_data, by = "Year") %>%
  inner_join(summer_bt_data, by = "Year") %>%
  arrange(Year)
climate_data$SST = NA
climate_data$BT = NA
climate_data$MZL = NA

climate_data$SST[1:length(1977:2020)] = climate_data$SST_value_ssp126[1:length(1977:2020)]

climate_data$BT[1:length(1977:2020)] = climate_data$BT_value_ssp126[1:length(1977:2020)]

climate_data$MZL[1:length(1977:2020)] = climate_data$MZL_value_ssp126[1:length(1977:2020)]


# SST Plots ----
MPcols <- c((gmri_pal("main")(3)), "black")

sst_plot <- climate_data %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y=SST_value_ssp126, color="SSP126 (low)"), linewidth = 1) +
  geom_line(aes(y=SST_value_ssp245, color="SSP245 (med)"), linewidth = 1) +
  geom_line(aes(y=SST_value_ssp585, color="SSP585 (high)"), linewidth = 1) +
  geom_line(aes(y=SST, color="Hindcast"), linewidth = 1) +
  ylab("Winter SST  (C)") + 
  scale_color_manual("Scenario", values=c("SSP126 (low)" = MPcols[1], "SSP245 (med)" = MPcols[2], "SSP585 (high)" = MPcols[3], "Hindcast" = MPcols[4])) + 
  theme_classic() +
  theme(legend.position = c(0.1, 0.8))

bt_plot <- climate_data %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y=BT_value_ssp126, color="SSP126"), linewidth = 1) +
  geom_line(aes(y=BT_value_ssp245, color="SSP245"), linewidth = 1) +
  geom_line(aes(y=BT_value_ssp585, color="SSP585"), linewidth = 1) +
  geom_line(aes(y=BT, color="Hindcast"), linewidth = 1) +
  ylab("Summert BT (C)") + 
  scale_color_manual(values=c("SSP126" = MPcols[1], "SSP245" = MPcols[2], "SSP585" = MPcols[3], "Hindcast" = MPcols[4])) + 
  theme_classic() + 
  theme(legend.position = "none")

zoo_plot <- climate_data %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y=MZL_value_ssp126, color="SSP126"), linewidth = 1) +
  geom_line(aes(y=MZL_value_ssp245, color="SSP245"), linewidth = 1) +
  geom_line(aes(y=MZL_value_ssp585, color="SSP585"), linewidth = 1) +
  geom_line(aes(y=MZL, color="Hindcast"), linewidth = 1) +
  ylab("Fall zooplankton biomass") + 
  scale_color_manual(values=c("SSP126" = MPcols[1], "SSP245" = MPcols[2], "SSP585" = MPcols[3], "Hindcast" = MPcols[4])) + 
  theme_classic() + 
  theme(legend.position = "none")

g1 <- plot_grid(bt_plot,
                sst_plot,
                zoo_plot,
                ncol = 1)

ggsave(filename = "Results/Climate MSE/Figures/ROMZ_climate_indices.png", g1, width = 7, height = 10, units = "in")

ggsave(filename = "Results/Climate MSE/Figures/ROMZ_sst.png", sst_plot, width = 7, height = 5, units = "in")


# * add to Rceattle object ----
# combined_data$fleet_control$Fleet_type[18] <- 0
ssp_dat_126 <- ssp_dat_245 <- ssp_dat_585 <- combined_data

ssp_dat_126$env_data <- climate_data %>%
  dplyr::select(Year, BT_value_ssp126, SST_value_ssp126z, SST_value_squared_ssp126z, MZL_value_ssp126z )

ssp_dat_245$env_data <- climate_data %>%
  dplyr::select(Year, BT_value_ssp245, SST_value_ssp245z, SST_value_squared_ssp245z, MZL_value_ssp245z )

ssp_dat_585$env_data <- climate_data %>%
  dplyr::select(Year, BT_value_ssp585, SST_value_ssp585z, SST_value_squared_ssp585z, MZL_value_ssp585z )


# Stock-recruit parameters ----
# * Density-independent recruitment ----
# - Climate naive
ms_mod <- Rceattle::fit_mod(
  data_list = combined_data,
  inits = mod_list_all[[3]]$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # Multi species mode
  verbose = 1,
  niter = 3,
  suit_meanyr = 2018,
  phase = NULL,
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  initMode = 1)

# -- SSP126
ms_mod_ssp126 <- Rceattle::fit_mod(
  data_list = ssp_dat_126,
  inits = ms_mod$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # Multi species mode
  verbose = 1,
  niter = 3,
  suit_meanyr = 2018,
  phase = NULL,
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 1,
                     srr_env_indices = c(2,3,4)),
  initMode = 1)

# * Ricker recruitment ----
# - Climate naive
ms_mod_ricker <- Rceattle::fit_mod(
  data_list = combined_data,
  inits = ms_mod$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # Multi species mode
  verbose = 1,
  niter = 3,
  suit_meanyr = 2018,
  phase = NULL,
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 0,
                     srr_pred_fun = 4,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha,
                     srr_prior_sd = 0.2,
                     Bmsy_lim = apply(ms_mod$quantities$biomassSSB, 1, max)
  ),
  initMode = 1)

# - SSP126
ms_mod_ricker_ssp126 <- Rceattle::fit_mod(
  data_list = ssp_dat_126,
  inits = ms_mod_ricker$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # Multi species mode
  verbose = 1,
  niter = 3,
  suit_meanyr = 2018,
  phase = NULL,
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 0,
                     srr_pred_fun = 5,
                     srr_env_indices = c(2,3,4),
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha,
                     srr_prior_sd = 0.2,
                     Bmsy_lim = apply(ms_mod$quantities$biomassSSB, 1, max)
  ),
  initMode = 1)


beta_rec_pars <- cbind(ms_mod_ssp126$estimated_params$beta_rec_pars, ms_mod_ricker_ssp126$estimated_params$beta_rec_pars)
write.csv(beta_rec_pars, "Results/Climate MSE/beta_rec_pars.csv")

# Plot EMs ----
library(Rceattle)
library(tidyr)
library(gmRi)
library(dplyr)
source("R/Climate_MSE_condition_GOA_EMs.R")
source("R/Climate_MSE_condition_GOA_OMs.R")

MPcols <- gmri_pal("main")(11)[c(1,3,5,7,9,11)]

ss_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = mod_list_all[[1]]$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 3, # Single species mode
                            verbose = 1,
                            phase = NULL,
                            initMode = 1)

ss_mod_M <- Rceattle::fit_mod(data_list = combined_data,
                              inits = mod_list_all[[2]]$estimated_params, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 3, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              verbose = 1,
                              phase = "default",
                              M1Fun = build_M1(M1_model = c(1,2,1),
                                               updateM1 = FALSE,
                                               M1_use_prior = FALSE,
                                               M2_use_prior = FALSE),
                              initMode = 1)
ms_mod2 <- ms_mod
ms_mod2$data_list$projyr <- 2020
mod_list <- list(ms_run_fb40iter, ms_run_fb40, ms_run_cmsy, ms_run_concmsy, ms_mod2, ms_mod)

plot_ssb(mod_list, 
         file = "Results/Climate MSE/Figures/Climate_OM_projected", 
         model_names =   c("NPFMC Fix-M", "NPFMC Est-M" , "MS-B40a", "MS-B40b", "MS-MSY", "MS-cMSY"), 
         width = 7, height = 8, 
         line_col = c(MPcols[-c(1:2)], 1, 1), lty = c(1,1,1,1,1,2), 
         species = c(1,3,2), maxyr = 2100)

plot_ssb(mod_list, 
         file = "Results/Climate MSE/Figures/Climate_OM_projected_pollock", 
         model_names =   c("MS-B40a", "MS-B40b", "MS-MSY", "MS-cMSY"), 
         width = 6, height = 4, 
         line_col = c(MPcols[-c(1:2)], 1, 1), lty = c(1,1,1,1,1,2), 
         species = c(1), maxyr = 2100)


plot_catch(list(ss_run_Tier3, ss_run_M_Tier3, ms_run_fb40iter, ms_run_fb40, ms_run_cmsy, ms_run_concmsy), 
           file = "Results/Climate MSE/Figures/Climate_OM_projected", 
           model_names =   c("NPFMC Fix-M", "NPFMC Est-M" , "MS-B40a", "MS-B40b", "MS-MSY", "MS-cMSY"), 
           width = 8, height = 8, top_adj = 1,
           line_col = MPcols, lwd = 3,
           species = c(1,3,2), maxyr = 2100)


plot_catch(list(ms_run_fb40iter, ms_run_fb40, ms_run_cmsy, ms_run_concmsy), 
           file = "Results/Climate MSE/Figures/Climate_OM_projected_pollock", 
           model_names =   c("MS-B40a", "MS-B40b", "MS-MSY", "MS-cMSY"), 
           width = 6, height = 4, top_adj = 1,
           line_col = MPcols[-c(1:2)], 
           species = c(1,3,2), maxyr = 2100, fleets = 1, lwd = 3.5)



MPcols2 <- c((gmri_pal("main")(3)), 1)
plot_ssb(list(ms_mod_ssp126, ms_mod_ssp245, ms_mod_ssp585, ms_mod), 
         # file = "Results/Climate MSE/Figures/Climate_OM_projected",
         model_names =   c("SSP126 (low)", "SSP245 (med)", "SSP585 (high)", "Naive"), 
         width = 7, height = 8, 
         line_col = MPcols2, 
         species = c(1, 3, 2), maxyr = 2100)


source("~/GitHub/Rceattle_MSE/R/Functions/Recruitment with env vars function.R")
plot_recruitment_hat(list(ms_mod, ms_mod_ssp126), 
                     file = "Results/Climate MSE/Figures/Climate_OM_rec_hat_",
                     model_names =   c("No climate", "Climate"), 
                     width = 6, height = 4, 
                     line_col = c(1, MPcols2[2], 1), 
                     minus_dev = c(FALSE, TRUE, FALSE),
                     pch = c(17, 16, 17),
                     species = c(1, 3, 2))

plot_recruitment_hat(list(ms_mod_ricker, ms_mod_ricker_ssp126), 
                     file = "Results/Climate MSE/Figures/Climate_OM_rec_hat_ricker_",
                     model_names =   c("No climate", "Climate"), 
                     width = 6, height = 4, 
                     line_col = c(1, MPcols2[2], 1), 
                     pch = c(17, 16, 17),
                     species = c(1, 3, 2))
