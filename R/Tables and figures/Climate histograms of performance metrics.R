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
climate_mse_histogram(species = "Cod", file = "Results/Climate MSE/Figures/Histograms by PM/Cod/", height = 6, width = 12)
climate_mse_histogram(species = "Arrowtooth flounder", file = "Results/Climate MSE/Figures/Histograms by PM/ATF/", height = 6, width = 12)

climate_mse_histogram(species = "Pollock", file = "Results/Climate MSE/Figures/Histograms by PM/Pollock/", height = 6, width = 12, legend.pos = "bottomleft")
climate_mse_histogram(species = "Cod", file = "Results/Climate MSE/Figures/Histograms by PM/Cod/", height = 6, width = 12, legend.pos = NA)
climate_mse_histogram(species = "Arrowtooth flounder", file = "Results/Climate MSE/Figures/Histograms by PM/ATF/", height = 6, width = 12, legend.pos = NA)

climate_mse_histogram(species = "Pollock", file = "Results/Climate MSE/Figures/Histograms by PM/Pollock/", height = 6, width = 12, legend.pos = "topright")
climate_mse_histogram(species = "Cod", file = "Results/Climate MSE/Figures/Histograms by PM/Cod/", height = 6, width = 12, legend.pos = "topright")
climate_mse_histogram(species = "Arrowtooth flounder", file = "Results/Climate MSE/Figures/Histograms by PM/ATF/", height = 6, width = 12, legend.pos = "topright")

climate_mse_histogram(species = "Pollock", file = "Results/Climate MSE/Figures/Histograms by PM/Pollock/", height = 6, width = 12, legend.pos = "bottomright")
climate_mse_histogram(species = "Cod", file = "Results/Climate MSE/Figures/Histograms by PM/Cod/", height = 6, width = 12, legend.pos = "bottomright")
climate_mse_histogram(species = "Arrowtooth flounder", file = "Results/Climate MSE/Figures/Histograms by PM/ATF/", height = 6, width = 12, legend.pos = "bottomright")



# * Look at convergence ----
# OM and EM names
om_names <- paste0(c(
  "ms_mod", "ms_mod_ricker",
  "ms_mod_ssp126", "ms_mod_ricker_ssp126",
  "ms_mod_ssp245", "ms_mod_ricker_ssp245",
  "ms_mod_ssp585", "ms_mod_ricker_ssp585" 
), "_OM")

em_hcr_names <- paste0(c("ss_run_Tier3", "ss_run_dynamicTier3", "ss_run_M_Tier3", "ss_run_M_dynamicTier3", "ms_run_fb40", "ms_run_fb40iter", "ms_run_cmsy", "ms_run_concmsy"), "_EM")


# - Get output ----
output_table = climate_pm_summary_table(om_names, em_hcr_names, cap = c(TRUE, FALSE), format = FALSE, reverse = FALSE)
output_table %>% group_by(OM, EM, Cap) %>% slice(n()) %>%
  arrange(Nsim) %>%
  arrange(EM) %>%
  as.data.frame()


# * Model parameters ----
pacman::p_load(Rceattle, readxl, dplyr, tidyr, writexl, ggplot2)
load("Models/GOA_20_1_1_mod_list.RData")
combined_data <- read_data(file = "Data/GOA_23_1_1_data_1977_2023_edited.xlsx")
combined_data$projyr <- 2100
combined_data$endyr <- 2020
alpha = exp(c(3.143, 1.975, 1.44))


## Ajust inits ----
turn_offs <- c("logH_1", "logH_1a", "logH_1b", "logH_2", "logH_3", "H_4", "log_gam_a", "log_gam_b", "log_phi", "ln_pop_scalar", "sel_coff_dev")
for(i in 1:length(mod_list_all)){
  mod_list_all[[i]]$estimated_params[turn_offs] <- NULL
  
  mod_list_all[[i]]$estimated_params$rec_dev <- cbind(
    mod_list_all[[i]]$estimated_params$rec_dev, matrix(0, nrow = 3, ncol = 50))
  
  mod_list_all[[i]]$estimated_params$beta_rec_pars <- matrix(0, 3, 1)
}


## Climate data ----
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


# Plots ----
MPcols <- gmri_pal("main")(3)

sst_plot <- climate_data %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y=SST_value_ssp245, color=MPcols[2]), linewidth = 1) +
  geom_line(aes(y=SST_value_ssp585, color=MPcols[1]), linewidth = 1) +
  geom_line(aes(y=SST_value_ssp126, color=MPcols[3]), linewidth = 1) +
  ylab("Winter SST  (C)") + 
  theme_classic() + 
  theme(legend.position = "none")

bt_plot <- climate_data %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y=BT_value_ssp126, color=MPcols[2]), linewidth = 1) +
  geom_line(aes(y=BT_value_ssp245, color=MPcols[1]), linewidth = 1) +
  geom_line(aes(y=BT_value_ssp585, color=MPcols[3]), linewidth = 1) +
  ylab("Summert bottom temperature (C)") + 
  theme_classic() + 
  theme(legend.position = "none")

zoo_plot <- climate_data %>%
  ggplot(aes(x = Year)) +
  geom_line(aes(y=MZL_value_ssp126, color=MPcols[2]), linewidth = 1) +
  geom_line(aes(y=MZL_value_ssp245, color=MPcols[1]), linewidth = 1) +
  geom_line(aes(y=MZL_value_ssp585, color=MPcols[3]), linewidth = 1) +
  ylab("Fall zooplankton biomass") + 
  theme_classic() + 
  theme(legend.position = "none")

g1 <- plot_grid(bt_plot,
          sst_plot,
          zoo_plot,
          ncol = 1)

ggsave(filename = "Results/Climate MSE/Figures/ROMZ_climate_indices.png", g1, width = 7, height = 10, units = "in")


# * add to Rceattle object ----
# combined_data$fleet_control$Fleet_type[18] <- 0
ssp_dat_126 <- ssp_dat_245 <- ssp_dat_585 <- combined_data

ssp_dat_126$env_data <- climate_data %>%
  dplyr::select(Year, BT_value_ssp126, SST_value_ssp126z, SST_value_squared_ssp126z, MZL_value_ssp126z )

ssp_dat_245$env_data <- climate_data %>%
  dplyr::select(Year, BT_value_ssp245, SST_value_ssp245z, SST_value_squared_ssp245z, MZL_value_ssp245z )

ssp_dat_585$env_data <- climate_data %>%
  dplyr::select(Year, BT_value_ssp585, SST_value_ssp585z, SST_value_squared_ssp585z, MZL_value_ssp585z )


# OM 3) Multi-species ----
# * Density-independent recruitment ----
# - Climate naive
ms_mod <- Rceattle::fit_mod(data_list = combined_data,
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
ms_mod_ssp126 <- Rceattle::fit_mod(data_list = ssp_dat_126,
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
ms_mod_ricker <- Rceattle::fit_mod(data_list = combined_data,
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
ms_mod_ricker_ssp126 <- Rceattle::fit_mod(data_list = ssp_dat_126,
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
