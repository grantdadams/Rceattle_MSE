pacman::p_load(Rceattle, readxl, dplyr, tidyr, writexl)
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
  select(-depthclass, -hind) %>%
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
  select(-varname)

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
  select(-varname)

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
  select(-varname)

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


# * add to Rceattle object ----
# combined_data$fleet_control$Fleet_type[18] <- 0
ssp_dat_126 <- ssp_dat_245 <- ssp_dat_585 <- combined_data

ssp_dat_126$env_data <- climate_data %>%
  select(Year, BT_value_ssp126, SST_value_ssp126z, SST_value_squared_ssp126z, MZL_value_ssp126z )

ssp_dat_245$env_data <- climate_data %>%
  select(Year, BT_value_ssp245, SST_value_ssp245z, SST_value_squared_ssp245z, MZL_value_ssp245z )

ssp_dat_585$env_data <- climate_data %>%
  select(Year, BT_value_ssp585, SST_value_ssp585z, SST_value_squared_ssp585z, MZL_value_ssp585z )


## Estimate OMs ----
# OM 1) Single-spp fix M ----
# * Density-independent recruitment ----
# - Climate naive
ss_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = mod_list_all[[1]]$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            verbose = 1,
                            phase = NULL,
                            initMode = 1)

# -- SSP126
ss_mod_ssp126 <- Rceattle::fit_mod(data_list = ssp_dat_126,
                                   inits = ss_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   recFun = build_srr(srr_fun = 1,
                                                      srr_env_indices = c(2,3,4)),
                                   msmMode = 0, # Single species mode
                                   verbose = 1,
                                   phase = NULL,
                                   initMode = 1)

# -- SSP245
ss_mod_ssp245 <- Rceattle::fit_mod(data_list = ssp_dat_245,
                                   inits = ss_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   recFun = build_srr(srr_fun = 1,
                                                      srr_env_indices = c(2,3,4)),
                                   msmMode = 0, # Single species mode
                                   verbose = 1,
                                   phase = NULL,
                                   initMode = 1)

# -- SSP585
ss_mod_ssp585 <- Rceattle::fit_mod(data_list = ssp_dat_585,
                                   inits = ss_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   recFun = build_srr(srr_fun = 1,
                                                      srr_env_indices = c(2,3,4)),
                                   msmMode = 0, # Single species mode
                                   verbose = 1,
                                   phase = NULL,
                                   initMode = 1)



# * Ricker recruitment ----
# - Find fixed alpha
SSB <- ss_mod$quantities$biomassSSB[,1:44]
R <- ss_mod$quantities$R[,1:44]
R_SSB <- log(R/SSB)

# R = a*SSB*e^(-B*SSB)
# R/SSB = a*e^(-B*SSB)
# log(R/SSB) = log(a) + -B*SSB
alpha1 <- summary(lm(R_SSB[1,] ~ SSB[1,]))$coefficients[1,1]
alpha2 <- summary(lm(R_SSB[2,] ~ SSB[2,]))$coefficients[1,1]
alpha3 <- summary(lm(R_SSB[3,] ~ SSB[3,]))$coefficients[1,1]
alpha_ss_mod <- exp(c(alpha1, alpha2, alpha3))


# - Climate naive
ss_mod_ricker <- Rceattle::fit_mod(data_list = combined_data,
                                   inits = ss_mod$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 0, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 0, # Single species mode
                                   verbose = 1,
                                   phase = "default",
                                   recFun = build_srr(srr_fun = 0,
                                                      srr_pred_fun = 4,
                                                      proj_mean_rec = FALSE,
                                                      srr_est_mode = 0,
                                                      srr_prior_mean = alpha_ss_mod,
                                                      srr_prior_sd = 0.2,
                                                      Bmsy_lim = apply(ss_mod$quantities$biomassSSB, 1, max)
                                   ),
                                   initMode = 1)

# - SSP126
ss_mod_ricker_ssp126 <- Rceattle::fit_mod(data_list = ssp_dat_126,
                                          inits = ss_mod_ricker$estimated_params, # Initial parameters = 0
                                          file = NULL, # Don't save
                                          estimateMode = 0, # Estimate
                                          random_rec = FALSE, # No random recruitment
                                          msmMode = 0, # Single species mode
                                          verbose = 1,
                                          phase = NULL,
                                          recFun = build_srr(srr_fun = 0,
                                                             srr_pred_fun = 5,
                                                             srr_env_indices = c(2,3,4),
                                                             proj_mean_rec = FALSE,
                                                             srr_est_mode = 0,
                                                             srr_prior_mean = alpha_ss_mod,
                                                             srr_prior_sd = 0.2,
                                                             Bmsy_lim = apply(ss_mod$quantities$biomassSSB, 1, max)
                                          ),
                                          initMode = 1)

# - SSP245
ss_mod_ricker_ssp245 <- Rceattle::fit_mod(data_list = ssp_dat_245,
                                          inits = ss_mod_ricker$estimated_params, # Initial parameters = 0
                                          file = NULL, # Don't save
                                          estimateMode = 0, # Estimate
                                          random_rec = FALSE, # No random recruitment
                                          msmMode = 0, # Single species mode
                                          verbose = 1,
                                          phase = NULL,
                                          recFun = build_srr(srr_fun = 0,
                                                             srr_pred_fun = 5,
                                                             srr_env_indices = c(2,3,4),
                                                             proj_mean_rec = FALSE,
                                                             srr_est_mode = 0,
                                                             srr_prior_mean = alpha_ss_mod,
                                                             srr_prior_sd = 0.2,
                                                             Bmsy_lim = apply(ss_mod$quantities$biomassSSB, 1, max)
                                          ),
                                          initMode = 1)

# - SSP585
ss_mod_ricker_ssp585 <- Rceattle::fit_mod(data_list = ssp_dat_585,
                                          inits = ss_mod_ricker$estimated_params, # Initial parameters = 0
                                          file = NULL, # Don't save
                                          estimateMode = 0, # Estimate
                                          random_rec = FALSE, # No random recruitment
                                          msmMode = 0, # Single species mode
                                          verbose = 1,
                                          phase = NULL,
                                          recFun = build_srr(srr_fun = 0,
                                                             srr_pred_fun = 5,
                                                             srr_env_indices = c(2,3,4),
                                                             proj_mean_rec = FALSE,
                                                             srr_est_mode = 0,
                                                             srr_prior_mean = alpha_ss_mod,
                                                             srr_prior_sd = 0.2,
                                                             Bmsy_lim = apply(ss_mod$quantities$biomassSSB, 1, max)
                                          ),
                                          initMode = 1)

# OM 2) Single-species estimated M ----
# * Density-independent recruitment ----
# - Climate naive
ss_mod_M <- Rceattle::fit_mod(data_list = combined_data,
                              inits = mod_list_all[[2]]$estimated_params, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 0, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              verbose = 1,
                              phase = "default",
                              M1Fun = build_M1(M1_model = c(1,2,1),
                                               updateM1 = FALSE,
                                               M1_use_prior = FALSE,
                                               M2_use_prior = FALSE),
                              initMode = 1)

# -- SSP126
ss_mod_M_ssp126 <- Rceattle::fit_mod(data_list = ssp_dat_126,
                                     inits = ss_mod_M$estimated_params, # Initial parameters = 0
                                     file = NULL, # Don't save
                                     estimateMode = 0, # Estimate
                                     random_rec = FALSE, # No random recruitment
                                     recFun = build_srr(srr_fun = 1,
                                                        srr_env_indices = c(2,3,4)),
                                     msmMode = 0, # Single species mode
                                     verbose = 1,
                                     phase = "default",
                                     M1Fun = build_M1(M1_model = c(1,2,1),
                                                      M1_use_prior = FALSE,
                                                      M2_use_prior = FALSE),
                                     initMode = 1)

# -- SSP245
ss_mod_M_ssp245 <- Rceattle::fit_mod(data_list = ssp_dat_245,
                                     inits = ss_mod_M$estimated_params, # Initial parameters = 0
                                     file = NULL, # Don't save
                                     estimateMode = 0, # Estimate
                                     random_rec = FALSE, # No random recruitment
                                     recFun = build_srr(srr_fun = 1,
                                                        srr_env_indices = c(2,3,4)),
                                     msmMode = 0, # Single species mode
                                     verbose = 1,
                                     phase = NULL,
                                     M1Fun = build_M1(M1_model = c(1,2,1),
                                                      M1_use_prior = FALSE,
                                                      M2_use_prior = FALSE),
                                     initMode = 1)

# -- SSP585
ss_mod_M_ssp585 <- Rceattle::fit_mod(data_list = ssp_dat_585,
                                     inits = ss_mod_M$estimated_params, # Initial parameters = 0
                                     file = NULL, # Don't save
                                     estimateMode = 0, # Estimate
                                     random_rec = FALSE, # No random recruitment
                                     recFun = build_srr(srr_fun = 1,
                                                        srr_env_indices = c(2,3,4)),
                                     msmMode = 0, # Single species mode
                                     verbose = 1,
                                     phase = NULL,
                                     M1Fun = build_M1(M1_model = c(1,2,1),
                                                      M1_use_prior = FALSE,
                                                      M2_use_prior = FALSE),
                                     initMode = 1)


# * Ricker recruitment ----
# - Find fixed alpha
SSB <- ss_mod_M$quantities$biomassSSB[,1:44]
R <- ss_mod_M$quantities$R[,1:44]
R_SSB <- log(R/SSB)

# R = a*SSB*e^(-B*SSB)
# R/SSB = a*e^(-B*SSB)
# log(R/SSB) = log(a) + -B*SSB
alpha1 <- summary(lm(R_SSB[1,] ~ SSB[1,]))$coefficients[1,1]
alpha2 <- summary(lm(R_SSB[2,] ~ SSB[2,]))$coefficients[1,1]
alpha3 <- summary(lm(R_SSB[3,] ~ SSB[3,]))$coefficients[1,1]
alpha_ss_mod_M <- exp(c(alpha1, alpha2, alpha3))


# - Climate naive
ss_mod_M_ricker <- Rceattle::fit_mod(data_list = combined_data,
                                     inits = ss_mod_M$estimated_params, # Initial parameters = 0
                                     file = NULL, # Don't save
                                     estimateMode = 0, # Estimate
                                     random_rec = FALSE, # No random recruitment
                                     msmMode = 0, # Single species mode
                                     verbose = 1,
                                     phase = NULL,
                                     M1Fun = build_M1(M1_model = c(1,2,1),
                                                      M1_use_prior = FALSE,
                                                      M2_use_prior = FALSE),
                                     recFun = build_srr(srr_fun = 0,
                                                        srr_pred_fun = 4,
                                                        proj_mean_rec = FALSE,
                                                        srr_est_mode = 1,
                                                        srr_prior_mean = alpha_ss_mod_M,
                                                        srr_prior_sd = 0.2,
                                                        Bmsy_lim = apply(ss_mod_M$quantities$biomassSSB, 1, max)
                                     ),
                                     initMode = 1)

# - SSP126
ss_mod_M_ricker_ssp126 <- Rceattle::fit_mod(data_list = ssp_dat_126,
                                            inits = ss_mod_M_ricker$estimated_params, # Initial parameters = 0
                                            file = NULL, # Don't save
                                            estimateMode = 0, # Estimate
                                            random_rec = FALSE, # No random recruitment
                                            msmMode = 0, # Single species mode
                                            verbose = 1,
                                            phase = NULL,
                                            M1Fun = build_M1(M1_model = c(1,2,1),
                                                             M1_use_prior = FALSE,
                                                             M2_use_prior = FALSE),
                                            recFun = build_srr(srr_fun = 0,
                                                               srr_pred_fun = 5,
                                                               srr_env_indices = c(2,3,4),
                                                               proj_mean_rec = FALSE,
                                                               srr_est_mode = 1,
                                                               srr_prior_mean = alpha_ss_mod_M,
                                                               srr_prior_sd = 0.2,
                                                               Bmsy_lim = apply(ss_mod_M$quantities$biomassSSB, 1, max)
                                            ),
                                            initMode = 1)

# - SSP245
ss_mod_M_ricker_ssp245 <- Rceattle::fit_mod(data_list = ssp_dat_245,
                                            inits = ss_mod_M_ricker$estimated_params, # Initial parameters = 0
                                            file = NULL, # Don't save
                                            estimateMode = 0, # Estimate
                                            random_rec = FALSE, # No random recruitment
                                            msmMode = 0, # Single species mode
                                            verbose = 1,
                                            phase = NULL,
                                            M1Fun = build_M1(M1_model = c(1,2,1),
                                                             M1_use_prior = FALSE,
                                                             M2_use_prior = FALSE),
                                            recFun = build_srr(srr_fun = 0,
                                                               srr_pred_fun = 5,
                                                               srr_env_indices = c(2,3,4),
                                                               proj_mean_rec = FALSE,
                                                               srr_est_mode = 1,
                                                               srr_prior_mean = alpha_ss_mod_M,
                                                               srr_prior_sd = 0.2,
                                                               Bmsy_lim = apply(ss_mod_M$quantities$biomassSSB, 1, max)
                                            ),
                                            initMode = 1)

# - SSP585
ss_mod_M_ricker_ssp585 <- Rceattle::fit_mod(data_list = ssp_dat_585,
                                            inits = ss_mod_M_ricker$estimated_params, # Initial parameters = 0
                                            file = NULL, # Don't save
                                            estimateMode = 0, # Estimate
                                            random_rec = FALSE, # No random recruitment
                                            msmMode = 0, # Single species mode
                                            verbose = 1,
                                            phase = NULL,
                                            M1Fun = build_M1(M1_model = c(1,2,1),
                                                             M1_use_prior = FALSE,
                                                             M2_use_prior = FALSE),
                                            recFun = build_srr(srr_fun = 0,
                                                               srr_pred_fun = 5,
                                                               srr_env_indices = c(2,3,4),
                                                               proj_mean_rec = FALSE,
                                                               srr_est_mode = 1,
                                                               srr_prior_mean = alpha_ss_mod_M,
                                                               srr_prior_sd = 0.2,
                                                               Bmsy_lim = apply(ss_mod_M$quantities$biomassSSB, 1, max)
                                            ),
                                            initMode = 1)



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

# -- SSP245
ms_mod_ssp245 <- Rceattle::fit_mod(data_list = ssp_dat_245,
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

# -- SSP585
ms_mod_ssp585 <- Rceattle::fit_mod(data_list = ssp_dat_585,
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
# - Find fixed alpha
SSB <- ms_mod$quantities$biomassSSB[,1:44]
R <- ms_mod$quantities$R[,1:44]
R_SSB <- log(R/SSB)

# R = a*SSB*e^(-B*SSB)
# R/SSB = a*e^(-B*SSB)
# log(R/SSB) = log(a) + -B*SSB
alpha1 <- summary(lm(R_SSB[1,] ~ SSB[1,]))$coefficients[1,1]
alpha2 <- summary(lm(R_SSB[2,] ~ SSB[2,]))$coefficients[1,1]
alpha3 <- summary(lm(R_SSB[3,] ~ SSB[3,]))$coefficients[1,1]
alpha_ms_mod <- exp(c(alpha1, alpha2, alpha3))

# - Climate naive
# ms_mod$estimated_params$rec_dev[2,45:124] <- -10
ms_mod_ricker2 <- Rceattle::fit_mod(data_list = combined_data,
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
                                                      srr_prior_mean = alpha_ms_mod,
                                                      srr_prior_sd = 0.2,
                                                      Bmsy_lim = apply(ms_mod$quantities$biomassSSB, 1, max)
                                   ),
                                   initMode = 1)
plot_biomass(list(ms_mod, ms_mod_ricker, ms_mod_ricker2), incl_proj = T)

# - SSP126
ms_mod_ricker_ssp126 <- Rceattle::fit_mod(data_list = ssp_dat_126,
                                          inits = ms_mod_ricker2$estimated_params, # Initial parameters = 0
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
                                                             srr_prior_mean = alpha_ms_mod,
                                                             srr_prior_sd = 0.2,
                                                             Bmsy_lim = apply(ms_mod$quantities$biomassSSB, 1, max)
                                          ),
                                          initMode = 1)

# - SSP245
ms_mod_ricker_ssp245 <- Rceattle::fit_mod(data_list = ssp_dat_245,
                                          inits = ms_mod_ricker2$estimated_params, # Initial parameters = 0
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
                                                             srr_prior_mean = alpha_ms_mod,
                                                             srr_prior_sd = 0.2,
                                                             Bmsy_lim = apply(ms_mod$quantities$biomassSSB, 1, max)
                                          ),
                                          initMode = 1)

# - SSP585
ms_mod_ricker_ssp585<- Rceattle::fit_mod(data_list = ssp_dat_585,
                                         inits = ms_mod_ricker2$estimated_params, # Initial parameters = 0
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
                                                            srr_prior_mean = alpha_ms_mod,
                                                            srr_prior_sd = 0.2,
                                                            Bmsy_lim = apply(ms_mod$quantities$biomassSSB, 1, max)
                                         ),
                                         initMode = 1)




## Adjust f prop ----
om_list <- list(
  # ss_mod, ss_mod_ssp126, ss_mod_ssp245, ss_mod_ssp585,
  # ss_mod_ricker, ss_mod_ricker_ssp126, ss_mod_ricker_ssp245, ss_mod_ricker_ssp585,
  # ss_mod_M, ss_mod_M_ssp126, ss_mod_M_ssp245, ss_mod_M_ssp585,
  # ss_mod_M_ricker, ss_mod_M_ricker_ssp126, ss_mod_M_ricker_ssp245, ss_mod_M_ricker_ssp585,
  ms_mod, ms_mod_ssp126, ms_mod_ssp245, ms_mod_ssp585,
  ms_mod_ricker, ms_mod_ricker_ssp126, ms_mod_ricker_ssp245, ms_mod_ricker_ssp585
)



for(i in 1:length(om_list)){
  avg_F <- (exp(om_list[[i]]$estimated_params$ln_mean_F+om_list[[i]]$estimated_params$F_dev)) # Average F from last 2 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
  f_ratio <- avg_F[14:16]
  f_ratio <- f_ratio/sum(f_ratio)
  
  # Adjust future F proportion to each fleet
  om_list[[i]]$data_list$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio, 0, 0)
  om_list[[i]]$estimated_params$proj_F_prop <- om_list[[i]]$data_list$fleet_control$proj_F_prop
}




om_names <- paste0(c(
  # "ss_mod", "ss_mod_ssp126", "ss_mod_ssp245", "ss_mod_ssp585",
  # "ss_mod_ricker", "ss_mod_ricker_ssp126", "ss_mod_ricker_ssp245", "ss_mod_ricker_ssp585",
  # "ss_mod_M", "ss_mod_M_ssp126", "ss_mod_M_ssp245", "ss_mod_M_ssp585",
  # "ss_mod_M_ricker", "ss_mod_M_ricker_ssp126", "ss_mod_M_ricker_ssp245", "ss_mod_M_ricker_ssp585",
  "ms_mod", "ms_mod_ssp126", "ms_mod_ssp245", "ms_mod_ssp585",
  "ms_mod_ricker", "ms_mod_ricker_ssp126", "ms_mod_ricker_ssp245", "ms_mod_ricker_ssp585"
), "_OM")


## Plots ----
plot_biomass(om_list[1:4], incl_proj = TRUE)
plot_biomass(om_list[5:8], incl_proj = TRUE)
plot_biomass(om_list[9:12], incl_proj = TRUE)
plot_biomass(om_list[13:16], incl_proj = TRUE)
plot_biomass(om_list[17:20], incl_proj = TRUE)
plot_biomass(om_list[21:24], incl_proj = TRUE)

plot_recruitment(om_list[1:4], incl_proj = TRUE)
plot_recruitment(om_list[5:8], incl_proj = TRUE)
plot_recruitment(om_list[9:12], incl_proj = TRUE)
plot_recruitment(om_list[13:16], incl_proj = TRUE)
plot_recruitment(om_list[17:20], incl_proj = TRUE)
plot_recruitment(om_list[21:24], incl_proj = TRUE)

plot_stock_recruit(om_list[5:8])
plot_stock_recruit(om_list[13:16])
plot_stock_recruit(om_list[21:24])

