pacman::p_load(Rceattle, readxl, dplyr, tidyr, writexl)
load("Models/GOA_23_1_1_mod_list.RData")
combined_data <- read_data(file = "Data/GOA_23_1_1_data_1977_2023_edited.xlsx")
combined_data$projyr <- 2100
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
summer_bt_data <- read.csv("Data/goa_temp_610_to_630_summer_300M.csv") %>%
  filter(depthclass == "Bottom", hind == "yes")  %>%
  mutate(varname = "summer bt") %>%
  rename(value = mean_value_dc_610_to_630)

fall_sst_data <- read.csv("Data/goa_temp_610_to_630_winter_300M.csv") %>%
  filter(depthclass == "Surface", hind == "yes")  %>%
  mutate(varname = "winter sst") %>%
  rename(value = mean_value_dc_610_to_630)

zoo_data <- read.csv("Data/goa_large_zoo_610_to_630_fall_300M.csv") %>%
  filter(depthclass == "Surface", hind == "yes") %>%
  mutate(varname = "mzl") %>%
  rename(value = mean_value_dc_610_to_630)

climate_data <- rbind(summer_bt_data, fall_sst_data, zoo_data) %>%
  mutate(value_squared = value^2) %>%
  pivot_wider(names_from = c(simulation), values_from = c(value, value_squared)) %>%
  select(-depthclass, -hind) %>%
  rename(Year = year) %>%
  group_by(varname) %>%
  mutate(value_ssp126z = scale(value_ssp126 ),
         value_ssp245z = scale(value_ssp245 ),
         value_ssp585z = scale(value_ssp585 ),
         value_squared_ssp126z = scale(value_squared_ssp126 ),
         value_squared_ssp245z = scale(value_squared_ssp245 ),
         value_squared_ssp585z = scale(value_squared_ssp585 )
  ) %>%
  ungroup() %>%
  as.data.frame()

# - Add missing years
# -- Fall SST
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


# -- Summer BT
fall_sst_data <- climate_data %>%
  filter(varname == "winter sst") %>%
  select(-varname)

temp_sub <- data.frame(Year = 1977:1979, 
                       value_ssp126 = mean(fall_sst_data$value_ssp126[1:10]), 
                       value_ssp245 = mean(fall_sst_data$value_ssp245[1:10]),
                       value_ssp585 = mean(fall_sst_data$value_ssp585[1:10]), 
                       value_squared_ssp126 = mean(fall_sst_data$value_squared_ssp126[1:10]), 
                       value_squared_ssp245 = mean(fall_sst_data$value_squared_ssp245[1:10]),
                       value_squared_ssp585 = mean(fall_sst_data$value_squared_ssp585[1:10]), 
                       value_ssp126z = 0,
                       value_ssp245z = 0,
                       value_ssp585z = 0,
                       value_squared_ssp126z = 0,
                       value_squared_ssp245z = 0,
                       value_squared_ssp585z = 0)

fall_sst_data <- rbind(temp_sub, fall_sst_data) 
colnames(fall_sst_data) <- c("Year", paste0("SST_", colnames(fall_sst_data)[2:ncol(fall_sst_data)]))


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
climate_data <- fall_sst_data %>% 
  inner_join(zoo_data, by = "Year") %>%
  inner_join(summer_bt_data, by = "Year") %>%
  arrange(Year)


# - add to Rceattle object
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

# * Ricker recruitment ----
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
                                                      srr_est_mode = 1,
                                                      srr_prior_mean = alpha,
                                                      srr_prior_sd = 0.2,
                                                      Bmsy_lim = apply(ss_mod$quantities$biomassSSB, 1, max)
                                   ),
                                   initMode = 1)


## OMs ----
load("Models/GOA_23_1_1_mod_list.RData")
combined_data <- read_data(file = "Data/GOA_23_1_1_data_1977_2023_edited.xlsx")
combined_data$projyr <- 2130


## Ajust inits ----
turn_offs <- c("logH_1", "logH_1a", "logH_1b", "logH_2", "logH_3", "H_4", "log_gam_a", "log_gam_b", "log_phi", "ln_pop_scalar", "sel_coff_dev")
for(i in 1:length(mod_list_all)){
  
  mod_list_all[[i]]$estimated_params[turn_offs] <- NULL
  
  # - Adjust rec-dev dimension
  mod_list_all[[i]]$estimated_params$rec_dev <- cbind(
    mod_list_all[[i]]$estimated_params$rec_dev, matrix(0, nrow = 3, ncol = 80))
  
  mod_list_all[[i]]$estimated_params$beta_rec_pars <- matrix(0, 3, 1)
  
  # - Adjust Fprop for Cod
  avg_F <- (exp(mod_list_all[[i]]$estimated_params$ln_mean_F+mod_list_all[[i]]$estimated_params$F_dev)) # Average F from last 2 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
  f_ratio <- avg_F[14:16]
  f_ratio <- f_ratio/sum(f_ratio)
  
  # Adjust future F proportion to each fleet
  mod_list_all[[i]]$data_list$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio, 0, 0)
  combined_data$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio, 0, 0)
  mod_list_all[[i]]$estimated_params$proj_F_prop <- mod_list_all[[i]]$data_list$fleet_control$proj_F_prop
}

# All EMs have density-independent recruitment ----

# EM 2) Single-species estimated M ----
# - Tier-3
ss_run_M_Tier3 <- Rceattle::fit_mod(data_list = combined_data,
                                    inits = mod_list_all[[2]]$estimated_params, # Initial parameters = 0
                                    file = NULL, # Don't save
                                    estimateMode = 0, # Estimate
                                    random_rec = FALSE, # No random recruitment
                                    msmMode = 0, # Single species mode
                                    verbose = 1,
                                    phase = NULL,
                                    M1Fun = build_M1(M1_model = c(1,2,1),
                                                     M1_use_prior = FALSE,
                                                     M2_use_prior = FALSE),
                                    HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                    DynamicHCR = FALSE, # Dont dynamic reference points
                                                    FsprTarget = 0.4, # F40%
                                                    FsprLimit = 0.35, # F35%
                                                    Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                                                    Alpha = 0.05),
                                    initMode = 1)

# EM 3) Multi-species ----
# -- F that acheives 40% of SB0, where SB0 is derived from projecting all species simultaneously under no fishing
ms_run_fb40 <- Rceattle::fit_mod(data_list = combined_data,
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
                                 HCR = build_hcr(HCR = 3, # Constant F HCR
                                                 DynamicHCR = FALSE, # Use dynamic reference points
                                                 FsprTarget = 0.4), # F that achieves 40% SB0
                                 initMode = 1)

## MSE ----
## Sampling period
sampling_period <- c(2,2,1,2,2,2,2,1,2,2,1,2,2,1,1,1,1,1)
source("D:/GitHub/Rceattle/R/11a-mse_run_parallel.R", echo=TRUE)

## Cap
# 1. Max historical catch for Arrowtooth flounder
# 2. No cap
max_catch <- ss_mod$data_list$fsh_biom %>%
  filter(Year < 2024) %>%
  group_by(Year, Species) %>%
  summarise(Catch = sum(Catch)) %>%
  group_by(Species) %>%
  summarise(MaxCatch = max(Catch))


# Pollock, cod, atf
cap_list <-  max_catch$MaxCatch

mse1a <- mse_run_parallel(om = ss_mod_ricker, em = ms_run_fb40, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, timeout = 30, file = "mse1a", dir = getwd())
mse1b <- mse_run_parallel(om = ss_mod_ricker, em = ms_run_fb40, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, timeout = 30, file = "mse1b", cap = cap_list, dir = getwd())

mse2a <- mse_run_parallel(om = ss_mod, em = ms_run_fb40, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, timeout = 30, file = "mse2a", dir = getwd())
mse2b <- mse_run_parallel(om = ss_mod, em = ms_run_fb40, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, timeout = 30, file = "mse2b", cap = cap_list, dir = getwd())

mse3a <- mse_run_parallel(om = ss_mod, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, timeout = 30, file = "mse3a", dir = getwd())
mse3b <- mse_run_parallel(om = ss_mod, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, timeout = 30, file = "mse3b", cap = cap_list, dir = getwd())

mse4a <- mse_run_parallel(om = ss_mod_ricker, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, timeout = 30, file = "mse4a", dir = getwd())
mse4b <- mse_run_parallel(om = ss_mod_ricker, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, timeout = 30, file = "mse4b", cap = cap_list, dir = getwd())
