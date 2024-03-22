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
summer_bt_data <- read.csv("Data/goa_temp_610_to_630_summer_300M_no_var_delta.csv") %>%
  filter(depthclass == "Bottom", hind == "yes")  %>%
  mutate(varname = "summer bt") %>%
  rename(value = mean_value_dc_610_to_630)

winter_sst_data <- read.csv("Data/goa_temp_610_to_630_winter_300M_no_var_delta.csv") %>%
  filter(depthclass == "Surface", hind == "yes")  %>%
  mutate(varname = "winter sst") %>%
  rename(value = mean_value_dc_610_to_630)

zoo_data <- read.csv("Data/goa_large_zoo_610_to_630_fall_300M_no_var_delta.csv") %>%
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



# * Adjust f prop ----
mod_list_all <- list(ss_mod, ss_mod_M, ms_mod, 
                     ss_mod_ssp126, ss_mod_ssp245, ss_mod_ssp585, 
                     ms_mod_ssp126, ms_mod_ssp245, ms_mod_ssp585)

for(i in 1:length(mod_list_all)){
  avg_F <- (exp(mod_list_all[[i]]$estimated_params$ln_mean_F+mod_list_all[[i]]$estimated_params$F_dev)) # Average F from last 2 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
  f_ratio <- avg_F[14:16]
  f_ratio <- f_ratio/sum(f_ratio)
  
  # Adjust future F proportion to each fleet
  mod_list_all[[i]]$data_list$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio, 0, 0)
  mod_list_all[[i]]$estimated_params$proj_F_prop <- mod_list_all[[i]]$data_list$fleet_control$proj_F_prop
}

ss_mod <- mod_list_all[[1]]
ss_mod_M <- mod_list_all[[2]]
ms_mod <- mod_list_all[[3]]
ss_mod_ssp126 <- mod_list_all[[4]]
ss_mod_ssp245 <- mod_list_all[[5]]
ss_mod_ssp585 <- mod_list_all[[6]]

ms_mod_ssp126 <- mod_list_all[[7]]
ms_mod_ssp245 <- mod_list_all[[8]]
ms_mod_ssp585 <- mod_list_all[[9]]


## Estimate EMs ----
# EM 1) Single-spp fix M ----
# - Tier-3
ss_run_Tier3 <- Rceattle::fit_mod(data_list = combined_data,
                                  inits = mod_list_all[[1]]$estimated_params, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 0, # Estimate
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  verbose = 1,
                                  phase = NULL,
                                  initMode = 1,
                                  HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                  DynamicHCR = FALSE, # Dont dynamic reference points
                                                  FsprTarget = 0.4, # F40%
                                                  FsprLimit = 0.35, # F35%
                                                  Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                                                  Alpha = 0.05))


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


# Plot ----
ss_col <- nmfspalette::nmfs_palette("seagrass")(7)[1:4]
ms_col <- nmfspalette::nmfs_palette("oceans")(7)[1:4]
model_names <- c("Climate naive", "SSP-126", "SSP-245", "SSP-585")

hcr_list <- list(ss_mod_tier3, ss_mod_M_tier3)

om_list_ss <- list(ss_mod, ss_mod_ssp126, ss_mod_ssp245, ss_mod_ssp585)
om_list_ms <- list(ms_mod, ms_mod_ssp126, ms_mod_ssp245, ms_mod_ssp585)


plot_biomass(c(om_list_ss, om_list_ms), incl_proj = TRUE, 
             model_names = paste0("SS ", model_names), 
             line_col = c(ss_col, ms_col),
             file = "Results/Projections/proj")

plot_ssb(c(om_list_ms, om_list_ss), incl_proj = TRUE, 
         model_names = paste0("MS ", model_names), 
         line_col = c(ms_col, ss_col),
         file = "Results/Projections/proj")

plot_b_eaten(om_list_ms, incl_proj = TRUE, 
             model_names = paste0("MS ", model_names), 
             line_col = ms_col,
             file = "Results/Projections/proj")

plot_recruitment(c(om_list_ss, om_list_ms), incl_proj = TRUE, 
                 line_col = c(ss_col, ms_col), 
                 file = "Results/Projections/proj")


## Save ----
# - Model
proj_list_all <- c(hcr_list, om_list_ss, om_list_ms)
save(proj_list_all, file = "Models/GOA_23_mod_projections.RData")



load(file = "Models/GOA_23_mod_projections.RData")


ss_mod_tier3 <- proj_list_all[[1]]
ss_mod_M_tier3 <- proj_list_all[[2]]
ss_mod <- proj_list_all[[3]]
ss_mod_ssp126 <- proj_list_all[[4]]
ss_mod_ssp245 <- proj_list_all[[5]]
ss_mod_ssp585 <- proj_list_all[[6]]
ms_mod <- proj_list_all[[7]]
ms_mod_ssp126 <- proj_list_all[[8]]
ms_mod_ssp245 <- proj_list_all[[9]]
ms_mod_ssp585 <- proj_list_all[[10]]

hcr_list <- list(ss_mod_tier3, ss_mod_M_tier3)

om_list_ss <- list(ss_mod, ss_mod_ssp126, ss_mod_ssp245, ss_mod_ssp585)
om_list_ms <- list(ms_mod, ms_mod_ssp126, ms_mod_ssp245, ms_mod_ssp585)

## MSE ----
# * OMs ----
om_list <- c(om_list_ss, om_list_ms)
model_names <- c("Climate naive", "SSP-126", "SSP-245", "SSP-585")
om_names <- paste0(rep(c("SS-", "MS-"), each = 4), model_names)

# * Test env significance
aic_vec <- sapply(om_list, function(x) x$opt$AIC)
write.csv(data.frame(model = om_names, AIC = aic_vec), file = "proj_aic.csv")


# * Management strategies ----
# 1. Single-species fix M
# 2. Single-species estimate M
# Tier 3 HCR
sampling_period <- c(2,2,1,2,2,2,2,1,2,2,1,2,2,1,1,1,1,1)
em_hcr_list <- hcr_list
em_hcr_names <- c("SS_fixM_Tier3_EM", "SS_estM_Tier3_EM")


# * Run MSE ----
source("R/Functions/Run_2023_projection_MSE_function.R", echo=TRUE)
run_mse(system = "GOA1977", om_list = om_list, om_names = om_names, em_hcr_list = em_hcr_list, em_hcr_names = em_hcr_names, sampling_period = sampling_period, nsim = 50, cap = c(800000, 800000, 800000), catch_mult = c(1, 0.17, 1))


## MSE Summary ----
# * Load MSE
model_names <- c("Climate naive", "SSP-126", "SSP-245", "SSP-585")
om_names <- paste0(rep(c("SS-", "MS-"), each = 4), model_names)
em_hcr_names <- c("SS_fixM_Tier3_EM", "SS_estM_Tier3_EM")

mse_list <- list()
ind <- 1
for(om in 1:length(om_names)){
  for(em in 1:length(em_hcr_names)){
    mse_list[[ind]] <- load_mse(dir = paste0("Runs/GOA1977/", om_names[om],"/", em_hcr_names[em]), file = NULL)
    names(mse_list)[ind] <- paste0(om_names[om]," OM - ", em_hcr_names[em])
    ind = ind+1
  }
}

# * Get catch ----
OMs <- list()
TEMs <- list()
catch_list <- list()

for(i in 1:length(mse_list)){
  OMs[[i]] <- list()
  TEMs[[i]] <- list()
  catch_list[[i]] <- list()
  
  # -- Rename
  names(OMs)[i] <- names(mse_list)[i]
  names(TEMs)[i] <- names(mse_list)[i]
  names(catch_list)[i] <- names(mse_list)[i]
  
  for(j in 1:length(mse_list[[i]])){
    OMs[[i]][[j]] <- mse_list[[i]][[j]]$OM
    OMs[[i]][[j]]$MSE = names(mse_list)[i]
    OMs[[i]][[j]]$SIM = j
    
    TEMs[[i]][[j]] <- mse_list[[i]][[j]]$EM[[length(mse_list[[i]][[j]]$EM)]]
    
    
    # -- Get catch
    catch_list[[i]][[j]] <- OMs[[i]][[j]]$data_list$fsh_biom
    OMs[[i]][[j]]$data_list$fsh_biom$MSE = names(mse_list)[i]
    OMs[[i]][[j]]$data_list$fsh_biom$SIM = j
    
    catch_list[[i]][[j]]$Catch[which(catch_list[[i]][[j]]$Year > 2023)] <- OMs[[i]][[j]]$quantities$fsh_bio_hat[which(catch_list[[i]][[j]]$Year > 2023)]
    
    catch_list[[i]][[j]] <- catch_list[[i]][[j]] %>%
      select(-Fleet_code, - Species, - Month, - Selectivity_block, -Log_sd) %>%
      mutate(MSE = names(mse_list)[i],
             SIM = j) %>%
      pivot_wider(names_from = Fleet_name, values_from = c(Catch)) %>%
      as.data.frame() %>%
      relocate(MSE, .before = Year)
  }
  
  # - Combine
  catch_list[[i]] <- do.call("rbind", catch_list[[i]])
}
writexl::write_xlsx(catch_list, path = "Results/Projections/MSE_catch_timeseries.xlsx")

# * Plot catch series ----
ss_col <- nmfspalette::nmfs_palette("seagrass")(7)[1:4]
ms_col <- nmfspalette::nmfs_palette("oceans")(7)[1:4]

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
## END


source("plot catch.R")

## FIX-M 
# - Get max catch
fixm_catch_mods <- c(OMs[[1]], OMs[[9]], OMs[[3]], OMs[[11]], OMs[[5]], OMs[[13]], OMs[[7]], OMs[[15]])
fixm_catch_mods <- do.call("rbind", lapply(fixm_catch_mods, function(x) x$data_list$fsh_biom))
max_catch <- fixm_catch_mods %>%
  group_by(Fleet_name, Fleet_code) %>%
  summarise(MaxCatch = max(Catch)) %>%
  arrange(Fleet_code)

# - Climate naive
plot_catch(c(OMs[[1]], OMs[[9]]), line_col = (c(rep(ss_col[3], 10), rep(ms_col[3], 10))), top_adj = 1.05, file = "Results/Projections/Catch/FixM_climate_naive", lwd = 0.5, width = 10, ymax = max_catch$MaxCatch)

# - SSP126
plot_catch(c(OMs[[3]], OMs[[11]]), line_col = c(rep(ss_col[3], 10), rep(ms_col[3], 10)), top_adj = 1.05, file = "Results/Projections/Catch/FixM_ssp126", lwd = 0.5, width = 10, ymax = max_catch$MaxCatch)

# - SSP245
plot_catch(c(OMs[[5]], OMs[[13]]), line_col = c(rep(ss_col[3], 10), rep(ms_col[3], 10)), top_adj = 1.05, file = "Results/Projections/Catch/FixM_ssp245", lwd = 0.5, width = 10, ymax = max_catch$MaxCatch)

# - SPP585
plot_catch(c(OMs[[7]], OMs[[15]]), line_col = (c(rep(ss_col[3], 10), rep(ms_col[3], 10))), top_adj = 1.05, file = "Results/Projections/Catch/FixM_ssp585", lwd = 0.5, width = 10, ymax = max_catch$MaxCatch)


mean_catch <- fixm_catch_mods %>%
  filter(Year > 2090) %>%
  group_by(Fleet_name, Fleet_code, MSE, Year) %>%
  summarise(Catch = mean(Catch)) %>%
  arrange(Fleet_code, MSE, Year)

write.csv(mean_catch, "mean_catch.csv")



fixm_catch_mods <- c(OMs[[1]], OMs[[9]], OMs[[3]], OMs[[11]], OMs[[5]], OMs[[13]], OMs[[7]], OMs[[15]])
mse_names <- sapply(fixm_catch_mods, function(x) x$MSE)
sim_names <- sapply(fixm_catch_mods, function(x) x$SIM)

recs <- lapply(fixm_catch_mods, function(x) x$quantities$R[1,])
for(i in 1:length(recs)){
  recs[[i]] <- data.frame(Year = names(recs[[i]]), R = recs[[i]], MSE = mse_names[i], SIM = sim_names[i])
}

recs <- do.call("rbind", recs)

mean_rec <- recs %>%
  filter(Year > 2090) %>%
  group_by(MSE, Year) %>%
  summarise(MeanR = mean(R)) %>%
  arrange(Year, MSE)

write.csv(mean_rec, "mean_rec.csv")




## Est-M 
# - Climate naive
plot_catch(c(OMs[[2]], OMs[[10]]), line_col = (c(rep(ss_col[3], 10), rep(ms_col[3], 10))), top_adj = 1.05, file = "Results/Projections/Catch/EstM_climate_naive", lwd = 0.5, width = 10)

# - SSP126
plot_catch(c(OMs[[4]], OMs[[12]]), line_col = c(rep(ss_col[3], 10), rep(ms_col[3], 10)), top_adj = 1.05, file = "Results/Projections/Catch/EstM_ssp126", lwd = 0.5, width = 10)

# - SSP245
plot_catch(c(OMs[[6]], OMs[[14]]), line_col = c(rep(ss_col[3], 10), rep(ms_col[3], 10)), top_adj = 1.05, file = "Results/Projections/Catch/EstM_ssp245", lwd = 0.5, width = 10)

# - SPP585
plot_catch(c(OMs[[8]], OMs[[16]]), line_col = (c(rep(ss_col[3], 10), rep(ms_col[3], 10))), top_adj = 1.05, file = "Results/Projections/Catch/EstM_ssp585", lwd = 0.5, width = 10)


rm(mse_list);gc()
# OMs <- lapply(mse_list, function(x) lapply(x, function(y) y$OM))


# * Plot SSB ----
## FIX-M 
# - Climate naive
plot_ssb(c(OMs[[1]], OMs[[9]]), line_col = (c(rep(ss_col[3], 10), rep(ms_col[3], 10))), file = "Results/Projections/SSB/FixM_climate_naive", lwd = 1, ymax = c(3, 1.2, 0.5))

# - SSP126
plot_ssb(c(OMs[[3]], OMs[[11]]), line_col = c(rep(ss_col[3], 10), rep(ms_col[3], 10)), file = "Results/Projections/SSB/FixM_ssp126", lwd = 1, ymax = c(3, 1.2, 0.5))

# - SSP245
plot_ssb(c(OMs[[5]], OMs[[13]]), line_col = c(rep(ss_col[3], 10), rep(ms_col[3], 10)), file = "Results/Projections/SSB/FixM_ssp245", lwd = 1, ymax = c(3, 1.2, 0.5))

# - SPP585
plot_ssb(c(OMs[[7]], OMs[[15]]), line_col = (c(rep(ss_col[3], 10), rep(ms_col[3], 10))), file = "Results/Projections/SSB/FixM_ssp585", lwd = 1, ymax = c(3, 1.2, 0.5))


## Est-M 
# - Climate naive
plot_ssb(c(OMs[[2]], OMs[[10]]), line_col = (c(rep(ms_col[3], 10), rep(ss_col[3], 10))), file = "Results/Projections/SSB/EstM_climate_naive", lwd = 1, ymax = c(4, 1.2, 0.5))

# - SSP126
plot_ssb(c(OMs[[4]], OMs[[12]]), line_col = c(rep(ss_col[3], 10), rep(ms_col[3], 10)), file = "Results/Projections/SSB/EstM_ssp126", lwd = 1, ymax = c(4, 1.2, 0.5))

# - SSP245
plot_ssb(c(OMs[[6]], OMs[[14]]), line_col = c(rep(ss_col[3], 10), rep(ms_col[3], 10)), file = "Results/Projections/SSB/EstM_ssp245", lwd = 1, ymax = c(4, 1.2, 0.5))

# - SPP585
plot_ssb(c(OMs[[8]], OMs[[16]]), line_col = (c(rep(ss_col[3], 10), rep(ms_col[3], 10))), file = "Results/Projections/SSB/EstM_ssp585", lwd = 1, ymax = c(4, 1.2, 0.5))



# * Plot R ----
## FIX-M 
# - Climate naive
plot_recruitment(c(OMs[[1]], OMs[[9]]), line_col = (c(rep(ms_col[3], 10), rep(ss_col[3], 10))), file = "Results/Projections/Recruitment/FixM_climate_naive", lwd = 1, ymax = c(80, 8, 0.7))

# - SSP126
plot_recruitment(c(OMs[[3]], OMs[[11]]), line_col = c(rep(ss_col[3], 10), rep(ms_col[3], 10)), file = "Results/Projections/Recruitment/FixM_ssp126", lwd = 1, ymax = c(80, 8, 0.7))

# - SSP245
plot_recruitment(c(OMs[[5]], OMs[[13]]), line_col = c(rep(ss_col[3], 10), rep(ms_col[3], 10)), file = "Results/Projections/Recruitment/FixM_ssp245", lwd = 1, ymax = c(80, 8, 0.7))

# - SPP585
plot_recruitment(c(OMs[[7]], OMs[[15]]), line_col = (c(rep(ss_col[3], 10), rep(ms_col[3], 10))), file = "Results/Projections/Recruitment/FixM_ssp585", lwd = 1, ymax = c(80, 8, 0.7))


## Est-M 
# - Climate naive
plot_recruitment(c(OMs[[2]], OMs[[10]]), line_col = (c(rep(ms_col[3], 10), rep(ss_col[3], 10))), file = "Results/Projections/Recruitment/EstM_climate_naive", lwd = 1, ymax = c(80, 8, 0.7))

# - SSP126
plot_recruitment(c(OMs[[4]], OMs[[12]]), line_col = c(rep(ss_col[3], 10), rep(ms_col[3], 10)), file = "Results/Projections/Recruitment/EstM_ssp126", lwd = 1, ymax = c(80, 8, 0.7))

# - SSP245
plot_recruitment(c(OMs[[6]], OMs[[14]]), line_col = c(rep(ss_col[3], 10), rep(ms_col[3], 10)), file = "Results/Projections/Recruitment/EstM_ssp245", lwd = 1, ymax = c(80, 8, 0.7))

# - SPP585
plot_recruitment(c(OMs[[8]], OMs[[16]]), line_col = (c(rep(ss_col[3], 10), rep(ms_col[3], 10))), file = "Results/Projections/Recruitment/EstM_ssp585", lwd = 1, ymax = c(80, 8, 0.7))