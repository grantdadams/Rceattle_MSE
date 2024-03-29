## Load packages and data ----
pacman::p_load(Rceattle, readxl, dplyr, tidyr, writexl)
combined_data <- read_data(file = "Data/GOA_18_5_1_data_1977-2018_no_halibut.xlsx")
combined_data$projyr <- 2130


## Estimate EMs with no HCR ----
# All EMs have density-independent recruitment ----

# * EM 1) Single-spp fix M ----
ss_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            verbose = 1,
                            phase = "default")

# * EM 2) Single-species estimated M ----
ss_mod_M <- Rceattle::fit_mod(data_list = combined_data,
                              inits = ss_mod$estimated_params, # Initial parameters = 0
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

# * EM 3) Multi-species ----
ms_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = ss_mod_M$estimated_params, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # Multi species mode
                            verbose = 1,
                            niter = 3,
                            suit_meanyr = 2018,
                            phase = "default",
                            M1Fun = build_M1(M1_model = c(1,2,1),
                                             M1_use_prior = FALSE,
                                             M2_use_prior = FALSE),
                            initMode = 1)


## Adjust Fprop for Cod ----
mod_list_all <- list(ss_mod, ss_mod_M, ms_mod)
for(i in 1:length(mod_list_all)){
  
  # - Adjust Fprop for Cod
  avg_F <- (exp(mod_list_all[[i]]$estimated_params$ln_mean_F+mod_list_all[[i]]$estimated_params$F_dev)) # Average F from last 2 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
  f_ratio <- avg_F[14:16]
  f_ratio <- f_ratio/sum(f_ratio)
  
  # Adjust future F proportion to each fleet
  mod_list_all[[i]]$data_list$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio)
  combined_data$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio)
  mod_list_all[[i]]$estimated_params$proj_F_prop <- mod_list_all[[i]]$data_list$fleet_control$proj_F_prop
}


## Estimate EMs w/ HCR ----
# * EM 1) Single-spp fix M ----
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

# - Dynamic Tier-3
ss_run_dynamicTier3 <- Rceattle::fit_mod(data_list = combined_data,
                                         inits = mod_list_all[[1]]$estimated_params, # Initial parameters = 0
                                         file = NULL, # Don't save
                                         estimateMode = 0, # Estimate
                                         random_rec = FALSE, # No random recruitment
                                         msmMode = 0, # Single species mode
                                         verbose = 1,
                                         phase = NULL,
                                         initMode = 1,
                                         HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                         DynamicHCR = TRUE, # dynamic reference points
                                                         FsprTarget = 0.4, # F40%
                                                         FsprLimit = 0.35, # F35%
                                                         Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                                                         Alpha = 0.05))


# * EM 2) Single-species estimated M ----
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
# - Dynamic Tier-3
ss_run_M_dynamicTier3 <- Rceattle::fit_mod(data_list = combined_data,
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
                                                           DynamicHCR = TRUE, # dynamic reference points
                                                           FsprTarget = 0.4, # F40%
                                                           FsprLimit = 0.35, # F35%
                                                           Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                                                           Alpha = 0.05),
                                           initMode = 1)



# * EM 3) Multi-species ----
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


# -- F that acheives 40% of SB0, where SB0 is derived from first projecting arrowtooth and cod under no fishing, then projecting pollock under no fishing and cod/arrowtooth at SB40.
ms_run_fb40iter <- Rceattle::fit_mod(data_list = combined_data,
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
                                                     FsprTarget = 0.4,
                                                     HCRorder = c(1,2,1)), # F that achieves 40% SB0
                                     initMode = 1)


# -- Multi-species CMSY
ms_run_cmsy <- Rceattle::fit_mod(data_list = combined_data,
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
                                 HCR = build_hcr(HCR = 1,
                                                 Plimit = 0.01),
                                 initMode = 1)

# -- Multi-species CMSY, constrained so that species don't fall below 20% SB0
# -- SB0 is derived from projecting all species simultaneously under no fishing
ms_run_concmsy <- Rceattle::fit_mod(data_list = combined_data,
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
                                    HCR = build_hcr(HCR = 1,
                                                    Plimit = 0.2),
                                    initMode = 1)



em_list <- list(ss_run_Tier3, ss_run_dynamicTier3, 
                ss_run_M_Tier3, ss_run_M_dynamicTier3, 
                ms_run_fb40, ms_run_fb40iter, ms_run_cmsy, ms_run_concmsy)
em_names <- paste0(c("ss_run_Tier3", "ss_run_dynamicTier3", 
                     "ss_run_M_Tier3", "ss_run_M_dynamicTier3", 
                     "ms_run_fb40", "ms_run_fb40iter", "ms_run_cmsy", "ms_run_concmsy"), "_EM")
