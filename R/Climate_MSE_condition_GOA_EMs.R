pacman::p_load(Rceattle, readxl, dplyr, tidyr, writexl)
load("Models/GOA_20_1_1_mod_list.RData")
combined_data_em <- read_data(file = "Data/GOA_23_1_1_data_1977_2023_edited.xlsx")
combined_data_em$endyr <- 2020
combined_data_em$projyr <- 2130


## Ajust inits ----
turn_offs <- c("logH_1", "logH_1a", "logH_1b", "logH_2", "logH_3", "H_4", "log_gam_a", "log_gam_b", "log_phi", "ln_pop_scalar", "sel_coff_dev")

## Ajust inits ----
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
  combined_data_em$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio, 0, 0)
  mod_list_all[[i]]$estimated_params$proj_F_prop <- mod_list_all[[i]]$data_list$fleet_control$proj_F_prop
}

# All EMs have density-independent recruitment ----

## Estimate OMs ----
# EM 1) Single-spp fix M ----
# - Tier-3
ss_run_Tier3 <- Rceattle::fit_mod(data_list = combined_data_em,
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
ss_run_dynamicTier3 <- Rceattle::fit_mod(data_list = combined_data_em,
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


# EM 2) Single-species estimated M ----
# - Tier-3
ss_run_M_Tier3 <- Rceattle::fit_mod(data_list = combined_data_em,
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
ss_run_M_dynamicTier3 <- Rceattle::fit_mod(data_list = combined_data_em,
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



# EM 3) Multi-species ----
# -- F that acheives 40% of SB0, where SB0 is derived from projecting all species simultaneously under no fishing
ms_run_fb40 <- Rceattle::fit_mod(data_list = combined_data_em,
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
ms_run_fb40iter <- Rceattle::fit_mod(data_list = combined_data_em,
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
ms_run_cmsy <- Rceattle::fit_mod(data_list = combined_data_em,
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
ms_run_concmsy <- Rceattle::fit_mod(data_list = combined_data_em,
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



em_list <- list(ss_run_Tier3, ss_run_dynamicTier3, ss_run_M_Tier3, ss_run_M_dynamicTier3, ms_run_fb40, ms_run_fb40iter, ms_run_cmsy, ms_run_concmsy)
em_names <- paste0(c("ss_run_Tier3", "ss_run_dynamicTier3", "ss_run_M_Tier3", "ss_run_M_dynamicTier3", "ms_run_fb40", "ms_run_fb40iter", "ms_run_cmsy", "ms_run_concmsy"), "_EM")
