################################################
# Data
################################################
library(Rceattle)
library(dplyr)
data("GOA2018SS") # Note: the only difference is the residual mortality (M1_base) is lower
GOA2018SS$projyr <- 2060

# For GOA MS:
alpha = exp(c(3.143, 1.975, 1.44))

# Only difference is that there is no reweighting of comp data


################################################
# Estimate OMs w no srr ----
################################################
ss_run <- Rceattle::fit_mod(
  data_list = GOA2018SS,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  phase = "default",
  initMode = 2,
  verbose = 1)

ss_run_M <- Rceattle::fit_mod(
  data_list = GOA2018SS,
  inits = NULL,#ss_run$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  M1Fun = build_M1(M1_model = c(1,2,1)), # Estimate M
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  phase = "default",
  initMode = 2,
  verbose = 1)

ms_run <- Rceattle::fit_mod(
  data_list = GOA2018SS,
  inits = ss_run_M$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  M1Fun = build_M1(M1_model = c(1,2,1)),
  niter = 3, # 3 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  initMode = 2,
  phase = "default",
  verbose = 1)


################################################
# Estimate OMs w beverton ----
################################################
ss_run_beverton <- Rceattle::fit_mod(
  data_list = ss_run$data_list,
  inits = ss_run$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  phase = "default",
  verbose = 1, 
  initMode = 2)

# Estimate single-species and estimate M
ss_run_beverton_M <- Rceattle::fit_mod(
  data_list = ss_run_M$data_list,
  inits = ss_run_beverton$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  phase = "default",
  verbose = 1, 
  initMode = 2)

ms_run_beverton <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ms_run$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = "default",
  verbose = 1, 
  initMode = 2)


################################################
# OMs: Ratio of F across Pcod fleets
################################################
mod_list_all <- list(ss_run_beverton, ss_run_beverton_M, ms_run_beverton)

for(i in 1:3){
  avg_F <- (exp(mod_list_all[[i]]$estimated_params$ln_mean_F+mod_list_all[[i]]$estimated_params$F_dev)) # Average F from last 2 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
  f_ratio <- avg_F[14:16]
  f_ratio <- f_ratio/sum(f_ratio)
  
  # Adjust future F proportion to each fleet
  mod_list_all[[i]]$data_list$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio)
  mod_list_all[[i]]$estimated_params$proj_F_prop <- mod_list_all[[i]]$data_list$fleet_control$proj_F_prop
}


ss_run_beverton <- mod_list_all[[1]]
ss_run_beverton_M <- mod_list_all[[2]]
ms_run_beverton <- mod_list_all[[3]]


################################################
# EMs: Multi-species w/ harvest control rules
################################################
ms_run_beverton_f25 <- Rceattle::fit_mod(
  data_list = ms_run_beverton$data_list,
  inits = ms_run_beverton$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 0, # Estimate projection only
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  niter = 3, # 10 iterations around population and predation dynamics
  HCR = build_hcr(HCR = 3, # Constant F HCR
                  DynamicHCR = FALSE, # Use dynamic reference points
                  FsprTarget = 0.25),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha),
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  verbose = 1, 
  phase = "default",
  initMode = 2)


################################################
# EMs: Fixed M w/ harvest control rules
################################################
# -- Avg F
avg_F <- (exp(ss_run_beverton$estimated_params$ln_mean_F+ss_run_beverton$estimated_params$F_dev)) # Average F from last 5 years
avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])[1:3]

ss_run_beverton_AvgF <- fit_mod(
  data_list = ss_run_beverton$data_list,
  inits = ss_run_beverton$estimated_params,
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 2, # Input F
                  FsprTarget = avg_F, # F40%
                  FsprLimit = 0.35,
                  Plimit = 0.2
  ),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)

# -- Constant Fspr
ss_run_beverton_Fspr <- Rceattle::fit_mod(
  data_list = ss_run_beverton$data_list,
  inits = ss_run_beverton$estimated_params, # Initial parameters from ss_run_beverton
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 4, # Tier3 HCR
                  FsprTarget = 0.4, # 0.75 * F40%
                  FsprLimit = 0.4, # F40%
                  Fmult = 0.75,
                  Plimit = 0.2
  ),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)


# -- NPFMC Tier 3
ss_run_beverton_Tier3 <- Rceattle::fit_mod(
  data_list = ss_run_beverton$data_list,
  inits = ss_run_beverton$estimated_params,
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 5, # Tier3 HCR
                  FsprTarget = 0.4, # F40%
                  FsprLimit = 0.35, # F35%
                  Plimit = c(0.2, 0.2, 0), # No fishing when SB<SB20
                  Alpha = 0.05),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)


ss_run_beverton_dynamicTier3 <- Rceattle::fit_mod(
  data_list = ss_run_beverton$data_list,
  inits = ss_run_beverton$estimated_params, # Initial parameters from ss_run_beverton
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 5, # Tier3 HCR
                  DynamicHCR = TRUE, # Use dynamic reference points
                  FsprTarget = 0.4, # F40%
                  FsprLimit = 0.35, # F35%
                  Plimit = c(0.2, 0.2, 0), # No fishing when SB<SB20
                  Alpha = 0.05),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)

# -- PFMC Category 1
ss_run_beverton_Cat1 <- Rceattle::fit_mod(
  data_list = ss_run_beverton$data_list,
  inits = ss_run_beverton$estimated_params, # Initial parameters from ss_run_beverton
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 6, # Cat 1 HCR
                  FsprLimit = c(0.45, 0.45,  0.3), # F45%
                  Ptarget = c(0.4, 0.4, 0.25), # Target is 40% B0
                  Plimit = c(0.1, 0.1, 0.05), # No fishing when SB<SB10
                  Pstar = 0.45,
                  Sigma = 0.5),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)

ss_run_beverton_dynamicCat1 <- Rceattle::fit_mod(
  data_list = ss_run_beverton$data_list,
  inits = ss_run_beverton$estimated_params, # Initial parameters from ss_run_beverton
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 6, # Cat 1 HCR
                  DynamicHCR = TRUE, # Use dynamic reference points
                  FsprLimit = c(0.45, 0.45,  0.3), # F45%
                  Ptarget = c(0.4, 0.4, 0.25), # Target is 40% B0
                  Plimit = c(0.1, 0.1, 0.05), # No fishing when SB<SB10
                  Pstar = 0.45,
                  Sigma = 0.5),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)

# -- SESSF Tier 1
ss_run_beverton_Tier1 <- Rceattle::fit_mod(
  data_list = ss_run_beverton$data_list,
  inits = ss_run_beverton$estimated_params, # Initial parameters from ss_run_beverton
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 7, # Tier 1 HCR
                  FsprTarget = 0.48, # F40%
                  FsprLimit = 0.20, # F20%
                  Ptarget = 0.35, # Target is 35% SSB0
                  Plimit = 0.20, # No fishing when B<B20
  ),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)


ss_run_beverton_dynamicTier1 <- Rceattle::fit_mod(
  data_list = ss_run_beverton$data_list,
  inits = ss_run_beverton$estimated_params, # Initial parameters from ss_run_beverton
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 7, # Tier 1 HCR
                  DynamicHCR = TRUE,
                  FsprTarget = 0.48, # F40%
                  FsprLimit = 0.20, # F20%
                  Ptarget = 0.35, # Target is 35% SSB0
                  Plimit = 0.20, # No fishing when B<B20
  ),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)



################################################
# EMs: Estimate M w/ harvest control rules
###############################################
# -- Avg F
avg_F <- (exp(ss_run_beverton_M$estimated_params$ln_mean_F+ss_run_beverton_M$estimated_params$F_dev)) # Average F from last 5 years
avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])[1:3]

ss_run_beverton_M_AvgF <- Rceattle::fit_mod(
  data_list = ss_run_beverton_M$data_list,
  inits = ss_run_beverton_M$estimated_params, 
  phase = "default",
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 2, # Input F
                  FsprTarget = avg_F, # F40%
                  FsprLimit = 0.35,
                  Plimit = 0.2
  ),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)

# -- Constant Fspr
ss_run_beverton_M_Fspr <- Rceattle::fit_mod(
  data_list = ss_run_beverton_M$data_list,
  inits = ss_run_beverton_M$estimated_params, 
  phase = "default",
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 4, # Fspr HCR
                  FsprTarget = 0.4, # 0.75 * F40%
                  FsprLimit = 0.4, # F40%
                  Fmult = 0.75,
                  Plimit = 0.2
  ),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)


# -- NPFMC Tier 3
ss_run_beverton_M_Tier3 <- Rceattle::fit_mod(
  data_list = ss_run_beverton_M$data_list,
  inits = ss_run_beverton_M$estimated_params, 
  phase = "default",
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 5, # Tier3 HCR
                  FsprTarget = 0.4, # F40%
                  FsprLimit = 0.35, # F35%
                  Plimit = c(0.2, 0.2, 0), # No fishing when SB<SB20
                  Alpha = 0.05),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)


ss_run_beverton_M_dynamicTier3 <- Rceattle::fit_mod(
  data_list = ss_run_beverton_M$data_list,
  inits = ss_run_beverton_M$estimated_params, 
  phase = "default",
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 5, # Tier3 HCR
                  DynamicHCR = TRUE, # Use dynamic reference points
                  FsprTarget = 0.4, # F40%
                  FsprLimit = 0.35, # F35%
                  Plimit = c(0.2, 0.2, 0), # No fishing when SB<SB20
                  Alpha = 0.05),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)

# -- PFMC Category 1
ss_run_beverton_M_Cat1 <- Rceattle::fit_mod(
  data_list = ss_run_beverton_M$data_list,
  inits = ss_run_beverton_M$estimated_params, 
  phase = "default",
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 6, # Cat 1 HCR
                  FsprLimit = c(0.45, 0.45,  0.3), # F45%
                  Ptarget = c(0.4, 0.4, 0.25), # Target is 40% B0
                  Plimit = c(0.1, 0.1, 0.05), # No fishing when SB<SB10
                  Pstar = 0.45,
                  Sigma = 0.5),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)

ss_run_beverton_M_dynamicCat1 <- Rceattle::fit_mod(
  data_list = ss_run_beverton_M$data_list,
  inits = ss_run_beverton_M$estimated_params, 
  phase = "default",
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 6, # Cat 1 HCR
                  DynamicHCR = TRUE, # Use dynamic reference points
                  FsprLimit = c(0.45, 0.45,  0.3), # F45%
                  Ptarget = c(0.4, 0.4, 0.25), # Target is 40% B0
                  Plimit = c(0.1, 0.1, 0.05), # No fishing when SB<SB10
                  Pstar = 0.45,
                  Sigma = 0.5),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)

# -- SESSF Tier 1
ss_run_beverton_M_Tier1 <- Rceattle::fit_mod(
  data_list = ss_run_beverton_M$data_list,
  inits = ss_run_beverton_M$estimated_params, 
  phase = "default",
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 7, # Tier 1 HCR
                  FsprTarget = 0.48, # F40%
                  FsprLimit = 0.20, # F20%
                  Ptarget = 0.35, # Target is 35% SSB0
                  Plimit = 0.20, # No fishing when B<B20
  ),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)


ss_run_beverton_M_dynamicTier1 <- Rceattle::fit_mod(
  data_list = ss_run_beverton_M$data_list,
  inits = ss_run_beverton_M$estimated_params, 
  phase = "default",
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,2,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 7, # Tier 1 HCR
                  DynamicHCR = TRUE,
                  FsprTarget = 0.48, # F40%
                  FsprLimit = 0.20, # F20%
                  Ptarget = 0.35, # Target is 35% SSB0
                  Plimit = 0.20, # No fishing when B<B20
  ),
  recFun = build_srr(srr_fun = 1,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)


###############################################
# Plot
###############################################
M_mod_list <- list(ss_run_beverton_M, ss_run_beverton_M_AvgF, ss_run_beverton_M_Fspr, ss_run_beverton_M_Tier3, ss_run_beverton_M_dynamicTier3, ss_run_beverton_M_Cat1, ss_run_beverton_M_dynamicCat1, ss_run_beverton_M_Tier1, ss_run_beverton_M_dynamicTier1 )
mod_list <- list(ss_run_beverton, ss_run_beverton_AvgF, ss_run_beverton_Fspr, ss_run_beverton_Tier3, ss_run_beverton_dynamicTier3, ss_run_beverton_Cat1, ss_run_beverton_dynamicCat1, ss_run_beverton_Tier1, ss_run_beverton_dynamicTier1 )

# - SS
plot_biomass(mod_list, incl_proj = T)
plot_ssb(mod_list, incl_proj = T)
plot_depletionSSB(mod_list, incl_proj = T)
plot_recruitment(mod_list, incl_proj = T)
plot_catch(mod_list, incl_proj = TRUE)
plot_stock_recruit(mod_list)

# - SS M
plot_biomass(M_mod_list, incl_proj = T)
plot_ssb(M_mod_list, incl_proj = T)
plot_depletionSSB(M_mod_list, incl_proj = T)
plot_recruitment(M_mod_list, incl_proj = T)
plot_catch(M_mod_list, incl_proj = TRUE)


plot_stock_recruit(list(ss_run_beverton, ss_run_beverton_M, ms_run_beverton))



