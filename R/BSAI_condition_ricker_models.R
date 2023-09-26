library(Rceattle)
library(dplyr)

################################################
# Data ----
################################################
# Example
# To run the 2017 single species assessment for the Bering Sea, a data file must first be loaded:
data(BS2017SS) # ?BS2017SS for more information on the data
BS2017SS$projyr <- 2100

data("BS2017MS") # Note: the only difference is the residual mortality (M1_base) is lower
BS2017MS$projyr <- 2100

BS2017SS$fleet_control$proj_F_prop <-rep(1,7)
BS2017MS$fleet_control$proj_F_prop <- rep(1, 7)


alpha = exp(c(4.121, 2.119, 1.553))

################################################
# Estimate OMs ----
################################################
# - Single-species fixed M
ss_run_ricker <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  M1Fun = build_M1(M1_model = 0,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 0,
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha,
                     srr_prior_sd = 0.2),
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  phase = "default",
  verbose = 1, 
  initMode = 2)
#plot_biomass(ss_run_ricker, incl_proj = TRUE)
#plot_stock_recruit(ss_run_ricker)


# Single-species and estimate M
BS2017SS$M1_base[2,3:23] <- 0.45
ss_run_ricker_M <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  M1Fun = build_M1(M1_model = c(1,0,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 0,
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha*2,
                     srr_prior_sd = 0.2,
                     Bmsy_lim = c(8000000, 400000, 500000)),
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  phase = "default",
  verbose = 1, 
  initMode = 2)
#plot_biomass(ss_run_ricker_M, incl_proj = TRUE)
#plot_stock_recruit(ss_run_ricker_M)

alpha = exp(ss_run_ricker_M$estimated_params$rec_pars[,2])
beta = exp(ss_run_ricker_M$estimated_params$rec_pars[,3])/1000000
SSB_MSY = log(alpha)/beta *(0.5-0.07*alpha)/1000000

# - Multi-species
BS2017MS$M1_base[1,3:23] <- 0.4
BS2017MS$M1_base[2,3:23] <- 0.4
ms_run_ricker <- Rceattle::fit_mod(
  data_list = BS2017MS,
  inits = ss_run_ricker_M$estimated_params, # Initial parameters from single species ests
  phase = NULL, 
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  M1Fun = build_M1(M1_model = c(0,0,1),
                   updateM1 = TRUE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 0,
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 2,
                     srr_prior_sd = 0.2),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  verbose = 1, 
  initMode = 2)

plot_ssb(list(ss_run_ricker, ss_run_ricker_M, ms_run_ricker), model_names = c("SS: fix M", "SS: est M", "MS"), incl_proj = TRUE)
plot_stock_recruit(list(ss_run_ricker, ss_run_ricker_M, ms_run_ricker), model_names = c("SS: fix M", "SS: est M", "MS"))


################################################
# EMs: Multi-species w/ harvest control rules ----
################################################
ms_run_ricker_f25 <- Rceattle::fit_mod(
  data_list = BS2017MS,
  inits = ms_run_ricker$estimated_params, # Initial parameters from single species ests
  phase = "default", 
  file = NULL, # Don't save
  estimateMode = 0, # Estimate projection only
  M1Fun = build_M1(M1_model = c(0,0,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  niter = 3, # 10 iterations around population and predation dynamics
  HCR = build_hcr(HCR = 3, # Constant F HCR
                  DynamicHCR = FALSE, # Use dynamic reference points
                  FsprTarget = 0.25),
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  verbose = 1, 
  initMode = 2)


################################################
# EMs: Single-species fixed M w/ harvest control rules ----
################################################
# -- Avg F
avg_F <- (exp(ss_run_ricker$estimated_params$ln_mean_F+ss_run_ricker$estimated_params$F_dev)) # Average F from last 5 years
avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])[1:3]

ss_run_ricker_AvgF <- fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker$estimated_params, # Initial parameters from ss_run_ricker
  phase = "default", 
  estimateMode = 0, # Run projection only
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  HCR = build_hcr(HCR = 2, # Input F
                  FsprTarget = avg_F, # F40%
                  FsprLimit = 0.35,
                  Plimit = 0.2
  ),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)

# -- Constant Fspr
ss_run_ricker_Fspr <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker$estimated_params, # Initial parameters from ss_run_ricker
  phase = "default", 
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 4, # Tier3 HCR
                  FsprTarget = 0.4, # 0.75 * F40%
                  FsprLimit = 0.4, # F40%
                  Fmult = 0.75,
                  Plimit = 0.2
  ),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)


# -- NPFMC Tier 3
ss_run_ricker_Tier3 <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker$estimated_params,
  phase = "default", 
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 5, # Tier3 HCR
                  FsprTarget = 0.4, # F40%
                  FsprLimit = 0.35, # F35%
                  Plimit = c(0.2, 0.2, 0), # No fishing when SB<SB20
                  Alpha = 0.05),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)


ss_run_ricker_dynamicTier3 <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker$estimated_params, # Initial parameters from ss_run_ricker
  phase = "default", 
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 5, # Tier3 HCR
                  DynamicHCR = TRUE, # Use dynamic reference points
                  FsprTarget = 0.4, # F40%
                  FsprLimit = 0.35, # F35%
                  Plimit = c(0.2, 0.2, 0), # No fishing when SB<SB20
                  Alpha = 0.05),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)

# -- PFMC Category 1
ss_run_ricker_Cat1 <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker$estimated_params, # Initial parameters from ss_run_ricker
  phase = "default", 
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 6, # Cat 1 HCR
                  FsprLimit = c(0.45, 0.45,  0.3), # F45%
                  Ptarget = c(0.4, 0.4, 0.25), # Target is 40% B0
                  Plimit = c(0.1, 0.1, 0.05), # No fishing when SB<SB10
                  Pstar = 0.45,
                  Sigma = 0.5),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)

ss_run_ricker_dynamicCat1 <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker$estimated_params, # Initial parameters from ss_run_ricker
  phase = "default", 
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 6, # Cat 1 HCR
                  DynamicHCR = TRUE, # Use dynamic reference points
                  FsprLimit = c(0.45, 0.45,  0.3), # F45%
                  Ptarget = c(0.4, 0.4, 0.25), # Target is 40% B0
                  Plimit = c(0.1, 0.1, 0.05), # No fishing when SB<SB10
                  Pstar = 0.45,
                  Sigma = 0.5),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)

# -- SESSF Tier 1
ss_run_ricker_Tier1 <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker$estimated_params, # Initial parameters from ss_run_ricker
  phase = "default", 
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 7, # Tier 1 HCR
                  FsprTarget = 0.48, # F40%
                  FsprLimit = 0.20, # F20%
                  Ptarget = 0.35, # Target is 35% SSB0
                  Plimit = 0.20, # No fishing when B<B20
  ),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)


ss_run_ricker_dynamicTier1 <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker$estimated_params, # Initial parameters from ss_run_ricker
  phase = "default", 
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = 7, # Tier 1 HCR
                  DynamicHCR = TRUE,
                  FsprTarget = 0.48, # F40%
                  FsprLimit = 0.20, # F20%
                  Ptarget = 0.35, # Target is 35% SSB0
                  Plimit = 0.20, # No fishing when B<B20
  ),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1, 
  initMode = 2)



################################################
# EMs: Single-species estimate M w/ harvest control rules ----
###############################################
# -- Avg F
avg_F <- (exp(ss_run_ricker_M$estimated_params$ln_mean_F+ss_run_ricker_M$estimated_params$F_dev)) # Average F from last 5 years
avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])[1:3]

ss_run_ricker_M_AvgF <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker_M$estimated_params, 
  phase = "default", # Initial parameters from ss_run_ricker_M
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,0,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 2, # Input F
                  FsprTarget = avg_F, # F40%
                  FsprLimit = 0.35,
                  Plimit = 0.2
  ),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)

# -- Constant Fspr
ss_run_ricker_M_Fspr <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker_M$estimated_params, 
  phase = "default", # Initial parameters from ss_run_ricker_M
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,0,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 4, # Fspr HCR
                  FsprTarget = 0.4, # 0.75 * F40%
                  FsprLimit = 0.4, # F40%
                  Fmult = 0.75,
                  Plimit = 0.2
  ),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)


# -- NPFMC Tier 3
ss_run_ricker_M_Tier3 <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker_M$estimated_params, 
  phase = "default", # Initial parameters from ss_run_ricker_M
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,0,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 5, # Tier3 HCR
                  FsprTarget = 0.4, # F40%
                  FsprLimit = 0.35, # F35%
                  Plimit = c(0.2, 0.2, 0), # No fishing when SB<SB20
                  Alpha = 0.05),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)


ss_run_ricker_M_dynamicTier3 <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker_M$estimated_params, 
  phase = "default", # Initial parameters from ss_run_ricker_M
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,0,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 5, # Tier3 HCR
                  DynamicHCR = TRUE, # Use dynamic reference points
                  FsprTarget = 0.4, # F40%
                  FsprLimit = 0.35, # F35%
                  Plimit = c(0.2, 0.2, 0), # No fishing when SB<SB20
                  Alpha = 0.05),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)

# -- PFMC Category 1
ss_run_ricker_M_Cat1 <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker_M$estimated_params, 
  phase = "default", # Initial parameters from ss_run_ricker_M
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,0,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 6, # Cat 1 HCR
                  FsprLimit = c(0.45, 0.45,  0.3), # F45%
                  Ptarget = c(0.4, 0.4, 0.25), # Target is 40% B0
                  Plimit = c(0.1, 0.1, 0.05), # No fishing when SB<SB10
                  Pstar = 0.45,
                  Sigma = 0.5),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)

ss_run_ricker_M_dynamicCat1 <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker_M$estimated_params, 
  phase = "default", # Initial parameters from ss_run_ricker_M
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,0,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 6, # Cat 1 HCR
                  DynamicHCR = TRUE, # Use dynamic reference points
                  FsprLimit = c(0.45, 0.45,  0.3), # F45%
                  Ptarget = c(0.4, 0.4, 0.25), # Target is 40% B0
                  Plimit = c(0.1, 0.1, 0.05), # No fishing when SB<SB10
                  Pstar = 0.45,
                  Sigma = 0.5),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)

# -- SESSF Tier 1
ss_run_ricker_M_Tier1 <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker_M$estimated_params, 
  phase = "default", # Initial parameters from ss_run_ricker_M
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,0,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 7, # Tier 1 HCR
                  FsprTarget = 0.48, # F40%
                  FsprLimit = 0.20, # F20%
                  Ptarget = 0.35, # Target is 35% SSB0
                  Plimit = 0.20, # No fishing when B<B20
  ),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)


ss_run_ricker_M_dynamicTier1 <- Rceattle::fit_mod(
  data_list = BS2017SS,
  inits = ss_run_ricker_M$estimated_params, 
  phase = "default", # Initial parameters from ss_run_ricker_M
  estimateMode = 0, # Run projection only
  M1Fun = build_M1(M1_model = c(1,0,1),
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  HCR = build_hcr(HCR = 7, # Tier 1 HCR
                  DynamicHCR = TRUE,
                  FsprTarget = 0.48, # F40%
                  FsprLimit = 0.20, # F20%
                  Ptarget = 0.35, # Target is 35% SSB0
                  Plimit = 0.20, # No fishing when B<B20
  ),
  recFun = build_srr(srr_fun = 0, 
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = 0.2,
                     srr_prior_sd = 0.2),
  msmMode = 0, # Single species mode
  verbose = 1,
  initMode = 2)



###############################################
# #plot ----
###############################################
M_mod_list <- list(ss_run_ricker_M, ss_run_ricker_M_AvgF, ss_run_ricker_M_Fspr, ss_run_ricker_M_Tier3, ss_run_ricker_M_dynamicTier3, ss_run_ricker_M_Cat1, ss_run_ricker_M_dynamicCat1, ss_run_ricker_M_Tier1, ss_run_ricker_M_dynamicTier1 )
mod_list <- list(ss_run_ricker, ss_run_ricker_AvgF, ss_run_ricker_Fspr, ss_run_ricker_Tier3, ss_run_ricker_dynamicTier3, ss_run_ricker_Cat1, ss_run_ricker_dynamicCat1, ss_run_ricker_Tier1, ss_run_ricker_dynamicTier1 )

# - SS
#plot_biomass(mod_list, incl_proj = T)
#plot_ssb(mod_list, incl_proj = T)
#plot_depletionSSB(mod_list, incl_proj = T)
#plot_recruitment(mod_list, incl_proj = T)
#plot_catch(mod_list, incl_proj = TRUE)

# - SS M
#plot_biomass(M_mod_list, incl_proj = T)
#plot_ssb(M_mod_list, incl_proj = T)
#plot_depletionSSB(M_mod_list, incl_proj = T)
#plot_recruitment(M_mod_list, incl_proj = T)
#plot_catch(M_mod_list, incl_proj = TRUE)

#plot_stock_recruit(list(ss_run_ricker, ss_run_ricker_M, ms_run_ricker))
