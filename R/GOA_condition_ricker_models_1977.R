library(Rceattle)
library(dplyr)

# Source models with no curve to initialize
if(!exists("fit_all")){fit_all = TRUE}
fit_alltmp <- fit_all
fit_all = FALSE
source("R/GOA_condition_models_1977.R")
fit_all <- fit_alltmp

alpha = exp(c(3.143, 1.975, 1.44))


################################################
# Estimate OMs w ricker ----
################################################
# - Single-species fixed M
ss_run_ricker <- Rceattle::fit_mod(
  data_list = ss_run$data_list,
  inits = ss_run$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  recFun = build_srr(srr_fun = 0,
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha,
                     srr_prior_sd = 0.2,
                     Bmsy_lim = apply(ss_run$quantities$biomassSSB, 1, max)
  ),
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  phase = "default",
  verbose = 1, 
  initMode = 2)
plot_biomass(ss_run_ricker, incl_proj = TRUE)
plot_stock_recruit(ss_run_ricker)

# -- Check Bmsy
alpha <- exp(ss_run_ricker$estimated_params$rec_pars[,2])
beta <- exp(ss_run_ricker$estimated_params$rec_pars[,3])
apply(ss_run$quantities$biomassSSB, 1, max)
1/(beta/1000000)


# Estimate single-species and estimate M
ss_run_ricker_M <- ss_run_M
ss_run_ricker_M <- Rceattle::fit_mod(
  data_list = ss_run_ricker_M$data_list,
  inits = ss_run_ricker_M$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  M1Fun = build_M1(M1_model = c(1,2,1),
                   updateM1 = TRUE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 0,
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha,
                     srr_prior_sd = 0.2,
                     Bmsy_lim = apply(ss_run_ricker_M$quantities$biomassSSB, 1, max)
  ),
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  phase = "default",
  verbose = 1, 
  initMode = 2)

plot_ssb(ss_run_ricker_M, incl_proj = TRUE)
plot_stock_recruit(ss_run_ricker_M)

# -- Check Bmsy
alpha <- exp(ss_run_ricker_M$estimated_params$rec_pars[,2])
beta <- exp(ss_run_ricker_M$estimated_params$rec_pars[,3])
apply(ss_run_M$quantities$biomassSSB, 1, max)
1/(beta/1000000)


# - Multi-species
# -- Update initial parameters
ms_run_ricker <- ms_run
ms_run_ricker$data_list$M1_base[1,3:23] <- 0.25
ms_run_ricker$data_list$M1_base[4,3:23] <- 0.35

alpha <- exp(ss_run_ricker_M$estimated_params$rec_pars[,2])
beta <- exp(ss_run_ricker_M$estimated_params$rec_pars[,3])

ms_run_ricker$estimated_params$rec_pars[,2] <- log(alpha)
ms_run_ricker$estimated_params$rec_pars[,3] <- log(beta)

ms_run_ricker <- Rceattle::fit_mod(
  data_list = ms_run_ricker$data_list,
  inits = ms_run_ricker$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = NULL,
  M1Fun = build_M1(M1_model = c(1,2,1),
                   updateM1 = TRUE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 0,
                     srr_pred_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     
                     Bmsy_lim = c(600000, apply(ms_run_ricker$quantities$biomassSSB, 1, max)[2:3])
  ),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = NULL,
  verbose = 1, 
  initMode = 0)

plot_biomass(ms_run_ricker, incl_proj = TRUE)
plot_stock_recruit(ms_run_ricker)

# -- Check Bmsy
alpha <- exp(ms_run_ricker$estimated_params$rec_pars[,2])
beta <- exp(ms_run_ricker$estimated_params$rec_pars[,3])
apply(ms_run_ricker$quantities$biomassSSB, 1, max)
apply(ms_run$quantities$biomassSSB, 1, max)
1/(beta/1000000)


# -- Plot everything
plot_ssb(list(ss_run_ricker, ss_run_ricker_M, ms_run_ricker), model_names = c("SS: fix M", "SS: est M", "MS"), incl_proj = TRUE)
plot_stock_recruit(list(ss_run_ricker, ss_run_ricker_M, ms_run_ricker), model_names = c("SS: fix M", "SS: est M", "MS"))


################################################
# OMs: Ratio of F across Pcod fleets ----
################################################
mod_list_all_ricker <- list(ss_run_ricker, ss_run_ricker_M, ms_run_ricker)

for(i in 1:3){
  avg_F <- (exp(mod_list_all_ricker[[i]]$estimated_params$ln_mean_F+mod_list_all_ricker[[i]]$estimated_params$F_dev)) # Average F from last 2 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
  f_ratio <- avg_F[14:16]
  f_ratio <- f_ratio/sum(f_ratio)
  
  # Adjust future F proportion to each fleet
  mod_list_all_ricker[[i]]$data_list$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio)
  mod_list_all_ricker[[i]]$estimated_params$proj_F_prop <- mod_list_all_ricker[[i]]$data_list$fleet_control$proj_F_prop
}


ss_run_ricker <- mod_list_all_ricker[[1]]
ss_run_ricker_M <- mod_list_all_ricker[[2]]
ms_run_ricker <- mod_list_all_ricker[[3]]


if(fit_all){
  ################################################
  # EMs: Multi-species w/ harvest control rules ----
  ################################################
  ms_run_ricker_f25 <- Rceattle::fit_mod( 
    
    data_list = ms_run_ricker$data_list,
    inits = ms_run_ricker$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ms_run_ricker$data_list$M1_model,
                     M1_use_prior = ms_run_ricker$data_list$M1_use_prior,
                     M2_use_prior = ms_run_ricker$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ms_run_ricker$data_list$srr_fun,
                       srr_pred_fun = ms_run_ricker$data_list$srr_pred_fun,
                       proj_mean_rec = ms_run_ricker$data_list$proj_mean_rec,
                       srr_est_mode = ms_run_ricker$data_list$srr_est_mode,
                       srr_prior_mean = ms_run_ricker$data_list$srr_prior_mean,
                       srr_prior_sd = ms_run_ricker$data_list$srr_prior_sd,
                       Bmsy_lim = ms_run_ricker$data_list$Bmsy_lim),
    msmMode = ms_run_ricker$data_list$msmMode,
    initMode = ms_run_ricker$data_list$initMode,
    suitMode = ms_run_ricker$data_list$suitMode, # empirical suitability
    verbose = 1,
    niter = ms_run_ricker$data_list$niter,
    HCR = build_hcr(HCR = 3, # Constant F HCR
                    DynamicHCR = FALSE, # Use dynamic reference points
                    FsprTarget = 0.25)
  )
  
  
  ################################################
  # EMs: Fixed M w/ harvest control rules ----
  ################################################
  # -- Avg F
  avg_F <- (exp(ss_run_ricker$estimated_params$ln_mean_F+ss_run_ricker$estimated_params$F_dev)) # Average F from last 5 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])
  avg_F <- data.frame(avg_F = avg_F, spp = ss_run_ricker$data_list$fleet_control$Species)
  avg_F <- avg_F %>%
    group_by(spp) %>%
    summarise(avg_F = sum(avg_F)) %>%
    arrange(spp)
  
  ss_run_ricker_AvgF <- fit_mod(
    data_list = ss_run_ricker$data_list,
    inits = ss_run_ricker$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker$data_list$M1_model,
                     M1_use_prior = ss_run_ricker$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker$data_list$Bmsy_lim),
    msmMode = ss_run_ricker$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker$data_list$initMode,
    HCR = build_hcr(HCR = 2, # Input F
                    FsprTarget = avg_F$avg_F, # F40%
                    FsprLimit = 0.35,
                    Plimit = 0.2
    )
  )
  
  # -- Constant Fspr
  ss_run_ricker_Fspr <- Rceattle::fit_mod(
    data_list = ss_run_ricker$data_list,
    inits = ss_run_ricker$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker$data_list$M1_model,
                     M1_use_prior = ss_run_ricker$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker$data_list$Bmsy_lim),
    msmMode = ss_run_ricker$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker$data_list$initMode,
    HCR = build_hcr(HCR = 4, # Tier3 HCR
                    FsprTarget = 0.4, # 0.75 * F40%
                    FsprLimit = 0.4, # F40%
                    Fmult = 0.75,
                    Plimit = 0.2
    )
  )
  
  
  # -- NPFMC Tier 3
  ss_run_ricker_Tier3 <- Rceattle::fit_mod(
    data_list = ss_run_ricker$data_list,
    inits = ss_run_ricker$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker$data_list$M1_model,
                     M1_use_prior = ss_run_ricker$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker$data_list$Bmsy_lim),
    msmMode = ss_run_ricker$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker$data_list$initMode,
    HCR = build_hcr(HCR = 5, # Tier3 HCR
                    FsprTarget = 0.4, # F40%
                    FsprLimit = 0.35, # F35%
                    Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                    Alpha = 0.05)
  )
  
  
  ss_run_ricker_dynamicTier3 <- Rceattle::fit_mod(
    data_list = ss_run_ricker$data_list,
    inits = ss_run_ricker$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker$data_list$M1_model,
                     M1_use_prior = ss_run_ricker$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker$data_list$Bmsy_lim),
    msmMode = ss_run_ricker$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker$data_list$initMode,
    HCR = build_hcr(HCR = 5, # Tier3 HCR
                    DynamicHCR = TRUE, # Use dynamic reference points
                    FsprTarget = 0.4, # F40%
                    FsprLimit = 0.35, # F35%
                    Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                    Alpha = 0.05)
  )
  
  # -- PFMC Category 1
  ss_run_ricker_Cat1 <- Rceattle::fit_mod(
    data_list = ss_run_ricker$data_list,
    inits = ss_run_ricker$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker$data_list$M1_model,
                     M1_use_prior = ss_run_ricker$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker$data_list$Bmsy_lim),
    msmMode = ss_run_ricker$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker$data_list$initMode,
    HCR = build_hcr(HCR = 6, # Cat 1 HCR
                    FsprLimit = c(0.45, 0.3, 0.45), # F45%
                    Ptarget = c(0.4, 0.25, 0.4), # Target is 40% B0
                    Plimit = c(0.1, 0.05, 0.1), # No fishing when SB<SB10
                    Pstar = 0.45,
                    Sigma = 0.5)
  )
  
  ss_run_ricker_dynamicCat1 <- Rceattle::fit_mod(
    data_list = ss_run_ricker$data_list,
    inits = ss_run_ricker$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker$data_list$M1_model,
                     M1_use_prior = ss_run_ricker$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker$data_list$Bmsy_lim),
    msmMode = ss_run_ricker$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker$data_list$initMode,
    HCR = build_hcr(HCR = 6, # Cat 1 HCR
                    DynamicHCR = TRUE, # Use dynamic reference points
                    FsprLimit = c(0.45, 0.3, 0.45), # F45%
                    Ptarget = c(0.4, 0.25, 0.4), # Target is 40% B0
                    Plimit = c(0.1, 0.05, 0.1), # No fishing when SB<SB10
                    Pstar = 0.45,
                    Sigma = 0.5)
  )
  
  # -- SESSF Tier 1
  ss_run_ricker_Tier1 <- Rceattle::fit_mod(
    data_list = ss_run_ricker$data_list,
    inits = ss_run_ricker$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker$data_list$M1_model,
                     M1_use_prior = ss_run_ricker$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker$data_list$Bmsy_lim),
    msmMode = ss_run_ricker$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker$data_list$initMode,
    HCR = build_hcr(HCR = 7, # Tier 1 HCR
                    FsprTarget = 0.48, # F40%
                    FsprLimit = 0.20, # F20%
                    Ptarget = 0.35, # Target is 35% SSB0
                    Plimit = 0.20, # No fishing when B<B20
    )
  )
  
  
  ss_run_ricker_dynamicTier1 <- Rceattle::fit_mod(
    data_list = ss_run_ricker$data_list,
    inits = ss_run_ricker$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker$data_list$M1_model,
                     M1_use_prior = ss_run_ricker$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker$data_list$Bmsy_lim),
    msmMode = ss_run_ricker$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker$data_list$initMode,
    HCR = build_hcr(HCR = 7, # Tier 1 HCR
                    DynamicHCR = TRUE,
                    FsprTarget = 0.48, # F40%
                    FsprLimit = 0.20, # F20%
                    Ptarget = 0.35, # Target is 35% SSB0
                    Plimit = 0.20, # No fishing when B<B20
    )
  )
  
  
  
  ################################################
  # EMs: Estimate M w/ harvest control rules ----
  ###############################################
  # -- Avg F
  avg_F <- (exp(ss_run_ricker_M$estimated_params$ln_mean_F+ss_run_ricker_M$estimated_params$F_dev)) # Average F from last 5 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])
  avg_F <- data.frame(avg_F = avg_F, spp = ss_run_ricker_M$data_list$fleet_control$Species)
  avg_F <- avg_F %>%
    group_by(spp) %>%
    summarise(avg_F = sum(avg_F)) %>%
    arrange(spp)
  
  
  ss_run_ricker_M_AvgF <- Rceattle::fit_mod(
    data_list = ss_run_ricker_M$data_list,
    inits = ss_run_ricker_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker_M$data_list$M1_model,
                     M1_use_prior = ss_run_ricker_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker_M$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker_M$data_list$Bmsy_lim),
    msmMode = ss_run_ricker_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker_M$data_list$initMode,
    HCR = build_hcr(HCR = 2, # Input F
                    FsprTarget = avg_F$avg_F, # F40%
                    FsprLimit = 0.35, 
                    Plimit = 0.2
    )
  )
  
  # -- Constant Fspr
  ss_run_ricker_M_Fspr <- Rceattle::fit_mod(
    data_list = ss_run_ricker_M$data_list,
    inits = ss_run_ricker_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker_M$data_list$M1_model,
                     M1_use_prior = ss_run_ricker_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker_M$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker_M$data_list$Bmsy_lim),
    msmMode = ss_run_ricker_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker_M$data_list$initMode,
    HCR = build_hcr(HCR = 4, # Fspr HCR
                    FsprTarget = 0.4, # 0.75 * F40%
                    FsprLimit = 0.4, # F40%
                    Fmult = 0.75,
                    Plimit = 0.2
    )
  )
  
  
  # -- NPFMC Tier 3
  ss_run_ricker_M_Tier3 <- Rceattle::fit_mod(
    data_list = ss_run_ricker_M$data_list,
    inits = ss_run_ricker_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker_M$data_list$M1_model,
                     M1_use_prior = ss_run_ricker_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker_M$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker_M$data_list$Bmsy_lim),
    msmMode = ss_run_ricker_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker_M$data_list$initMode,
    HCR = build_hcr(HCR = 5, # Tier3 HCR
                    FsprTarget = 0.4, # F40%
                    FsprLimit = 0.35, # F35%
                    Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                    Alpha = 0.05))
  
  
  ss_run_ricker_M_dynamicTier3 <- Rceattle::fit_mod(
    data_list = ss_run_ricker_M$data_list,
    inits = ss_run_ricker_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker_M$data_list$M1_model,
                     M1_use_prior = ss_run_ricker_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker_M$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker_M$data_list$Bmsy_lim),
    msmMode = ss_run_ricker_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker_M$data_list$initMode,
    HCR = build_hcr(HCR = 5, # Tier3 HCR
                    DynamicHCR = TRUE, # Use dynamic reference points
                    FsprTarget = 0.4, # F40%
                    FsprLimit = 0.35, # F35%
                    Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                    Alpha = 0.05)
  )
  
  # -- PFMC Category 1
  ss_run_ricker_M_Cat1 <- Rceattle::fit_mod(
    data_list = ss_run_ricker_M$data_list,
    inits = ss_run_ricker_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker_M$data_list$M1_model,
                     M1_use_prior = ss_run_ricker_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker_M$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker_M$data_list$Bmsy_lim),
    msmMode = ss_run_ricker_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker_M$data_list$initMode,
    HCR = build_hcr(HCR = 6, # Cat 1 HCR
                    FsprLimit = c(0.45, 0.3, 0.45), # F45%
                    Ptarget = c(0.4, 0.25, 0.4), # Target is 40% B0
                    Plimit = c(0.1, 0.05, 0.1), # No fishing when SB<SB10
                    Pstar = 0.45,
                    Sigma = 0.5)
  )
  
  ss_run_ricker_M_dynamicCat1 <- Rceattle::fit_mod(
    data_list = ss_run_ricker_M$data_list,
    inits = ss_run_ricker_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker_M$data_list$M1_model,
                     M1_use_prior = ss_run_ricker_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker_M$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker_M$data_list$Bmsy_lim),
    msmMode = ss_run_ricker_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker_M$data_list$initMode,
    HCR = build_hcr(HCR = 6, # Cat 1 HCR
                    DynamicHCR = TRUE, # Use dynamic reference points
                    FsprLimit = c(0.45, 0.3, 0.45), # F45%
                    Ptarget = c(0.4, 0.25, 0.4), # Target is 40% B0
                    Plimit = c(0.1, 0.05, 0.1), # No fishing when SB<SB10
                    Pstar = 0.45,
                    Sigma = 0.5)
  )
  
  # -- SESSF Tier 1
  ss_run_ricker_M_Tier1 <- Rceattle::fit_mod(
    data_list = ss_run_ricker_M$data_list,
    inits = ss_run_ricker_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker_M$data_list$M1_model,
                     M1_use_prior = ss_run_ricker_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker_M$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker_M$data_list$Bmsy_lim),
    msmMode = ss_run_ricker_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker_M$data_list$initMode,
    HCR = build_hcr(HCR = 7, # Tier 1 HCR
                    FsprTarget = 0.48, # F40%
                    FsprLimit = 0.20, # F20%
                    Ptarget = 0.35, # Target is 35% SSB0
                    Plimit = 0.20, # No fishing when B<B20
    )
  )
  
  
  ss_run_ricker_M_dynamicTier1 <- Rceattle::fit_mod(
    data_list = ss_run_ricker_M$data_list,
    inits = ss_run_ricker_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_ricker_M$data_list$M1_model,
                     M1_use_prior = ss_run_ricker_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_ricker_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_ricker_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_ricker_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_ricker_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_ricker_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_ricker_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_ricker_M$data_list$srr_prior_sd,
                       Bmsy_lim = ss_run_ricker_M$data_list$Bmsy_lim),
    msmMode = ss_run_ricker_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_ricker_M$data_list$initMode,
    HCR = build_hcr(HCR = 7, # Tier 1 HCR
                    DynamicHCR = TRUE,
                    FsprTarget = 0.48, # F40%
                    FsprLimit = 0.20, # F20%
                    Ptarget = 0.35, # Target is 35% SSB0
                    Plimit = 0.20, # No fishing when B<B20
    )
  )
  
  
  ###############################################
  # #plot ----
  ###############################################
  M_mod_list <- list(ss_run_ricker_M, ss_run_ricker_M_AvgF, ss_run_ricker_M_Fspr, ss_run_ricker_M_Tier3, ss_run_ricker_M_dynamicTier3, ss_run_ricker_M_Cat1, ss_run_ricker_M_dynamicCat1, ss_run_ricker_M_Tier1, ss_run_ricker_M_dynamicTier1 )
  
  mod_list <- list(ss_run_ricker, ss_run_ricker_AvgF, ss_run_ricker_Fspr, ss_run_ricker_Tier3, ss_run_ricker_dynamicTier3, ss_run_ricker_Cat1, ss_run_ricker_dynamicCat1, ss_run_ricker_Tier1, ss_run_ricker_dynamicTier1 )
  
  ms_mod_list <- list(ms_run_ricker, ms_run_ricker_f25)
  
  # - SS
  plot_biomass(mod_list, incl_proj = T, model_names = 1:length(mod_list))
  plot_ssb(mod_list, incl_proj = T, model_names = 1:length(mod_list))
  plot_depletionSSB(mod_list, incl_proj = T, model_names = 1:length(mod_list))
  plot_recruitment(mod_list, incl_proj = T, model_names = 1:length(mod_list))
  plot_stock_recruit(mod_list, model_names = 1:length(mod_list))
  
  
  # - SS M
  plot_biomass(M_mod_list, incl_proj = T, model_names = 1:length(M_mod_list))
  plot_ssb(M_mod_list, incl_proj = T, model_names = 1:length(M_mod_list))
  plot_depletionSSB(M_mod_list, incl_proj = T, model_names = 1:length(M_mod_list))
  plot_recruitment(M_mod_list, incl_proj = T, model_names = 1:length(M_mod_list))
  plot_stock_recruit(M_mod_list, model_names = 1:length(M_mod_list))
  
  # - MS
  plot_biomass(ms_mod_list, incl_proj = T)
  plot_stock_recruit(ms_mod_list, model_names = 1:length(ms_mod_list))
  
  
  plot_stock_recruit(list(ss_run_ricker, ss_run_ricker_M, ms_run_ricker))
  #plot_biomass(list(ss_run_ricker, ss_run_ricker_M, ms_run_ricker))
}
