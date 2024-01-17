library(Rceattle)
library(dplyr)

load("Models/GOA_18_5_1_mod_1-2_2023-07-05.RData")
mod_list_all <- mod_list_all #  <- list(ss_run_OM, ss_run_M_OM, ms_run_OM)


# Ratio of F across Pcod fleets
for(i in 1:3){
  avg_F <- (exp(mod_list_all[[i]]$estimated_params$ln_mean_F+mod_list_all[[i]]$estimated_params$F_dev)) # Average F from last 2 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
  f_ratio <- avg_F[14:16]
  f_ratio <- f_ratio/sum(f_ratio)
  
  # Adjust future F proportion to each fleet
  mod_list_all[[i]]$data_list$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio)
  mod_list_all[[i]]$estimated_params$proj_F_prop <- mod_list_all[[i]]$data_list$fleet_control$proj_F_prop
}


ss_run <- mod_list_all[[1]]
ss_run_M <- mod_list_all[[2]]
ss_run_M$data_list$M1_model <- c(1,2,1)
ms_run <- mod_list_all[[3]]
ms_run$data_list$M1_model <- c(1,2,1)

if(!exists("fit_all")){fit_all = TRUE}
if(fit_all){
  
  # EMs: Multi-species w/ harvest control rules ----
  ms_run_f25 <- Rceattle::fit_mod(
    data_list = ms_run$data_list,
    inits = ms_run$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ms_run$data_list$M1_model,
                     M1_use_prior = ms_run$data_list$M1_use_prior,
                     M2_use_prior = ms_run$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ms_run$data_list$srr_fun,
                       srr_pred_fun = ms_run$data_list$srr_pred_fun,
                       proj_mean_rec = ms_run$data_list$proj_mean_rec,
                       srr_est_mode = ms_run$data_list$srr_est_mode,
                       srr_prior_mean = ms_run$data_list$srr_prior_mean,
                       srr_prior_sd = ms_run$data_list$srr_prior_sd),
    msmMode = ms_run$data_list$msmMode,
    initMode = ms_run$data_list$initMode,
    suitMode = ms_run$data_list$suitMode, # empirical suitability
    verbose = 1,
    niter = ms_run$data_list$niter,
    HCR = build_hcr(HCR = 3, # Constant F HCR
                    DynamicHCR = FALSE, # Use dynamic reference points
                    FsprTarget = 0.25))
  
  
  
  # EMs: Fixed M w/ harvest control rules ----
  # * Avg F ----
  avg_F <- (exp(ss_run$estimated_params$ln_mean_F+ss_run$estimated_params$F_dev)) # Average F from last 5 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])
  avg_F <- data.frame(avg_F = avg_F, spp = ss_run$data_list$fleet_control$Species)
  avg_F <- avg_F %>%
    group_by(spp) %>%
    summarise(avg_F = sum(avg_F)) %>%
    arrange(spp)
  
  ss_run_AvgF <- fit_mod(
    data_list = ss_run$data_list,
    inits = ss_run$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run$data_list$M1_model,
                     M1_use_prior = ss_run$data_list$M1_use_prior,
                     M2_use_prior = ss_run$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run$data_list$srr_fun,
                       srr_pred_fun = ss_run$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run$data_list$proj_mean_rec,
                       srr_est_mode = ss_run$data_list$srr_est_mode,
                       srr_prior_mean = ss_run$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run$data_list$srr_prior_sd),
    msmMode = ss_run$data_list$msmMode,
    verbose = 1,
    initMode = ss_run$data_list$initMode,
    HCR = build_hcr(HCR = 2, # Input F
                    FsprTarget = avg_F$avg_F, # F40%
                    FsprLimit = 0.35,
                    Plimit = 0.2
    )
  )
  
  # * Constant Fspr ----
  ss_run_Fspr <- Rceattle::fit_mod(
    data_list = ss_run$data_list,
    inits = ss_run$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run$data_list$M1_model,
                     M1_use_prior = ss_run$data_list$M1_use_prior,
                     M2_use_prior = ss_run$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run$data_list$srr_fun,
                       srr_pred_fun = ss_run$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run$data_list$proj_mean_rec,
                       srr_est_mode = ss_run$data_list$srr_est_mode,
                       srr_prior_mean = ss_run$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run$data_list$srr_prior_sd),
    msmMode = ss_run$data_list$msmMode,
    verbose = 1,
    initMode = ss_run$data_list$initMode,
    HCR = build_hcr(HCR = 4, # Tier3 HCR
                    FsprTarget = 0.4, # 0.75 * F40%
                    FsprLimit = 0.4, # F40%
                    Fmult = 0.75,
                    Plimit = 0.2
    )
  )
  
  
  # * NPFMC Tier 3 ----
  ss_run_Tier3 <- Rceattle::fit_mod(
    data_list = ss_run$data_list,
    inits = ss_run$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run$data_list$M1_model,
                     M1_use_prior = ss_run$data_list$M1_use_prior,
                     M2_use_prior = ss_run$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run$data_list$srr_fun,
                       srr_pred_fun = ss_run$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run$data_list$proj_mean_rec,
                       srr_est_mode = ss_run$data_list$srr_est_mode,
                       srr_prior_mean = ss_run$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run$data_list$srr_prior_sd),
    msmMode = ss_run$data_list$msmMode,
    verbose = 1,
    initMode = ss_run$data_list$initMode,
    HCR = build_hcr(HCR = 5, # Tier3 HCR
                    FsprTarget = 0.4, # F40%
                    FsprLimit = 0.35, # F35%
                    Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                    Alpha = 0.05)
  )
  
  
  ss_run_dynamicTier3 <- Rceattle::fit_mod(
    data_list = ss_run$data_list,
    inits = ss_run$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run$data_list$M1_model,
                     M1_use_prior = ss_run$data_list$M1_use_prior,
                     M2_use_prior = ss_run$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run$data_list$srr_fun,
                       srr_pred_fun = ss_run$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run$data_list$proj_mean_rec,
                       srr_est_mode = ss_run$data_list$srr_est_mode,
                       srr_prior_mean = ss_run$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run$data_list$srr_prior_sd),
    msmMode = ss_run$data_list$msmMode,
    verbose = 1,
    initMode = ss_run$data_list$initMode,
    HCR = build_hcr(HCR = 5, # Tier3 HCR
                    DynamicHCR = TRUE, # Use dynamic reference points
                    FsprTarget = 0.4, # F40%
                    FsprLimit = 0.35, # F35%
                    Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                    Alpha = 0.05)
  )
  
  # * PFMC Category 1 ----
  ss_run_Cat1 <- Rceattle::fit_mod(
    data_list = ss_run$data_list,
    inits = ss_run$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run$data_list$M1_model,
                     M1_use_prior = ss_run$data_list$M1_use_prior,
                     M2_use_prior = ss_run$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run$data_list$srr_fun,
                       srr_pred_fun = ss_run$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run$data_list$proj_mean_rec,
                       srr_est_mode = ss_run$data_list$srr_est_mode,
                       srr_prior_mean = ss_run$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run$data_list$srr_prior_sd),
    msmMode = ss_run$data_list$msmMode,
    verbose = 1,
    initMode = ss_run$data_list$initMode,
    HCR = build_hcr(HCR = 6, # Cat 1 HCR
                    FsprLimit = c(0.45, 0.3, 0.45), # F45%
                    Ptarget = c(0.4, 0.25, 0.4), # Target is 40% B0
                    Plimit = c(0.1, 0.05, 0.1), # No fishing when SB<SB10
                    Pstar = 0.45,
                    Sigma = 0.5)
  )
  
  ss_run_dynamicCat1 <- Rceattle::fit_mod(
    data_list = ss_run$data_list,
    inits = ss_run$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run$data_list$M1_model,
                     M1_use_prior = ss_run$data_list$M1_use_prior,
                     M2_use_prior = ss_run$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run$data_list$srr_fun,
                       srr_pred_fun = ss_run$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run$data_list$proj_mean_rec,
                       srr_est_mode = ss_run$data_list$srr_est_mode,
                       srr_prior_mean = ss_run$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run$data_list$srr_prior_sd),
    msmMode = ss_run$data_list$msmMode,
    verbose = 1,
    initMode = ss_run$data_list$initMode,
    HCR = build_hcr(HCR = 6, # Cat 1 HCR
                    DynamicHCR = TRUE, # Use dynamic reference points
                    FsprLimit = c(0.45, 0.3, 0.45), # F45%
                    Ptarget = c(0.4, 0.25, 0.4), # Target is 40% B0
                    Plimit = c(0.1, 0.05, 0.1), # No fishing when SB<SB10
                    Pstar = 0.45,
                    Sigma = 0.5)
  )
  
  # * SESSF Tier 1 ----
  ss_run_Tier1 <- Rceattle::fit_mod(
    data_list = ss_run$data_list,
    inits = ss_run$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run$data_list$M1_model,
                     M1_use_prior = ss_run$data_list$M1_use_prior,
                     M2_use_prior = ss_run$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run$data_list$srr_fun,
                       srr_pred_fun = ss_run$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run$data_list$proj_mean_rec,
                       srr_est_mode = ss_run$data_list$srr_est_mode,
                       srr_prior_mean = ss_run$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run$data_list$srr_prior_sd),
    msmMode = ss_run$data_list$msmMode,
    verbose = 1,
    initMode = ss_run$data_list$initMode,
    HCR = build_hcr(HCR = 7, # Tier 1 HCR
                    FsprTarget = 0.48, # F40%
                    FsprLimit = 0.20, # F20%
                    Ptarget = 0.35, # Target is 35% SSB0
                    Plimit = 0.20, # No fishing when B<B20
    )
  )
  
  
  ss_run_dynamicTier1 <- Rceattle::fit_mod(
    data_list = ss_run$data_list,
    inits = ss_run$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run$data_list$M1_model,
                     M1_use_prior = ss_run$data_list$M1_use_prior,
                     M2_use_prior = ss_run$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run$data_list$srr_fun,
                       srr_pred_fun = ss_run$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run$data_list$proj_mean_rec,
                       srr_est_mode = ss_run$data_list$srr_est_mode,
                       srr_prior_mean = ss_run$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run$data_list$srr_prior_sd),
    msmMode = ss_run$data_list$msmMode,
    verbose = 1,
    initMode = ss_run$data_list$initMode,
    HCR = build_hcr(HCR = 7, # Tier 1 HCR
                    DynamicHCR = TRUE,
                    FsprTarget = 0.48, # F40%
                    FsprLimit = 0.20, # F20%
                    Ptarget = 0.35, # Target is 35% SSB0
                    Plimit = 0.20, # No fishing when B<B20
    )
  )
  
  
  
  
  # EMs: Estimate M w/ harvest control rules ----
  # * Avg F ----
  avg_F <- (exp(ss_run_M$estimated_params$ln_mean_F+ss_run_M$estimated_params$F_dev)) # Average F from last 5 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])
  avg_F <- data.frame(avg_F = avg_F, spp = ss_run_M$data_list$fleet_control$Species)
  avg_F <- avg_F %>%
    group_by(spp) %>%
    summarise(avg_F = sum(avg_F)) %>%
    arrange(spp)
  
  ss_run_M_AvgF <- Rceattle::fit_mod(
    data_list = ss_run_M$data_list,
    inits = ss_run_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_M$data_list$M1_model,
                     M1_use_prior = ss_run_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_M$data_list$srr_prior_sd),
    msmMode = ss_run_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_M$data_list$initMode,
    HCR = build_hcr(HCR = 2, # Input F
                    FsprTarget = avg_F$avg_F, # F40%
                    FsprLimit = 0.35, 
                    Plimit = 0.2
    )
  )
  
  # * Constant Fspr ----
  ss_run_M_Fspr <- Rceattle::fit_mod(
    data_list = ss_run_M$data_list,
    inits = ss_run_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_M$data_list$M1_model,
                     M1_use_prior = ss_run_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_M$data_list$srr_prior_sd),
    msmMode = ss_run_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_M$data_list$initMode,
    HCR = build_hcr(HCR = 4, # Fspr HCR
                    FsprTarget = 0.4, # 0.75 * F40%
                    FsprLimit = 0.4, # F40%
                    Fmult = 0.75,
                    Plimit = 0.2
    )
  )
  
  
  # * NPFMC Tier 3 ----
  ss_run_M_Tier3 <- Rceattle::fit_mod(
    data_list = ss_run_M$data_list,
    inits = ss_run_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_M$data_list$M1_model,
                     M1_use_prior = ss_run_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_M$data_list$srr_prior_sd),
    msmMode = ss_run_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_M$data_list$initMode,
    HCR = build_hcr(HCR = 5, # Tier3 HCR
                    FsprTarget = 0.4, # F40%
                    FsprLimit = 0.35, # F35%
                    Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                    Alpha = 0.05))
  
  
  ss_run_M_dynamicTier3 <- Rceattle::fit_mod(
    data_list = ss_run_M$data_list,
    inits = ss_run_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_M$data_list$M1_model,
                     M1_use_prior = ss_run_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_M$data_list$srr_prior_sd),
    msmMode = ss_run_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_M$data_list$initMode,
    HCR = build_hcr(HCR = 5, # Tier3 HCR
                    DynamicHCR = TRUE, # Use dynamic reference points
                    FsprTarget = 0.4, # F40%
                    FsprLimit = 0.35, # F35%
                    Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                    Alpha = 0.05)
  )
  
  # * PFMC Category 1 ----
  ss_run_M_Cat1 <- Rceattle::fit_mod(
    data_list = ss_run_M$data_list,
    inits = ss_run_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_M$data_list$M1_model,
                     M1_use_prior = ss_run_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_M$data_list$srr_prior_sd),
    msmMode = ss_run_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_M$data_list$initMode,
    HCR = build_hcr(HCR = 6, # Cat 1 HCR
                    FsprLimit = c(0.45, 0.3, 0.45), # F45%
                    Ptarget = c(0.4, 0.25, 0.4), # Target is 40% B0
                    Plimit = c(0.1, 0.05, 0.1), # No fishing when SB<SB10
                    Pstar = 0.45,
                    Sigma = 0.5)
  )
  
  ss_run_M_dynamicCat1 <- Rceattle::fit_mod(
    data_list = ss_run_M$data_list,
    inits = ss_run_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_M$data_list$M1_model,
                     M1_use_prior = ss_run_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_M$data_list$srr_prior_sd),
    msmMode = ss_run_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_M$data_list$initMode,
    HCR = build_hcr(HCR = 6, # Cat 1 HCR
                    DynamicHCR = TRUE, # Use dynamic reference points
                    FsprLimit = c(0.45, 0.3, 0.45), # F45%
                    Ptarget = c(0.4, 0.25, 0.4), # Target is 40% B0
                    Plimit = c(0.1, 0.05, 0.1), # No fishing when SB<SB10
                    Pstar = 0.45,
                    Sigma = 0.5)
  )
  
  # * SESSF Tier 1 ----
  ss_run_M_Tier1 <- Rceattle::fit_mod(
    data_list = ss_run_M$data_list,
    inits = ss_run_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_M$data_list$M1_model,
                     M1_use_prior = ss_run_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_M$data_list$srr_prior_sd),
    msmMode = ss_run_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_M$data_list$initMode,
    HCR = build_hcr(HCR = 7, # Tier 1 HCR
                    FsprTarget = 0.48, # F40%
                    FsprLimit = 0.20, # F20%
                    Ptarget = 0.35, # Target is 35% SSB0
                    Plimit = 0.20, # No fishing when B<B20
    )
  )
  
  
  ss_run_M_dynamicTier1 <- Rceattle::fit_mod(
    data_list = ss_run_M$data_list,
    inits = ss_run_M$estimated_params, 
    phase = NULL, 
    estimateMode = 0, # Run projection only
    M1Fun = build_M1(M1_model = ss_run_M$data_list$M1_model,
                     M1_use_prior = ss_run_M$data_list$M1_use_prior,
                     M2_use_prior = ss_run_M$data_list$M2_use_prior),
    recFun = build_srr(srr_fun = ss_run_M$data_list$srr_fun,
                       srr_pred_fun = ss_run_M$data_list$srr_pred_fun,
                       proj_mean_rec = ss_run_M$data_list$proj_mean_rec,
                       srr_est_mode = ss_run_M$data_list$srr_est_mode,
                       srr_prior_mean = ss_run_M$data_list$srr_prior_mean,
                       srr_prior_sd = ss_run_M$data_list$srr_prior_sd),
    msmMode = ss_run_M$data_list$msmMode,
    verbose = 1,
    initMode = ss_run_M$data_list$initMode,
    HCR = build_hcr(HCR = 7, # Tier 1 HCR
                    DynamicHCR = TRUE,
                    FsprTarget = 0.48, # F40%
                    FsprLimit = 0.20, # F20%
                    Ptarget = 0.35, # Target is 35% SSB0
                    Plimit = 0.20, # No fishing when B<B20
    )
  )
  
  
  
  # Plot ----
  M_mod_list <- list(ss_run_M, ss_run_M_AvgF, ss_run_M_Fspr, ss_run_M_Tier3, ss_run_M_dynamicTier3, ss_run_M_Cat1, ss_run_M_dynamicCat1, ss_run_M_Tier1, ss_run_M_dynamicTier1 )
  mod_list <- list(ss_run, ss_run_AvgF, ss_run_Fspr, ss_run_Tier3, ss_run_dynamicTier3, ss_run_Cat1, ss_run_dynamicCat1, ss_run_Tier1, ss_run_dynamicTier1 )
  
  # - SS
  plot_biomass(mod_list, incl_proj = T)
  # plot_ssb(mod_list, incl_proj = T)
  # plot_depletionSSB(mod_list, incl_proj = T)
  # plot_recruitment(mod_list, incl_proj = T)
  # plot_catch(mod_list, incl_proj = TRUE)
  
  # - SS M
  plot_biomass(M_mod_list, incl_proj = T)
  # plot_ssb(M_mod_list, incl_proj = T)
  # plot_depletionSSB(M_mod_list, incl_proj = T)
  # plot_recruitment(M_mod_list, incl_proj = T)
  # plot_catch(M_mod_list, incl_proj = TRUE)
}