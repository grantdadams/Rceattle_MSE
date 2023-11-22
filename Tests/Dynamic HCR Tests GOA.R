library(Rceattle)

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
library(dplyr)


# -- NPFMC Tier 3
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


mod_list <- list(ss_run, ss_run_Tier3, ss_run_dynamicTier3)
plot_recruitment(mod_list, incl_proj = TRUE)
plot_ssb(mod_list, incl_proj = TRUE)
plot_depletionSSB(mod_list, incl_proj = TRUE)
sapply(mod_list, function(x) tail(x$quantities$R[3,]))
sapply(mod_list, function(x) tail(x$quantities$SB0[3,82]))
sapply(mod_list, function(x) tail(x$quantities$DynamicSB0[3,82]))
sapply(mod_list, function(x) tail(x$quantities$NByage0[3,1,1,]))
sapply(mod_list, function(x) tail(x$quantities$DynamicNByage0[3,1,1,]))
sapply(mod_list, function(x) tail(x$quantities$DynamicNByageF[1,1,1,]))