library(Rceattle)

load("~/GitHub/Rceattle_MSE/Models/GOA_18_5_1_2022-02-18.RData")

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
ms_run <- mod_list_all[[3]]




# ################################################
# # Fixed M w/ harvest control rules
# ################################################
# # -- Avg F
# avg_F <- (exp(ss_run$estimated_params$ln_mean_F+ss_run$estimated_params$F_dev)) # Average F from last 5 years
# avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])[1:3]
# 
# ss_run_AvgF <- fit_mod(data_list = ss_run$data_list,
#                        inits = ss_run$estimated_params, # Initial parameters from ss_run_M
#                        estimateMode = 2, # Run projection only
#                        HCR = build_hcr(HCR = 2, # Input F
#                                        FsprTarget = avg_F # F40%
#                        ),
#                        msmMode = 0, # Single species mode
#                        verbose = 2)
# 
# # -- Constant Fspr
# ss_run_Fspr <- Rceattle::fit_mod(data_list = ss_run$data_list,
#                                  inits = ss_run$estimated_params, # Initial parameters from ss_run
#                                  estimateMode = 2, # Run projection only
#                                  HCR = build_hcr(HCR = 4, # Tier3 HCR
#                                                  FsprTarget = 0.4 # F40%
#                                  ),
#                                  msmMode = 0, # Single species mode
#                                  verbose = 1)
# 
# 
# 
# # -- Dynamic F as a percentage of SB0
# ss_run_dynamicfb0 <- Rceattle::fit_mod(data_list = ss_run$data_list,
#                                        inits = ss_run$estimated_params, # Initial parameters from ss_run
#                                        estimateMode = 2, # Run projection only
#                                        HCR = build_hcr(HCR = 3, # Constant F HCR
#                                                        DynamicHCR = TRUE, # Use dynamic reference points
#                                                        FsprTarget = 0.4), # F that achieves 40% SB0
#                                        msmMode = 0, # Single species mode
#                                        verbose = 1)


# -- NPFMC Tier 3
ss_run_Tier3 <- Rceattle::fit_mod(data_list = ss_run$data_list,
                                  inits = ss_run$estimated_params, # Initial parameters from ss_run
                                  estimateMode = 2, # Run projection only
                                  HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                  FsprTarget = 0.4, # F40%
                                                  FsprLimit = 0.35, # F35%
                                                  Plimit = 0.2, # No fishing when SB<SB20
                                                  Alpha = 0.2),
                                  msmMode = 0, # Single species mode
                                  verbose = 1)

# 
# ss_run_dynamicTier3 <- Rceattle::fit_mod(data_list = ss_run$data_list,
#                                          inits = ss_run$estimated_params, # Initial parameters from ss_run
#                                          estimateMode = 2, # Run projection only
#                                          HCR = build_hcr(HCR = 5, # Tier3 HCR
#                                                          DynamicHCR = TRUE, # Use dynamic reference points
#                                                          FsprTarget = 0.4, # F40%
#                                                          FsprLimit = 0.35, # F35%
#                                                          Plimit = 0.2, # No fishing when SB<SB20
#                                                          Alpha = 0.2),
#                                          msmMode = 0, # Single species mode
#                                          verbose = 1)
# 
# # -- PFMC Category 1
# ss_run_Cat1 <- Rceattle::fit_mod(data_list = ss_run$data_list,
#                                  inits = ss_run$estimated_params, # Initial parameters from ss_run
#                                  estimateMode = 2, # Run projection only
#                                  HCR = build_hcr(HCR = 6, # Cat 1 HCR
#                                                  FsprLimit = 0.45, # F45%
#                                                  Ptarget = 0.4, # Target is 40% B0
#                                                  Plimit = 0.1, # No fishing when SB<SB10
#                                                  Pstar = 0.45,
#                                                  Sigma = 0.5),
#                                  msmMode = 0, # Single species mode
#                                  verbose = 1)
# 
# ss_run_dynamicCat1 <- Rceattle::fit_mod(data_list = ss_run$data_list,
#                                         inits = ss_run$estimated_params, # Initial parameters from ss_run
#                                         estimateMode = 2, # Run projection only
#                                         HCR = build_hcr(HCR = 6, # Cat 1 HCR
#                                                         DynamicHCR = TRUE, # Use dynamic reference points
#                                                         FsprLimit = 0.45, # F45%
#                                                         Ptarget = 0.4, # Target is 40% SB0
#                                                         Plimit = 0.1, # No fishing when SB<SB10
#                                                         Pstar = 0.45,
#                                                         Sigma = 0.5),
#                                         msmMode = 0, # Single species mode
#                                         verbose = 1)
# 
# # -- SESSF Tier 1
# ss_run_Tier1 <- Rceattle::fit_mod(data_list = ss_run$data_list,
#                                   inits = ss_run$estimated_params, # Initial parameters from ss_run
#                                   estimateMode = 2, # Run projection only
#                                   HCR = build_hcr(HCR = 7, # Tier 1 HCR
#                                                   FsprTarget = 0.48, # F40%
#                                                   FsprLimit = 0.20, # F20%
#                                                   Ptarget = 0.35, # Target is 35% SSB0
#                                                   Plimit = 0.20, # No fishing when B<B20
#                                   ),
#                                   msmMode = 0, # Single species mode
#                                   verbose = 1)
# 
# 
# ss_run_dynamicTier1 <- Rceattle::fit_mod(data_list = ss_run$data_list,
#                                          inits = ss_run$estimated_params, # Initial parameters from ss_run
#                                          estimateMode = 2, # Run projection only
#                                          HCR = build_hcr(HCR = 7, # Tier 1 HCR
#                                                          DynamicHCR = TRUE,
#                                                          FsprTarget = 0.48, # F40%
#                                                          FsprLimit = 0.20, # F20%
#                                                          Ptarget = 0.35, # Target is 35% SSB0
#                                                          Plimit = 0.20, # No fishing when B<B20
#                                          ),
#                                          msmMode = 0, # Single species mode
#                                          verbose = 1)
# 
# 
# 
# ################################################
# # Estimate M w/ harvest control rules
# ###############################################
# # -- Avg F
# avg_F <- (exp(ss_run_M$estimated_params$ln_mean_F+ss_run_M$estimated_params$F_dev)) # Average F from last 5 years
# avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])
# 
# ss_run_M_AvgF <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
#                                    inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
#                                    estimateMode = 2, # Run projection only
#                                    HCR = build_hcr(HCR = 2, # Input F
#                                                    FsprTarget = avg_F # F40%
#                                    ),
#                                    msmMode = 0, # Single species mode
#                                    verbose = 1)
# 
# # -- Constant Fspr
# ss_run_M_Fspr <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
#                                    inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
#                                    estimateMode = 2, # Run projection only
#                                    HCR = build_hcr(HCR = 4, # Tier3 HCR
#                                                    FsprTarget = 0.4 # F40%
#                                    ),
#                                    msmMode = 0, # Single species mode
#                                    verbose = 1)
# 
# 
# 
# # -- Dynamic F as a percentage of SB0
# ss_run_M_dynamicfb0 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
#                                          inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
#                                          estimateMode = 2, # Run projection only
#                                          HCR = build_hcr(HCR = 3, # Constant F HCR
#                                                          DynamicHCR = TRUE, # Use dynamic reference points
#                                                          FsprTarget = 0.4), # F that achieves 40% SB0
#                                          msmMode = 0, # Single species mode
#                                          verbose = 1)


# -- NPFMC Tier 3
ss_run_M_Tier3 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
                                    inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                    estimateMode = 2, # Run projection only
                                    HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                    FsprTarget = 0.4, # F40%
                                                    FsprLimit = 0.35, # F35%
                                                    Plimit = 0.2, # No fishing when SB<SB20
                                                    Alpha = 0.2),
                                    msmMode = 0, # Single species mode
                                    verbose = 1)


# ss_run_M_dynamicTier3 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
#                                            inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
#                                            estimateMode = 2, # Run projection only
#                                            HCR = build_hcr(HCR = 5, # Tier3 HCR
#                                                            DynamicHCR = TRUE, # Use dynamic reference points
#                                                            FsprTarget = 0.4, # F40%
#                                                            FsprLimit = 0.35, # F35%
#                                                            Plimit = 0.2, # No fishing when SB<SB20
#                                                            Alpha = 0.2),
#                                            msmMode = 0, # Single species mode
#                                            verbose = 1)
# 
# # -- PFMC Category 1
# ss_run_M_Cat1 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
#                                    inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
#                                    estimateMode = 2, # Run projection only
#                                    HCR = build_hcr(HCR = 6, # Cat 1 HCR
#                                                    FsprLimit = 0.45, # F45%
#                                                    Ptarget = 0.4, # Target is 40% B0
#                                                    Plimit = 0.1, # No fishing when SB<SB10
#                                                    Pstar = 0.45,
#                                                    Sigma = 0.5),
#                                    msmMode = 0, # Single species mode
#                                    verbose = 1)
# 
# ss_run_M_dynamicCat1 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
#                                           inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
#                                           estimateMode = 2, # Run projection only
#                                           HCR = build_hcr(HCR = 6, # Cat 1 HCR
#                                                           DynamicHCR = TRUE, # Use dynamic reference points
#                                                           FsprLimit = 0.45, # F45%
#                                                           Ptarget = 0.4, # Target is 40% SB0
#                                                           Plimit = 0.1, # No fishing when SB<SB10
#                                                           Pstar = 0.45,
#                                                           Sigma = 0.5),
#                                           msmMode = 0, # Single species mode
#                                           verbose = 1)
# 
# # -- SESSF Tier 1
# ss_run_M_Tier1 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
#                                     inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
#                                     estimateMode = 2, # Run projection only
#                                     HCR = build_hcr(HCR = 7, # Tier 1 HCR
#                                                     FsprTarget = 0.48, # F40%
#                                                     FsprLimit = 0.20, # F20%
#                                                     Ptarget = 0.35, # Target is 35% SSB0
#                                                     Plimit = 0.20, # No fishing when B<B20
#                                     ),
#                                     msmMode = 0, # Single species mode
#                                     verbose = 1)
# 
# 
# ss_run_M_dynamicTier1 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
#                                            inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
#                                            estimateMode = 2, # Run projection only
#                                            HCR = build_hcr(HCR = 7, # Tier 1 HCR
#                                                            DynamicHCR = TRUE,
#                                                            FsprTarget = 0.48, # F40%
#                                                            FsprLimit = 0.20, # F20%
#                                                            Ptarget = 0.35, # Target is 35% SSB0
#                                                            Plimit = 0.20, # No fishing when B<B20
#                                            ),
#                                            msmMode = 0, # Single species mode
#                                            verbose = 1)
