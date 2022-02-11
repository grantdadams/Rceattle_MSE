library(Rceattle)
library(dplyr)

################################################
# Set up initial models
################################################
load("~/GitHub/RceattleRuns/GOA/Model runs/GOA_18.5.1/Models/18_5_1_Niter3_2021-06-14.RData")
run_list <- list(mod_list_all[[12]], mod_list_all[[13]])

# Remove halibut
for(i in 1:2){
  # Controls
  run_list[[i]]$data_list$nspp <- 3
  run_list[[i]]$data_list$projyr <- 2060
  run_list[[i]]$data_list$spnames <- run_list[[i]]$data_list$spnames[1:3]
  run_list[[i]]$data_list$nsex <- run_list[[i]]$data_list$nsex[1:3]
  run_list[[i]]$data_list$R_sexr <- run_list[[i]]$data_list$R_sexr[1:3]
  run_list[[i]]$data_list$nages <- run_list[[i]]$data_list$nages[1:3]
  run_list[[i]]$data_list$minage <- run_list[[i]]$data_list$minage[1:3]
  run_list[[i]]$data_list$nlengths <- run_list[[i]]$data_list$nlengths[1:3]
  run_list[[i]]$data_list$pop_wt_index <- run_list[[i]]$data_list$pop_wt_index[1:3]
  run_list[[i]]$data_list$ssb_wt_index <- run_list[[i]]$data_list$ssb_wt_index[1:3]
  run_list[[i]]$data_list$pop_age_transition_index <- run_list[[i]]$data_list$pop_age_transition_index[1:3]
  run_list[[i]]$data_list$sigma_rec_prior <- run_list[[i]]$data_list$sigma_rec_prior[1:3]
  run_list[[i]]$data_list$other_food <- run_list[[i]]$data_list$other_food[1:3]
  run_list[[i]]$data_list$estDynamics <- run_list[[i]]$data_list$estDynamics[1:3]
  run_list[[i]]$data_list$proj_F <- run_list[[i]]$data_list$proj_F[1:3]
  run_list[[i]]$data_list$est_sex_ratio <- run_list[[i]]$data_list$est_sex_ratio[1:3]
  run_list[[i]]$data_list$sex_ratio_sigma <- run_list[[i]]$data_list$sex_ratio_sigma[1:3]
  run_list[[i]]$data_list$est_M1 <- run_list[[i]]$data_list$est_M1[1:3]
  
  # Data sheets
  run_list[[i]]$data_list$emp_sel <- run_list[[i]]$data_list$emp_sel %>%
    filter(Species != 4)
  
  run_list[[i]]$data_list$NByageFixed <- run_list[[i]]$data_list$NByageFixed %>%
    filter(Species != 4) 
  
  run_list[[i]]$data_list$age_trans_matrix <- run_list[[i]]$data_list$age_trans_matrix %>%
    filter(Species != 4) 
  
  run_list[[i]]$data_list$age_error <- run_list[[i]]$data_list$age_error %>%
    filter(Species != 4) 
  
  run_list[[i]]$data_list$wt <- run_list[[i]]$data_list$wt %>%
    filter(Species != 4) 
  
  run_list[[i]]$data_list$pmature <- run_list[[i]]$data_list$pmature %>%
    filter(Species != 4) 
  
  run_list[[i]]$data_list$sex_ratio <- run_list[[i]]$data_list$sex_ratio %>%
    filter(Species != 4) 
  
  run_list[[i]]$data_list$M1_base <- run_list[[i]]$data_list$M1_base %>%
    filter(Species != 4) 
  
  run_list[[i]]$data_list$Mn_LatAge <- run_list[[i]]$data_list$Mn_LatAge %>%
    filter(Species != 4) 
  
  run_list[[i]]$data_list$aLW <- run_list[[i]]$data_list$aLW %>%
    filter(Species != 4) 
  
  # Bioenergetics
  run_list[[i]]$data_list$Ceq <- run_list[[i]]$data_list$Ceq[1:3] 
  run_list[[i]]$data_list$Cindex <- run_list[[i]]$data_list$Cindex[1:3] 
  run_list[[i]]$data_list$Pvalue <- run_list[[i]]$data_list$Pvalue[1:3] 
  run_list[[i]]$data_list$fday <- run_list[[i]]$data_list$fday[1:3] 
  run_list[[i]]$data_list$CA <- run_list[[i]]$data_list$CA[1:3] 
  run_list[[i]]$data_list$CB <- run_list[[i]]$data_list$CB[1:3] 
  run_list[[i]]$data_list$Qc <- run_list[[i]]$data_list$Qc[1:3] 
  run_list[[i]]$data_list$Tco <- run_list[[i]]$data_list$Tco[1:3] 
  run_list[[i]]$data_list$Tcm <- run_list[[i]]$data_list$Tcm[1:3] 
  run_list[[i]]$data_list$Tcl <- run_list[[i]]$data_list$Tcl[1:3] 
  run_list[[i]]$data_list$CK1 <- run_list[[i]]$data_list$CK1[1:3] 
  run_list[[i]]$data_list$CK4 <- run_list[[i]]$data_list$CK4[1:3] 
  
  # Diet data
  run_list[[i]]$data_list$Pyrs <- run_list[[i]]$data_list$Pyrs %>%
    filter(Species != 4) 
  
  run_list[[i]]$data_list$UobsAge <- run_list[[i]]$data_list$UobsAge %>%
    filter(Pred != 4) %>%
    filter(Prey != 4) 
  
  run_list[[i]]$data_list$UobsWtAge <- run_list[[i]]$data_list$UobsWtAge %>%
    filter(Pred != 4) %>%
    filter(Prey != 4) 
}
ss_run <-  run_list[[1]]
ms_run <-  run_list[[2]]


# Ratio of F across Pcod fleets
avg_F <- (exp(ss_run$estimated_params$ln_mean_F+ss_run$estimated_params$F_dev)) # Average F from last 2 years
avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
f_ratio <- avg_F[14:16]
f_ratio <- f_ratio/sum(f_ratio)

# Adjust future F proportion to each fleet
ss_run$data_list$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio)
ss_run$estimated_params$proj_F_prop <- ss_run$data_list$fleet_control$proj_F_prop

# Single species run - Fixed M
ss_run$data_list$sigma_rec_prior <- exp(ss_run$estimated_params$ln_rec_sigma)[1:3]
ss_run <- Rceattle::fit_mod(data_list = ss_run$data_list,
                            inits = NULL, # Initial parameters from single species ests
                            file = NULL, # Don't save
                            estimate = 0, # Estimate
                            niter = 1, # 10 iterations around population and predation dynamics
                            msmMode = ss_run$data_list$msmMode, # MSVPA based
                            phase = "default",
                            verbose = 1)

# Single-species run - Estimate M
data_listM <- ss_run$data_list
data_listM$est_M1 <- c(1,2,1)
ss_run_M <- Rceattle::fit_mod(data_list = data_listM,
                              inits = ss_run$estimated_params, # Initial parameters from single species ests
                              file = NULL, # Don't save
                              estimate = 0, # Estimate
                              niter = 1, # 10 iterations around population and predation dynamics
                              msmMode = ss_run$data_list$msmMode, # MSVPA based
                              phase = "default",
                              verbose = 1)

# Update projections
ms_run$data_list$sigma_rec_prior <- exp(ms_run$estimated_params$ln_rec_sigma)[1:3]
ms_run <- Rceattle::fit_mod(data_list = ms_run$data_list,
                            inits = ss_run$estimated_params, # Initial parameters from single species ests
                            estimate = 0, # Estimate
                            niter = 3, # 10 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = ms_run$data_list$msmMode, # MSVPA based
                            suitMode = 0, # empirical suitability
                            phase = "default",
                            verbose = 1)

save(ss_run, ss_run_M, ms_run, file = "Models/GOA_Models.RData")

################################################
# Fixed M w/ harvest control rules
################################################
# -- Avg F
avg_F <- (exp(ss_run$estimated_params$ln_mean_F+ss_run$estimated_params$F_dev)) # Average F from last 5 years
avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])[1:3]

ss_run_AvgF <- fit_mod(data_list = ss_run$data_list,
                       inits = ss_run$estimated_params, # Initial parameters from ss_run_M
                       estimateMode = 2, # Run projection only
                       HCR = build_hcr(HCR = 2, # Input F
                                       FsprTarget = avg_F # F40%
                       ),
                       msmMode = 0, # Single species mode
                       verbose = 2)

# -- Constant Fspr
ss_run_Fspr <- Rceattle::fit_mod(data_list = ss_run$data_list,
                                 inits = ss_run$estimated_params, # Initial parameters from ss_run
                                 estimateMode = 2, # Run projection only
                                 HCR = build_hcr(HCR = 4, # Tier3 HCR
                                                 FsprTarget = 0.4 # F40%
                                 ),
                                 msmMode = 0, # Single species mode
                                 verbose = 1)



# -- Dynamic F as a percentage of SB0
ss_run_dynamicfb0 <- Rceattle::fit_mod(data_list = ss_run$data_list,
                                       inits = ss_run$estimated_params, # Initial parameters from ss_run
                                       estimateMode = 2, # Run projection only
                                       HCR = build_hcr(HCR = 3, # Constant F HCR
                                                       DynamicHCR = TRUE, # Use dynamic reference points
                                                       FsprTarget = 0.4), # F that achieves 40% SB0
                                       msmMode = 0, # Single species mode
                                       verbose = 1)


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


ss_run_dynamicTier3 <- Rceattle::fit_mod(data_list = ss_run$data_list,
                                         inits = ss_run$estimated_params, # Initial parameters from ss_run
                                         estimateMode = 2, # Run projection only
                                         HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                         DynamicHCR = TRUE, # Use dynamic reference points
                                                         FsprTarget = 0.4, # F40%
                                                         FsprLimit = 0.35, # F35%
                                                         Plimit = 0.2, # No fishing when SB<SB20
                                                         Alpha = 0.2),
                                         msmMode = 0, # Single species mode
                                         verbose = 1)

# -- PFMC Category 1
ss_run_Cat1 <- Rceattle::fit_mod(data_list = ss_run$data_list,
                                 inits = ss_run$estimated_params, # Initial parameters from ss_run
                                 estimateMode = 2, # Run projection only
                                 HCR = build_hcr(HCR = 6, # Cat 1 HCR
                                                 FsprLimit = 0.45, # F45%
                                                 Ptarget = 0.4, # Target is 40% B0
                                                 Plimit = 0.1, # No fishing when SB<SB10
                                                 Pstar = 0.45,
                                                 Sigma = 0.5),
                                 msmMode = 0, # Single species mode
                                 verbose = 1)

ss_run_dynamicCat1 <- Rceattle::fit_mod(data_list = ss_run$data_list,
                                        inits = ss_run$estimated_params, # Initial parameters from ss_run
                                        estimateMode = 2, # Run projection only
                                        HCR = build_hcr(HCR = 6, # Cat 1 HCR
                                                        DynamicHCR = TRUE, # Use dynamic reference points
                                                        FsprLimit = 0.45, # F45%
                                                        Ptarget = 0.4, # Target is 40% SB0
                                                        Plimit = 0.1, # No fishing when SB<SB10
                                                        Pstar = 0.45,
                                                        Sigma = 0.5),
                                        msmMode = 0, # Single species mode
                                        verbose = 1)

# -- SESSF Tier 1
ss_run_Tier1 <- Rceattle::fit_mod(data_list = ss_run$data_list,
                                  inits = ss_run$estimated_params, # Initial parameters from ss_run
                                  estimateMode = 2, # Run projection only
                                  HCR = build_hcr(HCR = 7, # Tier 1 HCR
                                                  FsprTarget = 0.48, # F40%
                                                  FsprLimit = 0.20, # F20%
                                                  Ptarget = 0.35, # Target is 35% SSB0
                                                  Plimit = 0.20, # No fishing when B<B20
                                  ),
                                  msmMode = 0, # Single species mode
                                  verbose = 1)


ss_run_dynamicTier1 <- Rceattle::fit_mod(data_list = ss_run$data_list,
                                         inits = ss_run$estimated_params, # Initial parameters from ss_run
                                         estimateMode = 2, # Run projection only
                                         HCR = build_hcr(HCR = 7, # Tier 1 HCR
                                                         DynamicHCR = TRUE,
                                                         FsprTarget = 0.48, # F40%
                                                         FsprLimit = 0.20, # F20%
                                                         Ptarget = 0.35, # Target is 35% SSB0
                                                         Plimit = 0.20, # No fishing when B<B20
                                         ),
                                         msmMode = 0, # Single species mode
                                         verbose = 1)



################################################
# Estimate M w/ harvest control rules
###############################################
# -- Avg F
avg_F <- (exp(ss_run_M$estimated_params$ln_mean_F+ss_run_M$estimated_params$F_dev)) # Average F from last 5 years
avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])

ss_run_M_AvgF <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
                                   inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                   estimateMode = 2, # Run projection only
                                   HCR = build_hcr(HCR = 2, # Input F
                                                   FsprTarget = avg_F # F40%
                                   ),
                                   msmMode = 0, # Single species mode
                                   verbose = 1)

# -- Constant Fspr
ss_run_M_Fspr <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
                                   inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                   estimateMode = 2, # Run projection only
                                   HCR = build_hcr(HCR = 4, # Tier3 HCR
                                                   FsprTarget = 0.4 # F40%
                                   ),
                                   msmMode = 0, # Single species mode
                                   verbose = 1)



# -- Dynamic F as a percentage of SB0
ss_run_M_dynamicfb0 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
                                         inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                         estimateMode = 2, # Run projection only
                                         HCR = build_hcr(HCR = 3, # Constant F HCR
                                                         DynamicHCR = TRUE, # Use dynamic reference points
                                                         FsprTarget = 0.4), # F that achieves 40% SB0
                                         msmMode = 0, # Single species mode
                                         verbose = 1)


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


ss_run_M_dynamicTier3 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
                                           inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                           estimateMode = 2, # Run projection only
                                           HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                           DynamicHCR = TRUE, # Use dynamic reference points
                                                           FsprTarget = 0.4, # F40%
                                                           FsprLimit = 0.35, # F35%
                                                           Plimit = 0.2, # No fishing when SB<SB20
                                                           Alpha = 0.2),
                                           msmMode = 0, # Single species mode
                                           verbose = 1)

# -- PFMC Category 1
ss_run_M_Cat1 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
                                   inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                   estimateMode = 2, # Run projection only
                                   HCR = build_hcr(HCR = 6, # Cat 1 HCR
                                                   FsprLimit = 0.45, # F45%
                                                   Ptarget = 0.4, # Target is 40% B0
                                                   Plimit = 0.1, # No fishing when SB<SB10
                                                   Pstar = 0.45,
                                                   Sigma = 0.5),
                                   msmMode = 0, # Single species mode
                                   verbose = 1)

ss_run_M_dynamicCat1 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
                                          inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                          estimateMode = 2, # Run projection only
                                          HCR = build_hcr(HCR = 6, # Cat 1 HCR
                                                          DynamicHCR = TRUE, # Use dynamic reference points
                                                          FsprLimit = 0.45, # F45%
                                                          Ptarget = 0.4, # Target is 40% SB0
                                                          Plimit = 0.1, # No fishing when SB<SB10
                                                          Pstar = 0.45,
                                                          Sigma = 0.5),
                                          msmMode = 0, # Single species mode
                                          verbose = 1)

# -- SESSF Tier 1
ss_run_M_Tier1 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
                                    inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                    estimateMode = 2, # Run projection only
                                    HCR = build_hcr(HCR = 7, # Tier 1 HCR
                                                    FsprTarget = 0.48, # F40%
                                                    FsprLimit = 0.20, # F20%
                                                    Ptarget = 0.35, # Target is 35% SSB0
                                                    Plimit = 0.20, # No fishing when B<B20
                                    ),
                                    msmMode = 0, # Single species mode
                                    verbose = 1)


ss_run_M_dynamicTier1 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
                                           inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                           estimateMode = 2, # Run projection only
                                           HCR = build_hcr(HCR = 7, # Tier 1 HCR
                                                           DynamicHCR = TRUE,
                                                           FsprTarget = 0.48, # F40%
                                                           FsprLimit = 0.20, # F20%
                                                           Ptarget = 0.35, # Target is 35% SSB0
                                                           Plimit = 0.20, # No fishing when B<B20
                                           ),
                                           msmMode = 0, # Single species mode
                                           verbose = 1)



################################################
# Management strategy evaluation
################################################
### OMS
# 1. Single-species estimate M
# 2. Multi-species type II
om_list <- list(ss_run_M, ms_run)

## Rec scenarios
# 1. Constant
# 2. Linear increase 1.5
# 3. Linear decrease
rec_scen <- list(1, 1.5, 0.5)

### Management strategies
## EM
# 1. Single-species fix M
# 2. Single-species estimate M

# HCR
# 1, NPFMC Tier 3 HCR
# 1b. NPFMC Tier 3 dynamic HCR 
# 2. PFMC Category 1 40-10 HCR
# 2b. PFMC Category 1 40-10 dynamic HCR
# 3. SESSF Tier 1 HCR
# 3b. SESSF Tier 1 dynamic HCR
# 4. Average F
# 5. Equilibrium F_(40%)
# 6. Dynamic F_40
# 7. SESSF Tier 4 HCR (CPUE based)
# 8. No catch
em1_hcr_list <- list(ss_run_Tier3, ss_run_dynamicTier3, ss_run_Cat1, ss_run_dynamicCat1, ss_run_Tier1, ss_run_dynamicTier1, ss_run_AvgF, ss_run_Fspr, ss_run_dynamicfb0)

em2_hcr_list <- list(ss_run_M_Tier3, ss_run_M_dynamicTier3, ss_run_M_Cat1, ss_run_M_dynamicCat1, ss_run_M_Tier1, ss_run_M_dynamicTier1, ss_run_M_AvgF, ss_run_M_Fspr, ss_run_M_dynamicfb0)

## Cap
# 1. 1,500,000 mt cap for pollock and Max historical catch for Arrowtooth flounder
# 2. Max historical catch for Arrowtooth flounder
# 3. No cap
max_atf <- ss_run$data_list$fsh_biom
max_atf <- max_atf[which(max_atf$Species == 2),]

# Pollock, cod, atf
cap_list <- list(
  one = c(1e10, max(max_atf$Catch, na.rm = TRUE), 1e10, 1e10),
  two = c(1e10, max(max_atf$Catch, na.rm = TRUE), 1e10, 1e10)
)


### Run MSEs
## Loop across OMs,
for(om in 1:length(om_list)){ # OM model
  for(rec in 1:length(rec_scen)){ # Future recruitment
    for(var in 1:length(data_var)){ # Data sampling
      
      
      
    }
  }
}

ss_run$data_list$fleet_control[,1:3]
sampling_period <- c(2,2,1,2,2,2,2,1,2,2,1,2,2,1,1,1)

# -- NPFMC Tier 3 HCRs
# - MS-OM: SS-EM Tier 3 HCR
mse1 <- mse_run(om = ms_run, em = ss_run_Tier3, nsim = 10, assessment_period = 1, sampling_period = sampling_period, simulate = TRUE, cap = cap_list[[1]], dir = "Runs/GOA/MS_OM/SS_Tier3_EM/ConstantR/Cap1", file = NULL)

# - SS-OM: SS-EM Tier 3 HCR
mse2 <- mse_run(om = ss_run_M, em = ss_run_Tier3, nsim = 10, assessment_period = 1, sampling_period = sampling_period, simulate = TRUE, cap = cap_list[[1]], dir = "Runs/GOA/SS_M_OM/SS_Tier3_EM/ConstantR/Cap1", file = NULL)
