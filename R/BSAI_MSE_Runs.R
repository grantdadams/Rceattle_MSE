library(Rceattle)

################################################
# Data
################################################
# Example
# To run the 2017 single species assessment for the Bering Sea, a data file must first be loaded:
data(BS2017SS) # ?BS2017SS for more information on the data
BS2017SS$projyr <- 2060


################################################
# Estimation
################################################
# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
BS2017SS$fleet_control$proj_F_prop <-rep(1,7)
ss_run <- Rceattle::fit_mod(data_list = BS2017SS,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 1, # Estimate hindcast only
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            phase = "default",
                            verbose = 1)

# Estimate single-species and estimate M
BS2017SS_M <- BS2017SS
BS2017SS_M$est_M1 <- c(1,1,1)
ss_run_M <- Rceattle::fit_mod(data_list = BS2017SS_M,
                              inits = ss_run$estimated_params, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 1, # Estimate hindcast only
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              phase = "default",
                              verbose = 1)



# For the a multispecies model starting from the single species parameters, the following can be specified to load the data:
data("BS2017MS") # Note: the only difference is the residual mortality (M1_base) is lower
BS2017MS$est_M1 <- c(1,1,1) # Estimate residual M
BS2017MS$projyr <- 2060
ms_run <- Rceattle::fit_mod(data_list = BS2017MS,
                            inits = ss_run_M$estimated_params, # Initial parameters from single species ests
                            file = NULL, # Don't save
                            estimateMode = 1, # Estimate hindcast only
                            niter = 3, # 10 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # MSVPA based
                            suitMode = 0, # empirical suitability
                            verbose = 1)



################################################
# Fixed M w/ harvest control rules
################################################
# -- Avg F
avg_F <- (exp(ss_run$estimated_params$ln_mean_F+ss_run$estimated_params$F_dev)) # Average F from last 5 years
avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])[1:3]

ss_run_AvgF <- fit_mod(data_list = BS2017SS,
                                   inits = ss_run$estimated_params, # Initial parameters from ss_run_M
                                   estimateMode = 2, # Run projection only
                                   HCR = build_hcr(HCR = 2, # Input F
                                                   FsprTarget = avg_F # F40%
                                   ),
                                   msmMode = 0, # Single species mode
                                   verbose = 2)

# -- Constant Fspr
ss_run_Fspr <- Rceattle::fit_mod(data_list = BS2017SS,
                                 inits = ss_run$estimated_params, # Initial parameters from ss_run
                                 estimateMode = 2, # Run projection only
                                 HCR = build_hcr(HCR = 4, # Tier3 HCR
                                                 FsprTarget = 0.4 # F40%
                                 ),
                                 msmMode = 0, # Single species mode
                                 verbose = 1)



# -- Dynamic F as a percentage of SB0
ss_run_dynamicfb0 <- Rceattle::fit_mod(data_list = BS2017SS,
                                       inits = ss_run$estimated_params, # Initial parameters from ss_run
                                       estimateMode = 2, # Run projection only
                                       HCR = build_hcr(HCR = 3, # Constant F HCR
                                                       DynamicHCR = TRUE, # Use dynamic reference points
                                                       FsprTarget = 0.4), # F that achieves 40% SB0
                                       msmMode = 0, # Single species mode
                                       verbose = 1)


# -- NPFMC Tier 3
ss_run_Tier3 <- Rceattle::fit_mod(data_list = BS2017SS,
                                  inits = ss_run$estimated_params, # Initial parameters from ss_run
                                  estimateMode = 2, # Run projection only
                                  HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                  FsprTarget = 0.4, # F40%
                                                  FsprLimit = 0.35, # F35%
                                                  Plimit = 0.2, # No fishing when SB<SB20
                                                  Alpha = 0.2),
                                  msmMode = 0, # Single species mode
                                  verbose = 1)


ss_run_dynamicTier3 <- Rceattle::fit_mod(data_list = BS2017SS,
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
ss_run_Cat1 <- Rceattle::fit_mod(data_list = BS2017SS,
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

ss_run_dynamicCat1 <- Rceattle::fit_mod(data_list = BS2017SS,
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
ss_run_Tier1 <- Rceattle::fit_mod(data_list = BS2017SS,
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


ss_run_dynamicTier1 <- Rceattle::fit_mod(data_list = BS2017SS,
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

ss_run_M_AvgF <- Rceattle::fit_mod(data_list = BS2017SS_M,
                                   inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                   estimateMode = 2, # Run projection only
                                   HCR = build_hcr(HCR = 2, # Input F
                                                   FsprTarget = avg_F # F40%
                                   ),
                                   msmMode = 0, # Single species mode
                                   verbose = 1)

# -- Constant Fspr
ss_run_M_Fspr <- Rceattle::fit_mod(data_list = BS2017SS_M,
                                   inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                   estimateMode = 2, # Run projection only
                                   HCR = build_hcr(HCR = 4, # Tier3 HCR
                                                   FsprTarget = 0.4 # F40%
                                   ),
                                   msmMode = 0, # Single species mode
                                   verbose = 1)



# -- Dynamic F as a percentage of SB0
ss_run_M_dynamicfb0 <- Rceattle::fit_mod(data_list = BS2017SS_M,
                                         inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                         estimateMode = 2, # Run projection only
                                         HCR = build_hcr(HCR = 3, # Constant F HCR
                                                         DynamicHCR = TRUE, # Use dynamic reference points
                                                         FsprTarget = 0.4), # F that achieves 40% SB0
                                         msmMode = 0, # Single species mode
                                         verbose = 1)


# -- NPFMC Tier 3
ss_run_M_Tier3 <- Rceattle::fit_mod(data_list = BS2017SS_M,
                                    inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                    estimateMode = 2, # Run projection only
                                    HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                    FsprTarget = 0.4, # F40%
                                                    FsprLimit = 0.35, # F35%
                                                    Plimit = 0.2, # No fishing when SB<SB20
                                                    Alpha = 0.2),
                                    msmMode = 0, # Single species mode
                                    verbose = 1)


ss_run_M_dynamicTier3 <- Rceattle::fit_mod(data_list = BS2017SS_M,
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
ss_run_M_Cat1 <- Rceattle::fit_mod(data_list = BS2017SS_M,
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

ss_run_M_dynamicCat1 <- Rceattle::fit_mod(data_list = BS2017SS_M,
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
ss_run_M_Tier1 <- Rceattle::fit_mod(data_list = BS2017SS_M,
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


ss_run_M_dynamicTier1 <- Rceattle::fit_mod(data_list = BS2017SS_M,
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
max_atf <- max_atf[which(max_atf$Species == 3),]

# Pollock, cod, atf
cap_list <- list(
  one = c(1500000, 1e10, max(max_atf$Catch, na.rm = TRUE)),
  two = c(1e10, 1e10, max(max_atf$Catch, na.rm = TRUE)),
  three = c(1e10, 1e10, 1e10)
)


### Run MSEs
## Loop across OMs,
for(om in 1:length(om_list)){ # OM model
  for(rec in 1:length(rec_scen)){ # Future recruitment
    for(var in 1:length(data_var)){ # Data sampling
      
      
      
    }
  }
}



# -- NPFMC Tier 3 HCRs
# - MS-OM: SS-EM Tier 3 HCR
mse1 <- mse_run(om = ms_run, em = ss_run_Tier3, nsim = 1, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, cap = cap_list[[1]], dir = "Runs/EBS/MS_OM/SS_Tier3_EM/ConstantR/Cap1", file = NULL)

# - SS-OM: SS-EM Tier 3 HCR
mse2 <- mse_run(om = ss_run_M, em = ss_run_Tier3, nsim = 10, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, cap = cap_list[[1]], dir = "Runs/EBS/SS_M_OM/SS_Tier3_EM/ConstantR/Cap1", file = NULL)
