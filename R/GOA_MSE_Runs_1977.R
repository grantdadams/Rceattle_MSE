################################################
# Set-up
################################################
source("R/GOA_condition_models_1977.R")

## Cap
# 1. Max historical catch for Arrowtooth flounder
# 2. No cap
max_atf <- ss_run$data_list$fsh_biom
max_atf <- max_atf[which(max_atf$Species == 2),]

# Pollock, cod, atf
cap_list <- list(
  one = c(1e10, max(max_atf$Catch, na.rm = TRUE), 1e10), # Historical ATF
  two = c(1e10, 1e10, 1e10) # No cap
)

## Sampling period
sampling_period <- c(2,2,1,2,2,2,2,1,2,2,1,2,2,1,1,1)


################################################
# Base MSEs (Tier 3 HCR no cap)
################################################
## No cap
# -- NPFMC Tier 3 HCRs
# - MS-OM: SS-EM Tier 3 HCR
mse1 <- mse_run(om = ms_run, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, cap = NULL, dir = "Runs/GOA1977/MS_OM/SS_Tier3_EM/ConstantR/No cap", file = NULL)

# - MS-OM: SSM-EM Tier 3 HCR
mse2 <- mse_run(om = ms_run, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, cap = NULL, dir = "Runs/GOA1977/MS_OM/SS_M_Tier3_EM/ConstantR/No cap", file = NULL)

# - SS-OM: SS-EM Tier 3 HCR
mse3 <- mse_run(om = ss_run_Tier3, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, cap = NULL, dir = "Runs/GOA1977/SS_OM/SS_Tier3_EM/ConstantR/No cap", file = NULL)

# - SS-OM: SSM-EM Tier 3 HCR
mse4 <- mse_run(om = ss_run_Tier3, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, cap = NULL, dir = "Runs/GOA1977/SS_OM/SS_M_Tier3_EM/ConstantR/No cap", file = NULL)

# - SS-OM: SS-EM Tier 3 HCR
mse5 <- mse_run(om = ss_run_M_Tier3, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, cap = NULL, dir = "Runs/GOA1977/SSM_OM/SS_Tier3_EM/ConstantR/No cap", file = NULL)

# - SS-OM: SSM-EM Tier 3 HCR
mse6 <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, cap = NULL, dir = "Runs/GOA1977/SSM_OM/SS_M_Tier3_EM/ConstantR/No cap", file = NULL)


################################################
# Management strategy evaluation
################################################
### OMS
# 1. Single-species estimate M
# 2. Multi-species type II
om_list <- list(ss_run_Tier3, ss_run_M_Tier3, ms_run_f25)
om_names = c("SS_OM", "SSM_OM", "MS_OM")

## Rec scenarios
# 1. Constant
# 2. Linear increase 1.5
# 3. Linear decrease
rec_scen <- list(1)

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
em_hcr_list <- list(ss_run_Tier3, ss_run_dynamicTier3, ss_run_Cat1, ss_run_dynamicCat1, ss_run_Tier1, ss_run_dynamicTier1, ss_run_AvgF, ss_run_Fspr, # Fixed M
                    ss_run_M_Tier3, ss_run_M_dynamicTier3, ss_run_M_Cat1, ss_run_M_dynamicCat1, ss_run_M_Tier1, ss_run_M_dynamicTier1, ss_run_M_AvgF, ss_run_M_Fspr # Estimate M
)

em_hcr_names <- c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_AvgF_EM", "SS_fixM_Fspr_EM", # Fixed M
                  "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_AvgF_EM", "SS_estM_Fspr_EM")



### Set up parallel processing
library(foreach)
library(doParallel)

cores = detectCores() - 2
registerDoParallel(cores)


### Run MSEs
## Loop across OMs,
stime <- system.time({
  mse <- foreach(om = 1:length(om_list)) %:%  # OM model
    foreach(em = 1:length(em_hcr_list)) %dopar% { # EM and HCR
      
      library(Rceattle)
      library(dplyr)
      
      mse <- mse_run(om = om_list[[om]], em = em_hcr_list[[em]], 
                     nsim = 1, 
                     assessment_period = 1, sampling_period = sampling_period, 
                     simulate_data = TRUE, sample_rec = TRUE, 
                     cap = NULL, 
                     dir = paste0("Runs/GOA1977/", om_names[om],"/", em_hcr_names[em],"/ConstantR/No cap"), 
                     file = NULL)
    }
})


# When you're done, clean up the cluster
stopImplicitCluster()

