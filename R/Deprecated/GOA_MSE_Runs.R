source("R/GOA_condition_models.R")



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


## Adjust rec var
load("~/GitHub/Rceattle_MSE/Models/GOA_18.5.1_random_effects_models_3iter_w_hessian/18_5_1_re_3iter_Mod_12_2021-12-12.Rdata")
run_list <- list(mod_re)
load("~/GitHub/Rceattle_MSE/Models/GOA_18.5.1_random_effects_models_3iter_w_hessian/18_5_1_re_3iter_Mod_13_2021-12-13.Rdata")
run_list[[2]] <- mod_re

ss_run$estimated_params$ln_rec_sigma <- run_list[[1]]$estimated_params$ln_rec_sigma[1:3]
ss_run_M$estimated_params$ln_rec_sigma <- run_list[[1]]$estimated_params$ln_rec_sigma[1:3]
ms_run$estimated_params$ln_rec_sigma <- run_list[[2]]$estimated_params$ln_rec_sigma[1:3]

## Sampling period
sampling_period <- c(2,2,1,2,2,2,2,1,2,2,1,2,2,1,1,1)


# -- NPFMC Tier 3 HCRs
# - MS-OM: SS-EM Tier 3 HCR
mse1 <- mse_run(om = ms_run, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = sampling_period, simulate = TRUE, cap = cap_list[[1]], dir = "Runs/GOA/MS_OM/SS_Tier3_EM/ConstantR/Cap1", file = NULL)

# - MS-OM: SSM-EM Tier 3 HCR
mse2 <- mse_run(om = ms_run, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = sampling_period, simulate = TRUE, cap = cap_list[[1]], dir = "Runs/GOA/MS_OM/SS_M_Tier3_EM/ConstantR/Cap1", file = NULL)

# - SSM-OM: SS-EM Tier 3 HCR
mse3 <- mse_run(om = ss_run_M, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = sampling_period, simulate = TRUE, cap = cap_list[[1]], dir = "Runs/GOA/SS_M_OM/SS_Tier3_EM/ConstantR/Cap1", file = NULL)

# - SSM-OM: SSM-EM Tier 3 HCR
mse4 <- mse_run(om = ss_run_M, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = sampling_period, simulate = TRUE, cap = cap_list[[1]], dir = "Runs/GOA/SS_M_OM/SS_M_Tier3_EM/ConstantR/Cap1", file = NULL)

# - SS-OM: SS-EM Tier 3 HCR
mse5 <- mse_run(om = ss_run, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = sampling_period, simulate = TRUE, cap = cap_list[[1]], dir = "Runs/GOA/SS_OM/SS_Tier3_EM/ConstantR/Cap1", file = NULL)

# - SS-OM: SSM-EM Tier 3 HCR
mse6 <- mse_run(om = ss_run, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = sampling_period, simulate = TRUE, cap = cap_list[[1]], dir = "Runs/GOA/SS_OM/SS_M_Tier3_EM/ConstantR/Cap1", file = NULL)


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

### Run MSEs
## Loop across OMs,
for(om in 1:length(om_list)){ # OM model
  for(rec in 1:length(rec_scen)){ # Future recruitment
    for(var in 1:length(data_var)){ # Data sampling
      
      
      
    }
  }
}