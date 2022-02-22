source("R/BSAI_condition_models.R")


################################################
# Management strategy evaluation
################################################


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



# -- NPFMC Tier 3 HCRs
# - MS-OM: SS-EM Tier 3 HCR
mse1 <- mse_run(om = ms_run, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, cap = cap_list[[1]], dir = "Runs/EBS/MS_OM/SS_Tier3_EM/ConstantR/Cap1", file = NULL)

# - MS-OM: SSM-EM Tier 3 HCR
mse2 <- mse_run(om = ms_run, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, cap = cap_list[[1]], dir = "Runs/EBS/MS_OM/SS_M_Tier3_EM/ConstantR/Cap1", file = NULL)

# - SSM-OM: SS-EM Tier 3 HCR
mse3 <- mse_run(om = ss_run_M, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, cap = cap_list[[1]], dir = "Runs/EBS/SS_M_OM/SS_Tier3_EM/ConstantR/Cap1", file = NULL)

# - SSM-OM: SSM-EM Tier 3 HCR
mse4 <- mse_run(om = ss_run_M, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, cap = cap_list[[1]], dir = "Runs/EBS/SS_M_OM/SS_M_Tier3_EM/ConstantR/Cap1", file = NULL)

# - SS-OM: SS-EM Tier 3 HCR
mse5 <- mse_run(om = ss_run, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, cap = cap_list[[1]], dir = "Runs/EBS/SS_OM/SS_Tier3_EM/ConstantR/Cap1", file = NULL)

# - SS-OM: SSM-EM Tier 3 HCR
mse6 <- mse_run(om = ss_run, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, cap = cap_list[[1]], dir = "Runs/EBS/SS_OM/SS_M_Tier3_EM/ConstantR/Cap1", file = NULL)


### NO Cap
# -- NPFMC Tier 3 HCRs
# - MS-OM: SS-EM Tier 3 HCR
mse1 <- mse_run(om = ms_run, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, dir = "Runs/EBS/MS_OM/SS_Tier3_EM/ConstantR/No cap", file = NULL)

# - MS-OM: SSM-EM Tier 3 HCR
mse2 <- mse_run(om = ms_run, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, dir = "Runs/EBS/MS_OM/SS_M_Tier3_EM/ConstantR/No cap", file = NULL)

# - SSM-OM: SS-EM Tier 3 HCR
mse3 <- mse_run(om = ss_run_M, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, dir = "Runs/EBS/SS_M_OM/SS_Tier3_EM/ConstantR/No cap", file = NULL)

# - SSM-OM: SSM-EM Tier 3 HCR
mse4 <- mse_run(om = ss_run_M, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, dir = "Runs/EBS/SS_M_OM/SS_M_Tier3_EM/ConstantR/No cap", file = NULL)

# - SS-OM: SS-EM Tier 3 HCR
mse5 <- mse_run(om = ss_run, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, dir = "Runs/EBS/SS_OM/SS_Tier3_EM/ConstantR/No cap", file = NULL)

# - SS-OM: SSM-EM Tier 3 HCR
mse6 <- mse_run(om = ss_run, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate = TRUE, dir = "Runs/EBS/SS_OM/SS_M_Tier3_EM/ConstantR/No cap", file = NULL)





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
# em1_hcr_list <- list(ss_run_Tier3, ss_run_dynamicTier3, ss_run_Cat1, ss_run_dynamicCat1, ss_run_Tier1, ss_run_dynamicTier1, ss_run_AvgF, ss_run_Fspr, ss_run_dynamicfb0)
# 
# em2_hcr_list <- list(ss_run_M_Tier3, ss_run_M_dynamicTier3, ss_run_M_Cat1, ss_run_M_dynamicCat1, ss_run_M_Tier1, ss_run_M_dynamicTier1, ss_run_M_AvgF, ss_run_M_Fspr, ss_run_M_dynamicfb0)




### Run MSEs
## Loop across OMs,
for(om in 1:length(om_list)){ # OM model
  for(rec in 1:length(rec_scen)){ # Future recruitment
    for(var in 1:length(data_var)){ # Data sampling
      
      
      
    }
  }
}
