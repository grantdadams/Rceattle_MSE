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

sampling_period = c(1,1,1,1,1,1,2)


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
em_hcr_list <- list(ss_run_Tier3, ss_run_dynamicTier3, ss_run_Cat1, ss_run_dynamicCat1, # ss_run_Tier1, ss_run_dynamicTier1, ss_run_Fspr, ss_run_AvgF, # Fixed M
                    ss_run_M_Tier3, ss_run_M_dynamicTier3, ss_run_M_Cat1, ss_run_M_dynamicCat1) #, ss_run_M_Tier1, ss_run_M_dynamicTier1, ss_run_M_Fspr, ss_run_M_AvgF) # Estimate M


em_hcr_names <- c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", # "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", "SS_fixM_AvgF_EM", # Fixed M
                  "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM") #, "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM", "SS_estM_AvgF_EM")


### Run the MSE
source("R/Functions/Run_full_MSE_function_not_parallel.R")

# No rec trend
run_mse_np(system = "EBS", recname = "ConstantR", om_list = om_list, om_names = om_names, em_hcr_list = em_hcr_list, em_hcr_names = em_hcr_names, sampling_period = sampling_period, nsim = 300)

# ATF up and down
# - NPFMC and PFMC
run_mse_np(system = "EBS", recname = c("AllUp", "AllDown", "ATFRup", "ATFRdown"), om_list = om_list, om_names = om_names, em_hcr_list = em_hcr_list, em_hcr_names = em_hcr_names, sampling_period = sampling_period, rec_scen = list(c(1,1,1), c(-0.5,-0.5,-0.5), c(0,0,1), c(0,0,-0.5)), nsim = 300)



# - All others
em_hcr_list <- list(#ss_run_Tier3, ss_run_dynamicTier3, ss_run_Cat1, ss_run_dynamicCat1, 
  ss_run_Tier1, ss_run_dynamicTier1, ss_run_Fspr, ss_run_AvgF, # Fixed M
  # ss_run_M_Tier3, ss_run_M_dynamicTier3, ss_run_M_Cat1, ss_run_M_dynamicCat1, 
  ss_run_M_Tier1, ss_run_M_dynamicTier1, ss_run_M_Fspr, ss_run_M_AvgF) # Estimate M


em_hcr_names <- c(#"SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", # 
  "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", "SS_fixM_AvgF_EM", # Fixed M
                  #"SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM") #, 
"SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM", "SS_estM_AvgF_EM")

run_mse_np(system = "EBS", recname = c("AllUp", "AllDown", "ATFRup", "ATFRdown"), om_list = om_list[3], om_names = om_names[3], em_hcr_list = em_hcr_list, em_hcr_names = em_hcr_names, sampling_period = sampling_period, rec_scen = list(c(1,1,1), c(-0.5,-0.5,-0.5), c(0,0,1), c(0,0,-0.5)), nsim = 300)


################################################
# Base MSEs (Tier 3 HCR no cap)
################################################
# -- NPFMC Tier 3 HCRs
# - MS-OM: SS-EM Tier 3 HCR
mse1 <- mse_run(om = ms_run_f25, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate_data = TRUE, sample_rec = TRUE, dir = "Runs/EBS/MS_OM/SS_Tier3_EM/ConstantR/No cap", file = NULL)

# - MS-OM: SSM-EM Tier 3 HCR
mse2 <- mse_run(om = ms_run_f25, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate_data = TRUE, sample_rec = TRUE, dir = "Runs/EBS/MS_OM/SS_M_Tier3_EM/ConstantR/No cap", file = NULL)

# - SS-OM: SS-EM Tier 3 HCR
mse3 <- mse_run(om = ss_run_Tier3, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate_data = TRUE, sample_rec = TRUE, dir = "Runs/EBS/SS_OM/SS_Tier3_EM/ConstantR/No cap", file = NULL)

# - SS-OM: SSM-EM Tier 3 HCR
mse4 <- mse_run(om = ss_run_Tier3, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate_data = TRUE, sample_rec = TRUE, dir = "Runs/EBS/SS_OM/SS_M_Tier3_EM/ConstantR/No cap", file = NULL)

# - SSM-OM: SS-EM Tier 3 HCR
mse5 <- mse_run(om = ss_run_M_Tier3, em = ss_run_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate_data = TRUE, sample_rec = TRUE, dir = "Runs/EBS/SSM_OM/SS_Tier3_EM/ConstantR/No cap", file = NULL)

# - SSM-OM: SSM-EM Tier 3 HCR
mse6 <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 200, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate_data = TRUE, sample_rec = TRUE, dir = "Runs/EBS/SSM_OM/SS_M_Tier3_EM/ConstantR/No cap", file = NULL)

