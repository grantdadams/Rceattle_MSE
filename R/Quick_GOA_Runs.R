# Install packages
install.packages(c("dplyr",
                   "ggplot2",
                   "MASS",
                   "oce",
                   "readxl",
                   "TMB",
                   "devtools",
                   "writexl",
                   "reshape2",
                   "gplots",
                   "tidyr",
                   "testthat",
                   "foreach",
                   "doParallel"))

devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")

################################################
# Set-up
################################################
source("Models/GOA_18.5.1_models_1_2_from_excel.R")
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
rec_scen <- list(0)

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
em_hcr_list <- list(ss_run_Tier3, ss_run_dynamicTier3, ss_run_Cat1, ss_run_dynamicCat1, ss_run_Tier1, ss_run_dynamicTier1, ss_run_Fspr, ss_run_AvgF, # Fixed M
                    ss_run_M_Tier3, ss_run_M_dynamicTier3, ss_run_M_Cat1, ss_run_M_dynamicCat1, ss_run_M_Tier1, ss_run_M_dynamicTier1, ss_run_M_Fspr, ss_run_M_AvgF # Estimate M
)

em_hcr_names <- c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", "SS_fixM_AvgF_EM", # Fixed M
                  "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM", "SS_estM_AvgF_EM")


### Run the MSE
source("R/Functions/Run_full_MSE_function_not_parallel.R")

# All up/down 313-12
i = 1
j = 1

# 313-2 i = 1, j = 1
# 313-2 i = 1, j = 2
# 313-2 i = 1, j = 3
# 313-2 i = 1, j = 4

# 313-2 i = 2, j = 1
# 313-2 i = 2, j = 2
# 313-2 i = 2, j = 3
# 313-2 i = 2, j = 4


run_mse_np(system = "GOA1977", recname = c("AllUp", "AllDown", "ATFRup", "ATFRdown")[i], om_list = om_list, om_names = om_names, em_hcr_list = em_hcr_list[c(1:2,9:10)[j]], em_hcr_names = em_hcr_names[c(1:2,9:10)[j]], sampling_period = sampling_period, rec_scen = list(c(1,1,1), c(-0.5,-0.5,-0.5), c(0,1,0), c(0,-0.5,0))[i], nsim = 300)

