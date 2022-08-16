################################################
# Set-up
################################################
source("R/BSAI_condition_models.R")
library(Rceattle)
library(tidyr)

ms_run$quantities$depletionSSB <- ms_run$quantities$biomassSSB/ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)]


################################################
# Management strategy evaluation
################################################
### OMS
# 1. Single-species fix M
# 2. Single-species estimate M
# 3. Multi-species type II
om_list <- list(ss_run_Tier3, ss_run_M_Tier3, ms_run_f25)
projected_OM_no_F = list(ss_run, ss_run_M, ms_run)
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

# Lists to update reference points in OM (can remove later)
em_hcr_list_fixM <- list(ss_run_Tier3, ss_run_dynamicTier3, ss_run_Cat1, ss_run_dynamicCat1, ss_run_Tier1, ss_run_dynamicTier1, ss_run_Fspr, ss_run_AvgF) # Fixed M
em_hcr_list_fixM = c(em_hcr_list_fixM, em_hcr_list_fixM)

em_hcr_list_estM <- list(ss_run_M_Tier3, ss_run_M_dynamicTier3, ss_run_M_Cat1, ss_run_M_dynamicCat1, ss_run_M_Tier1, ss_run_M_dynamicTier1, ss_run_M_Fspr, ss_run_M_AvgF) # Estimate M
em_hcr_list_estM = c(em_hcr_list_estM, em_hcr_list_estM)

em_hcr_names <- c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", "SS_fixM_AvgF_EM", # Fixed M
                  "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM", "SS_estM_AvgF_EM")


# FIXME - check Ftarget of estM and fixM for tier 3


plot_biomass(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names, width = 6, height = 5)
plot_ssb(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names, width = 6, height = 5)
plot_recruitment(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names, width = 6, height = 5)
plot_b_eaten_prop(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names, width = 6, height = 5)

################################################
# Load and run summary
################################################
# Do summary
source("R/Functions/Run_MSE_summary.R")


# SAFS 313-11
# - No rec trend
summary_fun(system = "EBS"; recname = "ConstantR"; om_list_no_F = projected_OM_no_F; om_names = om_names; em_hcr_list_fixM = em_hcr_list_fixM; em_hcr_list_estM = em_hcr_list_estM; em_hcr_names = em_hcr_names; trend = FALSE; species = c(1,2,3))


# - ATF Up and Down
summary_fun(system = "EBS", recname = c("ATFRup"), om_list_no_F = lapply(om_list, function(x) project_trend(x, c(0, 0, 1))), om_names = om_names, em_hcr_list_fixM = em_hcr_list_fixM, em_hcr_list_estM = em_hcr_list_estM, em_hcr_names = em_hcr_names, species = c(1,2,3), trend = TRUE, om_list_no_rdev_or_F = lapply(om_list, function(x) remove_rec_dev_and_F(x, c(0, 0, 1))))

summary_fun(system = "EBS", recname = c("ATFRdown"), om_list_no_F = lapply(om_list, function(x) project_trend(x, c(0, 0, -0.5))), om_names = om_names, em_hcr_list_fixM = em_hcr_list_fixM, em_hcr_list_estM = em_hcr_list_estM, em_hcr_names = em_hcr_names, species = c(1,2,3), trend = TRUE, om_list_no_rdev_or_F = lapply(om_list, function(x) remove_rec_dev_and_F(x, c(0, 0, -0.5))))


# - All Up and Down
summary_fun(system = "EBS", recname = c("AllUp"), om_list_no_F = lapply(om_list, function(x) project_trend(x, c(1,1,1))), om_names = om_names, em_hcr_list_fixM = em_hcr_list_fixM, em_hcr_list_estM = em_hcr_list_estM, em_hcr_names = em_hcr_names, species = c(1,2,3), trend = TRUE, om_list_no_rdev_or_F = lapply(om_list, function(x) remove_rec_dev_and_F(x, c(1,1,1))))

summary_fun(system = "EBS", recname = c("AllDown"), om_list_no_F = lapply(om_list, function(x) project_trend(x, c(-0.5,-0.5,-0.5))), om_names = om_names, em_hcr_list_fixM = em_hcr_list_fixM, em_hcr_list_estM = em_hcr_list_estM, em_hcr_names = em_hcr_names, species = c(1,2,3), trend = TRUE, om_list_no_rdev_or_F = lapply(om_list, function(x) remove_rec_dev_and_F(x, c(-0.5,-0.5,-0.5))))

