################################################
# Management strategy evaluation
################################################
library(dplyr)
source("R/BSAI_condition_models.R") 
source("R/BSAI_condition_ricker_models.R") # Some operating models may not fully converge and that is OK

sampling_period = c(1,1,1,1,1,1,2)


################################################
# Management strategy evaluation
################################################
### OMS
# 2. Multi-species type II
om_list <- list(ms_run_f25, ms_run_ricker_f25)
om_names = c("MS_OM", "MS_Ricker_OM")


### Management strategies
## EM
# 1. Single-species fix M
# 2. Single-species estimate M

# HCR
# 1, NPFMC Tier 3 HCR
em_hcr_list <- list(ss_run_Tier3,
                    ss_run_M_Tier3) 

em_hcr_names <- c("SS_fixM_Tier3_EM",
                  "SS_estM_Tier3_EM") 

### Run the MSE
source("R/Run_MSE_function.R")
run_mse(system = "EBS", om_list = om_list, om_names = om_names, em_hcr_list = em_hcr_list, em_hcr_names = em_hcr_names, sampling_period = sampling_period, nsim = 300)
