################################################
# Set-up
################################################
source("R/BSAI_condition_models.R")
source("R/BSAI_condition_ricker_models.R")
library(gmRi)
library(Rceattle)
library(tidyr)


################################################
# Management strategy evaluation
################################################
### OMS
# 3. Multi-species type II
om_names = c("MS_OM", "MS_Ricker_OM")
om_names_print = c("Multi-spp",
                   "Multi-spp w/ Ricker")
projected_OM_no_F <- list(ms_run, ms_run_ricker)


# Lists to update reference points in OM
# - No ricker
om_hcr_list_fixM <- list(ss_run_Tier3)
om_hcr_list_fixM = c(om_hcr_list_fixM, om_hcr_list_fixM)

om_hcr_list_estM <- list(ss_run_M_Tier3)
om_hcr_list_estM = c(om_hcr_list_estM, om_hcr_list_estM)

# - Ricker
om_hcr_list_ricker_fixM <- list(ss_run_ricker_Tier3)
om_hcr_list_ricker_fixM <- c(om_hcr_list_ricker_fixM, om_hcr_list_ricker_fixM)

om_hcr_list_ricker_estM <- list(ss_run_ricker_M_Tier3)
om_hcr_list_ricker_estM <- c(om_hcr_list_ricker_estM, om_hcr_list_ricker_estM)


### Management strategies
## EM
# 1. Single-species fix M
# 2. Single-species estimate M

# HCR
# 1, NPFMC Tier 3 HCR
em_hcr_names <- c("SS_fixM_Tier3_EM",
                  "SS_estM_Tier3_EM")


################################################
# Load and run summary
################################################
# Do summary ----
source("R/MSE_performance_metrics.R") # Performance metric function
source("R/Summarize_MSE_function.R")  # Load and summarize sims function


# - No SRR OMs
summary_fun(system = "EBS", spname_system = "EBS",
            om_list_no_F = projected_OM_no_F[1], om_names = om_names[1],
            om_hcr_list_fixM = om_hcr_list_fixM, 
            om_hcr_list_estM = om_hcr_list_estM, 
            em_hcr_names = em_hcr_names, species = 1:3) 

# - Ricker SRR OMs
summary_fun(system = "EBS", spname_system = "EBS",
            om_list_no_F = projected_OM_no_F[2], om_names = om_names[2],
            om_hcr_list_fixM = om_hcr_list_ricker_fixM, 
            om_hcr_list_estM = om_hcr_list_ricker_estM, 
            em_hcr_names = em_hcr_names, species = 1:3)

