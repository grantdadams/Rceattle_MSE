# Table 5. Summary of the performance metrics for pollock in the in the EBS and GOA from estimation models (EMs) that pre-specified time-invariant but sometimes age-varying M (fix M) or estimated time- and age-invariant M (Est M), combined with HCR 1a (NPFMC Tier 3 harvest control rule) across operating models (OMs) with no stock-recruit relationship (“SRR”) or with a Ricker stock-recruit relationship (“Ricker”). 

## Libraries ----
library(tidyr)
library(dplyr)
source("R/PM_table_functions.R")

## Table of PMs for Tier 3 HCR ----
pm_names <- c("Average Catch", "Catch IAV", "P(Closed)", "Avg terminal SSB Relative MSE" , "EM: P(Fy > Flimit)"  , "EM: P(SSB < SSBlimit)" , "OM: P(Fy > Flimit)", "OM: P(SSB < SSBlimit)" , "OM: Terminal SSB Depletion", "OM: Terminal SSB Depletion (Dynamic)") # Names in table
om_names = c("SS_OM", "SS_Ricker_OM", "SSM_OM", "SSM_Ricker_OM", "MS_OM", "MS_Ricker_OM")
EM_names <- c("SS_fixM_Tier3_EM", "SS_estM_Tier3_EM")
output_table = pm_summary_table(om_names, EM_names)

# - Tier 3
output_EBS <- output_table$EBS %>%
  filter(Performance.metric %in% pm_names) %>%
  pivot_wider(names_from = c(OM, EM), values_from = Value)
write.csv(output_EBS, file = paste0("Results/Tables/Table 5", "a_EBS_","_Tier3_summary.csv"))

output_GOA <- output_table$GOA %>%
  filter(Performance.metric %in% pm_names) %>%
  pivot_wider(names_from = c(OM, EM), values_from = Value)
write.csv(output_GOA, file = paste0("Results/Tables/Table 5", "b_GOA_","_Tier3_summary.csv"))