# Histogram and table of performance metrics (only NPFMC is used) rest is coommented out

## Libraries ----
library(fmsb)
library(gmRi) # devtools::install_github("https://github.com/gulfofmaine/gmRi")
library(scales)
source("R/Functions/Summary_table_functions.R")
source("R/Functions/Summary_histogram_function.R")


## Figures for each PM and Species ----
# - EBS
mse_histogram(system = "EBS", species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 5, width = 8)
mse_histogram(system = "EBS", species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 5, width = 8)
mse_histogram(system = "EBS", species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 5, width = 8)

# - GOA
mse_histogram(system = "GOA", species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 5, width = 8)
mse_histogram(system = "GOA", species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 5, width = 8)
mse_histogram(system = "GOA", species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 5, width = 8)


## Figures for all PMs for each Species ----
recname = "ConstantR"

# - EBS
mse_histogram(system = "EBS", species = "Pollock", file = "Results/Figures/Histograms/", allHCR = TRUE, single = FALSE, height = 8, width = 10)
mse_histogram(system = "EBS", species = "Cod", file = "Results/Figures/Histograms/", allHCR = TRUE, single = FALSE, height = 8, width = 10)
mse_histogram(system = "EBS", species = "Arrowtooth flounder", file = "Results/Figures/Histograms/", allHCR = TRUE, single = FALSE, height = 8, width = 10)

# - GOA
mse_histogram(system = "GOA", species = "Pollock", file = "Results/Figures/Histograms/", allHCR = TRUE, single = FALSE, height = 8, width = 10)
mse_histogram(system = "GOA", species = "Cod", file = "Results/Figures/Histograms/", allHCR = TRUE, single = FALSE, height = 8, width = 10)
mse_histogram(system = "GOA", species = "Arrowtooth flounder", file = "Results/Figures/Histograms/", allHCR = TRUE, single = FALSE, height = 8, width = 10)


## Table of PMs for specific EMs ----
# - Tier 3
ind <- 1
om_names = c("SS_OM", "SSM_OM", "MS_OM")
EM_names <- c("SS_fixM_Tier3_EM", "SS_estM_Tier3_EM")
output_table = pm_summary_table(om_names, EM_names, recname = recname)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"a_EBS_", recname,"_Tier3_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"c_GOA_", recname,"_Tier3_summary.csv"))

# - Dynamic Tier 3
EM_names <- c("SS_fixM_dynamicTier3_EM", "SS_estM_dynamicTier3_EM")
output_table = pm_summary_table(om_names, EM_names, recname = recname)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"b_EBS_", recname,"_dynamicTier3_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"d_GOA_", recname,"_dynamicTier3_summary.csv"))

# - Fix M Cat 1
EM_names <-  c("SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM")
output_table = pm_summary_table(om_names, EM_names, recname = recname)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"e_EBS_", recname,"_cat1_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"f_GOA_", recname,"_cat1_summary.csv"))

# - Est M Cat 1
EM_names <-  c("SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM")
output_table = pm_summary_table(om_names, EM_names, recname = recname)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"g_EBS_", recname,"_dynamicCat1_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"h_GOA_", recname,"_dynamicCat1_summary.csv"))

# - Fix M Tier 1
EM_names <-  c("SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM")
output_table = pm_summary_table(om_names, EM_names, recname = recname)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"i_EBS_", recname,"_Tier1_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"j_GOA_", recname,"_Tier1_summary.csv"))

# - Est M Tier 1
EM_names <-  c("SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM")
output_table = pm_summary_table(om_names, EM_names, recname = recname)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"k_EBS_", recname,"_dynamicTier1_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"l_GOA_", recname,"_dynamicTier1_summary.csv"))

# - Fspr NEFMC
EM_names <-  c("SS_fixM_Fspr_EM", "SS_estM_Fspr_EM")
output_table = pm_summary_table(om_names, EM_names, recname = recname)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"m_EBS_", recname,"_F40_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"n_GOA_", recname,"_F40_summary.csv"))

# - Avg F
EM_names <-  c("SS_fixM_AvgF_EM", "SS_estM_AvgF_EM")
output_table = pm_summary_table(om_names, EM_names, recname = recname)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"o_EBS_", recname,"_AvgF_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"p_GOA_", recname,"_AvgF_summary.csv"))

