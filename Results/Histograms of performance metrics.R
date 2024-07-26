# Histogram and table of performance metrics (only NPFMC is used) rest is coommented out

## Libraries ----
library(fmsb)
library(gmRi) # devtools::install_github("https://github.com/gulfofmaine/gmRi")
library(scales)
library(tidyr)
library(dplyr)
source("R/Functions/PM_table_functions.R")
source("R/Functions/PM_histogram_function.R")
source("R/Functions/PM_histogram_function_defense.R")


## Figures for each PM and Species ----
# - EBS
mse_histogram(system = "EBS", species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "EBS", species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "EBS", species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 4, width = 12)

# - GOA
mse_histogram(system = "GOA", species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "GOA", species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "GOA", species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 4, width = 12)


# - Combined
mse_histogram_two_system(species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 6, width = 12)
mse_histogram_defense(species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", height = 6, width = 12)

mse_histogram_two_system(species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 6, width = 12)
mse_histogram_two_system(species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 6, width = 12)

mse_histogram_two_system(species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 6, width = 12, legend.pos = "bottomleft")
mse_histogram_two_system(species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 6, width = 12, legend.pos = "bottomleft")
mse_histogram_two_system(species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 6, width = 12, legend.pos = "bottomleft")

mse_histogram_two_system(species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 6, width = 12, legend.pos = "topright")
mse_histogram_two_system(species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 6, width = 12, legend.pos = "topright")
mse_histogram_two_system(species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 6, width = 12, legend.pos = "topright")

mse_histogram_two_system(species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 6, width = 12, legend.pos = "bottomright")
mse_histogram_two_system(species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 6, width = 12, legend.pos = "bottomright")
mse_histogram_two_system(species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 6, width = 12, legend.pos = "bottomright")


## Figures for all PMs for each Species ----
# - EBS
mse_histogram(system = "EBS", species = "Pollock", file = "Results/Figures/Histograms/", allHCR = TRUE, single = FALSE, height = 8, width = 10)
mse_histogram(system = "EBS", species = "Cod", file = "Results/Figures/Histograms/", allHCR = TRUE, single = FALSE, height = 8, width = 10)
mse_histogram(system = "EBS", species = "Arrowtooth flounder", file = "Results/Figures/Histograms/", allHCR = TRUE, single = FALSE, height = 8, width = 10)

# - GOA
mse_histogram(system = "GOA", species = "Pollock", file = "Results/Figures/Histograms/", allHCR = TRUE, single = FALSE, height = 8, width = 10)
mse_histogram(system = "GOA", species = "Cod", file = "Results/Figures/Histograms/", allHCR = TRUE, single = FALSE, height = 8, width = 10)
mse_histogram(system = "GOA", species = "Arrowtooth flounder", file = "Results/Figures/Histograms/", allHCR = TRUE, single = FALSE, height = 8, width = 10)


## Table of PMs for specific EMs ----

pm_names <- c("Average Catch", "Catch IAV", "P(Closed)", "Avg terminal SSB Relative MSE" , "EM: P(Fy > Flimit)"  , "EM: P(SSB < SSBlimit)" , "OM: P(Fy > Flimit)", "OM: P(SSB < SSBlimit)" , "OM: Terminal SSB Depletion", "OM: Terminal SSB Depletion (Dynamic)") # Names in table

# - Tier 3
om_names = c("SS_OM", "SS_Ricker_OM", "SSM_OM", "SSM_Ricker_OM", "MS_OM", "MS_Ricker_OM")
EM_names <- c("SS_fixM_Tier3_EM", "SS_estM_Tier3_EM")
output_table = pm_summary_table(om_names, EM_names)

output_EBS <- output_table$EBS %>%
  filter(Performance.metric %in% pm_names) %>%
  pivot_wider(names_from = c(OM, EM), values_from = Value)
write.csv(output_EBS, file = paste0("Results/Tables/Table 5", "a_EBS_","_Tier3_summary.csv"))

output_GOA <- output_table$GOA %>%
  filter(Performance.metric %in% pm_names) %>%
  pivot_wider(names_from = c(OM, EM), values_from = Value)
write.csv(output_GOA, file = paste0("Results/Tables/Table 5", "b_GOA_","_Tier3_summary.csv"))

# - Dynamic Tier 3
EM_names <- c("SS_fixM_dynamicTier3_EM", "SS_estM_dynamicTier3_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", "b_EBS_", "_dynamicTier3_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", "d_GOA_", "_dynamicTier3_summary.csv"))

# - Fix M Cat 1
EM_names <-  c("SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", "e_EBS_", "_cat1_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", "f_GOA_", "_cat1_summary.csv"))

# - Est M Cat 1
EM_names <-  c("SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", "g_EBS_", "_dynamicCat1_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", "h_GOA_", "_dynamicCat1_summary.csv"))

# - Fix M Tier 1
EM_names <-  c("SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", "i_EBS_", "_Tier1_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", "j_GOA_", "_Tier1_summary.csv"))

# - Est M Tier 1
EM_names <-  c("SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", "k_EBS_", "_dynamicTier1_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", "l_GOA_", "_dynamicTier1_summary.csv"))

# - Fspr NEFMC
EM_names <-  c("SS_fixM_Fspr_EM", "SS_estM_Fspr_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", "m_EBS_", "_F40_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", "n_GOA_", "_F40_summary.csv"))

# - Avg F
EM_names <-  c("SS_fixM_AvgF_EM", "SS_estM_AvgF_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", "o_EBS_", "_AvgF_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", "p_GOA_", "_AvgF_summary.csv"))

