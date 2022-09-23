# Histogram and table of performance metrics (only NPFMC is used) rest is coommented out
source("R/Functions/Summary_histogram_and _table_functions.R")
source("R/Functions/Summary_histogram_by_PM.R")

# Library
library(fmsb)
library(gmRi)
library(scales)


# make all PMs relative to maximum and minimum values
normalize <- function(x) {
  return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
}



# - EBS
histogram_by_pm(system = "EBS", species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 6.5, width = 5)
histogram_by_pm(system = "EBS", species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 6.5, width = 5)
histogram_by_pm(system = "EBS", species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 6.5, width = 5)

# - GOA
histogram_by_pm(system = "GOA", species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 6.5, width = 5)
histogram_by_pm(system = "GOA", species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 6.5, width = 5)
histogram_by_pm(system = "GOA", species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 6.5, width = 5)


histogram_by_om(system = "EBS", species = "Pollock", allHCR = TRUE, height = 6.5, width = 5, single = TRUE)


goa_m <- m_summary_table()$GOA
write.csv(goa_m, file = "Results/Tables/Avg M/GOA M summary.csv")



# File names
om_names = c("SS_OM", "SSM_OM", "MS_OM")


# - Plot histograms and save summary tables
ind=5
for(rec in 1:5){
  recname = c("ConstantR", "AllUp", "AllDown", "ATFRup", "ATFRdown")[rec]
  
  # - EBS
  histogram_by_om(system = "EBS", recname = recname, species = "Pollock", file = "Results/Figures/Histograms/", allHCR = TRUE)
  histogram_by_om(system = "EBS", recname = recname, species = "Cod", file = "Results/Figures/Histograms/", allHCR = TRUE)
  histogram_by_om(system = "EBS", recname = recname, species = "Arrowtooth flounder", file = "Results/Figures/Histograms/", allHCR = TRUE)
  
  # - GOA
  histogram_by_om(system = "GOA", recname = recname, species = "Pollock", file = "Results/Figures/Histograms/", allHCR = TRUE)
  histogram_by_om(system = "GOA", recname = recname, species = "Cod", file = "Results/Figures/Histograms/", allHCR = TRUE)
  histogram_by_om(system = "GOA", recname = recname, species = "Arrowtooth flounder", file = "Results/Figures/Histograms/", allHCR = TRUE)
  
  
  # Save supp table 6 - AvgF
  # Save table 6 - Tier 3
  EM_names <- c("SS_fixM_Tier3_EM", "SS_estM_Tier3_EM")
  output_table = pm_summary_table(om_names, EM_names, recname = recname)
  write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"a_EBS_", recname,"_Tier3_summary.csv"))
  write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"c_GOA_", recname,"_Tier3_summary.csv"))
  
  # Save supp table 2
  EM_names <- c("SS_fixM_dynamicTier3_EM", "SS_estM_dynamicTier3_EM")
  output_table = pm_summary_table(om_names, EM_names, recname = recname)
  write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"b_EBS_", recname,"_dynamicTier3_summary.csv"))
  write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"d_GOA_", recname,"_dynamicTier3_summary.csv"))
  
  EM_names <-  c("SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM")
  output_table = pm_summary_table(om_names, EM_names, recname = recname)
  write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"e_EBS_", recname,"_cat1_summary.csv"))
  write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"f_GOA_", recname,"_cat1_summary.csv"))
  
  EM_names <-  c("SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM")
  output_table = pm_summary_table(om_names, EM_names, recname = recname)
  write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"g_EBS_", recname,"_dynamicCat1_summary.csv"))
  write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"h_GOA_", recname,"_dynamicCat1_summary.csv"))
  
  
  EM_names <-  c("SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM")
  output_table = pm_summary_table(om_names, EM_names, recname = recname)
  write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"i_EBS_", recname,"_Tier1_summary.csv"))
  write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"j_GOA_", recname,"_Tier1_summary.csv"))
  
  EM_names <-  c("SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM")
  output_table = pm_summary_table(om_names, EM_names, recname = recname)
  write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"k_EBS_", recname,"_dynamicTier1_summary.csv"))
  write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"l_GOA_", recname,"_dynamicTier1_summary.csv"))
  
  EM_names <-  c("SS_fixM_Fspr_EM", "SS_estM_Fspr_EM")
  output_table = pm_summary_table(om_names, EM_names, recname = recname)
  write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"m_EBS_", recname,"_F40_summary.csv"))
  write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"n_GOA_", recname,"_F40_summary.csv"))
  
  EM_names <-  c("SS_fixM_AvgF_EM", "SS_estM_AvgF_EM")
  output_table = pm_summary_table(om_names, EM_names, recname = recname)
  write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"o_EBS_", recname,"_AvgF_summary.csv"))
  write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"p_GOA_", recname,"_AvgF_summary.csv"))
  ind = ind+1
}

# - Plot single
rec = 1
recname = c("ConstantR", "AllUp", "AllDown", "ATFRup", "ATFRdown")[rec]
histogram_by_om(system = "EBS", recname = recname, species = "Pollock", allHCR = ifelse(rec == 1, TRUE, FALSE), single = TRUE)

recname = c("ConstantR", "AllUp", "AllDown", "ATFRup", "ATFRdown")[rec]
histogram_by_om(system = "GOA", recname = recname, species = "Cod", allHCR = ifelse(rec == 1, TRUE, FALSE), single = TRUE)

