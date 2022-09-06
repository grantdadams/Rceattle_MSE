# Histogram and table of performance metrics (only NPFMC is used) rest is coommented out
source("R/Functions/Summary_histogram_and _table_functions.R")

# Library
library(fmsb)
library(gmRi)
library(scales)


# make all PMs relative to maximum and minimum values
normalize <- function(x) {
  return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
}



# File names
om_names = c("SS_OM", "SSM_OM", "MS_OM")


# - Plot histograms and save summary tables
ind=5
for(rec in 1:5){
  recname = c("ConstantR", "AllUp", "AllDown", "ATFRup", "ATFRdown")[rec]
  
  # - EBS
  #histogram_by_om(system = "EBS", recname = recname, species = "Pollock", file = "Results/Figures/Histograms/", allHCR = TRUE)
  #histogram_by_om(system = "EBS", recname = recname, species = "Cod", file = "Results/Figures/Histograms/", allHCR = TRUE)
  #histogram_by_om(system = "EBS", recname = recname, species = "Arrowtooth flounder", file = "Results/Figures/Histograms/", allHCR = TRUE)
  
  # - GOA
  histogram_by_om(system = "GOA", recname = recname, species = "Pollock", file = "Results/Figures/Histograms/", allHCR = TRUE)
  histogram_by_om(system = "GOA", recname = recname, species = "Cod", file = "Results/Figures/Histograms/", allHCR = TRUE)
  histogram_by_om(system = "GOA", recname = recname, species = "Arrowtooth flounder", file = "Results/Figures/Histograms/", allHCR = TRUE)
  
  
  # Save supp table 6 - AvgF
  # Save table 6 - Tier 3
  EM_names <- c("SS_fixM_Tier3_EM", "SS_estM_Tier3_EM")
  output_table = pm_summary_table(om_names, EM_names, recname = recname)
  
  #write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"a_EBS_", recname,"_Tier3_summary.csv"))
  write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"c_GOA_", recname,"_Tier3_summary.csv"))
  
  # Save supp table 2
  EM_names <- c("SS_fixM_dynamicTier3_EM", "SS_estM_dynamicTier3_EM")
  output_table = pm_summary_table(om_names, EM_names, recname = recname)
  write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"b_EBS_", recname,"_dynamicTier3_summary.csv"))
  write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"d_GOA_", recname,"_dynamicTier3_summary.csv"))
  ind = ind+1
}

# - Plot single
rec = 1
recname = c("ConstantR", "AllUp", "AllDown", "ATFRup", "ATFRdown")[rec]
histogram_by_om(system = "EBS", recname = recname, species = "Pollock", allHCR = ifelse(rec == 1, TRUE, FALSE), single = TRUE)

recname = c("ConstantR", "AllUp", "AllDown", "ATFRup", "ATFRdown")[rec]
histogram_by_om(system = "GOA", recname = recname, species = "Cod", allHCR = ifelse(rec == 1, TRUE, FALSE), single = TRUE)

