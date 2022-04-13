library(Rceattle)
library(tidyr)

## File names# -- NPFMC Tier 3 HCRs No cap
dir_no_cap_names <- c("Runs/EBS/MS_OM/SS_Tier3_EM/ConstantR/No cap", "Runs/EBS/MS_OM/SS_M_Tier3_EM/ConstantR/No cap", "Runs/EBS/SS_OM/SS_Tier3_EM/ConstantR/No cap",  "Runs/EBS/SS_OM/SS_M_Tier3_EM/ConstantR/No cap")

MSE_names <- c("MS-OM, Fix M-No cap", "MS-OM, Est M-No cap", "SS-OM, Fix M-No cap", "SS-OM, Est M-No cap")

## Load and run summary
for(i in 1:length(dir_no_cap_names)){
  # - Load
  mse3 <- load_mse(dir = dir_no_cap_names[i], file = NULL)
  
  # - Performance metrics
  mse_metrics <- mse_summary(mse3)
  mse_metrics <- mse_metrics[1:3,-c(2:3)]
  mse_metrics <- pivot_longer(mse_metrics, cols = 2:ncol(mse_metrics))
  write.csv(mse_metrics, file = paste0("Results/EBS_table", MSE_names[i]))
  
  # - Plot
  plot_depletionSSB(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/Depletion/EBS true ", MSE_names[i]), line_col  = "#04395E")
  plot_depletionSSB(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/Depletion/EBS Perceived ", MSE_names[i]), line_col = "#5F0F40")
  
  plot_ssb(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/SSB/EBS true ", MSE_names[i]), line_col  = "#04395E")
  plot_ssb(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/SSB/EBS Perceived ", MSE_names[i]), line_col = "#5F0F40")
  
  plot_biomass(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/B/EBS true ", MSE_names[i]), line_col  = "#04395E")
  plot_biomass(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/B/EBS Perceived ", MSE_names[i]), line_col = "#5F0F40")
  
  plot_recruitment(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/R/EBS true ", MSE_names[i]), line_col  = "#04395E")
  plot_recruitment(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/R/EBS Perceived ", MSE_names[i]), line_col = "#5F0F40")
  
  plot_f(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/F/EBS true ", MSE_names[i]), line_col  = "#04395E")
  plot_f(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/F/EBS true ", MSE_names[i]), line_col  = "#04395E")
  
  plot_catch(mse3, mse = TRUE, file = paste0("Results/Figures/Catch/EBS true ", MSE_names[i]), line_col  = "#04395E")
  
  # - Unload for memory
  rm(mse3)
}


