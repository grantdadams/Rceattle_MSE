################################################
# Set-up
################################################
source("R/GOA_condition_models_1977.R")
library(Rceattle)
library(tidyr)

## File names# -- NPFMC Tier 3 HCRs No cap
dir_no_cap_names_GOA <- c("Runs/GOA1977/MS_OM/SS_Tier3_EM/ConstantR/No cap", 
                          "Runs/GOA1977/MS_OM/SS_M_Tier3_EM/ConstantR/No cap", 
                          "Runs/GOA1977/SS_OM/SS_Tier3_EM/ConstantR/No cap", 
                          "Runs/GOA1977/SS_OM/SS_M_Tier3_EM/ConstantR/No cap",
                          "Runs/GOA1977/SSM_OM/SS_Tier3_EM/ConstantR/No cap", 
                          "Runs/GOA1977/SSM_OM/SS_M_Tier3_EM/ConstantR/No cap")

MSE_names <- c("MS-OM_Fix M-No cap", 
               "MS-OM_Est M-No cap", 
               "SS-OM_Fix M-No cap", 
               "SS-OM_Est M-No cap", 
               "SS-Est M-OM_Est M-No cap", 
               "SS-Est M-OM_Fix M-No cap")


ms_run$quantities$depletionSSB <- ms_run$quantities$biomassSSB/ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)]

projected_models_no_F = list(ms_run, ms_run, ss_run, ss_run, ss_run_M, ss_run_M)
projected_models_F = list(ms_run_f25 , ms_run_f25, ss_run_Tier3, ss_run_Tier3, ss_run_M_Tier3, ss_run_Tier3)

plot_biomass(list(ms_run, ms_run_f25), model_names = c("No F", "F25"), incl_proj = TRUE)
plot_depletionSSB(list(ss_run, ss_run_Tier3, ms_run, ms_run_f25), model_names = c("No F", "F25", "M No F", "M F25"), incl_proj = TRUE)
plot_biomass(list(ss_run, ss_run_Tier3, ms_run, ms_run_f25), model_names = c("No F", "F25", "M No F", "M F25"), incl_proj = TRUE)

plot_ssb(list(ss_run_Tier3, ss_run_M_Tier3, ms_run_f25), model_names = c("Single-species Fix M", "Single-species Est M", "Multi-species"), incl_proj = TRUE, file = "Results/GOA_OM")

################################################
# Load and run summary
################################################
for(i in 1:length(dir_no_cap_names_GOA)){
  
  # - Load
  mse3 <- load_mse(dir = dir_no_cap_names_GOA[i], file = NULL)
  
  
  # - Calculate depletion for multi-species models
  # -- Update Ftarget and Fspp
  for(j in 1:length(mse3)){
    mse3[[j]]$OM$quantities$Ftarget <- projected_models_F[[i]]$quantities$Ftarget #FIXME - remove upon reupdate of Rceattle
    
    if(mse3[[j]]$OM$data_list$msmMode == 1){
      mse3[[j]]$OM$quantities$depletionSSB <- mse3[[j]]$OM$quantities$biomassSSB / ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)] # Divide ssb by SSB in 2060 under no fishing
      
      mse3[[j]]$OM$quantities$SB0 <- ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)] # Update SB0
      
      mse3[[j]]$OM$data_list$Plimit <- 0.25 # Update Target
      mse3[[j]]$OM$data_list$Ptarget <- 0.45 # Update Limit
      
      mse3[[j]]$OM$quantities$Flimit <- ms_run_f25$quantities$Ftarget # Update Flimit from Ftarget that was optimized
      mse3[[j]]$OM$quantities$Ftarget <- ms_run_f25$quantities$Ftarget # Update Flimit from Ftarget that was optimized
    }
  }
  
  # - Performance metrics
  mse_metrics <- mse_summary(mse3)
  mse_metrics <- mse_metrics[1:3,-c(2:3)]
  mse_metrics <- pivot_longer(mse_metrics, cols = 2:ncol(mse_metrics))
  colnames(mse_metrics) <- c("Species", "Performance metric", MSE_names[i])
  
  if(i == 1){mse_metrics_complete = mse_metrics}
  if(i != 1){mse_metrics_complete = merge(mse_metrics_complete, mse_metrics, by = c("Species", "Performance metric"))}
  write.csv(mse_metrics, file = paste0("Results/GOA_table", MSE_names[i],".csv"))
  
  
  # - Plot
  plot_depletionSSB(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/Depletion/GOA true ", MSE_names[i]), line_col  = "#04395E", reference = projected_models_no_F[[i]], top_adj = 1, species = c(1,3,2), width = 3.3, height = 4)
  plot_depletionSSB(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/Depletion/GOA Perceived ", MSE_names[i]), line_col = "#5F0F40", top_adj = 1, species = c(1,3,2), width = 3.3, height = 4)
  
  plot_depletionSSB(mse3$Sim_18$EM, mse = FALSE, incl_proj = TRUE, file = paste0("Results/Figures/Depletion/GOA Perceived 1-Sim ", MSE_names[i]), species = c(1,3,2), width = 3.3, height = 4)
  
  plot_ssb(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/SSB/GOA true ", MSE_names[i]), line_col  = "#04395E", reference = projected_models_no_F[[i]], species = c(1,3,2), width = 3.3, height = 4)
  plot_ssb(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/SSB/GOA Perceived ", MSE_names[i]), line_col = "#5F0F40", species = c(1,3,2))
  
  plot_biomass(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/B/GOA true ", MSE_names[i]), line_col  = "#04395E", reference = projected_models_no_F[[i]], species = c(1,3,2), width = 3.3, height = 4)
  plot_biomass(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/B/GOA Perceived ", MSE_names[i]), line_col = "#5F0F40", species = c(1,3,2), width = 3.3, height = 4)
  
  plot_recruitment(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/R/GOA true ", MSE_names[i]), line_col  = "#04395E", species = c(1,3,2), width = 3.3, height = 4)
  plot_recruitment(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/R/GOA Perceived ", MSE_names[i]), line_col = "#5F0F40", species = c(1,3,2), width = 3.3, height = 4)
  
  plot_f(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/F/GOA true ", MSE_names[i]), line_col  = "#04395E", species = c(1,3,2), width = 3.3, height = 4)
  plot_f(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/F/GOA Perceived ", MSE_names[i]), line_col  = "#04395E", species = c(1,3,2), width = 3.3, height = 4)
  
  plot_catch(mse3, mse = TRUE, file = paste0("Results/Figures/Catch/GOA true ", MSE_names[i]), line_col  = "#04395E", ymax = c(1500000, 180000, 70000, 32000, 120000), width = 3.3, height = 4)
  
  # - Unload for memory
  rm(mse3)
}

write.csv(mse_metrics_complete, file = paste0("Results/GOA_table.csv"))
