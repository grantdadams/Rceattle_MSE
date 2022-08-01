
summary_fun <- function(system = "GOA1977", recname = "ConstantR", om_list = NULL, projected_OM_no_F = NULL, om_names = NULL, em_hcr_list_fixM = NULL, em_hcr_list_estM = NULL, em_hcr_names = NULL){
  ################################################
  # Load and run summary
  ################################################
  ## Loop across OMs,
  ### Set up parallel processing
  library(foreach)
  library(doParallel)
  
  cores = detectCores() - 2
  registerDoParallel(cores)
  
  
  ### Run MSEs
  ## Loop across OMs,
  summary <- foreach(om = 1:length(om_list)) %:%  # OM model
    foreach(em = 1:length(em_hcr_names)) %:% # EM and HCR
    foreach(rec = 1:length(recname)) %dopar% {   # Rec trends
      
      # STEP 1 -- Load MSE
      mse3 <- load_mse(dir = paste0("Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/",recname[rec],"/No cap"), file = NULL)
      MSE_names <- paste0(om_names[om],"__", em_hcr_names[em], "_", recname[rec])
      
      
      # STEP 2 -- Update Ftarget and Flimit for OMs
      for(j in 1:length(mse3)){
        
        # - Ftarget and Flimit for single-species models
        if(mse3[[j]]$OM$data_list$msmMode == 0){
          
          # -- Fix M
          if(unique(mse3[[j]]$OM$data_list$est_M1) == 0){
            mse3[[j]]$OM$data_list$Plimit <- em_hcr_list_fixM[[em]]$data_list$Plimit # Update Target
            mse3[[j]]$OM$data_list$Ptarget <- em_hcr_list_fixM[[em]]$data_list$Ptarget # Update Limit
            
            mse3[[j]]$OM$quantities$Flimit <- em_hcr_list_fixM[[em]]$quantities$Flimit # Update Flimit from Ftarget that was optimized
            mse3[[j]]$OM$quantities$Ftarget <- em_hcr_list_fixM[[em]]$quantities$Ftarget # Update Flimit from Ftarget that was optimized
          }
          
          # -- Estimate M
          if(unique(mse3[[j]]$OM$data_list$est_M1) == 1){
            # - Minus 8 because fix m is the first om in the list and there are 8 hcrs
            mse3[[j]]$OM$data_list$Plimit <- em_hcr_list_estM[[em]]$data_list$Plimit # Update Target
            mse3[[j]]$OM$data_list$Ptarget <- em_hcr_list_estM[[em]]$data_list$Ptarget # Update Limit
            
            mse3[[j]]$OM$quantities$Flimit <- em_hcr_list_estM[[em]]$quantities$Flimit # Update Flimit from Ftarget that was optimized
            mse3[[j]]$OM$quantities$Ftarget <- em_hcr_list_estM[[em]]$quantities$Ftarget # Update Flimit from Ftarget that was optimized
          }
        }
        
        
        # - Calculate depletion for multi-species models
        if(mse3[[j]]$OM$data_list$msmMode == 1){
          mse3[[j]]$OM$quantities$depletionSSB <- mse3[[j]]$OM$quantities$biomassSSB / ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)] # Divide ssb by SSB in 2060 under no fishing
          
          mse3[[j]]$OM$quantities$SB0 <- ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)] # Update SB0
          
          mse3[[j]]$OM$data_list$Plimit <- 0.25 # Update Target
          mse3[[j]]$OM$data_list$Ptarget <- 0.40 # Update Limit
          
          mse3[[j]]$OM$quantities$Flimit <- ms_run_f25$quantities$Ftarget # Update Flimit from Ftarget that was optimized
          mse3[[j]]$OM$quantities$Ftarget <- ms_run_f25$quantities$Ftarget # Update Flimit from Ftarget that was optimized
        }
      }
      
      # STEP 3 - Performance metrics
      mse_metrics <- mse_summary(mse3)
      mse_metrics <- mse_metrics[1:3,-c(2:3)]
      mse_metrics <- pivot_longer(mse_metrics, cols = 2:ncol(mse_metrics))
      colnames(mse_metrics) <- c("Species", "Performance metric", MSE_names)
      
      if(om == 1 & em == 1){mse_metrics_complete = mse_metrics}
      if(om != 1 | em != 1){mse_metrics_complete = cbind(mse_metrics_complete, mse_metrics[,-c(1,2)])}
      write.csv(mse_metrics, file = paste0("Results/",system, "_table", MSE_names,".csv"))
      
      
      # STEP 4 - Plot
      plot_depletionSSB(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/Depletion/", system, " true ", MSE_names), line_col  = "#04395E", reference = projected_OM_no_F[[om]], top_adj = 1, species = c(1,3,2), width = 4.3, height = 4)
      plot_depletionSSB(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/Depletion/", system, " Perceived ", MSE_names), line_col = "#5F0F40", top_adj = 1, species = c(1,3,2), width = 4.3, height = 4)
      
      plot_depletionSSB(mse3$Sim_1$EM, mse = FALSE, incl_proj = TRUE, file = paste0("Results/Figures/Depletion/", system, " 1 Sim/", system, " Perceived 1-Sim ", MSE_names), species = c(1,3,2), width = 4.3, height = 4)
      
      plot_ssb(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/SSB/", system, " true ", MSE_names), line_col  = "#04395E", reference = projected_OM_no_F[[om]], species = c(1,3,2), width = 4.3, height = 4)
      plot_ssb(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/SSB/", system, " Perceived ", MSE_names), line_col = "#5F0F40", species = c(1,3,2))
      
      plot_biomass(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/B/", system, " true ", MSE_names), line_col  = "#04395E", reference = projected_OM_no_F[[om]], species = c(1,3,2), width = 4.3, height = 4)
      plot_biomass(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/B/", system, " Perceived ", MSE_names), line_col = "#5F0F40", species = c(1,3,2), width = 4.3, height = 4)
      
      plot_recruitment(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/R/", system, " true ", MSE_names), line_col  = "#04395E", species = c(1,3,2), width = 4.3, height = 4)
      plot_recruitment(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/R/", system, " Perceived ", MSE_names), line_col = "#5F0F40", species = c(1,3,2), width = 4.3, height = 4)
      
      plot_f(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/F/",system, " true ", MSE_names), line_col  = "#04395E", species = c(1,3,2), width = 4.3, height = 4)
      plot_f(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/F/",system, " Perceived ", MSE_names), line_col  = "#5F0F40", species = c(1,3,2), width = 4.3, height = 4)
      
      plot_catch(mse3, mse = TRUE, file = paste0("Results/Figures/Catch/",system, " true ", MSE_names), line_col  = "#04395E", ymax = c(1500000, 180000, 70000, 32000, 120000), width = 4.3, height = 4)
      # 
      # - Unload for memory
      rm(mse3)
    }
  
  #write.csv(mse_metrics_complete, file = paste0("Results/Tables/", system, "_", recname[rec],".csv"))
}
