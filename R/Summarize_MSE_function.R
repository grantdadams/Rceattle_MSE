source("R/plot_m_through_time.R")

summary_fun <- function(system = "GOA1977", om_list_no_F =  NULL, om_names = NULL, om_hcr_list_fixM = NULL, om_hcr_list_estM = NULL, em_hcr_names = NULL, species = c(1,3,2)){
  ################################################
  # Load and run summary
  ################################################
  ### Run MSEs
  ## Loop across OMs, EMs, and Rec Scenarios
  for(om in 1:length(om_list_no_F)){  # OM model
    for(em in 1:length(em_hcr_names)){ # EM and HCR
      
      print(paste0("OM ", om, ": EM ", em))
      
      # STEP 1 -- Load MSE
      if(!dir.exists(paste0("D:/MSE Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/No cap"))){
        stop(paste0("D:/MSE Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/No cap DOES NOT EXIST"))
      }
      mse3 <- load_mse(dir = paste0("D:/MSE Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/No cap"), file = NULL)
      MSE_names <- paste0(om_names[om],"__", em_hcr_names[em])
      
      
      # STEP 2 -- Update Ftarget, Flimit, and depletion for OMs
      for(j in 1:length(mse3)){
        
        # - SINGLE-SPECIES
        if(mse3[[j]]$OM$data_list$msmMode == 0){
          
          # Adjust SB0 because MSE OM uses proj_mean_rec = FALSE
          mse3[[j]]$OM$quantities$SB0[,] <- om_list_no_F[[om]]$quantities$SB0[,ncol(om_list_no_F[[om]]$quantities$SB0)]
          mse3[[j]]$OM$quantities$depletionSSB <- mse3[[j]]$OM$quantities$biomassSSB/mse3[[j]]$OM$quantities$SB0 #FIXME: no longer necessary
          
          # -- Dynamic BRPs
          if(mse3[[j]]$EM[[1]]$data_list$DynamicHCR == 1){
            mse3[[j]]$OM$quantities$depletionSSB = mse3[[j]]$OM$quantities$biomassSSB/mse3[[j]]$OM$quantities$DynamicSB0
            mse3[[j]]$OM$quantities$depletion = mse3[[j]]$OM$quantities$biomass/mse3[[j]]$OM$quantities$DynamicB0
          }
          
          # -- Fix M
          if(sum(mse3[[j]]$OM$data_list$M1_model) == 0){
            
            mse3[[j]]$OM$quantities$SBF[,] <- om_hcr_list_fixM[[em]]$quantities$SBF[,ncol(om_hcr_list_fixM[[em]]$quantities$SBF)] # Adjust SBF because MSE OM uses proj_mean_rec = FALSE
            
            mse3[[j]]$OM$data_list$Plimit <- om_hcr_list_fixM[[em]]$data_list$Plimit # Update Target
            mse3[[j]]$OM$data_list$Ptarget <- om_hcr_list_fixM[[em]]$data_list$Ptarget # Update Limit
            
            mse3[[j]]$OM$quantities$Flimit <- om_hcr_list_fixM[[em]]$quantities$Flimit # Update Flimit from Ftarget that was optimized
            mse3[[j]]$OM$quantities$Ftarget <- om_hcr_list_fixM[[em]]$quantities$Ftarget # Update Flimit from Ftarget that was optimized
          }
          
          # -- Estimate M
          if(sum(mse3[[j]]$OM$data_list$M1_model) > 0){
            mse3[[j]]$OM$quantities$SBF[,] <- om_hcr_list_estM[[em]]$quantities$SBF[,ncol(om_hcr_list_estM[[em]]$quantities$SBF)] # Adjust SBF because MSE OM uses proj_mean_rec = FALSE
            
            mse3[[j]]$OM$data_list$Plimit <- om_hcr_list_estM[[em]]$data_list$Plimit # Update Target
            mse3[[j]]$OM$data_list$Ptarget <- om_hcr_list_estM[[em]]$data_list$Ptarget # Update Limit
            
            mse3[[j]]$OM$quantities$Flimit <- om_hcr_list_estM[[em]]$quantities$Flimit # Update Flimit from Ftarget that was optimized
            mse3[[j]]$OM$quantities$Ftarget <- om_hcr_list_estM[[em]]$quantities$Ftarget # Update Flimit from Ftarget that was optimized
          }
        }
        
        # - MULTI-SPECIES
        # - Calculate depletion for multi-species models
        if(mse3[[j]]$OM$data_list$msmMode == 1){
          mse3[[j]]$OM$quantities$depletionSSB <- mse3[[j]]$OM$quantities$biomassSSB / om_list_no_F[[om]]$quantities$biomassSSB[,ncol(om_list_no_F[[om]]$quantities$biomassSSB)] # Divide ssb by SSB in 2100 under no fishing
          
          mse3[[j]]$OM$quantities$SB0 <- om_list_no_F[[om]]$quantities$biomassSSB[,ncol(om_list_no_F[[om]]$quantities$biomassSSB)] # Update SB0
          
          mse3[[j]]$OM$data_list$Plimit[1:3] <- 0.25 # Update Target
          mse3[[j]]$OM$data_list$Ptarget[1:3] <- 0.40 # Update Limit
          
          mse3[[j]]$OM$quantities$Flimit <- ms_run_f25$quantities$Ftarget # Update Flimit from Ftarget that was optimized
          mse3[[j]]$OM$quantities$Ftarget <- ms_run_f25$quantities$Ftarget # Update Flimit from Ftarget that was optimized
        }
      }
      
      
      # STEP 3 - Performance metrics
      # - Get summary
      mse_metrics <- mse_summary(mse3)
      mse_metrics <- mse_metrics[1:3,-c(2:3)]
      mse_metrics <- tidyr::pivot_longer(mse_metrics, cols = 2:ncol(mse_metrics))
      colnames(mse_metrics) <- c("Species", "Performance metric", MSE_names)
      
      # - Save
      dir.create(paste0("Results/Tables/",system), recursive = TRUE, showWarnings = FALSE)
      write.csv(mse_metrics, file = paste0("Results/Tables/",system,"/",system, "_table", MSE_names,".csv"))
      
      
      # STEP 4 - Plot
      # - Create directories
      dir.create(paste0("Results/Figures/Time-series plots/Depletion/", system,  "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0("Results/Figures/Time-series plots/Depletion/", system,  "/True/"), recursive = TRUE, showWarnings = FALSE)
      
      dir.create(paste0("Results/Figures/Time-series plots/SSB/", system, "/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0("Results/Figures/Time-series plots/SSB/", system,  "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0("Results/Figures/Time-series plots/SSB/", system,  "/True/"), recursive = TRUE, showWarnings = FALSE)
      
      dir.create(paste0("Results/Figures/Time-series plots/B/", system,  "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0("Results/Figures/Time-series plots/B/", system,  "/True/"), recursive = TRUE, showWarnings = FALSE)
      
      dir.create(paste0("Results/Figures/Time-series plots/R/", system,  "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0("Results/Figures/Time-series plots/R/", system,  "/True/"), recursive = TRUE, showWarnings = FALSE)
      
      dir.create(paste0("Results/Figures/Time-series plots/M/", system,  "/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0("Results/Figures/Time-series plots/F/", system,  "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0("Results/Figures/Time-series plots/F/", system,  "/True/"), recursive = TRUE, showWarnings = FALSE)
      
      
      dir.create(paste0("Results/Figures/Time-series plots/Catch/", system,"/"), recursive = TRUE, showWarnings = FALSE)
      
      
      # - Max year for plots
      maxyr <- mse3$Sim_1$EM$EM$data_list$projyr
      
      # - Depletion
      plot_depletionSSB(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/Time-series plots/Depletion/", system,  "/True/", system, " True ", MSE_names),
                        line_col  = "#04395E", reference = om_list_no_F[[om]], top_adj = 1, species = species, width = 4.3, height = 4, maxyr = maxyr)
      
      plot_depletionSSB(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/Time-series plots/Depletion/", system,  "/Perceived/", system, " Perceived ", MSE_names),
                        line_col = "#5F0F40", top_adj = 1, species = species, width = 4.3, height = 4, maxyr = maxyr)
      
      # - SSB
      plot_ssb(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/Time-series plots/SSB/", system,  "/True/", system, " True ", MSE_names),
               line_col  = "#04395E", reference = om_list_no_F[[om]], species = species, width = 4.3, height = 4, maxyr = maxyr)
      plot_ssb(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/Time-series plots/SSB/", system,  "/Perceived/",  system, " Perceived ", MSE_names),
               line_col = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
      
      plot_ssb(c(list(mse3$Sim_1$OM), mse3$Sim_1$EM), # file = paste0("Results/Figures/Time-series plots/SSB/", system,  "/",  system, " single sim ", MSE_names),
               species = species, maxyr = maxyr)
      
      # - Biomass
      plot_biomass(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/Time-series plots/B/", system,  "/True/", system, " True ", MSE_names),
                   line_col  = "#04395E", reference = om_list_no_F[[om]], species = species, width = 4.3, height = 4, maxyr = maxyr)
      plot_biomass(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/Time-series plots/B/", system,  "/Perceived/", system, " Perceived ", MSE_names),
                   line_col = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
      
      # - Recruitment
      plot_recruitment(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/Time-series plots/R/", system,  "/True/", system, " True ", MSE_names),
                       line_col  = "#04395E", species = species, width = 4.3, height = 4, maxyr = maxyr)
      plot_recruitment(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/Time-series plots/R/", system,  "/Perceived/",  system, " Perceived ", MSE_names),
                       line_col = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
      
      # - F and M
      plot_f(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/Time-series plots/F/",system,  "/True/", system, " True ", MSE_names),
             line_col  = "#04395E", species = species, width = 4.3, height = 4, maxyr = maxyr)
      plot_f(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/Time-series plots/F/",system,  "/Perceived/", system, " Perceived ", MSE_names),
             line_col  = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
      plot_m_at_age_mse(mse3, file = paste0("Results/Figures/Time-series plots/M/", system,  "/", system, " Perceived ", MSE_names),
                        line_col = "#5F0F40", top_adj = 1, species = species, width = 4.3, height = 4, age = 1)
      
      
      # - Catch
      plot_catch(mse3, mse = TRUE, file = paste0("Results/Figures/Time-series plots/Catch/", system, "/", MSE_names), line_col  = "#04395E", width = 4.3, height = 4, maxyr = maxyr)
      
      # - Unload for memory
      rm(mse3); gc()
    }
  }
}
