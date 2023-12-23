source("R/Functions/plot_m_through_time.R")

summary_fun <- function(system = "GOA1977", recname = "ConstantR", om_list_no_F =  NULL, om_names = NULL, om_hcr_list_fixM = NULL, om_hcr_list_estM = NULL, em_hcr_names = NULL, species = c(1,3,2)){
  ################################################
  # Load and run summary
  ################################################
  ### Run MSEs
  ## Loop across OMs, EMs, and Rec Scenarios
  for(om in 1:length(om_list_no_F)){  # OM model
    for(em in 1:length(em_hcr_names)){ # EM and HCR
      for(rec in 1:length(recname)){   # Rec trends
        
        print(paste0("OM ", om, ": EM ", em))
        
        # STEP 1 -- Load MSE
        if(!dir.exists(paste0("Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/",recname[rec],"/No cap"))){
          stop(paste0("Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/",recname[rec],"/No cap DOES NOT EXIST"))
          }
        mse3 <- load_mse(dir = paste0("Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/",recname[rec],"/No cap"), file = NULL)
        MSE_names <- paste0(om_names[om],"__", em_hcr_names[em], "_", recname[rec])
        
        
        # STEP 2 -- Update Ftarget, Flimit, and depletion for OMs
        for(j in 1:length(mse3)){
          
          # - SINGLE-SPECIES
          if(mse3[[j]]$OM$data_list$msmMode == 0){
            
            # - Update depletion
            mse3[[j]]$OM$quantities$SB0 <- om_list_no_F[[om]]$quantities$SB0[,ncol(om_list_no_F[[om]]$quantities$SB0)]
            mse3[[j]]$OM$quantities$depletionSSB <- mse3[[j]]$OM$quantities$biomassSSB/mse3[[j]]$OM$quantities$SB0 #FIXME: no longer necessary
            
            # -- Dynamic BRPs
            if(mse3[[j]]$EM[[1]]$data_list$DynamicHCR == 1){
              mse3[[j]]$OM$quantities$depletionSSB = mse3[[j]]$OM$quantities$biomassSSB/mse3[[j]]$OM$quantities$DynamicSB0
              mse3[[j]]$OM$quantities$depletion = mse3[[j]]$OM$quantities$biomass/mse3[[j]]$OM$quantities$DynamicB0
            }
            
            # -- Fix M
            if(sum(mse3[[j]]$OM$data_list$M1_model) == 0){
              mse3[[j]]$OM$data_list$Plimit <- om_hcr_list_fixM[[em]]$data_list$Plimit # Update Target
              mse3[[j]]$OM$data_list$Ptarget <- om_hcr_list_fixM[[em]]$data_list$Ptarget # Update Limit
              
              mse3[[j]]$OM$quantities$Flimit <- om_hcr_list_fixM[[em]]$quantities$Flimit # Update Flimit from Ftarget that was optimized
              mse3[[j]]$OM$quantities$Ftarget <- om_hcr_list_fixM[[em]]$quantities$Ftarget # Update Flimit from Ftarget that was optimized
            }
            
            # -- Estimate M
            if(sum(mse3[[j]]$OM$data_list$M1_model) > 0){
              mse3[[j]]$OM$data_list$Plimit <- om_hcr_list_estM[[em]]$data_list$Plimit # Update Target
              mse3[[j]]$OM$data_list$Ptarget <- om_hcr_list_estM[[em]]$data_list$Ptarget # Update Limit
              
              mse3[[j]]$OM$quantities$Flimit <- om_hcr_list_estM[[em]]$quantities$Flimit # Update Flimit from Ftarget that was optimized
              mse3[[j]]$OM$quantities$Ftarget <- om_hcr_list_estM[[em]]$quantities$Ftarget # Update Flimit from Ftarget that was optimized
            }
          }
          
          # - MULTI-SPECIES
          # - Calculate depletion for multi-species models
          if(mse3[[j]]$OM$data_list$msmMode == 1){
            mse3[[j]]$OM$quantities$depletionSSB <- mse3[[j]]$OM$quantities$biomassSSB / om_list_no_F[[om]]$quantities$biomassSSB[,ncol(om_list_no_F[[om]]$quantities$biomassSSB)] # Divide ssb by SSB in 2060 under no fishing
            
            mse3[[j]]$OM$quantities$SB0 <- om_list_no_F[[om]]$quantities$biomassSSB[,ncol(om_list_no_F[[om]]$quantities$biomassSSB)] # Update SB0
            
            mse3[[j]]$OM$data_list$Plimit[1:3] <- 0.25 # Update Target
            mse3[[j]]$OM$data_list$Ptarget[1:3] <- 0.40 # Update Limit
            
            mse3[[j]]$OM$quantities$Flimit <- ms_run_f25$quantities$Ftarget # Update Flimit from Ftarget that was optimized
            mse3[[j]]$OM$quantities$Ftarget <- ms_run_f25$quantities$Ftarget # Update Flimit from Ftarget that was optimized
          }
        }
        
        
        # STEP 3 - Performance metrics
        mse_metrics <- mse_summary(mse3)
        mse_metrics <- mse_metrics[1:3,-c(2:3)]
        mse_metrics <- tidyr::pivot_longer(mse_metrics, cols = 2:ncol(mse_metrics))
        colnames(mse_metrics) <- c("Species", "Performance metric", MSE_names)
        
        #if(om == 1 & em == 1){mse_metrics_complete = mse_metrics}
        #if(om != 1 | em != 1){mse_metrics_complete = cbind(mse_metrics_complete, mse_metrics[,-c(1,2)])}
        dir.create(paste0("Results/Tables/",system), recursive = TRUE, showWarnings = FALSE)
        write.csv(mse_metrics, file = paste0("Results/Tables/",system,"/",system, "_", recname[rec], "_table", MSE_names,".csv"))
        
        
        # STEP 4 - Plot
        dir.create(paste0("Results/Figures/Depletion/", system, "/", recname[rec], "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0("Results/Figures/Depletion/", system, "/", recname[rec], "/True/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0("Results/Figures/SSB/", system, "/", recname[rec], "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0("Results/Figures/SSB/", system, "/", recname[rec], "/True/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0("Results/Figures/B/", system, "/", recname[rec], "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0("Results/Figures/B/", system, "/", recname[rec], "/True/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0("Results/Figures/R/", system, "/", recname[rec], "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0("Results/Figures/R/", system, "/", recname[rec], "/True/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0("Results/Figures/F/", system, "/", recname[rec], "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0("Results/Figures/F/", system, "/", recname[rec], "/True/"), recursive = TRUE, showWarnings = FALSE)
        
        dir.create(paste0("Results/Figures/M/", system, "/", recname[rec], "/"), recursive = TRUE, showWarnings = FALSE)

        
        # - Max year for plots
        maxyr <- mse3$Sim_1$EM$EM$data_list$projyr

        
        plot_depletionSSB(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/Depletion/", system, "/", recname[rec], "/True/", system, "_", recname[rec], " true ", MSE_names), 
                          line_col  = "#04395E", reference = om_list_no_F[[om]], top_adj = 1, species = species, width = 4.3, height = 4, maxyr = maxyr)
        
        plot_depletionSSB(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/Depletion/", system, "/", recname[rec], "/Perceived/", system, "_", recname[rec], " Perceived ", MSE_names), 
                          line_col = "#5F0F40", top_adj = 1, species = species, width = 4.3, height = 4, maxyr = maxyr)
        
        plot_m_at_age_mse(mse3, file = paste0("Results/Figures/M/", system, "/", recname[rec], "/", system, "_", recname[rec], " Perceived ", MSE_names), 
                          line_col = "#5F0F40", top_adj = 1, species = species, width = 4.3, height = 4, age = 1)

        # plot_ssb(c(list(mse3$Sim_1$OM), mse3$Sim_1$EM), mse = FALSE, incl_proj = TRUE, file = paste0("Results/Figures/SSB/", system, "_", recname[rec], " 1 Sim/", system, " Perceived 1-Sim ", MSE_names), species = species, width = 4.3, height = 4)

        plot_ssb(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/SSB/", system, "/", recname[rec], "/True/", system, "_", recname[rec], " true ", MSE_names), 
                 line_col  = "#04395E", reference = om_list_no_F[[om]], species = species, width = 4.3, height = 4, maxyr = maxyr)
        plot_ssb(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/SSB/", system, "/", recname[rec], "/Perceived/",  system, "_", recname[rec], " Perceived ", MSE_names), 
                 line_col = "#5F0F40", species = species, maxyr = maxyr)

        plot_biomass(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/B/", system, "/", recname[rec], "/True/", system, "_", recname[rec], " true ", MSE_names), 
                     line_col  = "#04395E", reference = om_list_no_F[[om]], species = species, width = 4.3, height = 4, maxyr = maxyr)
        plot_biomass(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/B/", system, "/", recname[rec], "/Perceived/", system, "_", recname[rec], " Perceived ", MSE_names), 
                     line_col = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)

        plot_recruitment(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/R/", system, "/", recname[rec], "/True/", system, "_", recname[rec], " true ", MSE_names), 
                         line_col  = "#04395E", species = species, width = 4.3, height = 4, maxyr = maxyr)
        plot_recruitment(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/R/", system, "/", recname[rec], "/Perceived/",  system, "_", recname[rec], " Perceived ", MSE_names), 
                         line_col = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)

        plot_f(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/F/",system, "/", recname[rec], "/True/", system, "_", recname[rec], " true ", MSE_names), 
               line_col  = "#04395E", species = species, width = 4.3, height = 4, maxyr = maxyr)
        plot_f(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/F/",system, "/", recname[rec], "/Perceived/", system, "_", recname[rec], " Perceived ", MSE_names), 
               line_col  = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
        #
        plot_catch(mse3, mse = TRUE, file = paste0("Results/Figures/Catch/",system, "_", recname[rec], " true ", MSE_names), line_col  = "#04395E", ymax = c(1500000, 180000, 70000, 32000, 120000), width = 4.3, height = 4)
        # #
        # # - Unload for memory
        rm(mse3); gc()
      }
    }
  }
  
  #write.csv(mse_metrics_complete, file = paste0("Results/Tables/", system, "_", recname[rec],".csv"))
}
