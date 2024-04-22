source("R/Functions/plot_m_through_time.R")

check_mse_convergence <- function(system = "GOA1977", regen = "FALSE", cap = FALSE, om_names = NULL, em_hcr_names = NULL){

  ### Run MSEs
  ## Loop across OMs, EMs, and Rec Scenarios
  mse_check <- list()
  ind <- 1
  for(om in 1:length(om_names)){  # OM model
    for(em in 1:length(em_hcr_names)){ # EM and HCR
      
      print(paste0("OM ", om, ": EM ", em))
      
      # STEP 1 -- Load MSE
      msedir <- paste0("E:/Rceattle_MSE/Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/", regen," regen/", cap, " cap")
      if(!dir.exists(msedir)){
        stop(paste0(msedir, " DOES NOT EXIST"))
      }
      mse_check[[ind]] <- check_mse(dir = msedir, file = NULL)
      ind <- ind+1
    }
  }
  
  return(do.call("rbind", mse_check))
}

summary_fun <- function(system = "GOA1977", regen = "FALSE", cap = FALSE, om_list_no_F =  NULL, om_names = NULL, em_hcr_names = NULL, species = c(1,3,2), savedir = "Results/Climate MSE/", exclude = NULL){
  
  # - Input checks
  if(length(om_names) != length(om_list_no_F)){
    stop("Length of OM names does not equal length of OMs")
  }
  
  ################################################
  # Load and run summary
  ################################################
  ### Run MSEs
  ## Loop across OMs, EMs, and Rec Scenarios
  for(om in 1:length(om_names)){  # OM model
    for(em in 1:length(em_hcr_names)){ # EM and HCR
      
      print(paste0("OM ", om, ": EM ", em))
      
      # STEP 1 -- Load MSE
      msedir <- paste0("E:/Rceattle_MSE/Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/", regen," regen/", cap, " cap")
      if(!dir.exists(msedir)){
        stop(paste0(msedir, " DOES NOT EXIST"))
      }
      mse3 <- load_mse(dir = msedir, file = NULL, exclude = exclude)
      MSE_names <- paste0(om_names[om],"__", em_hcr_names[em], "_", regen,"regen",  "_", cap, "cap")
      
      om_list_no_F[[om]]$quantities$depletionSSB <- om_list_no_F[[om]]$quantities$biomassSSB / om_list_no_F[[om]]$quantities$biomassSSB[,ncol(om_list_no_F[[om]]$quantities$biomassSSB)]
      
      
      # STEP 2 -- Update Ftarget, Flimit, and depletion for OMs
      for(j in 1:length(mse3)){
        
        # - SINGLE-SPECIES
        if(mse3[[j]]$OM$data_list$msmMode == 0){
          stop("No single-species OMs")
        }
        
        # - MULTI-SPECIES
        # - Calculate depletion for multi-species models
        if(mse3[[j]]$OM$data_list$msmMode == 1){
          mse3[[j]]$OM$quantities$depletionSSB <- mse3[[j]]$OM$quantities$biomassSSB / om_list_no_F[[om]]$quantities$biomassSSB[,ncol(om_list_no_F[[om]]$quantities$biomassSSB)] # Divide ssb by SSB in 2060 under no fishing
          
          mse3[[j]]$OM$quantities$SB0 <- om_list_no_F[[om]]$quantities$biomassSSB[,ncol(om_list_no_F[[om]]$quantities$biomassSSB)] # Update SB0
          
          mse3[[j]]$OM$data_list$Plimit[1:3] <- 0.25 # Update Target
          mse3[[j]]$OM$data_list$Ptarget[1:3] <- 0.40 # Update Limit
        }
      }
      
      
      # STEP 3 - Performance metrics
      # - Get summary
      mse_metrics <- mse_summary(mse3)
      mse_metrics <- mse_metrics[1:3,-c(2:3)]
      mse_metrics <- tidyr::pivot_longer(mse_metrics, cols = 2:ncol(mse_metrics))
      colnames(mse_metrics) <- c("Species", "Performance metric", MSE_names)
      mse_metrics$OM = om_names[om]
      mse_metrics$EM = em_hcr_names[em]
      mse_metrics$Cap = cap
      
      # - Save
      dir.create(paste0(savedir, "Tables/",system), recursive = TRUE, showWarnings = FALSE)
      write.csv(mse_metrics, file = paste0(savedir, "Tables/",system,"/",system, "_table", MSE_names,".csv"))
      
      
      # STEP 4 - Plot
      # - Create directories
      dir.create(paste0(savedir, "Figures/Depletion/", system,  "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0(savedir, "Figures/Depletion/", system,  "/True/"), recursive = TRUE, showWarnings = FALSE)
      
      dir.create(paste0(savedir, "Figures/SSB/", system, "/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0(savedir, "Figures/SSB/", system,  "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0(savedir, "Figures/SSB/", system,  "/True/"), recursive = TRUE, showWarnings = FALSE)
      
      dir.create(paste0(savedir, "Figures/B/", system,  "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0(savedir, "Figures/B/", system,  "/True/"), recursive = TRUE, showWarnings = FALSE)
      
      dir.create(paste0(savedir, "Figures/R/", system,  "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0(savedir, "Figures/R/", system,  "/True/"), recursive = TRUE, showWarnings = FALSE)
      
      dir.create(paste0(savedir, "Figures/M/", system,  "/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0(savedir, "Figures/F/", system,  "/Perceived/"), recursive = TRUE, showWarnings = FALSE)
      dir.create(paste0(savedir, "Figures/F/", system,  "/True/"), recursive = TRUE, showWarnings = FALSE)
      
      
      dir.create(paste0(savedir, "Figures/Catch/", system,"/"), recursive = TRUE, showWarnings = FALSE)
      
      
      # - Max year for plots
      maxyr <- min(c(mse3$Sim_1$EM$EM$data_list$projyr, mse3$Sim_1$OM$data_list$projyr))
      
      # - Depletion
      plot_depletionSSB(mse3, mse = TRUE, OM = TRUE, file = paste0(savedir, "Figures/Depletion/", system,  "/True/", system, " True ", MSE_names), 
                        line_col  = "#04395E", reference = om_list_no_F[[om]], top_adj = 1, species = species, width = 4.3, height = 4, maxyr = maxyr)
      
      plot_depletionSSB(mse3, mse = TRUE, OM = FALSE, file = paste0(savedir, "Figures/Depletion/", system,  "/Perceived/", system, " Perceived ", MSE_names), 
                        line_col = "#5F0F40", top_adj = 1, species = species, width = 4.3, height = 4, maxyr = maxyr)
      
      # - SSB
      plot_ssb(mse3, mse = TRUE, OM = TRUE, file = paste0(savedir, "Figures/SSB/", system,  "/True/", system, " True ", MSE_names),
               line_col  = "#04395E", reference = om_list_no_F[[om]], species = species, width = 4.3, height = 4, maxyr = maxyr)
      plot_ssb(mse3, mse = TRUE, OM = FALSE, file = paste0(savedir, "Figures/SSB/", system,  "/Perceived/",  system, " Perceived ", MSE_names),
               line_col = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
      
      plot_ssb(c(list(mse3$Sim_1$OM), mse3$Sim_1$EM), # file = paste0(savedir, "Figures/SSB/", system,  "/",  system, " single sim ", MSE_names),
               species = species, maxyr = maxyr)
      
      # - Biomass
      plot_biomass(mse3, mse = TRUE, OM = TRUE, file = paste0(savedir, "Figures/B/", system,  "/True/", system, " True ", MSE_names),
                   line_col  = "#04395E", reference = om_list_no_F[[om]], species = species, width = 4.3, height = 4, maxyr = maxyr)
      plot_biomass(mse3, mse = TRUE, OM = FALSE, file = paste0(savedir, "Figures/B/", system,  "/Perceived/", system, " Perceived ", MSE_names),
                   line_col = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
      
      # - Recruitment
      plot_recruitment(mse3, mse = TRUE, OM = TRUE, file = paste0(savedir, "Figures/R/", system,  "/True/", system, " True ", MSE_names),
                       line_col  = "#04395E", species = species, width = 4.3, height = 4, maxyr = maxyr)
      plot_recruitment(mse3, mse = TRUE, OM = FALSE, file = paste0(savedir, "Figures/R/", system,  "/Perceived/",  system, " Perceived ", MSE_names),
                       line_col = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
      
      # - F and M
      plot_f(mse3, mse = TRUE, OM = TRUE, file = paste0(savedir, "Figures/F/",system,  "/True/", system, " True ", MSE_names),
             line_col  = "#04395E", species = species, width = 4.3, height = 4, maxyr = maxyr)
      plot_f(mse3, mse = TRUE, OM = FALSE, file = paste0(savedir, "Figures/F/",system,  "/Perceived/", system, " Perceived ", MSE_names),
             line_col  = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
      # plot_m_at_age_mse(mse3, file = paste0(savedir, "Figures/M/", system,  "/", system, " Perceived ", MSE_names),
      #                   line_col = "#5F0F40", top_adj = 1, species = species, width = 4.3, height = 4, age = 1)
      
      
      # - Catch
      plot_catch(mse3, mse = TRUE, file = paste0(savedir, "Figures/Catch/", system, "/", MSE_names), line_col  = "#04395E", width = 4.3, height = 4, maxyr = maxyr)
      
      # - Unload for memory
      rm(mse3); gc()
    }
  }
}
