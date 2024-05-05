source("R/Functions/plot_m_through_time.R")

check_mse_convergence <- function(system = "GOA1977", regen = "FALSE", cap = FALSE, mse_om_names = NULL, em_hcr_names = NULL){
  
  ### Run MSEs
  ## Loop across OMs, EMs, and Rec Scenarios
  mse_check <- list()
  ind <- 1
  for(om in 1:length(mse_om_names)){  # OM model
    for(em in 1:length(em_hcr_names)){ # EM and HCR
      
      print(paste0("OM ", om, ": EM ", em))
      
      # STEP 1 -- Load MSE
      msedir <- paste0("Runs/", system,"/", mse_om_names[om],"/", em_hcr_names[em],"/", regen," regen/", cap, " cap")
      if(!dir.exists(msedir)){
        stop(paste0(msedir, " DOES NOT EXIST"))
      }
      mse_check[[ind]] <- check_mse(dir = msedir, file = NULL)
      ind <- ind+1
    }
  }
  
  return(do.call("rbind", mse_check))
}

summary_fun <- function(system = "GOA_Climate_2", regen = "FALSE", cap = FALSE, om_list_no_F =  NULL, mse_om_names = NULL, em_hcr_names = NULL, species = c(1,3,2), savedir = "Results/Climate MSE/", exclude = NULL){
  
  # - Input checks
  # if(length(mse_om_names) != length(om_list_no_F)){
  #   stop("Length of OM names does not equal length of OMs")
  # }
  
  ################################################
  # Load and run summary
  ################################################
  ### Run MSEs
  ## Loop across OMs, EMs, and Rec Scenarios
  for(om in 1:length(mse_om_names)){  # OM model
    for(em in 1:length(em_hcr_names)){ # EM and HCR
      
      print(paste0("OM ", om, ": EM ", em))
      
      # STEP 1 -- Load MSE
      msedir <- paste0("Runs/", system,"/", mse_om_names[om],"/", em_hcr_names[em],"/", regen," regen/", cap, " cap")
      if(!dir.exists(msedir)){
        stop(paste0(msedir, " DOES NOT EXIST"))
      }
      mse3 <- load_mse(dir = msedir, file = NULL, exclude = exclude)
      MSE_names <- paste0(mse_om_names[om],"__", em_hcr_names[em], "_", regen,"regen",  "_", cap, "cap")
      mse3 <- mse3[sapply(mse3, function(x) x$use_sim)]
      
      # om_list_no_F[[om]]$quantities$depletionSSB <- om_list_no_F[[om]]$quantities$biomassSSB / om_list_no_F[[om]]$quantities$biomassSSB[,ncol(om_list_no_F[[om]]$quantities$biomassSSB)]
      
      if(length(mse3) > 0){
        
        # STEP 2 -- Update Ftarget, Flimit, and depletion for OMs
        for(j in 1:length(mse3)){
          
          # - SINGLE-SPECIES
          if(mse3[[j]]$OM$data_list$msmMode == 0){
            stop("No single-species OMs")
          }
          
          # - MULTI-SPECIES
          # - Calculate depletion for multi-species models
          if(mse3[[j]]$OM$data_list$msmMode == 1){
            mse3[[j]]$OM$quantities$depletionSSB <- mse3[[j]]$OM$quantities$biomassSSB / mse3[[j]]$OM_no_F$quantities$biomassSSB # Dynamic RPs
            # 
            # mse3[[j]]$OM$quantities$SB0 <- om_list_no_F[[om]]$quantities$biomassSSB[,ncol(om_list_no_F[[om]]$quantities$biomassSSB)] # Update SB0
            # 
            # mse3[[j]]$OM$data_list$Plimit[1:3] <- 0.25 # Update Target
            # mse3[[j]]$OM$data_list$Ptarget[1:3] <- 0.40 # Update Limit
          }
        }
        
        
        # STEP 3 - Performance metrics
        # - Get summary
        mse_metrics <- mse_summary(mse3)
        mse_metrics <- mse_metrics[1:3,-c(2:3)]
        mse_metrics <- tidyr::pivot_longer(mse_metrics, cols = 2:ncol(mse_metrics))
        colnames(mse_metrics) <- c("Species", "Performance metric", MSE_names)
        mse_metrics$OM = mse_om_names[om]
        mse_metrics$EM = em_hcr_names[em]
        mse_metrics$Cap = cap
        
        # - Save
        dir.create(paste0(savedir, "Tables/",system), recursive = TRUE, showWarnings = FALSE)
        mse_metrics$nsim = length(mse3)
        write.csv(mse_metrics, file = paste0(savedir, "Tables/",system,"/",system, "_table", MSE_names,".csv"))
        
        
        # STEP 4 - Plot
        # - Create directories
        dir.create(paste0(savedir, "Figures/Depletion/Perceived/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0(savedir, "Figures/Depletion/True/"), recursive = TRUE, showWarnings = FALSE)
        
        dir.create(paste0(savedir, "Figures/SSB/Perceived/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0(savedir, "Figures/SSB/True/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0(savedir, "Figures/SSB/Comparison/"), recursive = TRUE, showWarnings = FALSE)
        
        dir.create(paste0(savedir, "Figures/B/Perceived/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0(savedir, "Figures/B/True/"), recursive = TRUE, showWarnings = FALSE)
        
        dir.create(paste0(savedir, "Figures/R/Perceived/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0(savedir, "Figures/R/True/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0(savedir, "Figures/R/Comparison/"), recursive = TRUE, showWarnings = FALSE)
        
        dir.create(paste0(savedir, "Figures/M/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0(savedir, "Figures/F/Perceived/"), recursive = TRUE, showWarnings = FALSE)
        dir.create(paste0(savedir, "Figures/F/True/"), recursive = TRUE, showWarnings = FALSE)
        
        dir.create(paste0(savedir, "Figures/Catch/"), recursive = TRUE, showWarnings = FALSE)
        
        
        # - Max year for plots
        maxyr <- min(c(mse3[[1]]$EM$EM$data_list$projyr, mse3[[1]]$OM$data_list$projyr))
        
        # - Depletion
        print("-P plot")
        plot_depletionSSB(mse3, mse = TRUE, OM = TRUE, file = paste0(savedir, "Figures/Depletion/True/", system, " True ", MSE_names), 
                          line_col  = "#04395E", reference = NULL, # om_list_no_F[[om]], 
                          top_adj = 1, species = species, width = 4.3, height = 4, maxyr = maxyr)
        
        plot_depletionSSB(mse3, mse = TRUE, OM = FALSE, file = paste0(savedir, "Figures/Depletion/Perceived/", system, " Perceived ", MSE_names), 
                          line_col = "#5F0F40", top_adj = 1, species = species, width = 4.3, height = 4, maxyr = maxyr)
        
        
        # - SSB
        print("-SSB plot")
        plot_ssb(mse3, mse = TRUE, OM = TRUE, file = paste0(savedir, "Figures/SSB/True/", system, " True ", MSE_names),
                 line_col  = "#04395E", reference = NULL, # om_list_no_F[[om]], 
                 species = species, width = 4.3, height = 4, maxyr = maxyr)
        plot_ssb(mse3, mse = TRUE, OM = FALSE, file = paste0(savedir, "Figures/SSB/Perceived/",  system, " Perceived ", MSE_names),
                 line_col = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
        
        plot_ssb(c(list(mse3[[1]]$OM), mse3[[1]]$EM), file = paste0(savedir, "Figures/SSB/",  system, " single sim ", MSE_names),
                 species = species, maxyr = maxyr)
        
        # -- SSB with and without fishing
        MPcols <- gmri_pal("main")(8)
        plot_ssb_mse(Rceattle = list(lapply(mse3, function(x) x$OM_no_F), lapply(mse3, function(x) x$OM)), 
                     file = paste0(savedir, "Figures/SSB/Comparison/",  system, " True with reference ", MSE_names),
                     line_col = MPcols[c(1,7)], species = species, width = 4.3, height = 4, maxyr = maxyr, alpha = 0.6)
        
        
        # - Biomass
        print("-B plot")
        plot_biomass(mse3, mse = TRUE, OM = TRUE, file = paste0(savedir, "Figures/B/True/", system, " True ", MSE_names),
                     line_col  = "#04395E", reference = NULL, #om_list_no_F[[om]], 
                     species = species, width = 4.3, height = 4, maxyr = maxyr)
        plot_biomass(mse3, mse = TRUE, OM = FALSE, file = paste0(savedir, "Figures/B/Perceived/", system, " Perceived ", MSE_names),
                     line_col = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
        
        # - Recruitment
        print("-R plot")
        plot_recruitment(mse3, mse = TRUE, OM = TRUE, file = paste0(savedir, "Figures/R/True/", system, " True ", MSE_names),
                         line_col  = "#04395E", species = species, width = 4.3, height = 4, maxyr = maxyr)
        plot_recruitment(mse3, mse = TRUE, OM = FALSE, file = paste0(savedir, "Figures/R/Perceived/",  system, " Perceived ", MSE_names),
                         line_col = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
        plot_ssb_mse(list(lapply(mse3, function(x) x$OM_no_F), lapply(mse3, function(x) x$OM)), 
                     file = paste0(savedir, "Figures/R/Comparison/",  system, " True with reference ", MSE_names),
                     line_col = MPcols[c(1,7)], rec = TRUE, species = species, width = 4.3, height = 4, maxyr = maxyr, alpha = 0.6)
        
        # - F and M
        print("-F plot")
        plot_f(mse3, mse = TRUE, OM = TRUE, file = paste0(savedir, "Figures/F/True/", system, " True ", MSE_names),
               line_col  = "#04395E", species = species, width = 4.3, height = 4, maxyr = maxyr)
        plot_f(mse3, mse = TRUE, OM = FALSE, file = paste0(savedir, "Figures/F/Perceived/", system, " Perceived ", MSE_names),
               line_col  = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
        # plot_m_at_age_mse(mse3, file = paste0(savedir, "Figures/M/", system, " Perceived ", MSE_names),
        #                   line_col = "#5F0F40", top_adj = 1, species = species, width = 4.3, height = 4, age = 1)
        
        
        # - Catch
        plot_catch(mse3, mse = TRUE, file = paste0(savedir, "Figures/Catch/", MSE_names), line_col  = "#04395E", width = 4.3, height = 4, maxyr = maxyr)
        

      } else{
        cat(paste0("NO SIMs: OM-", om," ", mse_om_names[om]," EM-", em, " ", em_hcr_names[em], " ", regen,"-regen",  " ", cap, "-cap"), sep = "\n", file = "MSE_log_output.txt", append = TRUE)
      }
      # - Unload for memorys
      rm(mse3); gc()
    }
  }
}
