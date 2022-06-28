
#' Function to run MSEs looping across EMs and OMs in parallel
#'
#' @param system 
#' @param recname 
#'
#' @return
#' @export
#'
#' @examples
run_mse <- function(system = "GOA1977", recname = "ConstantR", om_list = NULL, om_names = NULL, em_hcr_list = NULL, em_hcr_names = NULL, sampling_period = NULL, rec_scen = list(0)){
  ### Set up parallel processing
  library(foreach)
  library(doParallel)
  
  cores = detectCores() - 2
  registerDoParallel(cores)
  
  
  ### Run MSEs
  ## Loop across OMs,
  stime <- system.time({
    mse <- foreach(om = 1:length(om_list)) %:%  # OM model
      foreach(em = 1:length(em_hcr_list)) %:% # EM and HCR
        foreach(rec = 1:length(rec_scen)) %dopar% { # Rec trends
        
        # Load libraries
        library(Rceattle)
        library(dplyr)
        
        # Update avg F given model fit to regenerated data
        if(em_hcr_list[[em]]$data_list$HCR == 2){
          # - Simulate index and comp data and updatae EM
          sim_dat <- sim_mod(om_list[[om]], simulate = TRUE)
          
          em_hcr_list[[em]]$data_list$srv_biom <- sim_dat$srv_biom
          em_hcr_list[[em]]$data_list$comp_data <- sim_dat$comp_data
          
          # - Restimate
          em_hcr_list[[em]] <- fit_mod(
            data_list = em_hcr_list[[em]]$data_list,
            inits = em_hcr_list[[em]]$estimated_params,
            map =  NULL,
            bounds = NULL,
            file = NULL,
            estimateMode = 0,
            random_rec = em_hcr_list[[em]]$data_list$random_rec,
            niter = em_hcr_list[[em]]$data_list$niter,
            msmMode = em_hcr_list[[em]]$data_list$msmMode,
            avgnMode = em_hcr_list[[em]]$data_list$avgnMode,
            minNByage = em_hcr_list[[em]]$data_list$minNByage,
            suitMode = em_hcr_list[[em]]$data_list$suitMode,
            phase = "default",
            updateM1 = FALSE,
            loopnum = 3,
            getsd = FALSE,
            verbose = 0)
          
          # - Get avg F
          avg_F <- (exp(em_hcr_list[[em]]$estimated_params$ln_mean_F+em_hcr_list[[em]]$estimated_params$F_dev)) # Average F from last 5 years
          avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])
          avg_F <- data.frame(avg_F = avg_F, spp = em_hcr_list[[em]]$data_list$fleet_control$Species)
          avg_F <- avg_F %>% 
            group_by(spp) %>%
            summarise(avg_F = sum(avg_F)) %>%
            arrange(spp)
          
          # - Update model
          em_hcr_list[[em]] <- Rceattle::fit_mod(data_list = em_hcr_list[[em]]$data_list,
                                                 inits = em_hcr_list[[em]]$estimated_params,
                                                 estimateMode = 0, # Estimate everything
                                                 HCR = build_hcr(HCR = 2, # Input F
                                                                 FsprTarget = avg_F$avg_F 
                                                 ),
                                                 msmMode = 0, # Single species mode
                                                 verbose = 1, updateM1 = FALSE)
        }
        
        # Update OM if single-species to have save HCR as EM to calculate performance metrics
        if(om_list[[om]]$data_list$msmMode == 0){
          om_list[[om]] <- Rceattle::fit_mod(data_list = om_list[[om]]$data_list,
                                             inits = om_list[[om]]$estimated_params,
                                             estimateMode = 2, # Run projection only
                                             HCR = build_hcr(HCR = em_hcr_list[[em]]$data_list$HCR, # Tier3 HCR
                                                             DynamicHCR = em_hcr_list[[em]]$data_list$DynamicHCR,
                                                             FsprTarget = em_hcr_list[[em]]$data_list$FsprTarget,
                                                             FsprLimit = em_hcr_list[[em]]$data_list$FsprLimit,
                                                             Ptarget = em_hcr_list[[em]]$data_list$Ptarget,
                                                             Plimit = em_hcr_list[[em]]$data_list$Plimit,
                                                             Alpha = em_hcr_list[[em]]$data_list$Alpha,
                                                             Pstar = em_hcr_list[[em]]$data_list$Pstar,
                                                             Sigma = em_hcr_list[[em]]$data_list$Sigma
                                             ),
                                             msmMode = 0, # Single species mode
                                             verbose = 1, updateM1 = FALSE)
        }
        
        # Run MSE
        mse <- mse_run(om = om_list[[om]], em = em_hcr_list[[em]], 
                       nsim = 200, 
                       assessment_period = 1, sampling_period = sampling_period, 
                       simulate_data = TRUE, sample_rec = TRUE, 
                       rec_trend = rec_scen[[rec]],
                       cap = NULL, 
                       dir = paste0("Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/",recname[rec],"/No cap"), 
                       file = NULL,
                       regenerate_past = TRUE)
      }
  })
  
  
  # When you're done, clean up the cluster
  stopImplicitCluster()
}

