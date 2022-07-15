
#' Function to run MSEs looping across EMs and OMs in parallel
#'
#' @param system 
#' @param recname 
#'
#' @return
#' @export
#'
#' @examples
run_mse_np <- function(system = "GOA1977", recname = "ConstantR", om_list = NULL, om_names = NULL, em_hcr_list = NULL, em_hcr_names = NULL, sampling_period = NULL, rec_scen = list(0), nsim = 200, start_sim = 1){
  ### Set up parallel processing
  #library(foreach)
  #library(doParallel)
  
  #cores = detectCores() - 2
  #registerDoParallel(cores)
  
  
  ### Run MSEs
  ## Loop across OMs,
  for(om in 1:length(om_list)){  # OM model
    for(em in 1:length(em_hcr_list)){ # EM and HCR
      for(rec in 1:length(rec_scen)){   # Rec trends  
        
        #mse <- foreach(om = 1:length(om_list)) %:%  # OM model
        #foreach(em = 1:length(em_hcr_list)) %:% # EM and HCR
        #foreach(rec = 1:length(rec_scen)) %do% {   # Rec trends
        
        # Load libraries
        library(Rceattle)
        library(dplyr)
        library(foreach)
        library(doParallel)
        
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
                                             verbose = 0, updateM1 = FALSE)
        }
        
        print(paste0("Running OM ",om, " and EM ", em))
        
        # Run MSE
        mse <- mse_run_parallel(om = om_list[[om]], em = em_hcr_list[[em]], 
                                nsim = nsim, start_sim = start_sim, 
                                seed = 666, regenerate_seed = 666,
                                assessment_period = 1, sampling_period = sampling_period, 
                                simulate_data = TRUE, sample_rec = TRUE, 
                                rec_trend = rec_scen[[rec]],
                                cap = NULL, 
                                dir = paste0("Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/",recname[rec],"/No cap"), 
                                file = NULL,
                                regenerate_past = TRUE)
      }
    }
  }
}

