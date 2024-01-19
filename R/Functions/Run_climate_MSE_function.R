
#' Function to run MSEs looping across EMs and OMs in parallel
#'
#' @param system 
#' @param recname 
#'
#' @return
#' @export
#'
#' @examples
run_climate_mse <- function(system = "GOA1977", om_list = NULL, om_names = NULL, em_hcr_list = NULL, em_hcr_names = NULL, sampling_period = NULL, nsim = 200, start_sim = 1, regenerate_past = TRUE, cap = NULL){
  ### Set up parallel processing
  #library(foreach)
  #library(doParallel)
  
  #cores = detectCores() - 2
  #registerDoParallel(cores)
  
  
  ### Run MSEs
  ## Loop across OMs,
  for(om in 1:length(om_list)){  # OM model
    for(em in 1:length(em_hcr_list)){ # EM and HCR
      
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
        om_list[[om]] <- Rceattle::fit_mod(
          data_list = om_list[[om]]$data_list,
          inits = om_list[[om]]$estimated_params,
          map =  NULL,
          bounds = NULL,
          file = NULL,
          estimateMode = 0, # Run projection only
          HCR = build_hcr(HCR = em_hcr_list[[em]]$data_list$HCR, # Tier3 HCR
                          DynamicHCR = em_hcr_list[[em]]$data_list$DynamicHCR,
                          FsprTarget = em_hcr_list[[em]]$data_list$FsprTarget,
                          FsprLimit = em_hcr_list[[em]]$data_list$FsprLimit,
                          Ptarget = em_hcr_list[[em]]$data_list$Ptarget,
                          Plimit = em_hcr_list[[em]]$data_list$Plimit,
                          Alpha = em_hcr_list[[em]]$data_list$Alpha,
                          Pstar = em_hcr_list[[em]]$data_list$Pstar,
                          Sigma = em_hcr_list[[em]]$data_list$Sigma,
                          Fmult = em_hcr_list[[em]]$data_list$Fmult
          ),
          recFun = build_srr(srr_fun = om_list[[om]]$data_list$srr_fun,
                             srr_pred_fun  = om_list[[om]]$data_list$srr_pred_fun ,
                             srr_env_indices = om_list[[om]]$data_list$srr_env_indices,
                             proj_mean_rec  = om_list[[om]]$data_list$proj_mean_rec ,
                             srr_est_mode  = om_list[[om]]$data_list$srr_est_mode ,
                             srr_prior_mean  = om_list[[om]]$data_list$srr_prior_mean,
                             srr_prior_sd   = om_list[[om]]$data_list$srr_prior_sd,
                             srr_meanyr = om_list[[om]]$data_list$srr_meanyr,
                             Bmsy_lim = om_list[[om]]$data_list$Bmsy_lim),
          M1Fun =     build_M1(M1_model= om_list[[om]]$data_list$M1_model,
                               updateM1 = FALSE,
                               M1_use_prior = om_list[[om]]$data_list$M1_use_prior,
                               M2_use_prior = om_list[[om]]$data_list$M2_use_prior,
                               M1_prior_mean = om_list[[om]]$data_list$M1_prior_mean,
                               M1_prior_sd = om_list[[om]]$data_list$M1_prior_sd),
          suit_meanyr = om_list[[om]]$data_list$suit_meanyr,
          random_rec = om_list[[om]]$data_list$random_rec,
          niter = om_list[[om]]$data_list$niter,
          msmMode = om_list[[om]]$data_list$msmMode,
          avgnMode = om_list[[om]]$data_list$avgnMode,
          minNByage = om_list[[om]]$data_list$minNByage,
          suitMode = om_list[[om]]$data_list$suitMode,
          initMode = om_list[[om]]$data_list$initMode,
          phase = NULL,
          loopnum = 3,
          getsd = FALSE,
          verbose = 0)
      }
      
      print(paste0("Running OM ",om, " and EM ", em))
      
      # Run MSE
      mse <- mse_run_parallel(om = om_list[[om]], 
                              em = em_hcr_list[[em]], 
                              nsim = nsim, 
                              start_sim = start_sim, 
                              seed = 666, 
                              regenerate_seed = 666,
                              assessment_period = 1, 
                              sampling_period = sampling_period, 
                              simulate_data = TRUE, 
                              sample_rec = TRUE, 
                              rec_trend = 0,
                              cap = cap, 
                              dir = paste0("Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/",regenerate_past," regen/",!is.null(cap)," cap"), 
                              file = NULL,
                              regenerate_past = regenerate_past)
    }
  }
}

