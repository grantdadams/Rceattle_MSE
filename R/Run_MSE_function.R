
#' Function to run MSEs looping across EMs and OMs in parallel
#'
#' @param system 
#' @param recname 
#'
#' @return
#' @export
#'
#' @examples
run_mse <- function(system = "GOA1977", om_list = NULL, om_names = NULL, em_hcr_list = NULL, em_hcr_names = NULL, sampling_period = NULL, nsim = 200, start_sim = 1, regenerate_past = TRUE){
  ### Set up parallel processing
  #library(foreach)
  #library(doParallel)
  
  #cores = detectCores() - 2
  #registerDoParallel(cores)
  
  
  ### Run MSEs
  ## Loop across OMs,
  for(om in 1:length(om_list)){  # OM model
    for(em in 1:length(em_hcr_list)){ # EM and HCR
      
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
          estimateMode = 0,
          niter = em_hcr_list[[em]]$data_list$niter,
          msmMode = em_hcr_list[[em]]$data_list$msmMode,
          avgnMode = em_hcr_list[[em]]$data_list$avgnMode,
          suitMode = em_hcr_list[[em]]$data_list$suitMode,
          initMode = em_hcr_list[[em]]$data_list$initMode,
          suit_styr = em_hcr_list[[em]]$data_list$suit_styr,
          suit_endyr = em_hcr_list[[em]]$data_list$suit_endyr,
          HCR = build_hcr(HCR = em_hcr_list[[em]]$data_list$HCR,
                          DynamicHCR = em_hcr_list[[em]]$data_list$DynamicHCR,
                          Ftarget = em_hcr_list[[em]]$data_list$Ftarget,
                          Flimit = em_hcr_list[[em]]$data_list$Flimit,
                          Ptarget = em_hcr_list[[em]]$data_list$Ptarget,
                          Plimit = em_hcr_list[[em]]$data_list$Plimit,
                          Alpha = em_hcr_list[[em]]$data_list$Alpha,
                          Pstar = em_hcr_list[[em]]$data_list$Pstar,
                          Sigma = em_hcr_list[[em]]$data_list$Sigma,
                          Fmult = em_hcr_list[[em]]$data_list$Fmult,
                          HCRorder = em_hcr_list[[em]]$data_list$HCRorder
          ),
          recFun = build_srr(srr_fun = em_hcr_list[[em]]$data_list$srr_fun,
                             srr_pred_fun = em_hcr_list[[em]]$data_list$srr_pred_fun ,
                             proj_mean_rec = em_hcr_list[[em]]$data_list$proj_mean_rec,
                             srr_meanyr = em_hcr_list[[em]]$data_list$srr_meanyr,
                             srr_hat_styr = em_hcr_list[[em]]$data_list$srr_hat_styr,
                             srr_hat_endyr = em_hcr_list[[em]]$data_list$srr_hat_endyr,
                             srr_est_mode  = em_hcr_list[[em]]$data_list$srr_est_mode ,
                             srr_prior = em_hcr_list[[em]]$data_list$srr_prior,
                             srr_prior_sd = em_hcr_list[[em]]$data_list$srr_prior_sd,
                             Bmsy_lim = em_hcr_list[[em]]$data_list$Bmsy_lim,
                             srr_indices = em_hcr_list[[em]]$data_list$srr_indices),
          M1Fun = build_M1(M1_model = em_hcr_list[[em]]$data_list$M1_model,
                           M1_re = em_hcr_list[[em]]$data_list$M1_re,
                           updateM1 = FALSE,  # Dont update M1 frem_hcr_list[[em]] data, fix at previous parameters
                           M1_use_prior = em_hcr_list[[em]]$data_list$M1_use_prior,
                           M2_use_prior = em_hcr_list[[em]]$data_list$M2_use_prior,
                           M_prior = em_hcr_list[[em]]$data_list$M_prior,
                           M_prior_sd = em_hcr_list[[em]]$data_list$M_prior_sd,
                           M1_indices = em_hcr_list[[em]]$data_list$M1_indices),
          loopnum = 1,
          phase = FALSE,
          getsd = FALSE,
          verbose = 0)
      }
      
      print(paste0("Running OM ",om, " and EM ", em))
      
      # Run MSE
      mse <- Rceattle::run_mse(om = om_list[[om]], 
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
                              cap = NULL, 
                              dir = paste0("Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/",regenerate_past," regen/No cap"), 
                              file = NULL,
                              regenerate_past = regenerate_past,
                              timeout = 30)
      
      closeAllConnections()
    }
  }
}

