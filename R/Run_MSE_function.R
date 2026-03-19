
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

