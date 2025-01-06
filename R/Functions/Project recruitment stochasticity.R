
#' Project model with recruitment variation
#' 
#' Sample historic recruitment deviates for projection
#'
#' @param Rceattle 
#' @param seed 
#' @param sample_rec 
#'
#' @return
#' @export
#'
project_ssb <- function(Rceattle, seed = 666, sample_rec = TRUE){
  
  # Projection settings
  hind_yrs <- (Rceattle$data_list$styr):Rceattle$data_list$endyr
  hind_nyrs <- length(hind_yrs)
  
  om_proj_yrs <- (Rceattle$data_list$endyr + 1):Rceattle$data_list$projyr
  om_proj_nyrs <- length(om_proj_yrs)
  
  # - Sample rec devs
  for (sp in 1:Rceattle$data_list$nspp) {
    if (sample_rec) {
      rec_dev <- sample(x = Rceattle$estimated_params$rec_dev[sp, 
                                                              1:hind_nyrs], size = om_proj_nyrs, replace = TRUE) 
    }
    else {
      rec_dev <- log(mean(Rceattle$quantities$R[sp, 1:hind_nyrs])) - 
        log(Rceattle$quantities$R0[sp])
    }
    Rceattle$estimated_params$rec_dev[sp, om_proj_yrs - 
                                        Rceattle$data_list$styr + 1] <- replace(Rceattle$estimated_params$rec_dev[sp, 
                                                                                                                  om_proj_yrs - Rceattle$data_list$styr + 1], values = rec_dev)
  }
  
  # - Update model
  estMode <- Rceattle$data_list$estimateMode
  Rceattle <- fit_mod(data_list = Rceattle$data_list, 
                      inits = Rceattle$estimated_params, 
                      map = Rceattle$map, 
                      bounds = NULL, 
                      file = NULL, estimateMode = 3, 
                      HCR = build_hcr(HCR = Rceattle$data_list$HCR, 
                                      DynamicHCR = Rceattle$data_list$DynamicHCR, 
                                      FsprTarget = Rceattle$data_list$FsprTarget, 
                                      FsprLimit = Rceattle$data_list$FsprLimit, 
                                      Ptarget = Rceattle$data_list$Ptarget, 
                                      Plimit = Rceattle$data_list$Plimit, 
                                      Alpha = Rceattle$data_list$Alpha, 
                                      Pstar = Rceattle$data_list$Pstar, 
                                      Sigma = Rceattle$data_list$Sigma), 
                      recFun = build_srr(srr_fun = Rceattle$data_list$srr_fun, 
                                         srr_pred_fun = Rceattle$data_list$srr_pred_fun, 
                                         proj_mean_rec = !sample_rec, 
                                         srr_est_mode = Rceattle$data_list$srr_est_mode, 
                                         srr_prior_mean = Rceattle$data_list$srr_prior_mean,
                                         srr_prior_sd = Rceattle$data_list$srr_prior_sd), 
                      M1Fun = build_M1(M1_model = Rceattle$data_list$M1_model, 
                                       updateM1 = FALSE, 
                                       M1_use_prior = Rceattle$data_list$M1_use_prior, 
                                       M2_use_prior = Rceattle$data_list$M2_use_prior, 
                                       M1_prior_mean = Rceattle$data_list$M1_prior_mean, 
                                       M1_prior_sd = Rceattle$data_list$M1_prior_sd), 
                      random_rec = Rceattle$data_list$random_rec, 
                      niter = Rceattle$data_list$niter, 
                      msmMode = Rceattle$data_list$msmMode, 
                      avgnMode = Rceattle$data_list$avgnMode, 
                      minNByage = Rceattle$data_list$minNByage, 
                      suitMode = Rceattle$data_list$suitMode, 
                      initMode = Rceattle$data_list$initMode, 
                      phase = NULL, 
                      loopnum = 3, 
                      getsd = FALSE, 
                      verbose = 0)
  Rceattle$data_list$estimateMode <- estMode
  return(Rceattle$quantities$biomassSSB)
}
