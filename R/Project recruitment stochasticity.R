
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
      rec_dev <- mean(Rceattle$estimated_params$rec_dev[sp, 
                                                        1:hind_nyrs])
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
                                         proj_mean_rec = TRUE, 
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
  
  
  # Return only what we want
  Rceattle$quantities[! names(Rceattle$quantities) %in% c(
    "biomass",
    "biomassSSB",
    "BO",
    "SB0",
    "SBF",
    "R")] <- NULL
  
  Rceattle$estimated_params[! names(Rceattle$estimated_params) %in% c(
    "rec_dev")] <- NULL
  
  return(list(quantities = Rceattle$quantities,
              estimated_params = Rceattle$estimated_params))
}



#' Function to update mean R, SSB, SB0, and SBF based on stochastic recruitment (above)
#'
#' @param mod_list 
#' @param nsim 
#'
#' @return
#' @export
#'
#' @examples
update_quantities <- function(mod_list, nsim = 300){
  
  # Convert single one into a list
  if(class(mod_list) == "Rceattle"){
    mod_list <- list(mod_list)
  }
  
  # Set up parallel processing
  library(parallel)
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, c(library(Rceattle)))
  clusterExport(cl, c("project_ssb"), envir=environment())
  
  # Run through models
  for(i in 1:length(mod_list)){
    
    # - Run through rec dev sampling
    sim_quantities <- parLapply(cl, 667:(666+nsim), function(x) project_ssb(mod_list[[i]], seed = x, sample_rec = TRUE))
    
    # Get SSB
    sim_ssb <- lapply(sim_quantities, function(x) x$quantities$biomassSSB)
    sim_ssb <- apply(array(unlist(sim_ssb), c(dim(sim_ssb[[1]]), length(sim_ssb))), 2, rowMeans) # Mean across projections
    mod_list[[i]]$quantities$biomassSSB[] <- sim_ssb
    
    # Get SB0
    sim_sb0 <- lapply(sim_quantities, function(x) x$quantities$SB0)
    sim_sb0 <- apply(array(unlist(sim_sb0), c(dim(sim_sb0[[1]]), length(sim_sb0))), 2, rowMeans) # Mean across projections
    mod_list[[i]]$quantities$biomassSSB[] <- sim_sb0
    
    # Get SBF
    sim_sbf <- lapply(sim_quantities, function(x) x$quantities$SBF)
    sim_sbf <- apply(array(unlist(sim_sbf), c(dim(sim_sbf[[1]]), length(sim_sbf))), 2, rowMeans) # Mean across projections
    mod_list[[i]]$quantities$biomassSSB[] <- sim_sbf
    
    # Get R
    sim_r <- lapply(sim_quantities, function(x) x$quantities$R)
    sim_r <- apply(array(unlist(sim_r), c(dim(sim_r[[1]]), length(sim_r))), 2, rowMeans) # Mean across projections
    mod_list[[i]]$quantities$biomassSSB[] <- sim_r
    
    
    # Get R
    sim_rec_dev <- lapply(sim_quantities, function(x) x$estimated_params$rec_dev)
    sim_rec_dev <- apply(array(unlist(sim_rec_dev), c(dim(sim_rec_dev[[1]]), length(sim_rec_dev))), 2, rowMeans) # Mean across projections
    mod_list[[i]]$estimated_params$rec_dev[] <- sim_rec_dev
    
    # Refit projections (depletion based F would need)
    if(mod_list[[i]]$data_list$HCR > 0){
      estMode <- mod_list[[i]]$data_list$estimateMode
      mod_list[[i]] <- fit_mod(data_list = mod_list[[i]]$data_list, 
                               inits = mod_list[[i]]$estimated_params, 
                               map = mod_list[[i]]$map, 
                               bounds = NULL, 
                               file = NULL, estimateMode = 2, 
                               HCR = build_hcr(HCR = mod_list[[i]]$data_list$HCR, 
                                               DynamicHCR = mod_list[[i]]$data_list$DynamicHCR, 
                                               FsprTarget = mod_list[[i]]$data_list$FsprTarget, 
                                               FsprLimit = mod_list[[i]]$data_list$FsprLimit, 
                                               Ptarget = mod_list[[i]]$data_list$Ptarget, 
                                               Plimit = mod_list[[i]]$data_list$Plimit, 
                                               Alpha = mod_list[[i]]$data_list$Alpha, 
                                               Pstar = mod_list[[i]]$data_list$Pstar, 
                                               Sigma = mod_list[[i]]$data_list$Sigma), 
                               recFun = build_srr(srr_fun = mod_list[[i]]$data_list$srr_fun, 
                                                  srr_pred_fun = mod_list[[i]]$data_list$srr_pred_fun, 
                                                  proj_mean_rec = !sample_rec, 
                                                  srr_est_mode = mod_list[[i]]$data_list$srr_est_mode, 
                                                  srr_prior_mean = mod_list[[i]]$data_list$srr_prior_mean,
                                                  srr_prior_sd = mod_list[[i]]$data_list$srr_prior_sd), 
                               M1Fun = build_M1(M1_model = mod_list[[i]]$data_list$M1_model, 
                                                updateM1 = FALSE, 
                                                M1_use_prior = mod_list[[i]]$data_list$M1_use_prior, 
                                                M2_use_prior = mod_list[[i]]$data_list$M2_use_prior, 
                                                M1_prior_mean = mod_list[[i]]$data_list$M1_prior_mean, 
                                                M1_prior_sd = mod_list[[i]]$data_list$M1_prior_sd), 
                               random_rec = mod_list[[i]]$data_list$random_rec, 
                               niter = mod_list[[i]]$data_list$niter, 
                               msmMode = mod_list[[i]]$data_list$msmMode, 
                               avgnMode = mod_list[[i]]$data_list$avgnMode, 
                               minNByage = mod_list[[i]]$data_list$minNByage, 
                               suitMode = mod_list[[i]]$data_list$suitMode, 
                               initMode = mod_list[[i]]$data_list$initMode, 
                               phase = NULL, 
                               loopnum = 3, 
                               getsd = FALSE, 
                               verbose = 0)
    }
  }
  
  stopCluster(cl); gc()
  
  return(mod_list)
}
