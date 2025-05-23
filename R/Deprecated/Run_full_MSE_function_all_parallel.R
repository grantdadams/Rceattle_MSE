
#' Function to run MSEs looping across EMs and OMs in parallel
#'
#' @param system 
#' @param recname 
#'
#' @return
#' @export
#'
#' @examples
run_mse_all_par <- function(system = "GOA1977", recname = "ConstantR", om_list = NULL, om_names = NULL, em_hcr_list = NULL, em_hcr_names = NULL, sampling_period = NULL, rec_scen = list(0)){
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
      foreach(rec = 1:length(rec_scen)) %:% {   # Rec trends
        
        # Load libraries
        library(Rceattle)
        library(dplyr)
        
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
        fut_sample = 1; loopnum = 1;
        
        om = om_list[[om]]; em = em_hcr_list[[em]]; 
                       nsim = 200; 
                       seed = 666; regenerate_seed = 666;
                       assessment_period = 1; sampling_period = sampling_period; 
                       simulate_data = TRUE; sample_rec = TRUE; 
                       rec_trend = rec_scen[[rec]];
                       cap = NULL; 
                       dir = paste0("Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/",recname[rec],"/No cap"); 
                       file = NULL;
                       regenerate_past = TRUE
        
        '%!in%' <- function(x,y)!('%in%'(x,y))
        library(dplyr)
        set.seed(regenerate_seed)
        
        Rceattle_OM_list <- list()
        Rceattle_EM_list <- list()
        
        # - Adjust cap
        if(!is.null(cap)){
          if(length(cap) == 1){
            cap = rep(cap, om$data_list$nspp)
          }
          
          if(length(cap) != om$data_list$nspp){
            stop("cap is not length 1 or length nspp")
          }
        }
        
        # - Adjust rec trend
        if(length(rec_trend)==1){
          rec_trend = rep(rec_trend, om$data_list$nspp)
        }
        
        # - Years for simulations
        hind_yrs <- (em$data_list$styr) : em$data_list$endyr
        hind_nyrs <- length(hind_yrs)
        proj_yrs <- (em$data_list$endyr + 1) : em$data_list$projyr
        proj_nyrs <- length(proj_yrs)
        nflts = nrow(om$data_list$fleet_control)
        nselages_om <- max(om$data_list$fleet_control$Nselages, na.rm = TRUE)
        nselages_em <- max(em$data_list$fleet_control$Nselages, na.rm = TRUE)
        
        # - Assessment period
        assess_yrs <- seq(from = em$data_list$endyr + assessment_period, to = em$data_list$projyr,  by = assessment_period)
        
        # - Data sampling period
        if(length(sampling_period)==1){
          sampling_period = rep(sampling_period, nflts)
          
        }
        
        if(nflts != nrow(em$data_list$fleet_control)){
          stop("OM and EM fleets do not match or sampling period length is mispecified")
        }
        if(nflts != length(sampling_period)){
          stop("Sampling period length is mispecified, does not match number of fleets")
        }
        
        # - Set up years of data we are sampling for each fleet
        sample_yrs <- lapply(sampling_period, function(x) seq(from = em$data_list$endyr + x, to = em$data_list$projyr,  by = x))
        fleet_id <- sample_yrs
        for(i in 1:length(sample_yrs)){
          fleet_id[[i]] <- replace(fleet_id[[i]], values = i)
        }
        sample_yrs = data.frame(Fleet_code = unlist(fleet_id), Year = unlist(sample_yrs))
        
        #--------------------------------------------------
        # Regenerate past data from OM and refit EM
        #--------------------------------------------------
        if(regenerate_past){
          
          # - Simulate index and comp data and updatae EM
          sim_dat <- sim_mod(om, simulate = simulate_data)
          
          em$data_list$srv_biom <- sim_dat$srv_biom
          em$data_list$comp_data <- sim_dat$comp_data
          
          # Restimate
          em <- fit_mod(
            data_list = em$data_list,
            inits = em$estimated_params,
            map =  NULL,
            bounds = NULL,
            file = NULL,
            estimateMode = ifelse(em$data_list$estimateMode < 3, 0, em$data_list$estimateMode), # Run hindcast and projection, otherwise debug
            HCR = build_hcr(HCR = em$data_list$HCR, # Tier3 HCR
                            DynamicHCR = em$data_list$DynamicHCR,
                            FsprTarget = em$data_list$FsprTarget,
                            FsprLimit = em$data_list$FsprLimit,
                            Ptarget = em$data_list$Ptarget,
                            Plimit = em$data_list$Plimit,
                            Alpha = em$data_list$Alpha,
                            Pstar = em$data_list$Pstar,
                            Sigma = em$data_list$Sigma
            ),
            random_rec = em$data_list$random_rec,
            niter = em$data_list$niter,
            msmMode = em$data_list$msmMode,
            avgnMode = em$data_list$avgnMode,
            minNByage = em$data_list$minNByage,
            suitMode = em$data_list$suitMode,
            phase = "default",
            updateM1 = FALSE,
            loopnum = 3,
            getsd = FALSE,
            verbose = 0)
          
          # Update avg F given model fit to regenerated data
          if(em$data_list$HCR == 2){
            
            # - Get avg F
            avg_F <- (exp(em$estimated_params$ln_mean_F+em$estimated_params$F_dev)) # Average F from last 5 years
            avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])
            avg_F <- data.frame(avg_F = avg_F, spp = em$data_list$fleet_control$Species)
            avg_F <- avg_F %>%
              group_by(spp) %>%
              summarise(avg_F = sum(avg_F)) %>%
              arrange(spp)
            
            # - Update model
            em <- Rceattle::fit_mod(data_list = em$data_list,
                                    inits = em$estimated_params,
                                    estimateMode = 2, # Don't estimate
                                    HCR = build_hcr(HCR = 2, # Input F
                                                    FsprTarget = avg_F$avg_F,
                                                    Ptarget = em$data_list$Ptarget,
                                                    Plimit = em$data_list$Plimit
                                    ),
                                    random_rec = em$data_list$random_rec,
                                    niter = em$data_list$niter,
                                    msmMode = em$data_list$msmMode,
                                    avgnMode = em$data_list$avgnMode,
                                    minNByage = em$data_list$minNByage,
                                    suitMode = em$data_list$suitMode,
                                    phase = "default",
                                    updateM1 = FALSE,
                                    loopnum = 3,
                                    getsd = FALSE,
                                    verbose = 0)
          }
        }
        
        #--------------------------------------------------
        # Update data-files in OM so we can fill in updated years
        #--------------------------------------------------
        # -- srv_biom
        proj_srv <- om$data_list$srv_biom %>%
          group_by(Fleet_code) %>%
          slice(rep(n(),  proj_nyrs)) %>%
          mutate(Year = -proj_yrs)
        proj_srv$Log_sd <- proj_srv$Log_sd * 1/fut_sample
        om$data_list$srv_biom  <- rbind(om$data_list$srv_biom, proj_srv)
        om$data_list$srv_biom <- dplyr::arrange(om$data_list$srv_biom, Fleet_code, abs(Year))
        
        # -- Nbyage
        if(nrow(om$data_list$NByageFixed) > 0){
          proj_nbyage <- om$data_list$NByageFixed %>%
            group_by(Species, Sex) %>%
            slice(rep(n(),  proj_nyrs)) %>%
            mutate(Year = proj_yrs)
          proj_nbyage <- proj_nbyage[which(proj_yrs %!in% om$data_list$NByageFixed$Year),] # Subset rows already forcasted
          om$data_list$NByageFixed  <- rbind(om$data_list$NByageFixed, proj_nbyage)
          om$data_list$NByageFixed <- dplyr::arrange(om$data_list$NByageFixed, Species, Year)
        }
        
        # -- comp_data
        proj_comp <- om$data_list$comp_data %>%
          group_by(Fleet_code, Sex) %>%
          slice(rep(n(),  proj_nyrs)) %>%
          mutate(Year = -proj_yrs)
        proj_comp$Sample_size <- proj_comp$Sample_size * fut_sample # Adjust future sampling effort
        om$data_list$comp_data  <- rbind(om$data_list$comp_data, proj_comp)
        om$data_list$comp_data <- dplyr::arrange(om$data_list$comp_data, Fleet_code, abs(Year))
        
        # -- emp_sel - Use terminal year
        if(nrow(om$data_list$emp_sel) > 0){
          proj_emp_sel <- om$data_list$emp_sel %>%
            group_by(Fleet_code, Sex) %>%
            slice(rep(n(),  proj_nyrs)) %>%
            mutate(Year = proj_yrs)
          om$data_list$emp_sel  <- rbind(om$data_list$emp_sel, proj_emp_sel)
          om$data_list$emp_sel <- dplyr::arrange(om$data_list$emp_sel, Fleet_code, Year)
        }
        
        # -- wt
        #FIXME ignrores forecasted growth
        proj_wt <- om$data_list$wt %>%
          group_by(Wt_index , Sex) %>%
          slice(rep(n(),  proj_nyrs)) %>%
          mutate(Year = proj_yrs)
        om$data_list$wt  <- rbind(om$data_list$wt, proj_wt)
        om$data_list$wt <- dplyr::arrange(om$data_list$wt, Wt_index, Year)
        
        # -- Pyrs
        proj_Pyrs <- om$data_list$Pyrs %>%
          group_by(Species, Sex) %>%
          slice(rep(n(),  proj_nyrs)) %>%
          mutate(Year = proj_yrs)
        om$data_list$Pyrs  <- rbind(om$data_list$Pyrs, proj_Pyrs)
        om$data_list$Pyrs <- dplyr::arrange(om$data_list$Pyrs, Species, Year)
        
        
        #--------------------------------------------------
        # Update data in EM
        #--------------------------------------------------
        #FIXME - assuming same as terminal year of hindcast
        # -- EM emp_sel - Use terminal year
        proj_emp_sel <- em$data_list$emp_sel %>%
          group_by(Fleet_code, Sex) %>%
          slice(rep(n(),  proj_nyrs)) %>%
          mutate(Year = proj_yrs)
        em$data_list$emp_sel  <- rbind(em$data_list$emp_sel, proj_emp_sel)
        em$data_list$emp_sel <- dplyr::arrange(em$data_list$emp_sel, Fleet_code, Year)
        
        # -- EM wt
        proj_wt <- em$data_list$wt %>%
          group_by(Wt_index , Sex) %>%
          slice(rep(n(),  proj_nyrs)) %>%
          mutate(Year = proj_yrs)
        em$data_list$wt  <- rbind(em$data_list$wt, proj_wt)
        em$data_list$wt <- dplyr::arrange(em$data_list$wt, Wt_index, Year)
        
        # -- EM Pyrs
        proj_Pyrs <- em$data_list$Pyrs %>%
          group_by(Species, Sex) %>%
          slice(rep(n(),  proj_nyrs)) %>%
          mutate(Year = proj_yrs)
        em$data_list$Pyrs  <- rbind(em$data_list$Pyrs, proj_Pyrs)
        em$data_list$Pyrs <- dplyr::arrange(em$data_list$Pyrs, Species, Year)
        
        #--------------------------------------------------
        # Do the MSE
        #--------------------------------------------------
        foreach(sim = 1:nsim) %dopar% {
          library(Rceattle)
          library(dplyr)
          
          set.seed(seed = seed + sim) # setting unique seed for each simulation
          
          # Set models objects
          sim_list <- list(EM = list())# , OM = list())
          sim_list$EM[[1]] <- em
          # sim_list$OM[[1]] <- om
          
          em_use <- em
          om_use <- om
          
          # Replace future rec devs
          for(sp in 1:om_use$data_list$nspp){
            if(sample_rec){ # Sample devs from hindcast
              rec_dev <- sample(x = om_use$estimated_params$rec_dev[sp, 1:hind_nyrs], size = proj_nyrs, replace = TRUE) + log((1+(rec_trend[sp]/proj_nyrs) * 1:proj_nyrs)) # - Scale mean rec for rec trend
            } else{ # Set to mean rec otherwise
              rec_dev <- log(mean(om_use$quantities$R[sp,1:hind_nyrs]) * (1+(rec_trend[sp]/proj_nyrs) * 1:proj_nyrs))  - om_use$estimated_params$ln_mean_rec[sp] # - Scale mean rec for rec trend
            }
            
            # - Update OM with devs
            om_use$estimated_params$rec_dev[sp,proj_yrs - om_use$data_list$styr + 1] <- replace(
              om_use$estimated_params$rec_dev[sp,proj_yrs - om_use$data_list$styr + 1],
              values =  rec_dev)
          }
          
          
          
          
          # Run through assessment years
          for(k in 1:(length(assess_yrs))){
            
            # ------------------------------------------------------------
            # 1. GET RECOMMENDED TAC FROM EM-HCR
            # ------------------------------------------------------------
            new_years <- proj_yrs[which(proj_yrs <= assess_yrs[k] & proj_yrs > om_use$data_list$endyr)]
            
            # - Get projected catch data from EM
            new_catch_data <- em_use$data_list$fsh_biom
            dat_fill_ind <- which(new_catch_data$Year %in% new_years & is.na(new_catch_data$Catch))
            new_catch_data$Catch[dat_fill_ind] <- em_use$quantities$fsh_bio_hat[dat_fill_ind]
            if(!is.null(cap)){
              new_catch_data$Catch[dat_fill_ind] <- ifelse(new_catch_data$Catch[dat_fill_ind] > cap[new_catch_data$Species[dat_fill_ind]], cap[new_catch_data$Species[dat_fill_ind]], new_catch_data$Catch[dat_fill_ind])
            }
            
            # - Update catch data in OM and EM
            om_use$data_list$fsh_biom <- new_catch_data
            em_use$data_list$fsh_biom <- new_catch_data
            
            # ------------------------------------------------------------
            # 2. UPDATE OBSERVATION MODEL
            # ------------------------------------------------------------
            # - Update endyr of OM
            nyrs_hind <- om_use$data_list$endyr - om_use$data_list$styr + 1
            om_use$data_list$endyr <- assess_yrs[k]
            
            # - Update parameters
            # -- F_dev
            om_use$estimated_params$F_dev <- cbind(om_use$estimated_params$F_dev, matrix(0, nrow= nrow(om_use$estimated_params$F_dev), ncol = length(new_years)))
            
            # -- Time-varing survey catachbilitiy - Assume last year - filled by columns
            om_use$estimated_params$ln_srv_q_dev <- cbind(om_use$estimated_params$ln_srv_q_dev, matrix(om_use$estimated_params$ln_srv_q_dev[,ncol(om_use$estimated_params$ln_srv_q_dev)], nrow= nrow(om_use$estimated_params$ln_srv_q_dev), ncol = length(new_years)))
            
            # -- Time-varing selectivity - Assume last year - filled by columns
            ln_sel_slp_dev = array(0, dim = c(2, nflts, 2, nyrs_hind + length(new_years)))  # selectivity deviations paramaters for logistic
            sel_inf_dev = array(0, dim = c(2, nflts, 2, nyrs_hind + length(new_years)))  # selectivity deviations paramaters for logistic
            sel_coff_dev = array(0, dim = c(nflts, 2, nselages_om, nyrs_hind + length(new_years)))  # selectivity deviations paramaters for non-parameteric
            
            ln_sel_slp_dev[,,,1:nyrs_hind] <- om_use$estimated_params$ln_sel_slp_dev
            sel_inf_dev[,,,1:nyrs_hind] <- om_use$estimated_params$sel_inf_dev
            sel_coff_dev[,,,1:nyrs_hind] <- om_use$estimated_params$sel_coff_dev
            
            ln_sel_slp_dev[,,,(nyrs_hind + 1):(nyrs_hind + length(new_years))] <- ln_sel_slp_dev[,,,nyrs_hind]
            sel_inf_dev[,,,(nyrs_hind + 1):(nyrs_hind + length(new_years))] <- sel_inf_dev[,,,nyrs_hind]
            sel_coff_dev[,,,(nyrs_hind + 1):(nyrs_hind + length(new_years))] <- sel_coff_dev[,,,nyrs_hind]
            
            om_use$estimated_params$ln_sel_slp_dev <- ln_sel_slp_dev
            om_use$estimated_params$sel_inf_dev <- sel_inf_dev
            om_use$estimated_params$sel_coff_dev <- sel_coff_dev
            
            
            # - Update map (Only new parameter we are estimating in OM is the F_dev of the new years)
            om_use$map <- build_map(
              data_list = om_use$data_list,
              params = om_use$estimated_params,
              debug = TRUE,
              random_rec = om_use$data_list$random_rec)
            om_use$map$mapFactor$dummy <- as.factor(NA); om_use$map$mapList$dummy <- NA
            
            
            # -- Estimate terminal F for catch
            new_f_yrs <- (ncol(om_use$map$mapList$F_dev) - length(new_years) + 1) : ncol(om_use$map$mapList$F_dev) # - Years of new F
            f_fleets <- om_use$data_list$fleet_control$Fleet_code[which(om_use$data_list$fleet_control$Fleet_type == 1)] # Fleet rows for F
            om_use$map$mapList$F_dev[f_fleets,new_f_yrs] <- replace(om_use$map$mapList$F_dev[f_fleets,new_f_yrs], values = 1:length(om_use$map$mapList$F_dev[f_fleets,new_f_yrs]))
            
            # -- Map out Fdev for years with 0 catch to very low number
            fsh_biom <- om_use$data_list$fsh_biom
            fsh_ind <- fsh_biom$Fleet_code[which(fsh_biom$Catch == 0)]
            yr_ind <- fsh_biom$Year[which(fsh_biom$Catch == 0)] - om_use$data_list$styr + 1
            om_use$map$mapList$F_dev[fsh_ind, yr_ind] <- NA
            om_use$map$mapFactor$F_dev <- factor( om_use$map$mapList$F_dev)
            
            # - Fit OM with new catch data
            om_use <- fit_mod(
              data_list = om_use$data_list,
              inits = om_use$estimated_params,
              map =  om_use$map,
              bounds = NULL,
              file = NULL,
              estimateMode = ifelse(om_use$data_list$estimateMode < 3, 1, om_use$data_list$estimateMode), # Estimate hindcast only if estimating
              random_rec = om_use$data_list$random_rec,
              niter = om_use$data_list$niter,
              msmMode = om_use$data_list$msmMode,
              avgnMode = om_use$data_list$avgnMode,
              minNByage = om_use$data_list$minNByage,
              suitMode = om_use$data_list$suitMode,
              meanyr = om$data_list$endyr,
              updateM1 = FALSE, # Dont update M1 from data, fix at previous parameters
              loopnum = 2,
              phase = NULL,
              getsd = FALSE,
              verbose = 0)
            
            
            # ------------------------------------------------------------
            # 3. REFIT ESTIMATION MODEL AND HCR
            # ------------------------------------------------------------
            # - Simulate new survey and comp data
            sim_dat <- sim_mod(om_use, simulate = simulate_data)
            
            years_include <- sample_yrs[which(sample_yrs$Year > em_use$data_list$endyr & sample_yrs$Year <= assess_yrs[k]),]
            
            # -- Add newly simulated survey data to EM
            new_srv_biom <- sim_dat$srv_biom[which(abs(sim_dat$srv_biom$Year) %in% years_include$Year & sim_dat$srv_biom$Fleet_code %in% years_include$Fleet_code),]
            new_srv_biom$Year <- -new_srv_biom$Year
            em_use$data_list$srv_biom <- rbind(em_use$data_list$srv_biom, new_srv_biom)
            em_use$data_list$srv_biom <- em_use$data_list$srv_biom[
              with(em_use$data_list$srv_biom, order(Fleet_code, abs(Year))),]
            
            # -- Add newly simulated comp data to EM
            new_comp_data <- sim_dat$comp_data[which(abs(sim_dat$comp_data$Year) %in% years_include$Year & sim_dat$comp_data$Fleet_code %in% years_include$Fleet_code),]
            new_comp_data$Year <- -new_comp_data$Year
            em_use$data_list$comp_data <- rbind(em_use$data_list$comp_data, new_comp_data)
            em_use$data_list$comp_data <- em_use$data_list$comp_data[
              with(em_use$data_list$comp_data, order(Fleet_code, abs(Year))),]
            
            # Update end year and re-estimate
            em_use$data_list$endyr <- assess_yrs[k]
            
            # Update parameters
            # -- F_dev
            em_use$estimated_params$F_dev <- cbind(em_use$estimated_params$F_dev, matrix(0, nrow= nrow(em_use$estimated_params$F_dev), ncol = length(new_years)))
            
            # -- Time-varying survey catachbilitiy - Assume last year - filled by columns
            em_use$estimated_params$ln_srv_q_dev <- cbind(em_use$estimated_params$ln_srv_q_dev, matrix(em_use$estimated_params$ln_srv_q_dev[,ncol(em_use$estimated_params$ln_srv_q_dev)], nrow= nrow(em_use$estimated_params$ln_srv_q_dev), ncol = length(new_years)))
            
            # -- Time-varing selectivity - Assume last year - filled by columns
            ln_sel_slp_dev = array(0, dim = c(2, nflts, 2, nyrs_hind + length(new_years)))  # selectivity deviations paramaters for logistic
            sel_inf_dev = array(0, dim = c(2, nflts, 2, nyrs_hind + length(new_years)))  # selectivity deviations paramaters for logistic
            sel_coff_dev = array(0, dim = c(nflts, 2, nselages_om, nyrs_hind + length(new_years)))  # selectivity deviations paramaters for non-parameteric
            
            ln_sel_slp_dev[,,,1:nyrs_hind] <- em_use$estimated_params$ln_sel_slp_dev
            sel_inf_dev[,,,1:nyrs_hind] <- em_use$estimated_params$sel_inf_dev
            sel_coff_dev[,,,1:nyrs_hind] <- em_use$estimated_params$sel_coff_dev
            
            # - Initialize new years with last year
            ln_sel_slp_dev[,,,(nyrs_hind + 1):(nyrs_hind + length(new_years))] <- ln_sel_slp_dev[,,,nyrs_hind]
            sel_inf_dev[,,,(nyrs_hind + 1):(nyrs_hind + length(new_years))] <- sel_inf_dev[,,,nyrs_hind]
            sel_coff_dev[,,,(nyrs_hind + 1):(nyrs_hind + length(new_years))] <- sel_coff_dev[,,,nyrs_hind]
            
            em_use$estimated_params$ln_sel_slp_dev <- ln_sel_slp_dev
            em_use$estimated_params$sel_inf_dev <- sel_inf_dev
            em_use$estimated_params$sel_coff_dev <- sel_coff_dev
            
            
            # Restimate
            em_use <- fit_mod(
              data_list = em_use$data_list,
              inits = em_use$estimated_params,
              map =  NULL,
              bounds = NULL,
              file = NULL,
              estimateMode = ifelse(em_use$data_list$estimateMode < 3, 0, em_use$data_list$estimateMode), # Run hindcast and projection, otherwise debug
              HCR = build_hcr(HCR = em_use$data_list$HCR, # Tier3 HCR
                              DynamicHCR = em_use$data_list$DynamicHCR,
                              FsprTarget = em_use$data_list$FsprTarget,
                              FsprLimit = em_use$data_list$FsprLimit,
                              Ptarget = em_use$data_list$Ptarget,
                              Plimit = em_use$data_list$Plimit,
                              Alpha = em_use$data_list$Alpha,
                              Pstar = em_use$data_list$Pstar,
                              Sigma = em_use$data_list$Sigma
              ),
              random_rec = em_use$data_list$random_rec,
              niter = em_use$data_list$niter,
              msmMode = em_use$data_list$msmMode,
              avgnMode = em_use$data_list$avgnMode,
              minNByage = em_use$data_list$minNByage,
              suitMode = em_use$data_list$suitMode,
              phase = NULL,
              meanyr = em_use$data_list$endyr, # Update end year
              updateM1 = FALSE,
              loopnum = loopnum,
              getsd = FALSE,
              verbose = 0)
            # plot_biomass(list(em_use, om_use), model_names = c("EM", "OM"))
            # End year of assessment
            
            # - Remove unneeded bits for memory reasons
            em_use$initial_params <- NULL
            em_use$bounds <- NULL
            em_use$map <- NULL
            em_use$obj <- NULL
            em_use$opt <- NULL
            em_use$sdrep <- NULL
            em_use$quantities[names(em_use$quantities) %!in% c("fsh_bio_hat",
                                                               "fsh_log_sd_hat",
                                                               "depletion",
                                                               "depletionSSB",
                                                               "biomass",
                                                               "F_spp",
                                                               "F_flt",
                                                               "mn_rec"  ,
                                                               "biomassSSB" ,
                                                               "R",
                                                               "M",
                                                               "M1",
                                                               "mean_rec",
                                                               "srv_bio_hat",
                                                               "srv_log_sd_hat",
                                                               "BO",
                                                               "SB0",
                                                               "DynamicB0",
                                                               "DynamicSB0",
                                                               "SPR0",
                                                               "SPRlimit",
                                                               "SPRtarget",
                                                               "DynamicNbyageSPR",
                                                               "DynamicSPR0",
                                                               "DynamicSPRlimit",
                                                               "DynamicSPRtarget",
                                                               "proj_F",
                                                               "Ftarget",
                                                               "Flimit",
                                                               "FlimitSPR",
                                                               "FtargetSPR",
                                                               "DynamicFlimitSPR",
                                                               "DynamicFtargetSPR")] <- NULL
            
            sim_list$EM[[k+1]] <- em_use
            #sim_list$OM[[k+1]] <- om_use
            message(paste0("Sim ",sim, " - EM Year ", assess_yrs[k], " COMPLETE"))
          }
          
          # Save models
          sim_list$OM <- om_use
          names(sim_list$EM) <- c("EM", paste0("OM_Sim_",sim,". EM_yr_", assess_yrs))
          #names(sim_list$OM) <- c("OM", paste0("OM_Sim_",sim,". OM_yr_", assess_yrs))
          dir.create(file.path(getwd(), dir), showWarnings = FALSE, recursive = TRUE)
          saveRDS(sim_list, file = paste0(dir, "/", file, "EMs_from_OM_Sim_",sim, ".rds"))
          sim_list <- NULL
        }
        
      }
  })
  
  
  # When you're done, clean up the cluster
  stopImplicitCluster()
}

