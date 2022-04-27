library(Rceattle)
library(dplyr)

################################################
# Set up initial models
################################################
load("~/GitHub/Rceattle_MSE/Models/GOA_18.5.1_random_effects_models_3iter_w_hessian/18_5_1_re_3iter_Mod_1_2021-11-16.Rdata")
run_list <- list(mod_re)
load("~/GitHub/Rceattle_MSE/Models/GOA_18.5.1_random_effects_models_3iter_w_hessian/18_5_1_re_3iter_Mod_2_2021-11-17.Rdata")
run_list[[2]] <- mod_re

# Remove halibut
for(i in 1:2){
  # Controls
  run_list[[i]]$data_list$nspp <- 3
  run_list[[i]]$data_list$projyr <- 2060
  run_list[[i]]$data_list$spnames <- run_list[[i]]$data_list$spnames[1:3]
  run_list[[i]]$data_list$nsex <- run_list[[i]]$data_list$nsex[1:3]
  run_list[[i]]$data_list$R_sexr <- run_list[[i]]$data_list$R_sexr[1:3]
  run_list[[i]]$data_list$nages <- run_list[[i]]$data_list$nages[1:3]
  run_list[[i]]$data_list$minage <- run_list[[i]]$data_list$minage[1:3]
  run_list[[i]]$data_list$nlengths <- run_list[[i]]$data_list$nlengths[1:3]
  run_list[[i]]$data_list$pop_wt_index <- run_list[[i]]$data_list$pop_wt_index[1:3]
  run_list[[i]]$data_list$ssb_wt_index <- run_list[[i]]$data_list$ssb_wt_index[1:3]
  run_list[[i]]$data_list$pop_age_transition_index <- run_list[[i]]$data_list$pop_age_transition_index[1:3]
  run_list[[i]]$data_list$sigma_rec_prior <- run_list[[i]]$data_list$sigma_rec_prior[1:3]
  run_list[[i]]$data_list$other_food <- run_list[[i]]$data_list$other_food[1:3]
  run_list[[i]]$data_list$estDynamics <- run_list[[i]]$data_list$estDynamics[1:3]
  run_list[[i]]$data_list$proj_F <- run_list[[i]]$data_list$proj_F[1:3]
  run_list[[i]]$data_list$est_sex_ratio <- run_list[[i]]$data_list$est_sex_ratio[1:3]
  run_list[[i]]$data_list$sex_ratio_sigma <- run_list[[i]]$data_list$sex_ratio_sigma[1:3]
  run_list[[i]]$data_list$est_M1 <- run_list[[i]]$data_list$est_M1[1:3]
  
  # Data sheets
  run_list[[i]]$data_list$emp_sel <- run_list[[i]]$data_list$emp_sel %>%
    filter(Species != 4)
  
  run_list[[i]]$data_list$NByageFixed <- run_list[[i]]$data_list$NByageFixed %>%
    filter(Species != 4)
  
  run_list[[i]]$data_list$age_trans_matrix <- run_list[[i]]$data_list$age_trans_matrix %>%
    filter(Species != 4)
  
  run_list[[i]]$data_list$age_error <- run_list[[i]]$data_list$age_error %>%
    filter(Species != 4)
  
  run_list[[i]]$data_list$wt <- run_list[[i]]$data_list$wt %>%
    filter(Species != 4)
  
  run_list[[i]]$data_list$pmature <- run_list[[i]]$data_list$pmature %>%
    filter(Species != 4)
  
  run_list[[i]]$data_list$sex_ratio <- run_list[[i]]$data_list$sex_ratio %>%
    filter(Species != 4)
  
  run_list[[i]]$data_list$M1_base <- run_list[[i]]$data_list$M1_base %>%
    filter(Species != 4)
  
  run_list[[i]]$data_list$Mn_LatAge <- run_list[[i]]$data_list$Mn_LatAge %>%
    filter(Species != 4)
  
  run_list[[i]]$data_list$aLW <- run_list[[i]]$data_list$aLW %>%
    filter(Species != 4)
  
  # Bioenergetics
  run_list[[i]]$data_list$Ceq <- run_list[[i]]$data_list$Ceq[1:3]
  run_list[[i]]$data_list$Cindex <- run_list[[i]]$data_list$Cindex[1:3]
  run_list[[i]]$data_list$Pvalue <- run_list[[i]]$data_list$Pvalue[1:3]
  run_list[[i]]$data_list$fday <- run_list[[i]]$data_list$fday[1:3]
  run_list[[i]]$data_list$CA <- run_list[[i]]$data_list$CA[1:3]
  run_list[[i]]$data_list$CB <- run_list[[i]]$data_list$CB[1:3]
  run_list[[i]]$data_list$Qc <- run_list[[i]]$data_list$Qc[1:3]
  run_list[[i]]$data_list$Tco <- run_list[[i]]$data_list$Tco[1:3]
  run_list[[i]]$data_list$Tcm <- run_list[[i]]$data_list$Tcm[1:3]
  run_list[[i]]$data_list$Tcl <- run_list[[i]]$data_list$Tcl[1:3]
  run_list[[i]]$data_list$CK1 <- run_list[[i]]$data_list$CK1[1:3]
  run_list[[i]]$data_list$CK4 <- run_list[[i]]$data_list$CK4[1:3]
  
  # Diet data
  run_list[[i]]$data_list$Pyrs <- run_list[[i]]$data_list$Pyrs %>%
    filter(Species != 4)
  
  run_list[[i]]$data_list$UobsAge <- run_list[[i]]$data_list$UobsAge %>%
    filter(Pred != 4) %>%
    filter(Prey != 4)
  
  run_list[[i]]$data_list$UobsWtAge <- run_list[[i]]$data_list$UobsWtAge %>%
    filter(Pred != 4) %>%
    filter(Prey != 4)
  
  
  #################
  # Params
  run_list[[i]]$data_list$sigma_rec_prior <- exp(run_list[[i]]$estimated_params$ln_rec_sigma)[1:3]
  run_list[[i]]$estimated_params$rec_dev <- cbind(run_list[[i]]$estimated_params$rec_dev[1:3,], matrix(0, 3, 41))
  run_list[[i]]$estimated_params$ln_pop_scalar <- run_list[[i]]$estimated_params$ln_pop_scalar[1:3,1:21]
  run_list[[i]]$estimated_params$ln_mean_rec <- run_list[[i]]$estimated_params$ln_mean_rec[1:3]
  run_list[[i]]$estimated_params$ln_rec_sigma <- run_list[[i]]$estimated_params$ln_rec_sigma[1:3]
  run_list[[i]]$estimated_params$ln_sex_ratio_sigma <- run_list[[i]]$estimated_params$ln_sex_ratio_sigma[1:3]
  run_list[[i]]$estimated_params$init_dev <- run_list[[i]]$estimated_params$init_dev[1:3,1:20]
  run_list[[i]]$estimated_params$ln_M1 <- run_list[[i]]$estimated_params$ln_M1[1:3,,1:21]
  run_list[[i]]$estimated_params$ln_FSPR <- NULL
  run_list[[i]]$estimated_params$sel_coff_dev <- array(0, dim = c(16,2,19,42))
  
  # Dynamic params
  run_list[[i]]$estimated_params$ln_Flimit <- matrix(0, 3, 84)
  run_list[[i]]$estimated_params$ln_Ftarget <- matrix(0, 3, 84)
  
  # Predation params
}
ss_run <-  run_list[[1]]
ms_run <-  run_list[[2]]


# Ratio of F across Pcod fleets
avg_F <- (exp(ss_run$estimated_params$ln_mean_F+ss_run$estimated_params$F_dev)) # Average F from last 2 years
avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
f_ratio <- avg_F[14:16]
f_ratio <- f_ratio/sum(f_ratio)

# Adjust future F proportion to each fleet
ss_run$data_list$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio)
ss_run$estimated_params$proj_F_prop <- ss_run$data_list$fleet_control$proj_F_prop

ms_run$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio)
ms_run$estimated_params$proj_F_prop <- ms_run$data_list$fleet_control$proj_F_prop

# Single species run - Fixed M
ss_run_OM <- Rceattle::fit_mod(data_list = ss_run$data_list,
                               inits = NULL, # Initial parameters from single species ests
                               map = NULL,
                               file = NULL, # Don't save
                               estimate = 0, # Estimate
                               niter = 1, # 10 iterations around population and predation dynamics
                               msmMode = 0, # MSVPA based
                               phase = "default",
                               verbose = 1)

ss_run_OM <- Rceattle::fit_mod(data_list = ss_run$data_list,
                               inits = ss_run_OM$estimated_params, # Initial parameters from single species ests
                               map = NULL,
                               file = NULL, # Don't save
                               estimate = 0, # Estimate
                               niter = 1, # 10 iterations around population and predation dynamics
                               msmMode = 0, # MSVPA based
                               phase = NULL,
                               verbose = 1)

# Single-species run - Estimate M
data_listM <- ss_run$data_list
data_listM$est_M1 <- c(1,2,1)
ss_run_M_OM <- Rceattle::fit_mod(data_list = data_listM,
                                 inits = ss_run_OM$estimated_params, # Initial parameters from single species ests
                                 file = NULL, # Don't save
                                 estimate = 0, # Estimate
                                 niter = 1, # 10 iterations around population and predation dynamics
                                 msmMode = 0, # MSVPA based
                                 phase = "default",
                                 verbose = 1)

# Multi-species
ms_run_OM <- Rceattle::fit_mod(data_list = ms_run$data_list,
                               inits = ss_run_M_OM$estimated_params, # Initial parameters from single species ests
                               estimate = 0, # Estimate
                               niter = 3, # 10 iterations around population and predation dynamics
                               random_rec = FALSE, # No random recruitment
                               msmMode = 1, # MSVPA based
                               suitMode = 0, # empirical suitability
                               phase = NULL,
                               verbose = 1)

save(ss_run_OM, ss_run_M_OM, ms_run_OM, file = "Models/GOA_1977_Models_from_previous.RData")
