
library(Rceattle)

################################################
# FIT GOA MODELS
################################################

load("Models/GOA_18_5_1_mod_1-2_2022-05-26.RData")
mod_list_all <- mod_list_all #  <- list(ss_run_OM, ss_run_M_OM, ms_run_OM)

# Ratio of F across Pcod fleets
for(i in 1:3){
  avg_F <- (exp(mod_list_all[[i]]$estimated_params$ln_mean_F+mod_list_all[[i]]$estimated_params$F_dev)) # Average F from last 2 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
  f_ratio <- avg_F[14:16]
  f_ratio <- f_ratio/sum(f_ratio)
  
  # Adjust future F proportion to each fleet
  mod_list_all[[i]]$data_list$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio)
  mod_list_all[[i]]$estimated_params$proj_F_prop <- mod_list_all[[i]]$data_list$fleet_control$proj_F_prop
}


ss_run_GOA <- mod_list_all[[1]]
ss_run_M_GOA <- mod_list_all[[2]]
ms_run_GOA <- mod_list_all[[3]]


################################################
# FIT EBS MODELS
################################################


# To run the 2017 single species assessment for the Bering Sea, a data file must first be loaded:
data(BS2017SS) # ?BS2017SS for more information on the data
BS2017SS$projyr <- 2060

# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
BS2017SS$fleet_control$proj_F_prop <-rep(1,7)
ss_run_EBS <- Rceattle::fit_mod(data_list = BS2017SS,
                                inits = NULL, # Initial parameters = 0
                                file = NULL, # Don't save
                                estimateMode = 1, # Estimate hindcast only
                                random_rec = FALSE, # No random recruitment
                                msmMode = 0, # Single species mode
                                phase = "default",
                                verbose = 1)

# Estimate single-species and estimate M
BS2017SS_M <- BS2017SS
BS2017SS_M$est_M1 <- c(1,1,1)
ss_run_M_EBS <- Rceattle::fit_mod(data_list = BS2017SS_M,
                                  inits = ss_run_EBS$estimated_params, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 1, # Estimate hindcast only
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  phase = "default",
                                  verbose = 1)

ss_run_M_EBS <- Rceattle::fit_mod(data_list = BS2017SS_M,
                                  inits = ss_run_M_EBS$estimated_params, # Initial parameters = 0
                                  file = NULL, # Don't save
                                  estimateMode = 1, # Estimate hindcast only
                                  random_rec = FALSE, # No random recruitment
                                  msmMode = 0, # Single species mode
                                  phase = "default",
                                  verbose = 1)



# For the a multispecies model starting from the single species parameters, the following can be specified to load the data:
data("BS2017MS") # Note: the only difference is the residual mortality (M1_base) is lower
BS2017MS$est_M1 <- c(0,0,0) # Estimate residual M
BS2017MS$projyr <- 2060
ms_run_EBS <- Rceattle::fit_mod(data_list = BS2017MS,
                                inits = ss_run_EBS$estimated_params, # Initial parameters from single species ests
                                file = NULL, # Don't save
                                estimateMode = 1, # Estimate hindcast only
                                niter = 3, # 10 iterations around population and predation dynamics
                                random_rec = FALSE, # No random recruitment
                                msmMode = 1, # MSVPA based
                                suitMode = 0, # empirical suitability
                                verbose = 1)


################################################
# SET UP TABLE
################################################

mod_list <- list(ss_run_EBS, ss_run_M_EBS, ss_run_GOA, ss_run_M_GOA)

supp_table_1 <- data.frame(matrix(NA, ncol = 4*4, nrow = 21))

ind = 1


mod = c(1:4,1:4,1:2,3:4,3:4)
species = c(1,1,1,1, 2,2,3,3,3,3,2,2,2,2)
sex = c(1,1,1,1,1,1,1,1,1,1,1,1,2,2)


for(i in 1:length(mod)){
  ages <- 1:mod_list[[mod[i]]]$data_list$nages[species[i]]
  supp_table_1[ages,i] <- exp(mod_list[[mod[i]]]$estimated_params$ln_M1[species[i],sex[i], ages])
}

write.csv(supp_table_1, file = "Tables/Supp_table_1_M1.csv")

