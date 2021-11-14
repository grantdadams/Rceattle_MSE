library(Rceattle)

load("~/GitHub/RceattleRuns/GOA/Model runs/GOA_18.5.1/Models/18_5_1_Niter3_2021-06-14.RData")
ss_run <- mod_list_all[[1]]
ms_run <- mod_list_all[[2]]


ss_run$data_list$projyr <- 2060
ms_run$data_list$projyr <- 2060

ss_run$data_list$fleet_control$proj_F_prop <- c(rep(0, 7), 1,0,0,1, 0,0,1/3,1/3,1/3)
ss_run$estimated_params$proj_F_prop <- ss_run$data_list$fleet_control$proj_F_prop

# Estimate FSPR
ss_run$estimated_params$rec_dev <- cbind(ss_run$estimated_params$rec_dev, matrix(0, 4, 41))
ss_run <- Rceattle::fit_mod(data_list = ss_run$data_list,
                                       inits = ss_run$estimated_params, # Initial parameters from single species ests
                                       file = NULL, # Don't save
                                       debug = FALSE, # Estimate
                                       niter = 3, # 10 iterations around population and predation dynamics
                                       random_rec = FALSE, # No random recruitment
                                       msmMode = ss_run$data_list$msmMode, # MSVPA based
                                       suitMode = 0, # empirical suitability
                                       phase = "default",
                                       verbose = 1)

# EST-M
data_list <- ss_run$data_list
data_list$est_M1 <- c(1,2,1,0)
ss_run_M <- Rceattle::fit_mod(data_list = data_list,
                            inits = ss_run$estimated_params, # Initial parameters from single species est
                            file = NULL, # Don't save
                            debug = FALSE, # Estimate
                            niter = 3, # 10 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = ss_run$data_list$msmMode, # MSVPA based
                            suitMode = 0, # empirical suitability
                            phase = "default",
                            verbose = 1)

# Update projections
ms_run$estimated_params$rec_dev <- cbind(ms_run$estimated_params$rec_dev, matrix(0, 4, 41))
ms_run <- Rceattle::fit_mod(data_list = ms_run$data_list,
                            inits = ms_run$estimated_params, # Initial parameters from single species ests
                            file = NULL, # Don't save
                            debug = FALSE, # Estimate
                            niter = 5, # 10 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = ms_run$data_list$msmMode, # MSVPA based
                            suitMode = 0, # empirical suitability
                            phase = "default",
                            verbose = 1)

# mse1 MS-OM, SS-Est M Tier 3 EM
# mse2 SS-OM, SS-Est M Tier 3 EM
# mse3 SS-OM, SS-Fixed M Tier 3 EM
# mse4 MS-OM, SS-Fixed M Tier 3 EM
# mse5 MS-OM, SS-Fixed M No Catch EM
# mse6 SS-OM, SS-Fixed M No Catch EM
save(ms_run, ss_run, ss_run_M, file = "GOA_2018_Runs.Rdata")

# Run MSE
# - MS-OM: SS-EM
mse1 <- mse_run(operating_model = ms_run, estimation_model = ss_run_M, nsim = 50, assessment_period = 2, sampling_period = 2, simulate = TRUE, cap = c(1500000))
save(mse1, file = "R/Runs/GOAmse1.RData")
gc()
rm(mse1)

# Run MSE
# - SS-OM: SS-EM
mse2 <- mse_run(operating_model = ss_run_M, estimation_model = ss_run_M, nsim = 50, assessment_period = 2, sampling_period = 2, simulate = TRUE, cap = c(1500000))
save(mse2, file = "R/Runs/GOAmse2.RData")
gc()
rm(mse2)

# Run MSE
# - MS-OM: SS-EM
mse3 <- mse_run(operating_model = ss_run_M, estimation_model = ss_run, nsim = 50, assessment_period = 2, sampling_period = 2, simulate = TRUE, cap = c(1500000))
save(mse3, file = "R/Runs/GOAmse3.RData")
gc()
rm(mse3)

# Run MSE
# - SS-OM: SS-EM
mse4 <- mse_run(operating_model = ms_run, estimation_model = ss_run, nsim = 50, assessment_period = 2, sampling_period = 2, simulate = TRUE, cap = c(1500000))
save(mse4, file = "R/Runs/GOAmse4.RData")
gc()
rm(mse4)

# Run MSE
# - SS-OM: SS-EM
mse5 <- mse_run(operating_model = ms_run, estimation_model = ss_run, nsim = 50, assessment_period = 2, sampling_period = 2, simulate = TRUE, cap = c(1))
save(mse5, file = "R/Runs/GOAmse5.RData")
gc()
rm(mse5)

# Run MSE
# - SS-OM: SS-EM
mse6 <- mse_run(operating_model = ss_run_M, estimation_model = ss_run, nsim = 50, assessment_period = 2, sampling_period = 2, simulate = TRUE, cap = c(1))
save(mse6, file = "R/Runs/GOAmse6.RData")
gc()
rm(mse6)




check <- c()

for(i in 1:length(operating_model_use$estimated_params)){
  check[i] <- sum(operating_model_use$estimated_params[[i]] != operating_model$estimated_params[[i]], na.rm = TRUE)
}
names(operating_model$estimated_params)[i]


checkmap <- c()
checkparams <- c()
for(i in 1:length(operating_model_use$map[[2]])){
  if(i %in% c(12,14)){
    checkmap[i] <- sum(operating_model_use$map[[2]][[i]][,1:42] != operating_model$map[[2]][[i]], na.rm = TRUE)
    checkparams[i] <- sum(operating_model_use$estimated_params[[i]][,1:42] != operating_model$estimated_params[[i]], na.rm = TRUE)
  }else if(i %in% c(20,21)){
    checkmap[i] <- sum(operating_model_use$map[[2]][[i]][,,,1:42] != operating_model$map[[2]][[i]], na.rm = TRUE)
    checkparams[i] <- sum(operating_model_use$estimated_params[[i]][,,,1:42] != operating_model$estimated_params[[i]], na.rm = TRUE)
  }else{
    checkmap[i] <- sum(operating_model_use$map[[2]][[i]] != operating_model$map[[2]][[i]], na.rm = TRUE)
    checkparams[i] <- sum(operating_model_use$estimated_params[[i]] != operating_model$estimated_params[[i]], na.rm = TRUE)
  }
}
checkmap
checkparams
names(operating_model$map[[2]])[i]


checkdata <- c()
for(i in 1:length(operating_model$data_list)){
  if(i!=26)
    checkdata[i] <- sum(operating_model$data_list[[i]] != operating_model$data_list[[i]], na.rm = TRUE)
}
names(operating_model$data_list)[i]

