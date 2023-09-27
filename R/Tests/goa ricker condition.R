library(Rceattle)
library(dplyr)

load("Models/GOA_18_5_1_mod_1-2_2023-07-05.RData")
mod_list_all <- mod_list_all 
data("GOA2018SS")


ss_run <- mod_list_all[[1]]
ss_run_M <- mod_list_all[[2]]
ss_run_M$data_list$M1_model <- c(1,2,1)
ms_run <- mod_list_all[[3]]
ms_run$data_list$M1_model <- c(1,2,1)

alpha = exp(c(3.143, 1.975, 1.44))

################################################
# Estimate OMs w ricker ----
################################################
# - Single-species fixed M
ss_run_ricker <- Rceattle::fit_mod(
  data_list = ss_run$data_list,
  inits = ss_run$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha,
                     srr_prior_sd = 0.2),
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  phase = "default",
  verbose = 1, 
  initMode = 2)
#plot_ssb(ss_run_ricker, incl_proj = TRUE)
#plot_stock_recruit(ss_run_ricker)

# - Check Bmsy
alpha <- exp(ss_run_ricker$estimated_params$rec_pars[,2])
beta <- exp(ss_run_ricker$estimated_params$rec_pars[,3])
apply(ss_run$quantities$biomassSSB, 1, max)
1/(beta/1000000)
#plot_ssb(list(ss_run_ricker, ss_run), incl_proj = TRUE)


# Estimate single-species and estimate M
ss_run_M$data_list$M1_base[4,3:23] <- 0.35
ss_run_ricker_M <- Rceattle::fit_mod(
  data_list = ss_run_M$data_list,
  inits = ss_run_M$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  M1Fun = build_M1(M1_model = c(1,2,0),
                   updateM1 = TRUE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha,
                     srr_prior_sd = 0.2),
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  phase = "default",
  verbose = 1, 
  initMode = 2)

# -----
# - Multi-species
ms_run_ricker <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ss_run_ricker$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = NULL,
  M1Fun = build_M1(M1_model = c(1,2,1),
                   updateM1 = FALSE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     Bmsy_lim = c(800000, apply(ms_run$quantities$biomassSSB, 1, max)[2:3])),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = "default",
  verbose = 1, 
  initMode = 0)


# - Multi-species
ms_run_ricker2 <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ss_run_ricker$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = NULL,
  M1Fun = build_M1(M1_model = c(1,2,1),
                   updateM1 = FALSE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     Bmsy_lim = c(800000, apply(ms_run$quantities$biomassSSB, 1, max)[2:3])),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = NULL,
  verbose = 1, 
  initMode = 0)


# - Multi-species
ms_run_ricker3 <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ss_run_ricker_M$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = NULL,
  M1Fun = build_M1(M1_model = c(1,2,1),
                   updateM1 = FALSE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     Bmsy_lim = c(800000, apply(ms_run$quantities$biomassSSB, 1, max)[2:3])),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = "default",
  verbose = 1, 
  initMode = 0)

# - Multi-species
ms_run_ricker4 <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ss_run_ricker_M$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = NULL,
  M1Fun = build_M1(M1_model = c(1,2,1),
                   updateM1 = FALSE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     Bmsy_lim = c(800000, apply(ms_run$quantities$biomassSSB, 1, max)[2:3])),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = NULL,
  verbose = 1, 
  initMode = 0)


# - Multi-species
ms_run_ricker5 <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ms_run$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = NULL,
  M1Fun = build_M1(M1_model = c(1,2,1),
                   updateM1 = FALSE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     Bmsy_lim = c(800000, apply(ms_run$quantities$biomassSSB, 1, max)[2:3])),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = "default",
  verbose = 1, 
  initMode = 0)

# - Multi-species
ms_run_ricker6 <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ms_run$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = NULL,
  M1Fun = build_M1(M1_model = c(1,2,1),
                   updateM1 = FALSE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     Bmsy_lim = c(800000, apply(ms_run$quantities$biomassSSB, 1, max)[2:3])),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = NULL,
  verbose = 1, 
  initMode = 0)



# - Multi-species
ms_run_ricker7 <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ss_run_ricker$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = NULL,
  M1Fun = build_M1(M1_model = c(1,2,0),
                   updateM1 = FALSE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     Bmsy_lim = c(800000, apply(ms_run$quantities$biomassSSB, 1, max)[2:3])),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = "default",
  verbose = 1, 
  initMode = 0)

# - Multi-species
ms_run_ricker8 <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ss_run_ricker$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = NULL,
  M1Fun = build_M1(M1_model = c(1,2,0),
                   updateM1 = FALSE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     Bmsy_lim = c(800000, apply(ms_run$quantities$biomassSSB, 1, max)[2:3])),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = NULL,
  verbose = 1, 
  initMode = 0)


# - Multi-species
ms_run_ricker9 <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ss_run_ricker_M$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = NULL,
  M1Fun = build_M1(M1_model = c(1,2,0),
                   updateM1 = FALSE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     Bmsy_lim = c(800000, apply(ms_run$quantities$biomassSSB, 1, max)[2:3])),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = "default",
  verbose = 1, 
  initMode = 0)

# - Multi-species
ms_run_ricker10 <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ss_run_ricker_M$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = NULL,
  M1Fun = build_M1(M1_model = c(1,2,0),
                   updateM1 = FALSE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     Bmsy_lim = c(800000, apply(ms_run$quantities$biomassSSB, 1, max)[2:3])),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = NULL,
  verbose = 1, 
  initMode = 0)


# - Multi-species
ms_run_ricker11 <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ms_run$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = NULL,
  M1Fun = build_M1(M1_model = c(1,2,0),
                   updateM1 = FALSE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     Bmsy_lim = c(800000, apply(ms_run$quantities$biomassSSB, 1, max)[2:3])),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = "default",
  verbose = 1, 
  initMode = 0)

# - Multi-species
ms_run_ricker12 <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ms_run$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = NULL,
  M1Fun = build_M1(M1_model = c(1,2,0),
                   updateM1 = FALSE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     Bmsy_lim = c(800000, apply(ms_run$quantities$biomassSSB, 1, max)[2:3])),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = NULL,
  verbose = 1, 
  initMode = 0)