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