library(Rceattle)
library(dplyr)

library(Rceattle)

################################################
# Data
################################################
# Example
# To run the 2018 single species assessment for the Gulf of Alaska, a data file must first be loaded:
data("GOA2018SS") # Single-species data. ?BS2017SS for more information on the data


################################################
# Estimation
################################################
# - Single-species
# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
GOA2018SS$fleet_control$proj_F_prop <- rep(1, nrow(GOA2018SS$fleet_control))
ss_run <- Rceattle::fit_mod(
  data_list = GOA2018SS,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  phase = "default",
  verbose = 1, 
  initMode = 0)

plot_biomass(ss_run, add_ci = TRUE)


# Single-species, but estimate M
# - Run to build map
ss_run_M <- Rceattle::fit_mod(
  data_list = GOA2018SS,
  inits = ss_run$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 3, # Estimate
  M1Fun = build_M1(M1_model = c(1,2,1)), # Estimate M
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  phase = "default",
  verbose = 1, 
  initMode = 0)

map <- ss_run_M$map
map$mapList$init_dev <- ifelse(exp(ss_run$estimated_params$init_dev) < 100, NA, map$mapList$init_dev)

map$mapFactor$init_dev <- factor(map$mapList$init_dev)

# - Rerun
ss_run_M <- Rceattle::fit_mod(
  data_list = GOA2018SS,
  inits = ss_run$estimated_params, # Initial parameters = 0
  file = map, # Don't save
  estimateMode = 1, # Estimate
  M1Fun = build_M1(M1_model = c(1,2,0)), # Estimate M
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # Single species mode
  phase = "default",
  verbose = 1, 
  initMode = 0)

plot_biomass(ss_run_M)

# Multi-species
# For the a multispecies model we from the single species parameters.
ms_run <- Rceattle::fit_mod(
  data_list = GOA2018SS,
  inits = ss_run_M$estimated_params, # Initial parameters from single species ests,
  map = map,
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  M1Fun = build_M1(M1_model = c(1,2,1)),
  niter = 3, # 3 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  verbose = 1, 
  initMode = 0)

plot_biomass(list(ss_run, ms_run))

################################################
# Estimate OMs w ricker ----
################################################


alpha = exp(c(3.143, 1.975, 1.44))


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
  initMode = 0)
plot_biomass(ss_run_ricker, incl_proj = TRUE)
plot_stock_recruit(ss_run_ricker)

# Estimate single-species and estimate M
ss_run_ricker_M <- Rceattle::fit_mod(
  data_list = ss_run_M$data_list,
  inits = ss_run_M$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 3, # Estimate hindcast only
  M1Fun = build_M1(M1_model = c(1,2,1),
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
  initMode = 0)
plot_biomass(ss_run_ricker_M, incl_proj = TRUE)
plot_stock_recruit(ss_run_ricker_M)

# - Setup map
map <- ss_run_M$map
map$mapList$init_dev <- ifelse(exp(ss_run_ricker$estimated_params$init_dev) < 100, NA,map$mapList$init_dev)

map$mapFactor$init_dev <- factor(map$mapList$init_dev)


# Estimate single-species and estimate M
ss_run_ricker_M <- Rceattle::fit_mod(
  data_list = ss_run_M$data_list,
  inits = ss_run_M$estimated_params, # Initial parameters = 0
  map = map,
  file = NULL, # Don't save
  estimateMode = 0, # Estimate hindcast only
  M1Fun = build_M1(M1_model = c(1,2,1),
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
  initMode = 0)
plot_biomass(ss_run_ricker_M, incl_proj = TRUE)
plot_stock_recruit(ss_run_ricker_M)


# - Multi-species
ms_run_ricker <- Rceattle::fit_mod(
  data_list = ms_run$data_list,
  inits = ms_run$estimated_params, # Initial parameters from single species ests
  file = NULL, # Don't save
  estimateMode = 1, # Estimate hindcast only
  map = map,
  M1Fun = build_M1(M1_model = c(1,2,1),
                   updateM1 = FALSE,
                   M1_use_prior = FALSE,
                   M2_use_prior = FALSE),
  recFun = build_srr(srr_fun = 3,
                     proj_mean_rec = FALSE,
                     srr_est_mode = 1,
                     srr_prior_mean = alpha * 3,
                     Bmsy_lim = apply(ms_run$quantities$biomassSSB, 1, max)),
  niter = 3, # 10 iterations around population and predation dynamics
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # MSVPA based
  suitMode = 0, # empirical suitability
  phase = "default",
  verbose = 1, 
  initMode = 0)
plot_biomass(ms_run_ricker, incl_proj = TRUE)
plot_stock_recruit(ms_run_ricker)

ms_run$quantities$M1[1,1,]
exp(ms_run_ricker$estimated_params$rec_pars)
ms_run_ricker$quantities$SPR0

ms_run_ricker$estimated_params$rec_pars[1,3] <- 0
ms_run_ricker$estimated_params$rec_pars[1,2] <- 4

