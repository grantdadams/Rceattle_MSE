hind_nyrs <- length(ms_mod_ricker$data_list$styr:ms_mod_ricker$data_list$endyr)
om_proj_nyrs <- length((ms_mod_ricker$data_list$endyr+1):ms_mod_ricker$data_list$projyr)

sp = 1
rec_dev <- sample(x = (log(ms_mod_ricker$quantities$R) - log(ms_mod_ricker$quantities$R_hat))[sp, 1:hind_nyrs],
                  size = om_proj_nyrs, replace = TRUE) # - Scale mean rec for rec trend

ms_mod_ricker$estimated_params$rec_dev[1,(hind_nyrs+1):(hind_nyrs+om_proj_nyrs)] <- rec_dev

# * Ricker recruitment ----
# - Climate naive
ms_mod_ricker2 <- Rceattle::fit_mod(data_list = combined_data,
                                   inits = ms_mod_ricker$estimated_params, # Initial parameters = 0
                                   file = NULL, # Don't save
                                   estimateMode = 4, # Estimate
                                   random_rec = FALSE, # No random recruitment
                                   msmMode = 1, # Multi species mode
                                   verbose = 1,
                                   niter = 3,
                                   suit_meanyr = 2018,
                                   phase = NULL,
                                   M1Fun = build_M1(M1_model = c(1,2,1),
                                                    M1_use_prior = FALSE,
                                                    M2_use_prior = FALSE),
                                   recFun = build_srr(srr_fun = 0,
                                                      srr_pred_fun = 4,
                                                      proj_mean_rec = FALSE,
                                                      srr_est_mode = 1,
                                                      srr_prior_mean = alpha,
                                                      srr_prior_sd = 0.2,
                                                      Bmsy_lim = apply(ms_mod$quantities$biomassSSB, 1, max)
                                   ),
                                   initMode = 1)

sp = 1
rec_dev <- sample(x = ms_mod_ricker$estimated_params$rec_dev[sp, 1:hind_nyrs],
                  size = om_proj_nyrs, replace = TRUE) # - Scale mean rec for rec trend

ms_mod_ricker$estimated_params$rec_dev[1,(hind_nyrs+1):(hind_nyrs+om_proj_nyrs)] <- rec_dev


# * Ricker recruitment ----
# - Climate naive
ms_mod_ricker3 <- Rceattle::fit_mod(data_list = combined_data,
                                    inits = ms_mod_ricker$estimated_params, # Initial parameters = 0
                                    file = NULL, # Don't save
                                    estimateMode = 4, # Estimate
                                    random_rec = FALSE, # No random recruitment
                                    msmMode = 1, # Multi species mode
                                    verbose = 1,
                                    niter = 3,
                                    suit_meanyr = 2018,
                                    phase = NULL,
                                    M1Fun = build_M1(M1_model = c(1,2,1),
                                                     M1_use_prior = FALSE,
                                                     M2_use_prior = FALSE),
                                    recFun = build_srr(srr_fun = 0,
                                                       srr_pred_fun = 4,
                                                       proj_mean_rec = FALSE,
                                                       srr_est_mode = 1,
                                                       srr_prior_mean = alpha,
                                                       srr_prior_sd = 0.2,
                                                       Bmsy_lim = apply(ms_mod$quantities$biomassSSB, 1, max)
                                    ),
                                    initMode = 1)
