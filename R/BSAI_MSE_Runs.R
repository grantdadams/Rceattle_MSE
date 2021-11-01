library(Rceattle)

################################################
# Data
################################################
# Example
# To run the 2017 single species assessment for the Bering Sea, a data file must first be loaded:
data(BS2017SS) # ?BS2017SS for more information on the data
BS2017SS$projyr <- 2060
mydata <- BS2017SS


################################################
# Estimation
################################################
# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
mydata$fleet_control$proj_F_prop <-rep(1,7)
ss_run <- Rceattle::fit_mod(data_list = mydata,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            debug = FALSE, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            phase = "default",
                            silent = TRUE)


# Estimate M
mydata_M <- mydata
mydata_M$est_M1 <- c(1,1,1)
ss_run_M <- Rceattle::fit_mod(data_list = mydata_M,
                              inits = ss_run$estimated_params, # Initial parameters = 0
                              file = NULL, # Don't save
                              debug = FALSE, # Estimate
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              phase = "default",
                              silent = TRUE)



# For the a multispecies model starting from the single species parameters, the following can be specified to load the data:
data("BS2017MS") # Note: the only difference is the residual mortality (M1_base) is lower
BS2017MS$est_M1 <- c(1,1,1)
BS2017MS$projyr <- 2060
ms_run <- Rceattle::fit_mod(data_list = BS2017MS,
                            inits = ss_run_M$estimated_params, # Initial parameters from single species ests
                            file = NULL, # Don't save
                            debug = 0, # Estimate
                            niter = 3, # 10 iterations around population and predation dynamics
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # MSVPA based
                            suitMode = 0, # empirical suitability
                            silent = TRUE)


# Run MSE
# - MS-OM: SS-EM
mse1 <- mse_run(operating_model = ms_run, estimation_model = ss_run, nsim = 50, assessment_period = 2, sampling_period = 2, simulate = TRUE, cap = c(1500000))

# Run MSE
# - SS-OM: SS-EM
mse2 <- mse_run(operating_model = ss_run_M, estimation_model = ss_run, nsim = 50, assessment_period = 2, sampling_period = 2, simulate = TRUE, cap = c(1500000))

# Run MSE
# - MS-OM: SS-EM
mse3 <- mse_run(operating_model = ss_run_M, estimation_model = ss_run_M, nsim = 50, assessment_period = 2, sampling_period = 2, simulate = TRUE, cap = c(1500000))

# Run MSE
# - SS-OM: SS-EM
mse4 <- mse_run(operating_model = ms_run, estimation_model = ss_run_M, nsim = 50, assessment_period = 2, sampling_period = 2, simulate = TRUE, cap = c(1500000))

# Run MSE
# - SS-OM: SS-EM
mse5 <- mse_run(operating_model = ss_run, estimation_model = ss_run_M, nsim = 50, assessment_period = 2, sampling_period = 2, simulate = TRUE, cap = c(1500000))

