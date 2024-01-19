################################################
# Set-up
################################################
source("R/Climate_MSE_condition_GOA_OMs.R")
source("R/Climate_MSE_condition_GOA_EMs.R")

## Cap
# 1. Max historical catch for Arrowtooth flounder
# 2. No cap
max_atf <- ss_run$data_list$fsh_biom
max_atf <- max_atf[which(max_atf$Species == 2),]

# Pollock, cod, atf
cap_list <- list(
  one = c(1e10, max(max_atf$Catch, na.rm = TRUE), 1e10), # Historical ATF
  two = c(1e10, 1e10, 1e10) # No cap
)

## Sampling period
sampling_period <- c(2,2,1,2,2,2,2,1,2,2,1,2,2,1,1,1)


################################################
# Management strategy evaluation
################################################
### Run the MSE
source("R/Functions/Run_full_MSE_function.R")

run_mse(system = "GOA1977", recname = "ConstantR", om_list = om_list[4], om_names = om_names[4], em_hcr_list = em_hcr_list[1:8], em_hcr_names = em_hcr_names[1:8], sampling_period = sampling_period, nsim = 300)

run_mse(system = "GOA1977", recname = "ConstantR", om_list = om_list[2], om_names = om_names[2], em_hcr_list = em_hcr_list[3:16], em_hcr_names = em_hcr_names[3:16], sampling_period = sampling_period, nsim = 300, regenerate_past = FALSE)


# 11 = 1, 12 = 2, 1 = 3, 4 = 4, 5 = 5, 8 = 6

# 12 has lognromal bias correction to simulation, 1 to simulation and fit (no), 11 has no regenerate past, 8 has fixedregenerate paste (yes), 5 has no accumulation age (no), 4 has no regenerate past and lognormal bias sim/fit (yes)

