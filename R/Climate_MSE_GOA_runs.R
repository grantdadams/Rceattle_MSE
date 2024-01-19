################################################
# Set-up
################################################
source("R/Climate_MSE_condition_GOA_OMs.R")
source("R/Climate_MSE_condition_GOA_EMs.R")

## Cap
# 1. Max historical catch for Arrowtooth flounder
# 2. No cap
max_atf <- ss_mod$data_list$fsh_biom
max_atf <- max_atf[which(max_atf$Species == 2),]

# Pollock, cod, atf
cap_list <-  c(1e10, max(max_atf$Catch, na.rm = TRUE), 1e10)

## Sampling period
sampling_period <- c(2,2,1,2,2,2,2,1,2,2,1,2,2,1,1,1,1,1)


################################################
# Management strategy evaluation
################################################
### Run the MSE
source("R/Functions/Run_climate_MSE_function.R")

# No cap
run_climate_mse(system = "GOA_Climate", om_list = om_list, om_names = om_names, em_hcr_list = em_list, em_hcr_names = em_names, sampling_period = sampling_period, nsim = 1, regenerate_past = FALSE, cap = NULL)

# Cap
run_mse(system = "GOA_Climate", om_list = om_list, om_names = om_names, em_hcr_list = em_list[1], em_hcr_names = em_names, sampling_period = sampling_period, nsim = 300, regenerate_past = FALSE, cap = cap_list)


# 11 = 1, 12 = 2, 1 = 3, 4 = 4, 5 = 5, 8 = 6

# 12 has lognromal bias correction to simulation, 1 to simulation and fit (no), 11 has no regenerate past, 8 has fixedregenerate paste (yes), 5 has no accumulation age (no), 4 has no regenerate past and lognormal bias sim/fit (yes)

