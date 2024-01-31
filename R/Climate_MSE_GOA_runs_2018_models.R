################################################
# Set-up
################################################
source("R/Climate_MSE_condition_GOA_OMs_2018_models.R")
source("R/Climate_MSE_condition_GOA_EMs_2018_models.R")

## Cap
# 1. Max historical catch for Arrowtooth flounder
# 2. No cap
max_atf <- ss_mod$data_list$fsh_biom
max_atf <- max_atf[which(max_atf$Species == 2),]

# Pollock, cod, atf
cap_list <-  c(1e10, max(max_atf$Catch, na.rm = TRUE), 1e10)

## Sampling period
sampling_period <- c(2,2,1,2,2,2,2,1,2,2,1,2,2,1,1,1)


################################################
# Management strategy evaluation
################################################
### Run the MSE
source("R/Functions/Run_climate_MSE_function.R")

# No cap
run_climate_mse(system = "GOA_Climate_2018", om_list = om_list, om_names = om_names, em_hcr_list = em_list[1:2], em_hcr_names = em_names[1:2], sampling_period = sampling_period, nsim = 200, regenerate_past = FALSE, cap = NULL)

# Cap
run_climate_mse(system = "GOA_Climate_2018", om_list = om_list, om_names = om_names, em_hcr_list = em_list[1:2], em_hcr_names = em_names[1:2], sampling_period = sampling_period, nsim = 200, regenerate_past = FALSE, cap = cap_list)


# 12 = HCRs 1/2, 1 = HCR 3/4, 3 = HCR 5, 4 = HCR 6, 5 = HCR 7, 9 =  HCR 8


