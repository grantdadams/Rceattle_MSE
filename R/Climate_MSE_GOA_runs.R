################################################
# Set-up
################################################
source("R/Climate_MSE_condition_GOA_OMs.R")
source("R/Climate_MSE_condition_GOA_EMs.R")

## Cap
# - Max historical catch for Arrowtooth flounder
max_atf <- ms_mod$data_list$fsh_biom
max_atf <- max_atf[which(max_atf$Species == 2),]

cap_list <-  c(1e10, max(max_atf$Catch, na.rm = TRUE), 1e10)

# - Max historical catch for all species
max_catch <- ms_mod$data_list$fsh_biom %>%
  filter(Year < 2024) %>%
  group_by(Year, Species) %>%
  summarise(Catch = sum(Catch)) %>%
  group_by(Species) %>%
  summarise(MaxCatch = max(Catch))

cap_list <-  max_catch$MaxCatch

## Sampling period
sampling_period <- c(2,2,1,2,2,2,2,1,2,2,1,2,2,1,1,1,1,1)


################################################
# Management strategy evaluation
################################################
### Run the MSE
source("D:/GitHub/Rceattle/R/11a-mse_run_parallel.R")
source("R/Functions/Run_climate_MSE_function.R")

# No cap
run_climate_mse(system = "GOA_Climate_2", om_list = om_list[c(1,5)], om_names = om_names[c(1,5)], em_hcr_list = em_list[1:2], em_hcr_names = em_names[1:2], sampling_period = sampling_period, nsim = 200, regenerate_past = FALSE, cap = NULL)
gc()

# Cap
run_climate_mse(system = "GOA_Climate_2", om_list = om_list[c(1,5)], om_names = om_names[c(1,5)], em_hcr_list = em_list[1:2], em_hcr_names = em_names[1:2], sampling_period = sampling_period, nsim = 200, regenerate_past = FALSE, cap = cap_list)
gc()
