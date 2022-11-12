library(Rceattle)
library(readxl)


######################### 
# Read in data
#########################
GOA_data <- Rceattle::read_data( file = "Models/GOA_18_5_1_data_1977-2018_no_halibut.xlsx")
GOA_data$styr <- 1977

# Set up model list
# The three models for 1977 to 2022 are: 
# •	Model 1: a model that does not include predation to represent a base single-species model. M is fixed
# •	Model 2: a model that does not include predation to represent a base single-species model. M is estimated
# •	Model 3: a mutlispecies model that did not include halibut predation.

mydata_list <- list(
  GOA_data, # 1 - single-species SAFE M 
  GOA_data, # 2 - single-species Estimated M 
  GOA_data # 3 - Multi-species no hal - Est M1
)


# Set up inits vectors
inits_M1_df <- data.frame(
  Model = 1:3,
  MsmMode = c(0, 0, 1), # Short 
  EstM1 = c(0, 1, 1), # Short
  InitModel = c(NA, 1, 2) # Short
) 


# Set up M1 estimation switches
for(i in 1:length(mydata_list)){
  mydata_list[[i]]$est_M1 = c(0,0,0)
  if(inits_M1_df$EstM1[i] == 1){
    mydata_list[[i]]$est_M1 = c(1,2,1)
  }
}



################################################
# Single species
################################################
mod_list_all <- list()

# - Fix M
mod_list_all[[1]] <- Rceattle::fit_mod(data_list = mydata_list[[1]],
                                       inits = NULL, # Initial parameters = 0
                                       file = NULL, # Don't save
                                       estimateMode = 0, # Estimate
                                       random_rec = FALSE, # No random recruitment
                                       msmMode = 0, # Single species mode
                                       verbose = 1,
                                       phase = "default")
# - Est M
mod_list_all[[2]] <- Rceattle::fit_mod(data_list = mydata_list[[2]],
                                       inits = mod_list_all[[1]]$estimated_params, # Initial parameters = 0
                                       file = NULL, # Don't save
                                       estimateMode = 0, # Estimate
                                       random_rec = FALSE, # No random recruitment
                                       msmMode = 0, # Single species mode
                                       verbose = 1,
                                       phase = "default")


################################################
# Model 2 - Run multi-species
################################################
mod_list_all[[3]] <- Rceattle::fit_mod(
  data_list = mydata_list[[3]],
  inits = mod_list_all[[2]]$estimated_params, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # Estimate
  random_rec = FALSE, # No random recruitment
  msmMode = 1, # Multi species mode
  verbose = 1,
  niter = 3,
  updateM1 = FALSE,
  use_gradient = TRUE,
  phase = "default")

plot_biomass(mod_list_all)
