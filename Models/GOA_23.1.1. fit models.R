library(Rceattle)
library(readxl)
library(dplyr)

combined_data <- read_data(file = "Data/GOA_23_1_1_data_1977_2023_edited.xlsx")
combined_data$endyr <- 2020

# - Est single-species fixed M
ss_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            verbose = 1,
                            phase = "default",
                            initMode = 1)



# - Est single-species estimate M
ssm <- Rceattle::fit_mod(data_list = combined_data,
                         inits = NULL, # Initial parameters = 0
                         file = NULL, # Don't save
                         estimateMode = 0, # Estimate
                         random_rec = FALSE, # No random recruitment
                         msmMode = 0, # Single species mode
                         verbose = 1,
                         phase = "default",
                         M1Fun = build_M1(M1_model = c(1,2,1),
                                          M1_use_prior = FALSE,
                                          M2_use_prior = FALSE),
                         initMode = 1)


# - Est multi-species
inits <- ss_mod$estimated_params
ms_mod <- Rceattle::fit_mod(data_list = combined_data,
                            inits = inits, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 0, # Estimate
                            random_rec = FALSE, # No random recruitment
                            msmMode = 1, # Multi species mode
                            verbose = 1,
                            suit_meanyr = 2018,
                            phase = "default",
                            M1Fun = build_M1(M1_model = c(1,2,1),
                                             M1_use_prior = FALSE,
                                             M2_use_prior = FALSE),
                            initMode = 1)


# Phase in predation if doesnt converge
if( class(ms_mod) == "try-error" ){
  
  fday_vec <- seq(0.5,1, by = 0.1)
  
  for(j in 1:length(fday_vec)){
    my_data_tmp <- data
    my_data_tmp$fday <- replace(my_data_tmp$fday, values = rep(fday_vec[j], length(my_data_tmp$fday))) # Set foraging days to half
    
    if(j > 1){
      inits <- ms_mod$estimated_params
    }
    
    # Re-estimate
    ms_mod <- Rceattle::fit_mod(
      data_list = my_data_tmp,
      inits = inits, # Initial parameters = 0
      file = NULL, # Don't save
      estimateMode = 0, # Estimate
      M1Fun = build_M1(M1_model = c(1,2,1),
                       M1_use_prior = FALSE,
                       M2_use_prior = FALSE),
      random_rec = FALSE, # No random recruitment
      msmMode = 1, # Multi species mode
      suit_meanyr = ms_mod$data_list$suit_meanyr,
      verbose = 1,
      initMode = 1,
      phase = NULL,
      initMode = 1)
  }
}


# If Hessian cant invert or is discontinuous - PHASE
if( is.null(ms_mod$opt$objective)){
  ms_mod <- try( Rceattle::fit_mod(
    data_list = data,
    inits = inits, # Initial parameters = 0
    file = NULL, # Don't save
    estimateMode = 0, # Estimate
    M1Fun = build_M1(M1_model = c(1,2,1),
                     M1_use_prior = FALSE,
                     M2_use_prior = FALSE),
    random_rec = FALSE, # No random recruitment
    msmMode = 1, # Multi species mode
    suit_meanyr = ms_mod$data_list$suit_meanyr,
    verbose = 1,
    phase = "default",
    initMode = 1),
    silent = TRUE)
}

# Discontinuous ll
if(!is.null(ms_mod$opt$objective)){
  if(abs(ms_mod$opt$objective -  ms_mod$quantities$jnll) > 1 ){
    ms_mod <- try( Rceattle::fit_mod(
      data_list = data,
      inits = ms_mod$estimated_params, # Initial parameters = 0
      file = NULL, # Don't save
      M1Fun = build_M1(M1_model = c(1,2,1),
                       M1_use_prior = FALSE,
                       M2_use_prior = FALSE),
      estimateMode = 0, # Estimate
      random_rec = FALSE, # No random recruitment
      msmMode = 1, # Multi species mode
      suit_meanyr = ms_mod$data_list$suit_meanyr,
      verbose = 1,
      initMode = 1,
      phase = "default"),
      silent = TRUE)
  }
} 

# - Plot
mod_list_all <- list(ss_mod, ssm, ms_mod)
plot_biomass(mod_list_all)
plot_b_eaten(mod_list_all)
plot_recruitment(mod_list_all)

# - Save
save(mod_list_all, file = "Models/GOA_20_1_1_mod_list.RData")