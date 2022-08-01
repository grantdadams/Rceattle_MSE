library(Rceattle)
library(readxl)


######################### 
# Read in data
#########################
mydata_short <- Rceattle::read_data( file = "Models/GOA_18_5_1_data_1977-2018_no_halibut.xlsx")
mydata_short$styr <- 1977
mydata_short$projyr <- 2060

# Set up model list
# The two short term models for 1996 to 2018 are: 
# •	Model 1: a model that does not include predation to represent a base single-species model. M is fixed (Model 12 from Adams et al)
# •	Model 2: a model that does not include predation to represent a base single-species model. M is estimated
# •	Model 3: a mutlispecies model that did not include halibut predation (Model 13 from Adams et al). 

mydata_list <- list(
  # Short time series 1996-2018
  mydata_short, # 1 - single-species SAFE M 
  mydata_short, # 2 - single-species Estimated M 
  mydata_short # 3 - Multi-species no hal - Est M1
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
for(i in 1:length(mydata_list)){
  if(inits_M1_df$MsmMode[i] == 0){
    mod_list_all[[i]] <- Rceattle::fit_mod(data_list = mydata_list[[i]],
                                           inits = NULL, # Initial parameters = 0
                                           file = NULL, # Don't save
                                           estimateMode = 0, # Estimate
                                           random_rec = FALSE, # No random recruitment
                                           msmMode = 0, # Single species mode
                                           verbose = 1,
                                           phase = "default")
  }
}

mod_list_unweighted <- mod_list_all[which(inits_M1_df$MsmMode == 0)]

# Reweight the single species Cod model
for(i in 1:length(mydata_list)){
  if(inits_M1_df$MsmMode[i] == 0){
    
    data <- mydata_list[[i]]
    subs <- which(data$fleet_control$Species == 3) # Species 3 is cod
    data$fleet_control$Comp_weights[subs] <- mod_list_all[[i]]$data_list$fleet_control$Est_weights_mcallister[subs]
    
    inits = mod_list_all[[i]]$estimated_params
    inits$comp_weights[subs] <- data$fleet_control$Comp_weights[subs]
    
    # Refit
    mod_list_all[[i]] <- Rceattle::fit_mod(data_list = data,
                                           inits = inits, # Initial parameters = 0
                                           file = NULL, # Don't save
                                           estimateMode = 0, # Estimate
                                           random_rec = FALSE, # No random recruitment
                                           msmMode = 0, # Single species mode
                                           verbose = 1,
                                           phase = "default")
  }
}


################################################
# Model 2 - Run multi-species
################################################
# - Run models
inits_M1_df$Divergent_jnll <- NA
for(i in 1:length(mydata_list)){
  if(inits_M1_df$MsmMode[i] == 1){
    if(is.na(inits_M1_df$Divergent_jnll[i])){
      
      # Set up inits
      init_model <- inits_M1_df$InitModel[i]
      inits = mod_list_all[[init_model]]$estimated_params
      
      # Fit model
      mod_list_all[[i]] <- try( Rceattle::fit_mod(
        data_list = data,
        inits = inits, # Initial parameters = 0
        file = NULL, # Don't save
        estimateMode = 0, # Estimate
        random_rec = FALSE, # No random recruitment
        msmMode = 1, # Multi species mode
        verbose = 1,
        phase = NULL),
        silent = TRUE)
      
      
      # Phase in predation if doesnt converge
      if( class(mod_list_all[[i]]) == "try-error" ){
        
        fday_vec <- seq(0.5,1, by = 0.1)
        
        for(j in 1:length(fday_vec)){
          my_data_tmp <- data
          my_data_tmp$fday <- replace(my_data_tmp$fday, values = rep(fday_vec[j], length(my_data_tmp$fday))) # Set foraging days to half
          
          if(j > 1){
            inits <- mod_list_all[[i]]$estimated_params
          }
          
          # Re-estimate
          mod_list_all[[i]] <- Rceattle::fit_mod(
            data_list = my_data_tmp,
            inits = inits, # Initial parameters = 0
            file = NULL, # Don't save
            estimateMode = 0, # Estimate
            random_rec = FALSE, # No random recruitment
            msmMode = 1, # Multi species mode
            verbose = 1,
            phase = NULL)
        }
      }
      
      
      # If Hessian cant invert or is discontinuous - PHASE
      if( is.null(mod_list_all[[i]]$opt$objective)){
        mod_list_all[[i]] <- try( Rceattle::fit_mod(
          data_list = data,
          inits = inits, # Initial parameters = 0
          file = NULL, # Don't save
          estimateMode = 0, # Estimate
          random_rec = FALSE, # No random recruitment
          msmMode = 1, # Multi species mode
          verbose = 1,
          phase = "default"),
          silent = TRUE)
      }
      
      # Discontinuous ll
      if(!is.null(mod_list_all[[i]]$opt$objective)){
        if(abs(mod_list_all[[i]]$opt$objective -  mod_list_all[[i]]$quantities$jnll) > 1 ){
          mod_list_all[[i]] <- try( Rceattle::fit_mod(
            data_list = data,
            inits = mod_list_all[[i]]$estimated_params, # Initial parameters = 0
            file = NULL, # Don't save
            estimateMode = 0, # Estimate
            random_rec = FALSE, # No random recruitment
            msmMode = 1, # Multi species mode
            verbose = 1,
            phase = "default"),
            silent = TRUE)
        }
      } 
    }
  }
}


# Check convergence
inits_M1_df$Objective <- sapply(mod_list_all, function(x) x$opt$objective)
inits_M1_df$Divergent_jnll <- NA
for(i in 1:length(mod_list_all)){
  if(!is.null(mod_list_all[[i]]$opt$objective)){
    inits_M1_df$Divergent_jnll[i] <- round(mod_list_all[[i]]$quantities$jnll - mod_list_all[[i]]$opt$objective,3)
  }
}

mod_list_all_save <- mod_list_all


# Save
save(mod_list_all, file = "Models/GOA_18_5_1_mod_1-2_2022-05-26.RData")
