
# Summary table function
climate_pm_summary_table <- function(om_names, em_hcr_names, cap = FALSE, regen = FALSE, format = TRUE, reverse = FALSE){
  
  # - Get data we want
  for(om in 1:length(om_names)){  # OM model
    for(em in 1:length(em_hcr_names)){ # EM and HCR
      for(c in 1:length(cap)){
        
        # STEP 1 -- File names
        MSE_names <- paste0(om_names[om],"__", em_hcr_names[em], "_", regen,"regen",  "_", cap[c], "cap")
        
        GOA_mse_sum_tmp <- read.csv(file = paste0("Results/Climate MSE/Tables/GOA_Climate_2/GOA_Climate_2_table", MSE_names,".csv"))[,-1] # May need to add "_" after table for later iterations
        colnames(GOA_mse_sum_tmp) = c("Species", "Performance.metric", "Value", "OM", "EM", "Cap", "Nsim")

        GOA_mse_sum_tmp <- GOA_mse_sum_tmp %>%
          mutate(Value = ifelse(Nsim < 10, NA, Value))
        
        if(om * em * c == 1){
          GOA_mse_sum = GOA_mse_sum_tmp
        } else {
          GOA_mse_sum = rbind(GOA_mse_sum, GOA_mse_sum_tmp)
        }
      }
    }
  }
  
  # P-Closed to P-Open
  reverse_percentage <- c("P(Closed)")
  row_id <- which(GOA_mse_sum$Performance.metric %in% reverse_percentage)
  GOA_mse_sum[row_id, "Value"] <- 1-GOA_mse_sum[row_id, "Value"]
  
  
  if(reverse){
    # Make larger number better
    reverse_percentage <- c(
      "Catch IAV",
      "Catch IAV","EM: P(Fy > Flimit)",
      "EM: P(SSB < SSBlimit)",
      "OM: P(Fy > Flimit)",
      "OM: P(SSB < SSBlimit)",
      "EM: P(Fy > Flimit) but OM: P(Fy < Flimit)",
      "EM: P(Fy < Flimit) but OM: P(Fy > Flimit)",
      "EM: P(SSB < SSBlimit) but OM: P(SSB > SSBlimit)",
      "EM: P(SSB > SSBlimit) but OM: P(SSB < SSBlimit)")
    row_id <- which(GOA_mse_sum$Performance.metric %in% reverse_percentage)
    
    GOA_mse_sum[row_id, "Value"] <- 1-GOA_mse_sum[row_id, "Value"]
    
    # Inverse
    inverse_pm <- c("Avg terminal SSB MSE")
    row_id <- which(GOA_mse_sum$Performance.metric %in% inverse_pm)
    
    GOA_mse_sum[row_id, "Value"] <- 1/GOA_mse_sum[row_id, "Value"]
  }
  
  
  # Format tables
  if(format){
    # - Percentages
    percent_form <- c("P(Closed)",
                      "Catch IAV",
                      "EM: P(Fy > Flimit)",
                      "EM: P(SSB < SSBlimit)",
                      "OM: P(Fy > Flimit)",
                      "OM: P(SSB < SSBlimit)",
                      "EM: P(Fy > Flimit) but OM: P(Fy < Flimit)",
                      "EM: P(Fy < Flimit) but OM: P(Fy > Flimit)",
                      "EM: P(SSB < SSBlimit) but OM: P(SSB > SSBlimit)",
                      "EM: P(SSB > SSBlimit) but OM: P(SSB < SSBlimit)",
                      # "OM: Recovery Time",
                      "OM: Terminal SSB Depletion",
                      "OM: Terminal SSB Depletion (Dynamic)",
                      "Avg terminal SSB Relative MSE") # May need to make to relative in future iterations
    
    row_id <- which(GOA_mse_sum$Performance.metric %in% percent_form)
    
    GOA_mse_sum[row_id, "Value"] <- round(GOA_mse_sum[row_id, "Value"], 2)
    
    # - Large numbers
    sci_form <- c("Average Catch",
                  "OM: Terminal B",
                  "OM: Terminal SSB",
                  "OM: Terminal Dynamic SB0")
    row_id <- which(GOA_mse_sum$Performance.metric %in% sci_form)
    
    GOA_mse_sum[row_id, "Value"] <- format(round(GOA_mse_sum[row_id, "Value"]/1000, 0), nsmall=0, big.mark=",")
  }
  
  # Swap cod and ATF for the GOA
  row_id <- which(GOA_mse_sum$Species == "Arrowtooth flounder")
  atf_sub <- GOA_mse_sum[row_id,]
  GOA_mse_sum <- GOA_mse_sum[-row_id,]
  GOA_mse_sum <- rbind(GOA_mse_sum, atf_sub)
  
  return(GOA = GOA_mse_sum)
}




# make all PMs relative to maximum and minimum values
normalize <- function(x) {
  return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
}

