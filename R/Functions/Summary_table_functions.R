
# Summary table function
pm_summary_table <- function(om_names, em_hcr_names, format = TRUE, reverse = FALSE){
  
  # - Get data we want
  for(om in 1:length(om_names)){  # OM model
    for(em in 1:length(em_hcr_names)){ # EM and HCR
      
      # STEP 1 -- File names
      MSE_names <- paste0(om_names[om],"__", em_hcr_names[em])
      
      GOA_mse_sum_tmp <- read.csv(file = paste0("Results/Tables/GOA1977/GOA1977", "_Table", MSE_names,".csv"))[,-1] # May need to add "_" after table for later iterations
      EBS_mse_sum_tmp <- read.csv(file = paste0("Results/Tables/EBS/EBS", "_Table", MSE_names,".csv"))[,-1]
      colnames(GOA_mse_sum_tmp) = c("Species", "Performance.metric", "Value")
      colnames(EBS_mse_sum_tmp) = c("Species", "Performance.metric", "Value")
      
      # - Add info
      data_info <- data.frame(OM = rep(om_names[om], nrow(GOA_mse_sum_tmp)), EM = rep(em_hcr_names[em], nrow(GOA_mse_sum_tmp)))
      GOA_mse_sum_tmp <- cbind(data_info, GOA_mse_sum_tmp)
      EBS_mse_sum_tmp <- cbind(data_info, EBS_mse_sum_tmp)
      
      if(om * em == 1){
        GOA_mse_sum = GOA_mse_sum_tmp
        EBS_mse_sum = EBS_mse_sum_tmp
      } else {
        GOA_mse_sum = rbind(GOA_mse_sum, GOA_mse_sum_tmp)
        
        EBS_mse_sum = rbind(EBS_mse_sum, EBS_mse_sum_tmp)
      }
    }
  }
  
  
  if(reverse){
    # Make larger number better
    reverse_percentage <- c("P(Closed)",
                            "EM: P(Fy > Flimit)",
                            "EM: P(SSB < SSBlimit)",
                            "OM: P(Fy > Flimit)",
                            "OM: P(SSB < SSBlimit)",
                            "EM: P(Fy > Flimit) but OM: P(Fy < Flimit)",
                            "EM: P(Fy < Flimit) but OM: P(Fy > Flimit)",
                            "EM: P(SSB < SSBlimit) but OM: P(SSB > SSBlimit)",
                            "EM: P(SSB > SSBlimit) but OM: P(SSB < SSBlimit)")
    
    row_id <- which(GOA_mse_sum$Performance.metric %in% reverse_percentage)
    
    EBS_mse_sum[row_id, "Value"] <- 1-EBS_mse_sum[row_id, "Value"]
    GOA_mse_sum[row_id, "Value"] <- 1-GOA_mse_sum[row_id, "Value"]
    
    # Inverse
    inverse_pm <- c("Avg terminal SSB MSE", "Catch IAV")
    row_id <- which(GOA_mse_sum$Performance.metric %in% inverse_pm)
    
    EBS_mse_sum[row_id, "Value"] <- 1/EBS_mse_sum[row_id, "Value"]
    GOA_mse_sum[row_id, "Value"] <- 1/GOA_mse_sum[row_id, "Value"]
  }
  
  
  # Format tables
  if(format){
    # - Percentages
    percent_form <- c("P(Closed)",
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
                      "Avg terminal SSB Relative MSE") # May need to make to relative in future iterations
    
    row_id <- which(GOA_mse_sum$Performance.metric %in% percent_form)
    
    EBS_mse_sum[row_id, "Value"] <- round(EBS_mse_sum[row_id, "Value"], 2)
    GOA_mse_sum[row_id, "Value"] <- round(GOA_mse_sum[row_id, "Value"], 2)
    
    # - Large numbers
    sci_form <- c("Average Catch",
                  "Catch IAV")
    row_id <- which(GOA_mse_sum$Performance.metric %in% sci_form)
    
    EBS_mse_sum[row_id, "Value"] <- format(round(EBS_mse_sum[row_id, "Value"], 0), nsmall=0, big.mark=",")
    GOA_mse_sum[row_id, "Value"] <- format(round(GOA_mse_sum[row_id, "Value"], 0), nsmall=0, big.mark=",")
  }
  
  # Swap cod and ATF for the GOA
  row_id <- which(GOA_mse_sum$Species == "Arrowtooth flounder")
  atf_sub <- GOA_mse_sum[row_id,]
  GOA_mse_sum <- GOA_mse_sum[-row_id,]
  GOA_mse_sum <- rbind(GOA_mse_sum, atf_sub)
  
  return(list(EBS = EBS_mse_sum, GOA = GOA_mse_sum))
}




# Summary table function
m_summary_table <- function(om_names = c("SSM_OM"), em_hcr_names = c("SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM")){
  # Get data we want
  for(om in 1:length(om_names)){  # OM model
    for(em in 1:length(em_hcr_names)){ # EM and HCR
      
      # STEP 1 -- File names
      MSE_names <- paste0(om_names[om],"__", em_hcr_names[em])
      
      GOA_mse_sum_tmp <- na.omit(read.csv(file = paste0("Results/Tables/Avg M/GOA1977/Avg MGOA1977", "_Table", MSE_names,".csv")))[,-1] # May need to add "_" after table for later iterations
      GOA_mse_sum_tmp$OM <- om_names[om]
      GOA_mse_sum_tmp$EM <- em_hcr_names[em]
      GOA_mse_sum_tmp$Spp <- c("Pollock", "ATF F", "ATF M", "Cod")
      
      # EBS_mse_sum_tmp <- na.omit(read.csv(file = paste0("Results/Tables/Avg M/EBS/Avg MEBS", "_", recnames[rec], "_Table", MSE_names, "_", recnames[rec],".csv")))[,-1]
      # EBS_mse_sum_tmp$OM <- om_names[om]
      # EBS_mse_sum_tmp$EM <- em_hcr_names[em]
      # EBS_mse_sum_tmp$Rec <- recnames[rec]
      # EBS_mse_sum_tmp$Spp <- c("Pollock", "ATF", "Cod")
      
      if(om * em * rec == 1){
        GOA_mse_sum = GOA_mse_sum_tmp
        # EBS_mse_sum = EBS_mse_sum_tmp
      } else {
        GOA_mse_sum = rbind(GOA_mse_sum, GOA_mse_sum_tmp)
        
        # EBS_mse_sum = cbind(EBS_mse_sum, EBS_mse_sum_tmp)
      }
    }
  }
  
  return(list(GOA = GOA_mse_sum)) # EBS = EBS_mse_sum, 
}




# make all PMs relative to maximum and minimum values
normalize <- function(x) {
  return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
}

