
# File names
om_names = c("SS_OM", "SSM_OM", "MS_OM")

em_hcr_names <- c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", "SS_fixM_AvgF_EM", # Fixed M
                  "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM", "SS_estM_AvgF_EM")



# Save table 6 - Tier 3
EM_names <- c("SS_fixM_Tier3_EM", "SS_estM_Tier3_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/Table6a_EBS_Tier3_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/Table6b_GOA_Tier3_summary.csv"))


# Save supp table 2
EM_names <- c("SS_fixM_dynamicTier3_EM", "SS_estM_dynamicTier3_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS2a_EBS_Tier3_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS2b_GOA_Tier3_summary.csv"))


# Save supp table 3 - Cat1
EM_names <- c("SS_fixM_Cat1_EM", "SS_estM_Cat1_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS3a_EBS_Tier3_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS3b_GOA_Tier3_summary.csv"))

EM_names <- c("SS_fixM_dynamicCat1_EM", "SS_estM_dynamicCat1_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS3c_EBS_Tier3_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS3d_GOA_Tier3_summary.csv"))


# Save supp table 4 - SESSF
EM_names <- c("SS_fixM_Tier1_EM", "SS_estM_Tier1_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS4a_EBS_Tier3_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS4b_GOA_Tier3_summary.csv"))

EM_names <- c("SS_fixM_dynamicTier1_EM", "SS_estM_dynamicTier1_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS4c_EBS_Tier3_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS4d_GOA_Tier3_summary.csv"))


# Save supp table 5 - F40%
EM_names <- c("SS_fixM_Fspr_EM", "SS_estM_Fspr_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS5a_EBS_Tier3_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS5b_GOA_Tier3_summary.csv"))


# Save supp table 6 - AvgF
EM_names <- c("SS_fixM_AvgF_EM", "SS_estM_AvgF_EM")
output_table = pm_summary_table(om_names, EM_names)
write.csv(output_table$EBS, file = paste0("Results/Tables/TableS6a_EBS_Tier3_summary.csv"))
write.csv(output_table$GOA, file = paste0("Results/Tables/TableS6b_GOA_Tier3_summary.csv"))


# Summary table function
pm_summary_table <- function(om_names, em_hcr_names, format = TRUE, reverse = FALSE){
  # Get data we want
  for(om in 1:length(om_names)){  # OM model
    for(em in 1:length(em_hcr_names)){ # EM and HCR
      
      # STEP 1 -- File names
      MSE_names <- paste0(om_names[om],"__", em_hcr_names[em])
      GOA_mse_sum_tmp <- read.csv(file = paste0("Results/Tables/GOA/GOA_table", MSE_names,".csv"))[,-1] # May need to add "_" after table for later iterations
      EBS_mse_sum_tmp <- read.csv(file = paste0("Results/Tables/EBS/EBS_table", MSE_names,".csv"))[,-1]
      
      if(om * em == 1){
        GOA_mse_sum = GOA_mse_sum_tmp
        EBS_mse_sum = EBS_mse_sum_tmp
      } else {
        GOA_mse_sum = cbind(GOA_mse_sum, GOA_mse_sum_tmp[,3])
        colnames(GOA_mse_sum)[ncol(GOA_mse_sum)] <- colnames(GOA_mse_sum_tmp)[3]
        
        EBS_mse_sum = cbind(EBS_mse_sum, EBS_mse_sum_tmp[,3])
        colnames(EBS_mse_sum)[ncol(EBS_mse_sum)] <- colnames(EBS_mse_sum_tmp)[3]
      }
    }
  }
  
  # Make % to prob - 
  #FIXME - remove later
  reverse_percentage <- c("% Years closed")
  
  row_id <- which(GOA_mse_sum$Performance.metric %in% reverse_percentage)
  
  EBS_mse_sum[row_id, 3:ncol(EBS_mse_sum)] <- EBS_mse_sum[row_id, 3:ncol(EBS_mse_sum)]/100
  GOA_mse_sum[row_id, 3:ncol(GOA_mse_sum)] <- GOA_mse_sum[row_id, 3:ncol(GOA_mse_sum)]/100
  
  if(reverse){
    # Make larger number better
    reverse_percentage <- c("% Years closed",
                            "EM: P(Fy > Flimit)",
                            "EM: P(SSB < SSBlimit)",
                            "OM: P(Fy > Flimit)",
                            "OM: P(SSB < SSBlimit)",
                            "EM: P(Fy > Flimit) but OM: P(Fy < Flimit)",
                            "EM: P(Fy < Flimit) but OM: P(Fy > Flimit)",
                            "EM: P(SSB < SSBlimit) but OM: P(SSB > SSBlimit)",
                            "EM: P(SSB > SSBlimit) but OM: P(SSB < SSBlimit)")
    
    row_id <- which(GOA_mse_sum$Performance.metric %in% reverse_percentage)
    
    EBS_mse_sum[row_id, 3:ncol(EBS_mse_sum)] <- 1-EBS_mse_sum[row_id, 3:ncol(EBS_mse_sum)]
    GOA_mse_sum[row_id, 3:ncol(GOA_mse_sum)] <- 1-GOA_mse_sum[row_id, 3:ncol(GOA_mse_sum)]
    
    # Inverse
    inverse_pm <- c("Avg terminal SSB MSE", "Catch IAV")
    row_id <- which(GOA_mse_sum$Performance.metric %in% inverse_pm)
    
    EBS_mse_sum[row_id, 3:ncol(EBS_mse_sum)] <- 1/EBS_mse_sum[row_id, 3:ncol(EBS_mse_sum)]
    GOA_mse_sum[row_id, 3:ncol(GOA_mse_sum)] <- 1/GOA_mse_sum[row_id, 3:ncol(GOA_mse_sum)]
  }
  
  
  
  # Format tables
  if(format){
    # - Percentages
    percent_form <- c("% Years closed",
                      "EM: P(Fy > Flimit)",
                      "EM: P(SSB < SSBlimit)",
                      "OM: P(Fy > Flimit)",
                      "OM: P(SSB < SSBlimit)",
                      "EM: P(Fy > Flimit) but OM: P(Fy < Flimit)",
                      "EM: P(Fy < Flimit) but OM: P(Fy > Flimit)",
                      "EM: P(SSB < SSBlimit) but OM: P(SSB > SSBlimit)",
                      "EM: P(SSB > SSBlimit) but OM: P(SSB < SSBlimit)",
                      # "OM: Recovery Time",
                      "OM: Terminal SSB/SSBtarget",
                      "Avg terminal SSB MSE") # May need to make to relative in future iterations
    
    row_id <- which(GOA_mse_sum$Performance.metric %in% percent_form)
    
    EBS_mse_sum[row_id, 3:ncol(EBS_mse_sum)] <- round(EBS_mse_sum[row_id, 3:ncol(EBS_mse_sum)], 2)
    GOA_mse_sum[row_id, 3:ncol(GOA_mse_sum)] <- round(GOA_mse_sum[row_id, 3:ncol(GOA_mse_sum)], 2)
    
    # - Large numbers
    sci_form <- c("Average Catch",
                  "Catch IAV")
    row_id <- which(GOA_mse_sum$Performance.metric %in% sci_form)
    
    EBS_mse_sum[row_id, 3:ncol(EBS_mse_sum)] <- format(round(EBS_mse_sum[row_id, 3:ncol(EBS_mse_sum)], 0), nsmall=0, big.mark=",")
    GOA_mse_sum[row_id, 3:ncol(GOA_mse_sum)] <- format(round(GOA_mse_sum[row_id, 3:ncol(GOA_mse_sum)], 0), nsmall=0, big.mark=",")
  }
  
  # Swap cod and ATF for the GOA
  row_id <- which(GOA_mse_sum$Species == "Arrowtooth flounder")
  atf_sub <- GOA_mse_sum[row_id,]
  GOA_mse_sum <- GOA_mse_sum[-row_id,]
  GOA_mse_sum <- rbind(GOA_mse_sum, atf_sub)
  
  return(list(EBS = EBS_mse_sum, GOA = GOA_mse_sum))
}
