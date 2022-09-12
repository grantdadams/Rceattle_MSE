
histogram_by_pm <- function(system = "GOA", species = "Pollock", file = NULL, height = 7, width = 6, allHCR = FALSE){
  
  # EMs - Tier 3 NPFMC only
  EM_names <-  c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", # "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", # Fixed M
                 "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM") #, "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM")
  
  EM_names_print <-  c("Fix M: NPFMC", "Est M: NPFMC") # , "Fix M: HCR 2", "Est M: HCR 2", "Fix M: HCR 3", "Est M: HCR 3", "Fix M: HCR 4", "Est M: HCR 4")
  
  recname = c("ConstantR", "AllUp", "AllDown", "ATFRup", "ATFRdown")
  
  recname_print <- c("Experiment 1","Experiment 2","Experiment 3","Experiment 4","Experiment 5")
  
  if(allHCR){
    EM_names <-  c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", # Fixed M
                   "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM")
    
    EM_names_print <-  c("Fix M: HCR 1", "Est M: HCR 1" , "Fix M: HCR 2", "Est M: HCR 2", "Fix M: HCR 3", "Est M: HCR 3", "Fix M: HCR 4", "Est M: HCR 4")
  }
  
  # OM Names
  om_names = c("SS_OM", "SSM_OM", "MS_OM")
  
  pm_names <- c("Catch", "1/(Catch IAV)", "P(Open)", "1/(SSB RMSE)", "EM: P(Not overfishing)", "EM: P(Not overfished)", "OM: P(Not overfishing)", "OM: P(Not overfished)", "1-P(EM Overfishing & OM Underfishing)", "1-P(EM Underfishing & OM Overfishing)", "1-P(EM Overfished & OM Underfished)", "1-P(EM Underfished & OM Overfished)","Depletion")
  
  performance_metrics <- c("Catch", "Catch IAV", "P(Open)", "SSB RMSE", "EM- P(Not overfishing)", "EM- P(Not overfished)", "OM- P(Not overfishing)", "OM- P(Not overfished)", "P(EM Overfishing & OM Underfishing)", "P(EM Underfishing & OM Overfishing)", "P(EM Overfished & OM Underfished)", "P(EM Underfished & OM Overfished)","Terminal Depletion")
  
  # Get output
  OM.res <- list()
  data_list <- list()
  for(rec in 1:length(recname)){
    output_table = pm_summary_table(om_names, EM_names, recname = recname[rec], format = FALSE, reverse = TRUE)
    OM.res[[rec]] = output_table[[system]][which(output_table[[system]]$Species == species),-1]
    rownames(OM.res[[rec]]) <- OM.res[[rec]]$Performance.metric
    OM.res[[rec]] <- OM.res[[rec]][,-1]
    OM.res[[rec]] = t(OM.res[[rec]])
    OM.res[[rec]] <- as.data.frame(OM.res[[rec]])
    
    # Subset based on OM
    data_list[[rec]] <- list(
      # SS OM
      ss_om = OM.res[[rec]][c(1:4),],
      
      # SS-M OM
      ssm_om = OM.res[[rec]][c(5:8),],
      
      # MS OM
      ms_om =  OM.res[[rec]][c(9:12),] 
    )
    
    if(allHCR){
      # Subset based on OM
      data_list[[rec]] <- list(
        # SS OM
        ss_om = OM.res[[rec]][c(1:2, 8:9,3:4,10:11,5:6,12:13,7,14),],
        
        # SS-M OM
        ssm_om = OM.res[[rec]][c(15:16,22:23,17:18,24:25,19:20,26:27,21,28),],
        
        # MS OM
        ms_om =  OM.res[[rec]][c(29:30,36:37,31:32,38:39,33:34,40:41,35,42),] 
      )
    }
  }
  
  # Colors
  MPcols <- gmri_pal("main")(8)
  MPcolsalpha <- alpha(MPcols[1:6], alpha = 0.6)
  point_type = c()
  colors <- c()
  for(i in 1:6){
    colors <- c(colors, MPcols[i], MPcols[i])
    point_type <- c(point_type, 21, 24)
  }
  colors <- c(colors, MPcols[7:8])
  point_type <- c(point_type, 21, 21)
  
  
  # Plot
  for(pm in c(1:8, 13)){
    
    if(!is.null(file)){
      png(filename = paste0(file, system, "_", species, "_", pm, "_", performance_metrics[pm], ".png"), width = width, height = height, units = "in", res = 300)
    }
    
    par(oma=c(0,1.5,0.15,0.1), mar=rep(0,4), mai = c(0,0.3,0.25,0))
    layout(mat = matrix(1:(length(recname)+1), (length(recname)+1), 1, byrow = TRUE),
           heights = c(rep(1,length(recname)), 0.25), # Heights of the two rows
           widths = 1) # Widths of the two columns
    
    
    # Plot ranges
    if(pm%in%c(3,5:8)){
      ylim = c(0,1)
    }else{
      ylim = range(sapply(OM.res, function(x) range(x[,pm])))
    }
    if(pm == 13){
      ylim[1] <- 0
    }
    
    # Rec scenarios
    for(rec in 1:length(recname)){
      plot(NA, NA, ylim = ylim, xlim = c(0.65,3.35), main = recname_print[rec], xaxt = "na", xlab="", ylab="")
      
      for(i in 1:length(data_list[[rec]])){
        points(x = seq(i-0.35, i+ 0.35, length.out = nrow(data_list[[rec]][[i]])), y = data_list[[rec]][[i]][,pm], bg = alpha(colors, alpha = 0.5), pch = point_type, cex = 3)
      }
      
      abline(v = 1.5, col = "grey")
      abline(v = 2.5, col = "grey")
      
      if(rec %in% c(2,4)){
        mtext(pm_names[pm], side = 2, line = 2.5)
      }
      
      if(rec == length(recname)){
        axis(side = 1, at = 1:3, labels = c("SS fix M", "SS est M", "MS"), cex.axis = 1.4)
      }
    }
    
    if(!is.null(file)){
      dev.off() 
    }
  }
}





# Summary table function
pm_summary_table <- function(om_names, em_hcr_names, recname, format = TRUE, reverse = FALSE){
  # Get data we want
  for(om in 1:length(om_names)){  # OM model
    for(em in 1:length(em_hcr_names)){ # EM and HCR
      
      # STEP 1 -- File names
      MSE_names <- paste0(om_names[om],"__", em_hcr_names[em])
      GOA_mse_sum_tmp <- read.csv(file = paste0("Results/Tables/GOA1977/GOA1977", "_", recname, "_Table", MSE_names, "_", recname,".csv"))[,-1] # May need to add "_" after table for later iterations
      EBS_mse_sum_tmp <- read.csv(file = paste0("Results/Tables/EBS/EBS", "_", recname, "_Table", MSE_names, "_", recname,".csv"))[,-1]
      
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

