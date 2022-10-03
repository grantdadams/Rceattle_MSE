
histogram_by_pm <- function(system = "GOA", species = "Pollock", file = NULL, height = 7, width = 6, allHCR = FALSE){
  
  # EMs - Tier 3 NPFMC only
  EM_names <-  c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", # "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", # Fixed M
                 "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM") #, "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM")
  
  EM_names_print <-  c("Fix M: NPFMC", "Est M: NPFMC") # , "Fix M: HCR 2", "Est M: HCR 2", "Fix M: HCR 3", "Est M: HCR 3", "Fix M: HCR 4", "Est M: HCR 4")
  
  recname = c("ConstantR", "AllUp", "AllDown", "ATFRup", "ATFRdown")
  
  recname_print <- c("Experiment 1","Experiment 2","Experiment 3","Experiment 4","Experiment 5")
  
  if(allHCR){
    EM_names <-  c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", "SS_fixM_AvgF_EM", # Fixed M
                   "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM", "SS_estM_AvgF_EM")
    
    EM_names_print <-  c("Fix M: HCR 1", "Est M: HCR 1" , "Fix M: HCR 2", "Est M: HCR 2", "Fix M: HCR 3", "Est M: HCR 3", "Fix M: HCR 4", "Est M: HCR 4", "Fix M: HCR 5", "Est M: HCR 5")
  }
  
  # OM Names
  om_names = c("SS_OM", "SSM_OM", "MS_OM")
  
  pm_names <- c("Catch", "Catch IAV", "P(Closed)", "1/(SSB RMSE)", "EM: P(Overfishing)", "EM: P(overfished)", "OM: P(Overfishing)", "OM: P(Overfished)", "1-P(EM Overfishing & OM Underfishing)", "1-P(EM Underfishing & OM Overfishing)", "1-P(EM Overfished & OM Underfished)", "1-P(EM Underfished & OM Overfished)","Depletion")
  
  performance_metrics <- c("Catch", "Catch IAV", "P(Closed)", "SSB RMSE", "EM- P(Overfishing)", "EM- P(overfished)", "OM- P(Overfishing)", "OM- P(Overfished)", "P(EM Overfishing & OM Underfishing)", "P(EM Underfishing & OM Overfishing)", "P(EM Overfished & OM Underfished)", "P(EM Underfished & OM Overfished)","Terminal Depletion")
  
  # Get output
  OM.res <- list()
  data_list <- list()
  for(rec in 1:length(recname)){
    output_table = pm_summary_table(om_names, EM_names, recname = recname[rec], format = FALSE, reverse = FALSE)
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
      
      em_order <- c(1:2, 9:10,3:4,11:12,5:6,13:14,7,15,8, 16)
      data_list[[rec]] <- list(
        
        # SS OM
        ss_om = OM.res[[rec]][em_order,],
        
        # SS-M OM
        ssm_om = OM.res[[rec]][em_order+16,],
        
        # MS OM
        ms_om =  OM.res[[rec]][em_order+32,] 
      )
    }
  }
  
  # Colors
  MPcols <- gmri_pal("main")(10)
  MPcolsalpha <- alpha(MPcols[1:6], alpha = 0.6)
  point_type = c()
  colors <- c()
  for(i in 1:6){
    colors <- c(colors, MPcols[i], MPcols[i])
    point_type <- c(point_type, 21, 24)
  }
  colors <- c(colors, MPcols[7:10])
  point_type <- c(point_type, 21, 21, 21, 21)
  
  
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
      
      if(pm == 13){
        abline(h = 0.4, col = "blue", lty = 2)
        #abline(h = 0.2, col = "red", lty = 2)
      }
      
      for(i in 1:length(data_list[[rec]])){
        points(x = seq(i-0.4, i+ 0.4, length.out = nrow(data_list[[rec]][[i]])), y = data_list[[rec]][[i]][,pm], bg = alpha(colors, alpha = 0.5), pch = point_type, cex = 3)
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



