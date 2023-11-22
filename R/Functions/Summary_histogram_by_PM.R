
histogram_by_pm <- function(system = "GOA", species = "Pollock", file = NULL, height = 7, width = 6, allHCR = FALSE){
  
  # OM and EM names ----
  recname = c("ConstantR")
  om_names = c("SS_OM", "SSM_OM", "MS_OM", "SS_Ricker_OM", "SSM_Ricker_OM", "MS_Ricker_OM")
  
  # - EMs - Tier 3 NPFMC only
  EM_names <-  c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", # "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", # Fixed M
                 "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM") #, "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM")
  
  EM_names_print <-  c("Fix M: NPFMC", "Est M: NPFMC") # , "Fix M: HCR 2", "Est M: HCR 2", "Fix M: HCR 3", "Est M: HCR 3", "Fix M: HCR 4", "Est M: HCR 4")
  
  # - All EMs
  if(allHCR){
    EM_names <-  c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", "SS_fixM_AvgF_EM", # Fixed M
                   "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM", "SS_estM_AvgF_EM")
    
    EM_names_print <-  c("Fix M: HCR 1", "Est M: HCR 1" , "Fix M: HCR 2", "Est M: HCR 2", "Fix M: HCR 3", "Est M: HCR 3", "Fix M: HCR 4", "Est M: HCR 4", "Fix M: HCR 5", "Est M: HCR 5")
  }
  
  # - PM names
  pm_names <- c("Catch", "Catch IAV", "P(Closed)", "1/(SSB RMSE)", "EM: P(Overfishing)", "EM: P(overfished)", "OM: P(Overfishing)", "OM: P(Overfished)", "1-P(EM Overfishing & OM Underfishing)", "1-P(EM Underfishing & OM Overfishing)", "1-P(EM Overfished & OM Underfished)", "1-P(EM Underfished & OM Overfished)","Depletion")
  
  performance_metrics <- c("Catch", "Catch IAV", "P(Closed)", "SSB RMSE", "EM- P(Overfishing)", "EM- P(overfished)", "OM- P(Overfishing)", "OM- P(Overfished)", "P(EM Overfishing & OM Underfishing)", "P(EM Underfishing & OM Overfishing)", "P(EM Overfished & OM Underfished)", "P(EM Underfished & OM Overfished)","Terminal Depletion")
  
  
  # Get output ----
  output_table = pm_summary_table(om_names, EM_names, recname = recname, format = FALSE, reverse = FALSE)
  OM.res = output_table[[system]][which(output_table[[system]]$Species == species),-1]
  rownames(OM.res) <- OM.res$Performance.metric
  OM.res <- OM.res[,-1]
  OM.res = t(OM.res)
  OM.res <- as.data.frame(OM.res)
  
  # Subset based on OM
  data_list <- list(
    # SS OM
    ss_om = OM.res[c(1:4),],
    
    # SS Ricker OM
    ss_om_ricker = OM.res[c(13:16),],
    
    # SS-M OM
    ssm_om = OM.res[c(5:8),],
    
    # SS-M Ricker OM
    ssm_om_ricker = OM.res[c(17:20),],
    
    # MS OM
    ms_om =  OM.res[c(9:12),],
    
    # MS Ricker OM
    ms_om_ricker =  OM.res[c(21:24),] 
  )
  
  if(allHCR){
    # Subset based on OM
    
    em_order <- c(1:2, 9:10,3:4,11:12,5:6,13:14,7,15, 8, 16)
    data_list <- list(
      
      # SS OM
      ss_om = OM.res[em_order,],
      
      # SS Ricker OM
      ss_om_ricker = OM.res[em_order+16*3,],
      
      # SS-M OM
      ssm_om = OM.res[em_order+16,],
      
      # SS-M Ricker OM
      ssm_om_ricker = OM.res[em_order+16*4,],
      
      # MS OM
      ms_om =  OM.res[em_order+16*2,],
      
      # MS Ricker OM
      ms_om_ricker =  OM.res[em_order+16*5,] 
    )
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
    
    par(oma=c(0,2,0.15,0.1), mar=c(0,2,0,0), mai = c(0,0.3,0.25,0))
    layout(mat = matrix(1:(length(recname)+1), (length(recname)+1), 1, byrow = TRUE),
           heights = c(rep(1,length(recname)), 0.25), # Heights of the two rows
           widths = 1) # Widths of the two columns
    
    
    # - Plot ranges
    if(pm%in%c(3,5:8)){ylim = c(0,1)}else{
      ylim = range(OM.res[,pm], na.rm = TRUE)
    }
    if(pm == 13){
      ylim[1] <- 0
    }
    
    # - Plot
    plot(NA, NA, ylim = ylim, xlim = c(0.65,6.35), xaxt = "na", xlab="", ylab="")
    
    if(pm == 13){abline(h = 0.4, col = "blue", lty = 2)}
    
    for(i in 1:length(data_list)){
      points(x = seq(i-0.4, i+ 0.4, length.out = nrow(data_list[[i]])), y = data_list[[i]][,pm], bg = alpha(colors, alpha = 0.5), pch = point_type, cex = 3)
    }
    
    abline(v = 1:5 + 0.5, col = "grey")
    
    # - Legends
    mtext(pm_names[pm], side = 2, line = 2.5)
    axis(side = 1, at = 1:6, labels = c("SS fix M", "w/ Ricker", "SS est M", "w/ Ricker", "MS", "w/ Ricker"), cex.axis = 1.4)
    
    
    if(!is.null(file)){
      dev.off() 
    }
  }
}



