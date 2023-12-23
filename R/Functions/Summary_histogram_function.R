
mse_histogram <- function(system = "GOA", species = "Pollock", recname = "ConstantR", file = NULL, height = 7, width = 6, allHCR = FALSE, single = TRUE){
  
  library(dplyr)
  
  # OM and EM names ----
  om_names = c("SS_OM", "SS_Ricker_OM", "SSM_OM", "SSM_Ricker_OM", "MS_OM", "MS_Ricker_OM")
  
  # - EMs - Tier 3 NPFMC only
  EM_names <-  c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM",
                 "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM") 
  EM_names_print <-  c("Fix M: NPFMC", "Est M: NPFMC") 
  
  # - All EMs
  if(allHCR){
    EM_names <-  c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", 
                   "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM",
                   "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", 
                   "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", 
                   "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", 
                   "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", 
                   "SS_fixM_Fspr_EM", "SS_fixM_AvgF_EM", 
                    "SS_estM_Fspr_EM", "SS_estM_AvgF_EM")
    
    EM_names_print <-  c("Fix M: HCR 1", "Est M: HCR 1" , "Fix M: HCR 2", "Est M: HCR 2", "Fix M: HCR 3", "Est M: HCR 3", "Fix M: HCR 4", "Est M: HCR 4", "Fix M: HCR 5", "Est M: HCR 5")
  }
  
  # - PM names
  pm_names <- c("Average Catch", "Catch IAV", "P(Closed)", "Avg terminal SSB Relative MSE" , "EM: P(Fy > Flimit)"  , "EM: P(SSB < SSBlimit)" , "OM: P(Fy > Flimit)", "OM: P(SSB < SSBlimit)" , "EM: P(Fy > Flimit) but OM: P(Fy < Flimit)", "EM: P(Fy < Flimit) but OM: P(Fy > Flimit)" , "EM: P(SSB < SSBlimit) but OM: P(SSB > SSBlimit)", "EM: P(SSB > SSBlimit) but OM: P(SSB < SSBlimit)", "OM: Terminal SSB Depletion") # Names in table
  
  # pm_labels <- c("Catch", "Catch IAV", "P(Closed)", "1/(SSB RMSE)", "EM: P(Overfishing)", "EM: P(overfished)", "OM: P(Overfishing)", "OM: P(Overfished)", "1-P(EM Overfishing & OM Underfishing)", "1-P(EM Underfishing & OM Overfishing)", "1-P(EM Overfished & OM Underfished)", "1-P(EM Underfished & OM Overfished)","Depletion") # Not reversed
  
  pm_labels <- c("Catch", "1/(Catch IAV)", "P(Open)", "1/(SSB RMSE)", "EM: P(Not overfishing)", "EM: P(Not overfished)", "OM: P(Not overfishing)", "OM: P(Not overfished)", "1-P(EM Overfishing & OM Underfishing)", "1-P(EM Underfishing & OM Overfishing)", "1-P(EM Overfished & OM Underfished)", "1-P(EM Underfished & OM Overfished)","Depletion")
  
  
  # - Get output ----
  output_table = pm_summary_table(om_names, EM_names, recname = recname, format = FALSE, reverse = TRUE)
  OM.res = output_table[[system]] %>%
    filter(Species == species)
  
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
  
  
  ## Plot single ----
  if(single){
    for(pm in c(1:8, 13)){
      
      # - Subset data
      data_sub <- OM.res %>%
        filter(Performance.metric == pm_names[pm])
      
      # - Save plot dimensions
      if(!is.null(file)){
        png(filename = paste0(file, system, "_", species, "_", pm, "_", gsub("1/", "1-", gsub(":", "-", pm_labels))[pm], ".png"), width = width, height = height, units = "in", res = 300)
      }
      
      par(oma=c(0,2,0.15,0.1), mar=c(0,2,0,0), mai = c(0,0.3,0.25,0))
      layout(mat = matrix(1:(length(recname)+1), (length(recname)+1), 1, byrow = TRUE),
             heights = c(rep(1,length(recname)), 0.25), # Heights of the two rows
             widths = 1) # Widths of the two columns
      
      
      # - Plot ranges
      if(pm%in%c(3,5:8)){ylim = c(0,1)}else{
        ylim = range(data_sub %>% 
                       pull(Value), 
                     na.rm = TRUE)
      }
      if(pm == 13){
        ylim[1] <- 0
      }
      
      # - Plot
      plot(NA, NA, ylim = ylim, xlim = c(0.65,6.35), xaxt = "na", xlab="", ylab="")
      
      if(pm == 13){abline(h = 0.4, col = "blue", lty = 2)}
      abline(v = 1:5 + 0.5, col = "grey")
      
      # - Points for each OM/EM
      for(om in 1:length(om_names)){
        data_om_sub <- data_sub %>%
          filter(OM == om_names[om])
        
        points(x = seq(om-0.4, om+ 0.4, length.out = nrow(data_om_sub)), y = data_om_sub$Value, bg = alpha(colors, alpha = 0.5), pch = point_type, cex = 3)
      }
      
      # - Legends
      mtext(pm_labels[pm], side = 2, line = 2.5)
      axis(side = 1, at = 1:6, labels = c("SS fix M", "w/ Ricker", "SS est M", "w/ Ricker", "MS", "w/ Ricker"), cex.axis = 1.4)
      
      
      if(!is.null(file)){
        dev.off() 
      }
    }
  }
  
  
  ## Plot multi-panel ----
  if(!single){
    if(!is.null(file)){
      png(filename = paste0(file, system, "_", species, "_", recname, ".png"), width = width, height = height, units = "in", res = 300)
    }
    
    par(oma=c(0,0.5,0.15,0.1), mar=rep(0,4), mai = c(0,0.3,0.25,0))
    layout(mat = matrix(1:12, 6, 2, byrow = TRUE),
           heights = c(rep(1,5), 0.2), # Heights of the two rows
           widths = c(rep(1,2))) # Widths of the two columns
    
    
    # Plot it
    for(pm in c(1:8, 13)){
      
      # - Subset data
      data_sub <- OM.res %>%
        filter(Performance.metric == pm_names[pm])
      
      # - Data range
      if(pm%in%c(3,5:8)){
        ylim = c(0,1)
      }else{
        ylim = range(data_sub %>% 
                       pull(Value), 
                     na.rm = TRUE)
      }
      if(pm == 13){
        ylim[1] <- 0
      }
      
      # - Plot it
      plot(NA, NA, ylim = ylim, xlim = c(0.65,6.35), main = pm_labels[pm], xaxt = "na", xlab="", ylab="")
      
      if(pm == 13){abline(h = 0.4, col = "blue", lty = 2)}
      abline(v = 1:5 + 0.5, col = "grey")
      
      # - Points for each OM/EM
      for(om in 1:length(om_names)){
        data_om_sub <- data_sub %>%
          filter(OM == om_names[om])
        
        points(x = seq(om-0.4, om+ 0.4, length.out = nrow(data_om_sub)), y = data_om_sub$Value, bg = alpha(colors, alpha = 0.5), pch = point_type, cex = 3)
      }
      
      # - Labels
      if(pm %in% c(13,8)){
        axis(side = 1, at = 1:6, labels = c("SS fix M", "w/ Ricker", "SS est M", "w/ Ricker", "MS", "w/ Ricker"), cex.axis = 1.2)
      }
    }
    
    
    if(!is.null(file)){
      dev.off() 
    }
  }
}



