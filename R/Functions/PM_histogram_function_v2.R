# Reduced for defense


mse_histogram_defense <- function(species = "Pollock", file = NULL, height = 7, width = 6){
  
  library(dplyr)
  
  # OM and EM names ----
  om_names = c(
    # "SS_OM", "SS_Ricker_OM", 
    "SSM_OM", "SSM_Ricker_OM", "MS_OM", "MS_Ricker_OM")
  systems = c("EBS", "GOA")
  
  # - All EMs
  EM_names <-  c(
    # "SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", 
    #              "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", 
    #              "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", 
    #              "SS_fixM_Fspr_EM", 
    #              "SS_fixM_AvgF_EM", 
    
    "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM",
    "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", 
    "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", 
    "SS_estM_Fspr_EM", 
    "SS_estM_AvgF_EM")
  
  EM_names_print <-  c(
    #"Fix M: HCR 1", "Est M: HCR 1" , "Fix M: HCR 2", "Est M: HCR 2", "Fix M: HCR 3", 
    "Est M: HCR 3", "Fix M: HCR 4", "Est M: HCR 4", "Fix M: HCR 5", "Est M: HCR 5"
  )
  
  # - PM names
  pm_names <- c("Average Catch", "Catch IAV", "P(Closed)", "Avg terminal SSB Relative MSE" , "EM: P(Fy > Flimit)"  , "EM: P(SSB < SSBlimit)" , "OM: P(Fy > Flimit)", "OM: P(SSB < SSBlimit)" , "EM: P(Fy > Flimit) but OM: P(Fy < Flimit)", "EM: P(Fy < Flimit) but OM: P(Fy > Flimit)" , "EM: P(SSB < SSBlimit) but OM: P(SSB > SSBlimit)", "EM: P(SSB > SSBlimit) but OM: P(SSB < SSBlimit)", "OM: Terminal SSB Depletion", "OM: Terminal SSB Depletion (Dynamic)") # Names in table
  
  # pm_labels <- c("Catch", "Catch IAV", "P(Closed)", "1/(SSB RMSE)", "EM: P(Overfishing)", "EM: P(overfished)", "OM: P(Overfishing)", "OM: P(Overfished)", "1-P(EM Overfishing & OM Underfishing)", "1-P(EM Underfishing & OM Overfishing)", "1-P(EM Overfished & OM Underfished)", "1-P(EM Underfished & OM Overfished)","Depletion") # Not reversed
  
  pm_labels <- c("Catch", "Catch IAV", "P(Open)", "SSB RMSE", "EM: P(Overfishing)", "EM: P(Overfished)", "OM: P(Overfishing)", "OM: P(Overfished)", "1-P(EM Overfishing & OM Underfishing)", "1-P(EM Underfishing & OM Overfishing)", "1-P(EM Overfished & OM Underfished)", "1-P(EM Underfished & OM Overfished)","Depletion", "Depletion (dynamic B0)")
  
  
  # - Get output ----
  output_table = pm_summary_table(om_names, EM_names, format = FALSE, reverse = FALSE)
  
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
  
  colors <- c(colors[c(1,2,5,6,9,10,13,15)], colors[-c(1,2,5,6,9,10,13,15)])
  point_type <- c(point_type[c(1,2,5,6,9,10,13,15)], point_type[-c(1,2,5,6,9,10,13,15)])
  
  
  point_seq <- seq(-0.4, 0.4, length.out = 5)
  point_loc <- point_seq[c(1,1,2,2,3,3,4,5)]
  
  
  ## Plot single ----
  for(pm in c(1:8, 13, 14)){
    
    # - Save plot dimensions
    if(!is.null(file)){
      png(filename = paste0(file, species, "_", pm, "_", gsub("1/", "1-", gsub(":", "-", pm_labels))[pm],"_defense.png"), width = width, height = height, units = "in", res = 400)
    }
    
    
    # Plot it -----
    
    par(oma=c(0,2,0.5,0.25), mar=c(0,2,0.5,0.25), mai = c(0,0.3,0.4,0))
    layout(mat = matrix(1:3, 3, 1, byrow = TRUE),
           heights = c(1, 1, 0.25), # Heights of the two rows
           widths = 1) # Widths of the two columns
    
    for(sys in 1:2){
      
      # - Subset data
      data_sub <-   output_table[[systems[sys]]] %>%
        filter(Species == species) %>% 
        filter(Performance.metric == pm_names[pm])
      
      
      # - Plot ranges
      if(pm%in%c(3,5:8)){ylim = c(0,1)}else{
        ylim = range(data_sub %>% 
                       pull(Value), 
                     na.rm = TRUE)
      }
      if(pm %in% c(13, 14)){
        ylim[1] <- 0
      }
      
      # - Plot
      plot(NA, NA, ylim = ylim, xlim = c(0.65,6.35), xlab="", ylab="", xaxt = "na", cex = 2, cex.axis = 1.5)
      
      # Rect for Ricker
      rect(xleft = 1.5, -1, xright = 2.5, ylim[2]*2, density = NULL, angle = 45,
           col = "grey92", border = "grey92")
      rect(xleft = 3.5, -1, xright = 4.5, ylim[2]*2, density = NULL, angle = 45,
           col = "grey92", border = "grey92")
      
      
      # - Points for each OM/EM
      for(om in 1:length(om_names)){
        
        data_om_sub <- data_sub %>%
          filter(OM == om_names[om])
        
        points(x = om + point_loc, y = data_om_sub$Value, bg = alpha(colors, alpha = 0.5), pch = point_type, cex = 6)
      }
      
      
      if(pm %in% c(13, 14)){abline(h = 0.4, col = "blue", lty = 2)}
      abline(v = c(2,4) + 0.5, col = "black")
      
      # - Legends
      mtext(pm_labels[pm], side = 2, line = 2.5)
      mtext(c("Eastern Bering Sea", "Gulf of Alaska")[sys], side = 3, line = 0.2, cex = 1.5, font = 2)
      
      if(sys == 1){
        legend("topright", legend = (c("HCR 1 (NPFMC)", "HCR 2 (PFMC)" , "HCR 3 (SESSF)", "HCR 4 (NEFMC)", "HCR 5 (Avg F)", "Equilibrium B0", "Dynamic B0")), bty = "n", pch = c(rep(16, 5), 21, 24), cex = 1.5,  col = c(colors[c(1,3,5,7,8)],  "black", "black"), pt.cex = 2)
      }
      
      if(sys == 2){
        axis(side = 1, at = 1:4, labels = c("No SRR", "Ricker", "No SRR", "Ricker"), cex.axis = 1.5, padj = -0.2)
        mtext(side = 1, line = 3, at = c(1.5, 3.5), text = c("Single-spp", "Multi-spp"), cex = 1.5)
      }
      box(which = "plot", lty = "solid")
    }
    
    
    # Save ----
    if(!is.null(file)){
      dev.off() 
    }
  }
}


