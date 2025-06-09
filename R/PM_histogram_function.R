mse_histogram_two_system <- function(species = "Pollock", file = NULL, height = 7, width = 6, allHCR = FALSE, legend.pos = "topleft"){
  
  library(dplyr)
  
  # OM and EM names ----
  om_names = c("SSM_OM", "SSM_Ricker_OM", "SS_OM", "SS_Ricker_OM", "MS_OM", "MS_Ricker_OM")
  om_names_print = c("1. SS fix M", "2. w Ricker", "3. SS est M", "4. w Ricker", "5. MS", "6. w Ricker")
  systems = c("EBS", "GOA")
  
  
  # - EMs
  EM_names <- c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", "SS_fixM_AvgF_EM", # Fixed M
                "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM", "SS_estM_AvgF_EM")
  
  EM_names_print <-  c("HCR 1a (NPFMC)", "HCR 1b (Dynamic NPFMC)",
                       "HCR 2a (PFMC)", "HCR 2b (Dynamic PFMC)",
                       "HCR 3a (SESSF)", "HCR 3b (Dynamic SESSF)",
                       "HCR 4 (NEFMC)", "HCR 5 (Avg F)",
                       
                       "HCR 1a (NPFMC)", "HCR 1b (Dynamic NPFMC)",
                       "HCR 2a (PFMC)", "HCR 2b (Dynamic PFMC)",
                       "HCR 3a (SESSF)", "HCR 3b (Dynamic SESSF)",
                       "HCR 4 (NEFMC)", "HCR 5 (Avg F)")
  
  # - PM names
  pm_names <- c("Average Catch", "Catch IAV", "P(Closed)", "Avg terminal SSB Relative MSE" , "EM: P(Fy > Flimit)"  , "EM: P(SSB < SSBlimit)" , "OM: P(Fy > Flimit)", "OM: P(SSB < SSBlimit)" , "OM: Terminal SSB Depletion", "OM: Terminal SSB Depletion (Dynamic)") # Names in table
  
  pm_labels <- c("Catch", "Catch IAV", "P(Open)", "SSB RMSE", "EM: P(Overfishing)", "EM: P(Overfished)", "OM: P(Overfishing)", "OM: P(Overfished)", "Depletion", "Depletion (dynamic B0)")
  
  
  # - Get output ----
  output_table = pm_summary_table(om_names, EM_names, format = FALSE, reverse = FALSE)
  
  # Colors
  MPcols <- gmri_pal("mixed")(10)
  MPcolsalpha <- alpha(MPcols[1:6], alpha = 0.6)
  point_type = c()
  colors <- c()
  for(i in 1:6){
    colors <- c(colors, MPcols[i], MPcols[i])
    point_type <- c(point_type, 21, 24)
  }
  colors <- c(colors, MPcols[7:10])
  point_type <- c(point_type, 21, 21, 21, 21)
  
  colors <- c(colors[c(1,2,5,6,9,10,13,15)], 
              colors[c(1,2,5,6,9,10,13,15)])
  point_type <- c(point_type[c(1,2,5,6,9,10,13,15)], point_type[-c(1,2,5,6,9,10,13,15)])
  
  
  point_seq <- seq(-0.4, 0.4, length.out = 10)
  point_loc <- point_seq[c(1,1,2,2,3,3,4,5,6,6,7,7,8,8,9,10)]
  
  
  ## Plot single ----
  for(pm in c(1:10)){
    
    # - Save plot dimensions
    if(!is.null(file)){
      png(filename = paste0(file, species, "_", pm, "_", gsub("1/", "1-", gsub(":", "-", pm_labels))[pm],"_", legend.pos, ".png"), width = width, height = height, units = "in", res = 400)
    }
    
    par(oma=c(0,2,0.5,0.1), mar=c(0,2,0.5,0), mai = c(0,0.3,0.3,0))
    layout(mat = matrix(1:3, 3, 1, byrow = TRUE),
           heights = c(1, 1, 0.35), # Heights of the two rows
           widths = 1) # Widths of the two columns
    
    for(sys in 1:2){
      
      # - Subset data
      data_sub <-   output_table[[systems[sys]]] %>%
        filter(Species == species) %>% 
        filter(Performance.metric == pm_names[pm])
      
      
      # - Plot ranges
      ylim = range(data_sub %>% 
                     pull(Value), 
                   na.rm = TRUE)
      if(length(unique(ylim)) == 1){
        ylim <- c(0, 1)
      }
      
      if(pm %in% c(9:10)){
        ylim[1] <- 0
      }
      
      # - Plot
      plot(NA, NA, ylim = ylim, xlim = c(0.65,6.35), xlab="", ylab="", xaxt = "na", cex = 2, cex.axis = 1.5)
      
      # - Points for each OM/EM
      for(om in 1:length(om_names)){
        
        # Rect for est M
        rect(xleft = c(1:6)[om], -1, xright = c(1:6 + 0.5)[om], ylim[2]*2, density = NULL, angle = 45,
             col = "grey92", border = "grey92")
        if(om == length(om_names)){
          rect(xleft = 6, -1, xright = 7, ylim[2]*2, density = NULL, angle = 45,
               col = "grey92", border = "grey92")
        }
        
        data_om_sub <- data_sub %>%
          filter(OM == om_names[om])
        
        points(x = om + point_loc, y = data_om_sub$Value, bg = alpha(colors, alpha = 0.5), pch = point_type, cex = 4.5)
      }
      
      
      if(pm %in% c(9:10)){abline(h = 0.4, col = "blue", lty = 2)}
      abline(v = c(2,4) + 0.5, col = "black")
      
      # - Legends
      mtext(pm_labels[pm], side = 2, line = 2.5)
      mtext(systems[sys], side = 3, line = 0.2, cex = 1.5, font = 2)
      
      if(sys == 1){
        legend(legend.pos, legend = (c("HCR 1 (NPFMC)", "HCR 2 (PFMC)" , "HCR 3 (SESSF)", "HCR 4 (NEFMC)", "HCR 5 (Avg F)")), bty = "n", pch = 16, cex = 1.4,  col = colors[c(1,3,5,7,8)], pt.cex = 2)
      }
      
      if(sys == 2){
        axis(side = 1, at = 1:6, labels = c("No SRR", "Ricker", "No SRR", "Ricker", "No SRR", "Ricker"), cex.axis = 1.6, padj = -0.2)
        mtext(side = 1, line = 3, at = c(1.5, 3.5, 5.5), text = c("Age-invariant M", "Age-varying M", "Age- and time-varying M"), cex = 1.3)
        mtext(side = 1, line = 5.5, at = c(2.5, 5.5), text = c("Single-spp OM", "Multi-spp OM"), cex = 1.5, font = 2)
      }
      box(which = "plot", lty = "solid")
    }
    
    if(!is.null(file)){
      dev.off() 
    }
  }
}


