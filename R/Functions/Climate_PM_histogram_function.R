

climate_mse_histogram <- function(species = "Pollock", file = NULL, height = 7, width = 6, allHCR = FALSE, legend.pos = "topleft"){
  
  # OM and EM names ----
  om_names <- paste0(c(
    "ms_mod", "ms_mod_ricker",
    "ms_mod_ssp126", "ms_mod_ricker_ssp126",
    "ms_mod_ssp245", "ms_mod_ricker_ssp245",
    "ms_mod_ssp585", "ms_mod_ricker_ssp585" 
  ), "_OM")
  
  em_hcr_names <- paste0(c("ss_run_Tier3", "ss_run_dynamicTier3", "ss_run_M_Tier3", "ss_run_M_dynamicTier3", "ms_run_fb40", "ms_run_fb40iter", "ms_run_cmsy", "ms_run_concmsy"), "_EM")
  
  em_names_print <- (c("NPFMC Fix-M", "NPFMC Est-M" , "MS-B40a", "MS-B40b", "MS-MSY", "MS-cMSY"))
  
  
  # PM names ----
  pm_names <- c("Average Catch", "Catch IAV", "P(Closed)", "Avg terminal SSB Relative MSE" , "EM: P(Fy > Flimit)"  , "EM: P(SSB < SSBlimit)" , "OM: P(Fy > Flimit)", "OM: P(SSB < SSBlimit)" , "EM: P(Fy > Flimit) but OM: P(Fy < Flimit)", "EM: P(Fy < Flimit) but OM: P(Fy > Flimit)" , "EM: P(SSB < SSBlimit) but OM: P(SSB > SSBlimit)", "EM: P(SSB > SSBlimit) but OM: P(SSB < SSBlimit)", "OM: Terminal SSB Depletion", "OM: Terminal SSB Depletion (Dynamic)") # Names in table
  
  pm_labels <- c("Catch", "Catch IAV", "P(Open)", "SSB RMSE", "EM: P(Overfishing)", "EM: P(Overfished)", "OM: P(Overfishing)", "OM: P(Overfished)", "1-P(EM Overfishing & OM Underfishing)", "1-P(EM Underfishing & OM Overfishing)", "1-P(EM Overfished & OM Underfished)", "1-P(EM Underfished & OM Overfished)","Depletion", "Depletion (dynamic B0)")
  
  
  # - Get output ----
  output_table = climate_pm_summary_table(om_names, em_hcr_names, cap = c(TRUE, FALSE), format = FALSE, reverse = FALSE)
  OM.res = output_table %>%
    filter(Species == species)

  
  # Colors
  MPcols <- gmri_pal("main")(11)
  colors <- MPcols[c(1,1,3,3,5,7,9,11)]
  point_type <- c(21,24,21,24,rep(22,4))
  
  
  point_seq <- seq(-0.4, 0.4, length.out = 6)
  point_loc <- point_seq[c(1,1,2,2,3:6)]
  
  
  ## Plot single ----
  for(pm in c(1:8, 13, 14)){
    
    # - Save plot dimensions
    if(!is.null(file)){
      png(filename = paste0(file, species, "_", pm, "_", gsub("1/", "1-", gsub(":", "-", pm_labels))[pm],"_", legend.pos, ".png"), width = width, height = height, units = "in", res = 400)
    }
    
    par(oma=c(0,2,0.5,0.1), mar=c(0,2,0.5,0), mai = c(0,0.3,0.3,0))
    layout(mat = matrix(1:3, 3, 1, byrow = TRUE),
           heights = c(1, 1, 0.25), # Heights of the two rows
           widths = 1) # Widths of the two columns
    
    for(cap in 1:2){
      
      # - Subset data
      data_sub <-   output_table %>%
        filter(Species == species & Performance.metric == pm_names[pm], Cap == c(FALSE, TRUE)[cap])
      
      
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
      plot(NA, NA, ylim = ylim, xlim = c(0.65,8.35), xlab="", ylab="", xaxt = "na", cex = 2, cex.axis = 1.5)
      
      # - Points for each OM/EM
      for(om in 1:length(om_names)){
        
        # Rect for ricker
        rect(xleft = c(seq(2,8, by = 2) - 0.5)[om], -1, xright = c(seq(2,8, by = 2) + 0.5)[om], ylim[2]*2, density = NULL, angle = 45,
             col = "grey92", border = "grey92")
        
        data_om_sub <- data_sub %>%
          filter(OM == om_names[om])
        
        points(x = om + point_loc, y = data_om_sub$Value, bg = alpha(colors, alpha = 0.6), pch = point_type, cex = 4.5)
      }
      
      
      if(pm %in% c(13, 14)){abline(h = 0.4, col = "blue", lty = 2)}
      abline(v = c(2,4,6) + 0.5, col = "black")
      
      # - Legends
      mtext(pm_labels[pm], side = 2, line = 2.5)
      mtext(c("No cap", "Cap")[cap], side = 3, line = 0.2, cex = 1.5, font = 2)
      
      if(cap == 1){
        legend(legend.pos, legend = em_names_print, bty = "n", pch = 16, cex = 1.5,  col = colors[c(1,3,5:8)], pt.cex = 2)
      }
      
      if(cap == 2){
        axis(side = 1, at = 1:8, labels = rep(c("No SRR", "Ricker"), 4), cex.axis = 1, padj = -0.5)
        mtext(side = 1, line = 2, at = c(1.5, 3.5, 5.5, 7.5), text = c("Naive", "SSP-126", "SSP-245", "SSP-585"), cex = 1.5)
      }
      box(which = "plot", lty = "solid")
    }
    
    if(!is.null(file)){
      dev.off() 
    }
  }
}


