# Library
library(fmsb)
library(gmRi)
library(scales)


# make all PMs relative to maximum and minimum values
normalize <- function(x) {
  return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
}



# File names
om_names = c("SS_OM", "SSM_OM", "MS_OM")


ind=5
for(rec in 1:5){
  recname = c("ConstantR", "AllUp", "AllDown", "ATFRup", "ATFRdown")[rec]
  # - EBS
  histogram_by_om(system = "EBS", recname = recname, species = "Pollock", file = "Results/Figures/Histograms/")
  histogram_by_om(system = "EBS", recname = recname, species = "Cod", file = "Results/Figures/Histograms/")
  histogram_by_om(system = "EBS", recname = recname, species = "Arrowtooth flounder", file = "Results/Figures/Histograms/")
  
  # - GOA
  histogram_by_om(system = "GOA", recname = recname, species = "Pollock", file = "Results/Figures/Histograms/")
  histogram_by_om(system = "GOA", recname = recname, species = "Cod", file = "Results/Figures/Histograms/")
  histogram_by_om(system = "GOA", recname = recname, species = "Arrowtooth flounder", file = "Results/Figures/Histograms/")
  
  
  # Save supp table 6 - AvgF
  # Save table 6 - Tier 3
  EM_names <- c("SS_fixM_Tier3_EM", "SS_estM_Tier3_EM")
  output_table = pm_summary_table(om_names, EM_names, recname = recname)

  write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"a_EBS_", recname,"_Tier3_summary.csv"))
  write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"c_GOA_", recname,"_Tier3_summary.csv"))
  
  # Save supp table 2
  EM_names <- c("SS_fixM_dynamicTier3_EM", "SS_estM_dynamicTier3_EM")
  output_table = pm_summary_table(om_names, EM_names, recname = recname)
  write.csv(output_table$EBS, file = paste0("Results/Tables/TableS", ind,"b_EBS_", recname,"_dynamicTier3_summary.csv"))
  write.csv(output_table$GOA, file = paste0("Results/Tables/TableS", ind,"d_GOA_", recname,"_dynamicTier3_summary.csv"))
  ind = ind+1
}



histogram_by_om <- function(system = "GOA", recname = "ConstantR", species = "Pollock", file = NULL, height = 6, width = 6){
  
  # EMs
  EM_names <-  c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", # "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", # Fixed M
                 "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM") #, "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM")
  
  EM_names_print <-  c("Fix M: HCR 1", "Est M: HCR 1") # , "Fix M: HCR 2", "Est M: HCR 2", "Fix M: HCR 3", "Est M: HCR 3", "Fix M: HCR 4", "Est M: HCR 4")
  
  # OM Names
  om_names = c("SS_OM", "SSM_OM", "MS_OM")
  
  # Get output
  output_table = pm_summary_table(om_names, EM_names, recname = recname, format = FALSE, reverse = TRUE)
  OM.res = output_table[[system]]
  OM.res = OM.res[which(OM.res$Species == species),-1]
  rownames(OM.res) <- OM.res$Performance.metric
  OM.res <- OM.res[,-1]
  OM.res = t(OM.res)
  OM.res <- OM.res[,c(1,2,3,4,5,6,7,8,9:12)]
  OM.res <- as.data.frame(OM.res)
  
  # Subset based on OM
  data_list <- list(
    # SS OM
    ss_om = OM.res[c(1:4),],
    
    # SS-M OM
    ssm_om = OM.res[c(5:8),],
    
    # MS OM
    ms_om =  OM.res[c(9:12),] 
  )
  
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
  if(!is.null(file)){
    png(filename = paste0(file, system, "_", recname, "_", species, ".png"), width = width, height = height, units = "in", res = 300)
  }
  
  par(oma=c(0,0.5,0.15,0.1), mar=rep(0,4), mai = c(0,0.3,0.25,0))
  layout(mat = matrix(1:10, 5, 2, byrow = TRUE),
         heights = c(rep(1,4), 0.2), # Heights of the two rows
         widths = c(rep(1,2))) # Widths of the two columns
  
  
  # Header
  # plot.new()
  # for(i in 1:3){
  #   plot(c(-1, 1), c(-1, 1), type="n", frame.plot=FALSE, axes=FALSE, 
  #        xlab="", ylab="")
  #   text(0,0,c("Single-spp fix M", "Single-spp est M", "Multi-spp")[i], cex = 1.5, font = 2)
  # }
  
  
  # Plot it
  for(pm in 1:8){
    if(pm%in%c(3,5:8)){
      ylim = c(0,1)
    } else{
      ylim = range(OM.res[,pm])
    }
    plot(NA, NA, ylim = ylim, xlim = c(0.65,3.35), main = c("Catch", "1/(Catch IAV)", "P(Open)", "1/(SSB RMSE)", "EM: P(Not overfishing)", "EM: P(Not overfished)", "OM: P(Not overfishing)", "OM: P(Not overfished)", "1-P(EM Overfishing & OM Underfishing)", "1-P(EM Underfishing & OM Overfishing)", "1-P(EM Overfished & OM Underfished)", "1-P(EM Underfished & OM Overfished)")[pm], xaxt = "na", xlab="", ylab="")
    
    for(i in 1:length(data_list)){
      points(x = seq(i-0.35, i+ 0.35, length.out = nrow(data_list[[i]])), y = data_list[[i]][,pm], bg = alpha(colors, alpha = 0.5), pch = point_type, cex = 3)
    }
    
    abline(v = 1.5, col = "grey")
    abline(v = 2.5, col = "grey")
    
    if(pm %in% c(7,8)){
      axis(side = 1, at = 1:3, labels = c("SS fix M", "SS est M", "MS"), cex = 1.2)
    }
    
    if(pm == 12){
      #legend("bottomright", legend = c(EM_names_print, "Dynamic BRP"), pch = c(rep(16, 8), 17), bty = "n", col = c(MPcols, 1), pt.cex = 1.5)
    }
  }
  
  
  if(!is.null(file)){
    dev.off() 
  }
}




# Summary table function
pm_summary_table <- function(om_names, em_hcr_names, recname, format = TRUE, reverse = FALSE){
  # Get data we want
  for(om in 1:length(om_names)){  # OM model
    for(em in 1:length(em_hcr_names)){ # EM and HCR
      
      # STEP 1 -- File names
      MSE_names <- paste0(om_names[om],"__", em_hcr_names[em])
      GOA_mse_sum_tmp <- read.csv(file = paste0("Results/Tables/GOA/GOA1977", "_", recname, "_Table", MSE_names, "_", recname,".csv"))[,-1] # May need to add "_" after table for later iterations
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
                      "OM: Terminal SSB/SSBtarget",
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

