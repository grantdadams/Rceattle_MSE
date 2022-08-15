# Library
library(fmsb)
library(gmRi)
library(scales)


# make all PMs relative to maximum and minimum values
normalize <- function(x) {
  return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
}


histogram_by_om(system = "EBS", species = "Pollock")
histogram_by_om(system = "EBS", species = "Cod")
histogram_by_om(system = "EBS", species = "Arrowtooth flounder")

histogram_by_om(system = "GOA", species = "Pollock")
histogram_by_om(system = "GOA", species = "Cod")
histogram_by_om(system = "GOA", species = "Arrowtooth flounder")



histogram_by_om_single <- function(system = "GOA", species = "Pollock"){
  
  # EMs
  EM_names <-  c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", # Fixed M
                 "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM")
  
  EM_names_print <-  c("Fix M: NPFMC", "Est M: NPFMC", "Fix M: PFMC", "Est M: PFMC", "Fix M: SESSF", "Est M: SESSF", "Fix M: NEFMC", "Est M: Avg F")
  
  # OM Names
  om_names = c("SS_OM", "SSM_OM", "MS_OM")
  
  # Get output
  output_table = pm_summary_table(om_names, EM_names, format = FALSE, reverse = TRUE)
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
    ss_om = OM.res[c(1:2, 8:9,3:4,10:11,5:6,12:13,7,14),],
    
    # SS-M OM
    ssm_om = OM.res[c(15:16,22:23,17:18,24:25,19:20,26:27,21,28),],
    
    # MS OM
    ms_om =  OM.res[c(29:30,36:37,31:32,38:39,33:34,40:41,35,42),] 
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
  par(oma=c(2.5,0.5,0.15,0.1), mar=rep(0,4), mai = c(0,0.3,0.25,0), bg=NA)
  
  
  # Plot it
  for(pm in 1:12){
    if(pm%in%c(3,5:12)){
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
    axis(side = 1, at = 1:3, labels = c("SS fix M", "SS est M", "MS"), cex = 1.2)

  }
  
  plot.new()
  legend("center", legend = c(EM_names_print, "Dynamic BRP"), pch = c(rep(16, 8), 17), bty = "n", col = c(MPcols, 1), pt.cex = 1.5)
}

