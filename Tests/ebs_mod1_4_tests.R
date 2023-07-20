source("R/BSAI_condition_models.R")


################################################
# Management strategy evaluation
################################################

## Sampling period
sampling_period <- c(1)


################################################
# Check runs - no rec
################################################
# - SS-OM: SS-EM Tier 3 HCR
mse1 <- mse_run_parallel(om = ss_run_Tier3, em = ss_run_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = FALSE)

# - SS-OM: SSM-EM Tier 3 HCR
mse4 <- mse_run_parallel(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)

################################################
# Set up directories
################################################
mse_list <- list(mse1, mse4)

MSE_names <- c("Tests/EBS/Test1 - SS Fix M OM, Fix M EM/", 
               "Tests/EBS/Test4 - SS Est M OM, Est M EM/")

for(i in 1:length(MSE_names)){dir.create(MSE_names[i], recursive = TRUE)}


################################################
# Plot
################################################
ymin <- rep(NA, 3)
ymax <- rep(0, 3)

ymin_rec <- rep(NA, 3)
ymax_rec <- rep(0, 3)
for(i in 1:length(mse_list)){
  mod_list <- c(mse_list[[i]][[1]]$EM,list(mse_list[[i]][[1]]$OM))
  mort_list <- sapply(mod_list, function(x) x$quantities$M[,1,1,1])
  mn_rec_list <- sapply(mod_list, function(x) x$quantities$mean_rec)
  
  maxes <- apply(mort_list, 1, max)
  mins <- apply(mort_list, 1, min)
  
  rmaxes <- apply(mn_rec_list, 1, max)
  rmins <- apply(mn_rec_list, 1, min)
  
  for(sp in 1:3){
    ymin[sp] <- min(c(ymin[sp], mins[sp]), na.rm = TRUE)
    ymax[sp] <- max(c(ymax[sp], maxes[sp]))
    
    ymin_rec[sp] <- min(c(ymin_rec[sp], rmins[sp]), na.rm = TRUE)
    ymax_rec[sp] <- max(c(ymax_rec[sp], rmaxes[sp]))
  }
}

for(i in 1:length(mse_list)){
  mod_list <- c(mse_list[[i]][[1]]$EM,list(mse_list[[i]][[1]]$OM))
  mod_list2 <- mse_list[[i]][[1]]$EM
  model_names = c(paste0("EM-", 2017:2060), "OM")
  names(mod_list) <- model_names
  
  line_col <- c(rev(oce::oce.colorsViridis(length(mod_list))), 1)
  
  if(mse_list[[i]][[1]]$OM$data_list$msmMode == 1){
    mod_list[[length(mod_list)]]$quantities$depletionSSB <- mod_list[[length(mod_list)]]$quantities$biomassSSB / ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)] # Divide ssb by SSB in 2060 under no fishing
    mod_list[[length(mod_list)]]$quantities$SB0 <- ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)] # Update SB0
    mod_list[[length(mod_list)]]$data_list$Plimit <- 0.25 # Update SB0
    mod_list[[length(mod_list)]]$data_list$Ptarget <- 0.25 # Update SB0
    mod_list[[length(mod_list)]]$quantities$Flimit <- ms_run_f25$quantities$Ftarget # Update Flimit from Ftarget that was optimized
  }
  
  plot_depletionSSB(mod_list, incl_proj = TRUE, model_names = model_names, file = MSE_names[i], line_col = line_col)
  plot_depletionSSB(mod_list2, incl_proj = TRUE, model_names = model_names, file = paste0(MSE_names[i],"no_OM_"), line_col = line_col)
  plot_recruitment(mod_list, incl_proj = TRUE, model_names = model_names, file = MSE_names[i], line_col = line_col)
  plot_recruitment(mod_list2, incl_proj = TRUE, model_names = model_names, file = paste0(MSE_names[i],"no_OM_"), line_col = line_col)
  # plot_m_at_age(mod_list, incl_proj = TRUE, age = 1, model_names = model_names, file = MSE_names[i], line_col = line_col)
  plot_ssb(mod_list2, incl_proj = TRUE, model_names = model_names, file = paste0(MSE_names[i],"no_OM_"), line_col = line_col)
  plot_ssb(mod_list, incl_proj = TRUE, model_names = model_names, file = MSE_names[i], line_col = line_col)
  plot_f(mod_list, incl_proj = FALSE, model_names = model_names, file = MSE_names[i], line_col = line_col)
  plot_index(mod_list, file = MSE_names[i], line_col = line_col)
  plot_catch(mod_list, incl_proj = TRUE, file = MSE_names[i], line_col = line_col)
  
  
  Mort <- sapply(mod_list, function(x) x$quantities$M[,1,1,1])
  
  Year = 2017:2060
  species = c("Pollock", "Cod", "ATF")
  #png(filename = paste0(MSE_names[i], "_mortality.png"), width = 7, height = 9, units = "in", res = 300)
  par(mfrow = c(3,1))
  for(k in 1:3){
    plot(y = Mort[k,-ncol(Mort)], x = Year, type = "l", main = species[k], ylab = "Mortality")
    abline(h = Mort[k,ncol(Mort)], lty = 2)
    legend("topright", c("EM", "OM"), lty = c(1,2), bty = "n")
  }
  #dev.off()
  
  
  mean_rec <- sapply(mod_list, function(x) x$quantities$mean_rec)
  mean_rec2 <- sapply(mod_list, function(x) rowMeans(x$quantities$R[,1:length(x$data_list$styr:x$data_list$endyr)]))
  
  
  Year = 2017:2060
  species = c("Pollock", "Cod", "ATF")
  #png(filename = paste0(MSE_names[i], "_mean_rec_by_assess_year.png"), width = 7, height = 9, units = "in", res = 300)
  par(mfrow = c(3,1))
  for(k in 1:3){
    plot(y = mean_rec[k,-ncol(mean_rec)], x = Year, type = "l", main = species[k], ylab = "Mean recruitment by yr")
    abline(h = mean_rec[k,ncol(mean_rec)], lty = 2)
    legend("topright", c("EM", "OM"), lty = c(1,2), bty = "n")
  }
  # dev.off()
}



meanyrs <- list()
for(i in 1:length(mse_list)){
  mod_list <- c(mse_list[[i]][[1]],list(mse_list[[i]][[2]]))
  meanyrs[[i]] <- sapply(mod_list, function(x) x$data_list$meanyr)
}


