source("R/GOA_condition_models_1977.R")

################################################
# Management strategy evaluation
################################################
## Sampling period
sampling_period <- c(2,2,1,2,2,2,2,1,2,2,1,2,2,1,1,1)


################################################
# Check runs - no rec
################################################
# - Base
mse1 <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL)

# - Add sampling timing
mse2 <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL)

# - Add rec dev
mse3 <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = FALSE, sample_rec = TRUE, dir = NULL, file = NULL)

# - Add rec dev and sampling timing
mse4 <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = TRUE, dir = NULL, file = NULL)

# - Add sampling error
mse5 <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = TRUE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = FALSE)

# - Add rec dev and sampling timing and sampling error
mse6 <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, dir = NULL, file = NULL, regenerate_past = FALSE)


################################################
# WITH REGEN
################################################
# - Base
mse1r <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)

# - Add sampling timing
mse2r <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)

# - Add rec dev
mse3r <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = FALSE, sample_rec = TRUE, dir = NULL, file = NULL, regenerate_past = TRUE)

# - Add rec dev and sampling timing
mse4r <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = TRUE, dir = NULL, file = NULL, regenerate_past = TRUE)

# - Add sampling error
mse5r <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = TRUE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)

# - Add rec dev and sampling timing and sampling error
mse6r <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, dir = NULL, file = NULL, regenerate_past = TRUE)


################################################
# List
################################################
mse_list <- list(mse1, mse2, mse3, mse4, mse5, mse6, mse1r, mse2r, mse3r, mse4r, mse5r, mse6r)

MSE_names <- c("Tests/M tests/GOA/Test1 - SS Est M OM, Est M EM Base/", 
               "Tests/M tests/GOA/Test2 - SS Est M OM, Est M EM Sampling timing/", 
               "Tests/M tests/GOA/Test3 - SS Est M OM, Est M EM Rec dev/", 
               "Tests/M tests/GOA/Test4 - SS Est M OM, Est M EM Rec dev and sampling timing/", 
               "Tests/M tests/GOA/Test5 - SS Est M OM, Est M EM Sampling error/", 
               "Tests/M tests/GOA/Test6 - SS Est M OM, Est M EM Rec dev and sampling timing and sampling error/", 
               
               "Tests/M tests/GOA/Regen/Test1 - SS Est M OM, Est M EM Base/", 
               "Tests/M tests/GOA/Regen/Test2 - SS Est M OM, Est M EM Sampling timing/", 
               "Tests/M tests/GOA/Regen/Test3 - SS Est M OM, Est M EM Rec dev/", 
               "Tests/M tests/GOA/Regen/Test4 - SS Est M OM, Est M EM Rec dev and sampling timing/",
               "Tests/M tests/GOA/Regen/Test5 - SS Est M OM, Est M EM Sampling error/", 
               "Tests/M tests/GOA/Regen/Test6 - SS Est M OM, Est M EM Rec dev and sampling timing and sampling error/" 
)
for(i in 1:length(MSE_names)){dir.create(MSE_names[i], recursive = TRUE)}


################################################
# Plot
################################################
ymin <- rep(NA, 3)
ymax <- rep(0, 3)

ymin_rec <- rep(NA, 3)
ymax_rec <- rep(0, 3)
for(i in 1:length(mse_list)){
  mod_list <- c(mse_list[[i]][[1]],list(mse_list[[i]][[2]]))
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
  mod_list <- c(mse_list[[i]][[1]],list(mse_list[[i]][[2]]))
  mod_list2 <- mse_list[[i]][[1]]
  model_names = c(paste0("EM-", 2017:2060), "OM")
  line_col <- c(rev(oce::oce.colorsViridis(length(mse_list[[i]][[1]]))), 1)
  
  if(mse_list[[i]][[2]]$data_list$msmMode == 1){
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
  # plot_catch(mod_list, incl_proj = TRUE, file = MSE_names[i], line_col = line_col)
  
  
  Mort <- sapply(mod_list, function(x) x$quantities$M[,1,1,1])
  MortMales <- sapply(mod_list, function(x) x$quantities$M[,2,1,1])
  
  Year = 2018:2060
  species = c("Pollock", "Cod", "ATF")
  png(filename = paste0(MSE_names[i], "_mortality.png"), width = 7, height = 9, units = "in", res = 300)
  par(mfrow = c(4,1))
  for(k in 1:3){
    plot(y = Mort[k,-ncol(Mort)], x = Year, type = "l", main = species[k], ylab = "Mortality", ylim = c(ymin[k], ymax[k]))
    abline(h = Mort[k,ncol(Mort)], lty = 2)
    legend("topright", c("EM", "OM"), lty = c(1,2), bty = "n")
  }
  
  # ATF Males
  plot(y = MortMales[2,-ncol(Mort)], x = Year, type = "l", main = "ATF Males", ylab = "Mortality", ylim = c(ymin[k], ymax[k]))
  abline(h = MortMales[2,ncol(Mort)], lty = 2)
  legend("topright", c("EM", "OM"), lty = c(1,2), bty = "n")
  
  dev.off()
  
  
  mean_rec <- sapply(mod_list, function(x) x$quantities$mean_rec)
  mean_rec2 <- sapply(mod_list, function(x) rowMeans(x$quantities$R[,1:length(x$data_list$styr:x$data_list$endyr)]))
  
  
  Year = 2018:2060
  species = c("Pollock", "Cod", "ATF")
  png(filename = paste0(MSE_names[i], "_mean_rec_by_assess_year.png"), width = 7, height = 9, units = "in", res = 300)
  par(mfrow = c(3,1))
  for(k in 1:3){
    plot(y = mean_rec[k,-ncol(mean_rec)], x = Year, type = "l", main = species[k], ylab = "Mean recruitment by yr", ylim = c(0, max(mean_rec[k,])))
    abline(h = mean_rec[k,ncol(mean_rec)], lty = 2)
    legend("topright", c("EM", "OM"), lty = c(1,2), bty = "n")
  }
  dev.off()
}



meanyrs <- list()
for(i in 1:length(mse_list)){
  mod_list <- c(mse_list[[i]][[1]],list(mse_list[[i]][[2]]))
  meanyrs[[i]] <- sapply(mod_list, function(x) x$data_list$meanyr)
}


