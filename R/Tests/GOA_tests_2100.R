source("R/Tests/GOA_condition_models_1977_2100.R")

FixM_data <- ss_run_Tier3$data_list
FixM_data$M1_base[1,3:ncol(FixM_data$M1_base)] <- 0.4
ss_run_Tier3_fixm2 <- Rceattle::fit_mod(data_list = FixM_data,
                                        inits = ss_run_Tier3$estimated_params,
                                        estimateMode = 0, # Run projection only
                                        HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                        FsprTarget = 0.4, # F40%
                                                        FsprLimit = 0.35, # F35%
                                                        Plimit = 0.2, # No fishing when SB<SB20
                                                        Alpha = 0.05),
                                        msmMode = 0, # Single species mode
                                        updateM1 = TRUE,
                                        verbose = 1)


################################################
# Management strategy evaluation
################################################

## Cap
# 1. Max historical catch for Arrowtooth flounder
# 2. No cap
max_atf <- ss_run$data_list$fsh_biom
max_atf <- max_atf[which(max_atf$Species == 2),]

# Pollock, cod, atf
cap_list <- list(
  one = c(1e10, max(max_atf$Catch, na.rm = TRUE), 1e10), # Historical ATF
  two = c(1e10, 1e10, 1e10) # No cap
)


## Sampling period
sampling_period <- c(2,2,1,2,2,2,2,1,2,2,1,2,2,1,1,1)


################################################
# Check runs - no rec
################################################
# - SS-OM: SS-EM Tier 3 HCR
mse1 <- mse_run(om = ss_run_Tier3, em = ss_run_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL)

# - SS-OM: SSM-EM Tier 3 HCR
mse2 <- mse_run(om = ss_run_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL)

# - SS-OM: SSM-EM Tier 3 HCR
mse3 <- mse_run(om = ss_run_M_Tier3, em = ss_run_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL)

# - SS-OM: SSM-EM Tier 3 HCR
mse4 <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL)

mse5 <- mse_run(om = ms_run_f25, em = ss_run_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL)

# - MS-OM: SSM-EM Tier 3 HCR
mse6 <- mse_run(om = ms_run_f25, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL)

# - SS-OM: SSM-M Tier 3 HCR
mse7 <- mse_run(om = ss_run_Tier3_fixm2, em = ss_run_Tier3_fixm2, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL)

# - SS-OM: SSM-M Tier 3 HCR
mse8 <- mse_run(om = ss_run_Tier3_fixm2, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL)

# - SS-OM: SSM-M Tier 3 HCR
mse9 <- mse_run(om = ms_run_f25, em = ss_run_Tier3_fixm2, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL)


################################################
# With data regenerated
################################################
# - SS-OM: SS-EM Tier 3 HCR
mse1r <- mse_run(om = ss_run_Tier3, em = ss_run_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)

# - SS-OM: SSM-EM Tier 3 HCR
mse2r <- mse_run(om = ss_run_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)

# - SS-OM: SSM-EM Tier 3 HCR
mse3r <- mse_run(om = ss_run_M_Tier3, em = ss_run_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)

# - SS-OM: SSM-EM Tier 3 HCR
mse4r <- mse_run(om = ss_run_M_Tier3, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)

# - MS-OM: SS-EM Tier 3 HCR
mse5r <- mse_run(om = ms_run_f25, em = ss_run_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)

# - MS-OM: SSM-EM Tier 3 HCR
mse6r <- mse_run(om = ms_run_f25, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)

# - SS-OM: SSM-M Tier 3 HCR
mse7r <- mse_run(om = ss_run_Tier3_fixm2, em = ss_run_Tier3_fixm2, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)

# - SS-OM: SSM-M Tier 3 HCR
mse8r <- mse_run(om = ss_run_Tier3_fixm2, em = ss_run_M_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)

# - SS-OM: SSM-M Tier 3 HCR
mse9r <- mse_run(om = ms_run_f25, em = ss_run_Tier3_fixm2, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)


################################################
# Double sampling effort
################################################
# - SS-OM: SSM-EM Tier 3 HCR
fut_sample = 4
ss_run_Tier3_fixm2_double <- ss_run_Tier3_fixm2
ss_run_Tier3_fixm2_double$data_list$srv_biom$Log_sd <- ss_run_Tier3_fixm2_double$data_list$srv_biom$Log_sd / fut_sample
ss_run_Tier3_fixm2_double$data_list$comp_data$Sample_size <- ss_run_Tier3_fixm2_double$data_list$comp_data$Sample_size * fut_sample

ss_run_M_Tier3_double <- ss_run_M_Tier3
ss_run_M_Tier3_double$data_list$srv_biom$Log_sd <- ss_run_M_Tier3_double$data_list$srv_biom$Log_sd / fut_sample
ss_run_M_Tier3_double$data_list$comp_data$Sample_size <- ss_run_M_Tier3_double$data_list$comp_data$Sample_size * fut_sample

mse8rdouble <- mse_run(om = ss_run_Tier3_fixm2_double, em = ss_run_M_Tier3_double, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)


################################################
# Set up directories
################################################
mse_list <- list(mse1, mse2, mse3, mse4, mse5, mse6, mse7, mse8, mse9, mse1r, mse2r, mse3r, mse4r, mse5r, mse7r, mse7r, mse8r, mse8rdouble, mse9r)
save(mse_list, file = "Tests/GOA2100.RData")

MSE_names <- c("Tests/GOA_2100/Test1 - SS Fix M OM, Fix M EM/", 
               "Tests/GOA_2100/Test2 - SS Fix M OM, Est M EM/", 
               "Tests/GOA_2100/Test3 - SS Est M OM, Fix M EM/", 
               "Tests/GOA_2100/Test4 - SS Est M OM, Est M EM/", 
               "Tests/GOA_2100/Test5 - MS OM, Fix M EM/", 
               "Tests/GOA_2100/Test6 - MS OM, Est M EM/", 
               "Tests/GOA_2100/Test7 - SS Fix (age-invariant) M OM, Fix (age-invariant) M EM/", 
               "Tests/GOA_2100/Test8 - SS Fix (age-invariant) M OM, Est M EM/", 
               "Tests/GOA_2100/Test9 - MS OM, Fix M (age-invariant) EM/",
               "Tests/GOA_2100/Regen/Test1 - SS Fix M OM, Fix M EM/", 
               "Tests/GOA_2100/Regen/Test2 - SS Fix M OM, Est M EM/", 
               "Tests/GOA_2100/Regen/Test3 - SS Est M OM, Fix M EM/", 
               "Tests/GOA_2100/Regen/Test4 - SS Est M OM, Est M EM/", 
               "Tests/GOA_2100/Regen/Test5 - MS OM, Fix M EM/", 
               "Tests/GOA_2100/Regen/Test6 - MS OM, Est M EM/", 
               "Tests/GOA_2100/Regen/Test7 - SS Fix (age-invariant) M OM, Fix (age-invariant) M EM/", 
               "Tests/GOA_2100/Regen/Test8 - SS Fix (age-invariant) M OM, Est M EM/", 
               "Tests/GOA_2100/Regen/Test8 - SS Fix (age-invariant) M OM, Est M EM (double sampling)/", 
               "Tests/GOA_2100/Regen/Test9 - MS OM, Fix M (age-invariant) EM/")

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
  library(dplyr)
  plot_catch(mod_list, incl_proj = TRUE, file = MSE_names[i], line_col = line_col)
  
  
  Mort <- sapply(mod_list, function(x) x$quantities$M[,1,1,1])
  
  Year = 2018:2100
  species = c("Pollock", "Cod", "ATF")
  png(filename = paste0(MSE_names[i], "_mortality.png"), width = 7, height = 9, units = "in", res = 300)
  par(mfrow = c(3,1))
  for(k in 1:3){
    plot(y = Mort[k,-ncol(Mort)], x = Year, type = "l", main = species[k], ylab = "Mortality", ylim = c(ymin[k], ymax[k]))
    abline(h = Mort[k,ncol(Mort)], lty = 2)
    legend("topright", c("EM", "OM"), lty = c(1,2), bty = "n")
  }
  dev.off()
  
  
  mean_rec <- sapply(mod_list, function(x) x$quantities$mean_rec)
  mean_rec2 <- sapply(mod_list, function(x) rowMeans(x$quantities$R[,1:length(x$data_list$styr:x$data_list$endyr)]))
  
  
  Year = 2018:2100
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


