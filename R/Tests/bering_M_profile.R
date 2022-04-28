library(Rceattle)
library(dplyr)

################################################
# Data
################################################
# Example
# To run the 2017 single species assessment for the Bering Sea, a data file must first be loaded:
data(BS2017SS) # ?BS2017SS for more information on the data
BS2017SS$projyr <- 2060


################################################
# Estimation
################################################
# Then the model can be fit by setting `msmMode = 0` using the `Rceattle` function:
BS2017SS$fleet_control$proj_F_prop <-rep(1,7)
ss_run <- Rceattle::fit_mod(data_list = BS2017SS,
                            inits = NULL, # Initial parameters = 0
                            file = NULL, # Don't save
                            estimateMode = 1, # Estimate hindcast only
                            random_rec = FALSE, # No random recruitment
                            msmMode = 0, # Single species mode
                            phase = "default",
                            verbose = 1)

# Estimate single-species and estimate M
BS2017SS_M <- BS2017SS
BS2017SS_M$est_M1 <- c(1,1,1)
ss_run_M <- Rceattle::fit_mod(data_list = BS2017SS_M,
                              inits = ss_run$estimated_params, # Initial parameters = 0
                              file = NULL, # Don't save
                              estimateMode = 1, # Estimate hindcast only
                              random_rec = FALSE, # No random recruitment
                              msmMode = 0, # Single species mode
                              phase = "default",
                              verbose = 1)


# -- NPFMC Tier 3
ss_run_Tier3 <- Rceattle::fit_mod(data_list = BS2017SS,
                                  inits = ss_run$estimated_params,
                                  estimateMode = 2, # Run projection only
                                  HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                  FsprTarget = 0.4, # F40%
                                                  FsprLimit = 0.35, # F35%
                                                  Plimit = 0.2, # No fishing when SB<SB20
                                                  Alpha = 0.2),
                                  msmMode = 0, # Single species mode
                                  verbose = 1)


# -- NPFMC Tier 3
ss_run_M_Tier3 <- Rceattle::fit_mod(data_list = BS2017SS_M,
                                    inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                    estimateMode = 2, # Run projection only
                                    HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                    FsprTarget = 0.4, # F40%
                                                    FsprLimit = 0.35, # F35%
                                                    Plimit = 0.2, # No fishing when SB<SB20
                                                    Alpha = 0.2),
                                    msmMode = 0, # Single species mode
                                    verbose = 1,
                                    updateM1 = FALSE)


BS2017SS_FixM <- BS2017SS
BS2017SS_FixM$M1_base[1,3:4] <- 0.3
ss_run_Tier3_fixm2 <- Rceattle::fit_mod(data_list = BS2017SS_FixM,
                                        inits = ss_run_Tier3$estimated_params,
                                        estimateMode = 0, # Run projection only
                                        HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                        FsprTarget = 0.4, # F40%
                                                        FsprLimit = 0.35, # F35%
                                                        Plimit = 0.2, # No fishing when SB<SB20
                                                        Alpha = 0.2),
                                        msmMode = 0, # Single species mode
                                        updateM1 = TRUE,
                                        verbose = 1)


# - SS-OM: SSM-EM Tier 3 HCR
fut_sample = 8
ss_run_Tier3_fixm2_double <- ss_run_Tier3_fixm2
ss_run_Tier3_fixm2_double$data_list$srv_biom$Log_sd <- ss_run_Tier3_fixm2_double$data_list$srv_biom$Log_sd / fut_sample
ss_run_Tier3_fixm2_double$data_list$comp_data$Sample_size <- ss_run_Tier3_fixm2_double$data_list$comp_data$Sample_size * fut_sample

ss_run_M_Tier3_double <- ss_run_M_Tier3
ss_run_M_Tier3_double$data_list$srv_biom$Log_sd <- ss_run_M_Tier3_double$data_list$srv_biom$Log_sd / fut_sample
ss_run_M_Tier3_double$data_list$comp_data$Sample_size <- ss_run_M_Tier3_double$data_list$comp_data$Sample_size * fut_sample




# - Simulate index and comp data and updatae EM
sim_dat <- sim_mod(ss_run_Tier3_fixm2_double, simulate = FALSE)

ss_run_M_Tier3_double$data_list$srv_biom <- sim_dat$srv_biom
ss_run_M_Tier3_double$data_list$comp_data <- sim_dat$comp_data

# Restimate
ss_run_M_Tier3_double <- fit_mod(
  data_list = ss_run_M_Tier3_double$data_list,
  inits = ss_run_Tier3_fixm2$estimated_params,
  map =  NULL,
  bounds = NULL,
  file = NULL,
  estimateMode = 0, # Run hindcast and projection, otherwise debug
  HCR = build_hcr(HCR = ss_run_M_Tier3_double$data_list$HCR, # Tier3 HCR
                  DynamicHCR = ss_run_M_Tier3_double$data_list$DynamicHCR,
                  FsprTarget = ss_run_M_Tier3_double$data_list$FsprTarget,
                  FsprLimit = ss_run_M_Tier3_double$data_list$FsprLimit,
                  Ptarget = ss_run_M_Tier3_double$data_list$Ptarget,
                  Plimit = ss_run_M_Tier3_double$data_list$Plimit,
                  Alpha = ss_run_M_Tier3_double$data_list$Alpha,
                  Pstar = ss_run_M_Tier3_double$data_list$Pstar,
                  Sigma = ss_run_M_Tier3_double$data_list$Sigma
  ),
  random_rec = ss_run_M_Tier3_double$data_list$random_rec,
  niter = ss_run_M_Tier3_double$data_list$niter,
  msmMode = ss_run_M_Tier3_double$data_list$msmMode,
  avgnMode = ss_run_M_Tier3_double$data_list$avgnMode,
  minNByage = ss_run_M_Tier3_double$data_list$minNByage,
  suitMode = ss_run_M_Tier3_double$data_list$suitMode,
  phase = "default",
  updateM1 = FALSE,
  loopnum = 3,
  getsd = FALSE,
  verbose = 1)

ss_run_M_Tier3_double$quantities$M1[1,1,1]

# --------------------------------------
# Profile M1
# --------------------------------------
M1_seq <- seq(from = 0.15, to = 0.8, by = 0.05)
mod_list <- list()
for(i in 1:length(M1_seq)){
  mod_tmp <- ss_run_M_Tier3_double
  mod_tmp$estimated_params$ln_M1[1,,] <- log(M1_seq[i])
  mod_tmp$data_list$est_M1 <- c(0,0,0)
  
  mod_list[[i]] <- fit_mod(
    data_list = mod_tmp$data_list,
    inits = mod_tmp$estimated_params,
    map =  NULL,
    bounds = NULL,
    file = NULL,
    estimateMode = 0, # Run hindcast and projection, otherwise debug
    HCR = build_hcr(HCR = mod_tmp$data_list$HCR, # Tier3 HCR
                    DynamicHCR = mod_tmp$data_list$DynamicHCR,
                    FsprTarget = mod_tmp$data_list$FsprTarget,
                    FsprLimit = mod_tmp$data_list$FsprLimit,
                    Ptarget = mod_tmp$data_list$Ptarget,
                    Plimit = mod_tmp$data_list$Plimit,
                    Alpha = mod_tmp$data_list$Alpha,
                    Pstar = mod_tmp$data_list$Pstar,
                    Sigma = mod_tmp$data_list$Sigma
    ),
    random_rec = mod_tmp$data_list$random_rec,
    niter = mod_tmp$data_list$niter,
    msmMode = mod_tmp$data_list$msmMode,
    avgnMode = mod_tmp$data_list$avgnMode,
    minNByage = mod_tmp$data_list$minNByage,
    suitMode = mod_tmp$data_list$suitMode,
    phase = "default",
    updateM1 = FALSE,
    loopnum = 3,
    getsd = FALSE,
    verbose = 1)
}


jnll <- sapply(mod_list, function(x) x$quantities$jnll)
jnll_row <- sapply(mod_list, function(x) rowSums(x$quantities$jnll_comp))
jnll_row[5,] = jnll_row[5,] + jnll_row[8,]
jnll_row[11,] = jnll_row[11,] + jnll_row[12,]
rows <- c(1,2,3,5,11)

plot(x = M1_seq, y = (jnll-min(jnll))/(max(jnll)-min(jnll)), ylab = "Relative nll", xlab = "M", type = "l", lwd = 4)
for(j in 1:length(rows)){
  i = rows[j]
  lines(x = M1_seq, y = (jnll_row[i,]-min(jnll_row[i,]))/(max(jnll_row[i,])-min(jnll_row[i,])), col = j+1, lwd = 3)
}
legend("top", c("NLL", rownames(jnll_row)[rows]), bty = "n", col = 1:(length(rows)+1), lty = rep(1,(length(rows)+1)), lwd = rep(3,(length(rows)+1)))




# --------------------------------------
# plot mse
# --------------------------------------
mse8rdouble <- mse_run(om = ss_run_Tier3_fixm2_double, em = ss_run_M_Tier3_double, nsim = 1, assessment_period = 1, sampling_period = c(1,1,1,1,1,1,2), simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL, regenerate_past = TRUE)

mse_list <- list(mse8rdouble)

MSE_names <- c("Tests/Regen/Test8 - SS Fix (age-invariant) M OM, Est M EM (8 times sampling)/")

for(i in 1:length(MSE_names)){dir.create(MSE_names[i], recursive = TRUE)}

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
  plot_f(mod_list, incl_proj = TRUE, model_names = model_names, file = MSE_names[i], line_col = line_col)
  plot_index(mod_list, file = MSE_names[i], line_col = line_col)
  plot_catch(mod_list, incl_proj = TRUE, file = MSE_names[i], line_col = line_col)
  
  
  Mort <- sapply(mod_list, function(x) x$quantities$M[,1,1,1])
  
  Year = 2017:2060
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
  
  
  Year = 2017:2060
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

# --------------------------------------
# Turn off rec devs
# --------------------------------------
# - Add more sampling
fut_sample = 4
ss_run_Tier3_fixm2_double <- ss_run_Tier3_fixm2
ss_run_Tier3_fixm2_double$data_list$srv_biom$Log_sd <- ss_run_Tier3_fixm2_double$data_list$srv_biom$Log_sd / fut_sample
ss_run_Tier3_fixm2_double$data_list$comp_data$Sample_size <- ss_run_Tier3_fixm2_double$data_list$comp_data$Sample_size * fut_sample

ss_run_M_Tier3_double <- ss_run_M_Tier3
ss_run_M_Tier3_double$data_list$srv_biom$Log_sd <- ss_run_M_Tier3_double$data_list$srv_biom$Log_sd / fut_sample
ss_run_M_Tier3_double$data_list$comp_data$Sample_size <- ss_run_M_Tier3_double$data_list$comp_data$Sample_size * fut_sample

sim_mod_noRdev <- ss_run_Tier3_fixm2_double
hind_nyrs <- length(sim_mod_noRdev$data_list$styr:sim_mod_noRdev$data_list$endyr)

# Replace rec devs
for(sp in 1:3){
  rec_dev <- rep(log(mean(sim_mod_noRdev$quantities$R[sp,1:hind_nyrs])) - sim_mod_noRdev$estimated_params$ln_mean_rec[sp],
                 times = ncol(sim_mod_noRdev$quantities$R))
  
  # - Update OM with devs
  sim_mod_noRdev$estimated_params$rec_dev[sp,1:hind_nyrs] <- replace(
    sim_mod_noRdev$estimated_params$rec_dev[sp,1:hind_nyrs],
    values =  rec_dev)
}


# -- Update model with no rec devs
sim_mod_noRdev <- Rceattle::fit_mod(data_list = sim_mod_noRdev$data_list,
                                    inits = sim_mod_noRdev$estimated_params,
                                    estimateMode = 2, # Run projection only
                                    HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                    FsprTarget = 0.4, # F40%
                                                    FsprLimit = 0.35, # F35%
                                                    Plimit = 0.2, # No fishing when SB<SB20
                                                    Alpha = 0.2),
                                    msmMode = 0, # Single species mode
                                    verbose = 1)
plot_recruitment(sim_mod_noRdev, incl_proj = TRUE)
sim_mod_noRdev$quantities$M1[,1,]


# - Simulate index and comp data and updatae EM
sim_dat_dev <- sim_mod(ss_run_Tier3_fixm2_double, simulate = FALSE)
sim_dat <- sim_mod(sim_mod_noRdev, simulate = FALSE)

ss_run_M_Tier3_double$data_list$srv_biom <- sim_dat$srv_biom
ss_run_M_Tier3_double$data_list$comp_data <- sim_dat$comp_data

# Turn off rec dev
inits <- ss_run_M_Tier3_double$estimated_params
inits$rec_dev <- replace(inits$rec_dev, values = 0)

map <- build_map(data_list = ss_run_M_Tier3_double$data_list, params = inits, debug = FALSE, random_rec = FALSE)
map$mapList$rec_dev <- replace(map$mapList$rec_dev, values = NA)
map$mapFactor$rec_dev <- as.factor(map$mapList$rec_dev)

# Restimate and estimate M
ss_run_M_Tier3_double <- fit_mod(
  data_list = ss_run_M_Tier3_double$data_list,
  inits = inits,
  map =  map,
  bounds = NULL,
  file = NULL,
  estimateMode = 0, # Run hindcast and projection, otherwise debug
  HCR = build_hcr(HCR = ss_run_M_Tier3_double$data_list$HCR, # Tier3 HCR
                  DynamicHCR = ss_run_M_Tier3_double$data_list$DynamicHCR,
                  FsprTarget = ss_run_M_Tier3_double$data_list$FsprTarget,
                  FsprLimit = ss_run_M_Tier3_double$data_list$FsprLimit,
                  Ptarget = ss_run_M_Tier3_double$data_list$Ptarget,
                  Plimit = ss_run_M_Tier3_double$data_list$Plimit,
                  Alpha = ss_run_M_Tier3_double$data_list$Alpha,
                  Pstar = ss_run_M_Tier3_double$data_list$Pstar,
                  Sigma = ss_run_M_Tier3_double$data_list$Sigma
  ),
  random_rec = ss_run_M_Tier3_double$data_list$random_rec,
  niter = ss_run_M_Tier3_double$data_list$niter,
  msmMode = ss_run_M_Tier3_double$data_list$msmMode,
  avgnMode = ss_run_M_Tier3_double$data_list$avgnMode,
  minNByage = ss_run_M_Tier3_double$data_list$minNByage,
  suitMode = ss_run_M_Tier3_double$data_list$suitMode,
  phase = "default",
  updateM1 = FALSE,
  loopnum = 3,
  getsd = FALSE,
  verbose = 1)

plot_recruitment(ss_run_M_Tier3_double, incl_proj = TRUE)
ss_run_M_Tier3_double$quantities$M1[,1,]
ss_run_M_Tier3_double$estimated_params$init_dev[,]



# --------------------------------------
# Turn off rec devs and init devs
# --------------------------------------
# - Add more sampling
fut_sample = 4
ss_run_Tier3_fixm2_double <- ss_run_Tier3_fixm2
ss_run_Tier3_fixm2_double$data_list$srv_biom$Log_sd <- ss_run_Tier3_fixm2_double$data_list$srv_biom$Log_sd / fut_sample
ss_run_Tier3_fixm2_double$data_list$comp_data$Sample_size <- ss_run_Tier3_fixm2_double$data_list$comp_data$Sample_size * fut_sample

ss_run_M_Tier3_double <- ss_run_M_Tier3
ss_run_M_Tier3_double$data_list$srv_biom$Log_sd <- ss_run_M_Tier3_double$data_list$srv_biom$Log_sd / fut_sample
ss_run_M_Tier3_double$data_list$comp_data$Sample_size <- ss_run_M_Tier3_double$data_list$comp_data$Sample_size * fut_sample

sim_mod_noRdev <- ss_run_Tier3_fixm2_double
hind_nyrs <- length(sim_mod_noRdev$data_list$styr:sim_mod_noRdev$data_list$endyr)

# Replace rec devs
sim_mod_noRdev$estimated_params$rec_dev <- replace(sim_mod_noRdev$estimated_params$rec_dev, values = 0)
sim_mod_noRdev$estimated_params$init_dev <- replace(sim_mod_noRdev$estimated_params$init_dev, values = 0)

# -- Update model with no rec devs
map <- build_map(data_list = sim_mod_noRdev$data_list, params = sim_mod_noRdev$estimated_params, debug = FALSE, random_rec = FALSE)
map$mapList$rec_dev <- replace(map$mapList$rec_dev, values = NA)
map$mapFactor$rec_dev <- as.factor(map$mapList$rec_dev)

map$mapList$init_dev <- replace(map$mapList$init_dev, values = NA)
map$mapFactor$init_dev <- as.factor(map$mapList$init_dev)

sim_mod_noRdev <- Rceattle::fit_mod(data_list = sim_mod_noRdev$data_list,
                                    inits = sim_mod_noRdev$estimated_params,
                                    map = map,
                                    estimateMode = 0, # Run projection only
                                    HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                    FsprTarget = 0.4, # F40%
                                                    FsprLimit = 0.35, # F35%
                                                    Plimit = 0.2, # No fishing when SB<SB20
                                                    Alpha = 0.2),
                                    msmMode = 0, # Single species mode
                                    verbose = 1)
plot_recruitment(sim_mod_noRdev, incl_proj = TRUE)
sim_mod_noRdev$quantities$M1[,1,]
sim_mod_noRdev$estimated_params$rec_dev
sim_mod_noRdev$estimated_params$init_dev

# - Simulate index and comp data and updatae EM
sim_dat_dev <- sim_mod(ss_run_Tier3_fixm2_double, simulate = FALSE)
sim_dat <- sim_mod(sim_mod_noRdev, simulate = FALSE)

ss_run_M_Tier3_double$data_list$srv_biom <- sim_dat$srv_biom
ss_run_M_Tier3_double$data_list$comp_data <- sim_dat$comp_data


# Turn off rec dev and init dev in EM
inits <- ss_run_M_Tier3_double$estimated_params
inits$rec_dev <- replace(inits$rec_dev, values = 0)
inits$init_dev <- replace(inits$init_dev, values = 0)

map <- build_map(data_list = ss_run_M_Tier3_double$data_list, params = inits, debug = FALSE, random_rec = FALSE)
map$mapList$rec_dev <- replace(map$mapList$rec_dev, values = NA)
map$mapFactor$rec_dev <- as.factor(map$mapList$rec_dev)

map$mapList$init_dev <- replace(map$mapList$init_dev, values = NA)
map$mapFactor$init_dev <- as.factor(map$mapList$init_dev)

# Restimate and estimate M
ss_run_M_Tier3_double <- fit_mod(
  data_list = ss_run_M_Tier3_double$data_list,
  inits = inits,
  map =  map,
  bounds = NULL,
  file = NULL,
  estimateMode = 0, # Run hindcast and projection, otherwise debug
  HCR = build_hcr(HCR = ss_run_M_Tier3_double$data_list$HCR, # Tier3 HCR
                  DynamicHCR = ss_run_M_Tier3_double$data_list$DynamicHCR,
                  FsprTarget = ss_run_M_Tier3_double$data_list$FsprTarget,
                  FsprLimit = ss_run_M_Tier3_double$data_list$FsprLimit,
                  Ptarget = ss_run_M_Tier3_double$data_list$Ptarget,
                  Plimit = ss_run_M_Tier3_double$data_list$Plimit,
                  Alpha = ss_run_M_Tier3_double$data_list$Alpha,
                  Pstar = ss_run_M_Tier3_double$data_list$Pstar,
                  Sigma = ss_run_M_Tier3_double$data_list$Sigma
  ),
  random_rec = ss_run_M_Tier3_double$data_list$random_rec,
  niter = ss_run_M_Tier3_double$data_list$niter,
  msmMode = ss_run_M_Tier3_double$data_list$msmMode,
  avgnMode = ss_run_M_Tier3_double$data_list$avgnMode,
  minNByage = ss_run_M_Tier3_double$data_list$minNByage,
  suitMode = ss_run_M_Tier3_double$data_list$suitMode,
  phase = "default",
  updateM1 = FALSE,
  loopnum = 3,
  getsd = FALSE,
  verbose = 1)

plot_recruitment(ss_run_M_Tier3_double, incl_proj = TRUE)
ss_run_M_Tier3_double$quantities$M1[,1,]
ss_run_M_Tier3_double$estimated_params$init_dev[,]
ss_run_M_Tier3_double$estimated_params$rec_dev[,]




# --------------------------------------
# Profile R0
# --------------------------------------
R0_seq <- seq(from = 5, to = 25, by = 1)
mod_list <- list()
for(i in 1:length(R0_seq)){
  mod_tmp <- ss_run_M_Tier3_double
  mod_tmp$estimated_params$ln_mean_rec[1] <- R0_seq[i]
  
  # Fix R0 in map
  map$mapList$ln_mean_rec[1] <- NA
  map$mapFactor$ln_mean_rec <- as.factor(map$mapList$ln_mean_rec)
  
  # Estimate
  mod_list[[i]] <- fit_mod(
    data_list = mod_tmp$data_list,
    inits = mod_tmp$estimated_params,
    map =  map,
    bounds = NULL,
    file = NULL,
    estimateMode = 0, # Run hindcast and projection, otherwise debug
    HCR = build_hcr(HCR = mod_tmp$data_list$HCR, # Tier3 HCR
                    DynamicHCR = mod_tmp$data_list$DynamicHCR,
                    FsprTarget = mod_tmp$data_list$FsprTarget,
                    FsprLimit = mod_tmp$data_list$FsprLimit,
                    Ptarget = mod_tmp$data_list$Ptarget,
                    Plimit = mod_tmp$data_list$Plimit,
                    Alpha = mod_tmp$data_list$Alpha,
                    Pstar = mod_tmp$data_list$Pstar,
                    Sigma = mod_tmp$data_list$Sigma
    ),
    random_rec = mod_tmp$data_list$random_rec,
    niter = mod_tmp$data_list$niter,
    msmMode = mod_tmp$data_list$msmMode,
    avgnMode = mod_tmp$data_list$avgnMode,
    minNByage = mod_tmp$data_list$minNByage,
    suitMode = mod_tmp$data_list$suitMode,
    phase = "default",
    updateM1 = FALSE,
    loopnum = 3,
    getsd = FALSE,
    verbose = 1)
}


jnll <- sapply(mod_list, function(x) x$quantities$jnll)
jnll_row <- sapply(mod_list, function(x) rowSums(x$quantities$jnll_comp))
M1 <- sapply(mod_list, function(x) x$quantities$M1[1,1,1])
jnll_row[5,] = jnll_row[5,] + jnll_row[8,]
jnll_row[11,] = jnll_row[11,] + jnll_row[12,]
rows <- c(1,2,3,5,11)

plot(x = R0_seq, y = (jnll-min(jnll))/(max(jnll)-min(jnll)), ylab = "Relative nll or M", xlab = "log-R0", type = "l", lwd = 4)
for(j in 1:length(rows)){
  i = rows[j]
  lines(x = R0_seq, y = (jnll_row[i,]-min(jnll_row[i,]))/(max(jnll_row[i,])-min(jnll_row[i,])), col = j+1, lwd = 3)
}
lines(x = R0_seq, y = M1, col = 1, lwd = 3, lty = 2)
lines(x = R0_seq, y = (jnll-min(jnll))/(max(jnll)-min(jnll)), lwd = 3)
abline(h = 0.3, lty = 1, col = "grey60", lwd = 2)
abline(v = R0_seq[13], lty = 3)
text(y = 0.4, x = R0_seq[13], "M/R0 at min jnll", adj = c(-0.1,1))

legend("top", c("NLL", rownames(jnll_row)[rows], "Estimated M", "True M"), bty = "n", col = c(1:(length(rows)+1), 1, "grey60"), lty = c(rep(1,(length(rows)+1)), 2,1), lwd = rep(3,(length(rows)+2)))



