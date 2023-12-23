library(Rceattle)
source("R/GOA_condition_models_1977.R")
om <- em <- ss_run_M_Tier3

# - Simulate index and comp data and updatae EM
sim_dat <- sim_mod(om, simulate = FALSE)

em$data_list$srv_biom <- sim_dat$srv_biom
em$data_list$comp_data <- sim_dat$comp_data
# em$data_list$fsh_biom <- sim_dat$fsh_biom

# fut_sample = 4
# em$data_list$srv_biom$Log_sd <- em$data_list$srv_biom$Log_sd / fut_sample
# em$data_list$comp_data$Sample_size <- em$data_list$comp_data$Sample_size * fut_sample

# Restimate

em <- Rceattle::fit_mod(
  data_list = em$data_list,
  inits = em$estimated_params,
  map =  NULL,
  bounds = NULL,
  file = NULL,
  estimateMode = 0, # Run projection only
  HCR = build_hcr(HCR = em$data_list$HCR, # Tier3 HCR
                  DynamicHCR = em$data_list$DynamicHCR,
                  FsprTarget = em$data_list$FsprTarget,
                  FsprLimit = em$data_list$FsprLimit,
                  Ptarget = em$data_list$Ptarget,
                  Plimit = em$data_list$Plimit,
                  Alpha = em$data_list$Alpha,
                  Pstar = em$data_list$Pstar,
                  Sigma = em$data_list$Sigma
  ),
  recFun = build_srr(srr_fun = em$data_list$srr_fun,
                     srr_pred_fun  = em$data_list$srr_pred_fun ,
                     proj_mean_rec  = em$data_list$proj_mean_rec ,
                     srr_est_mode  = em$data_list$srr_est_mode ,
                     srr_prior_mean  = em$data_list$srr_prior_mean,
                     srr_prior_sd   = em$data_list$srr_prior_sd,
                     Bmsy_lim = em$data_list$Bmsy_lim),
  M1Fun =     build_M1(M1_model= em$data_list$M1_model,
                       updateM1 = FALSE,
                       M1_use_prior = em$data_list$M1_use_prior,
                       M2_use_prior = em$data_list$M2_use_prior,
                       M1_prior_mean = em$data_list$M1_prior_mean,
                       M1_prior_sd = em$data_list$M1_prior_sd),
  meanyr = em$data_list$meanyr,
  random_rec = em$data_list$random_rec,
  niter = em$data_list$niter,
  msmMode = em$data_list$msmMode,
  avgnMode = em$data_list$avgnMode,
  minNByage = em$data_list$minNByage,
  suitMode = em$data_list$suitMode,
  initMode = em$data_list$initMode,
  phase = NULL,
  loopnum = 3,
  getsd = FALSE,
  verbose = 0)

plot_biomass(list(om, em))
plot_recruitment(list(om, em))
plot_index(list(om, em))

om$quantities$Flimit
em$quantities$Flimit

em$quantities$M[,,1,1]
om$quantities$M[,,1,1]

check <- data.frame(Parname = names(om$estimated_params), Off = length(names(om$estimated_params)))

for(i in 1:length(om$estimated_params)){
  check[i,2] <- sum(om$estimated_params[[i]] != em$estimated_params[[i]], na.rm = TRUE)
}

check2 <- data.frame(Parname = names(om$data_list), Off = length(names(om$data_list)))

for(i in 1:length(om$data_list)){
  check2[i,2] <- sum(om$data_list[[i]] != em$data_list[[i]], na.rm = TRUE)
}


# Run test MSE and check M
mse <- mse_run(om = om, em = em, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL)


mod_list <- c(mse[[1]],list(mse[[2]]))
Mort <- sapply(mod_list, function(x) x$quantities$M[,1,1,1])

Year = 2017:2060
species = c("Pollock", "ATF", "Cod")
par(mfrow = c(3,1))
for(k in 1:3){
  plot(y = Mort[k,-ncol(Mort)], x = Year, type = "l", main = species[k], ylab = "Mortality", ylim = range(Mort[k,]))
  abline(h = Mort[k,ncol(Mort)], lty = 2)
  legend("topright", c("EM", "OM"), lty = c(1,2), bty = "n")
}

em$quantities$M1[,1,1]/om$quantities$M1[,1,1]


# Profile
M_vec <- seq(0.98,1.05, 0.00025)
em_list <- list()
for(i in 1:length(M_vec)){
  em$data_list$est_M1 <- rep(0,3)
  inits <- om$estimated_params
  inits$ln_M1 <- log(exp(inits$ln_M1) * M_vec[i])
  
  # Restimate
  em_list[[i]] <- fit_mod(
    data_list = em$data_list,
    inits = inits,
    map =  NULL,
    bounds = NULL,
    file = NULL,
    estimateMode = ifelse(em$data_list$estimateMode < 3, 0, em$data_list$estimateMode), # Run hindcast and projection, otherwise debug
    HCR = build_hcr(HCR = em$data_list$HCR, # Tier3 HCR
                    DynamicHCR = em$data_list$DynamicHCR,
                    FsprTarget = em$data_list$FsprTarget,
                    FsprLimit = em$data_list$FsprLimit,
                    Ptarget = em$data_list$Ptarget,
                    Plimit = em$data_list$Plimit,
                    Alpha = em$data_list$Alpha,
                    Pstar = em$data_list$Pstar,
                    Sigma = em$data_list$Sigma
    ),
    random_rec = em$data_list$random_rec,
    niter = em$data_list$niter,
    msmMode = em$data_list$msmMode,
    avgnMode = em$data_list$avgnMode,
    minNByage = em$data_list$minNByage,
    suitMode = em$data_list$suitMode,
    phase = "default",
    updateM1 = FALSE,
    loopnum = 3,
    getsd = FALSE,
    verbose = 0)
}


jnll_comp <- sapply(em_list, function(x) rowSums(x$quantities$jnll_comp))
jnll_comp[5,] <- jnll_comp[5,] + jnll_comp[8,]
jnll_comp <- rbind(jnll_comp , sapply(em_list, function(x) (x$quantities$jnll)))
jnll_comp_standard <- jnll_comp

# Standardize to 0-1
for(j  in 1:nrow(jnll_comp)){
  jnll_comp_standard[j,] <- (jnll_comp[j,] - min(jnll_comp[j,])) / (range(jnll_comp[j,])[2] - range(jnll_comp[j,])[1])
}

# Plot
care_rows <- c(1,2,3,5,11:13,19)
plot(NA, NA, ylim = c(0,0.2), xlim = range(M_vec))
for(j  in 1:length(care_rows)){
  lines(y = jnll_comp_standard[care_rows[j],], x = M_vec, col = j, lwd = 3)
}
legend("topright", rownames(jnll_comp_standard)[care_rows], col = 1:length(care_rows), bty = "n", lty = 1, lwd = 3)
abline(v = 1, lty = 2, lwd = 2)


M1 <- sapply(em_list, function(x) (x$quantities$M1[,1,1]))



