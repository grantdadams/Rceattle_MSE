library(Rceattle)


# -- NPFMC Tier 3
data("GOA2018SS")

data("BS2017SS")

# SS M
ss_run_M <- Rceattle::fit_mod(data_list = BS2017SS,
                              inits = NULL, # Initial parameters from ss_run_M
                              estimateMode = 0, # Run projection only
                              M1Fun = build_M1(M1_model = c(1,1,1),
                                               M1_use_prior = FALSE,
                                               M2_use_prior = FALSE),
                              msmMode = 0, # Single species mode
                              phase = "default",
                              initMode = 1,
                              verbose = 1)

# Ratio of F across Pcod fleets
avg_F <- (exp(ss_run_M$estimated_params$ln_mean_F+ss_run_M$estimated_params$F_dev)) # Average F from last 2 years
avg_F <- rowMeans(avg_F[,(ncol(avg_F)-2) : ncol(avg_F)])
f_ratio <- avg_F[14:16]
f_ratio <- f_ratio/sum(f_ratio)

# Adjust future F proportion to each fleet
ss_run_M$data_list$fleet_control$proj_F_prop <- rep(1,7)# <- c(rep(0, 7), 1,0,0,1, 0,0, f_ratio)


# Tier 3
ss_run_M_Tier3 <- Rceattle::fit_mod(data_list = ss_run_M$data_list,
                                    inits = ss_run_M$estimated_params, # Initial parameters from ss_run_M
                                    estimateMode = 0, # Run projection only
                                    M1Fun = build_M1(M1_model = c(1,1,1),
                                                     M1_use_prior = FALSE,
                                                     M2_use_prior = FALSE),
                                    HCR = build_hcr(HCR = 5, # Tier3 HCR
                                                    FsprTarget = 0.4, # F40%
                                                    FsprLimit = 0.35, # F35%
                                                    Plimit = c(0.2, 0, 0.2), # No fishing when SB<SB20
                                                    Alpha = 0.05),
                                    msmMode = 0, # Single species mode
                                    initMode = 1,
                                    verbose = 1)

om <- em <- ss_run_M_Tier3

# - Simulate index and comp data and updatae EM
sim_dat <- sim_mod(om, simulate = FALSE)

em$data_list$srv_biom <- sim_dat$srv_biom
em$data_list$comp_data <- sim_dat$comp_data

# Restimate
em <- fit_mod(
  data_list = em$data_list,
  inits = em$estimated_params,
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
  recFun = build_srr(srr_fun = em$data_list$srr_fun,
                     srr_pred_fun  = em$data_list$srr_pred_fun ,
                     proj_mean_rec  = em$data_list$proj_mean_rec ,
                     srr_est_mode  = em$data_list$srr_est_mode ,
                     srr_prior_mean  = em$data_list$srr_prior_mean,
                     srr_prior_sd   = em$data_list$srr_prior_sd ),
  M1Fun =     build_M1(M1_model= em$data_list$M1_model,
                       updateM1 = FALSE,
                       M1_use_prior = em$data_list$M1_use_prior,
                       M2_use_prior = em$data_list$M2_use_prior,
                       M1_prior_mean = em$data_list$M1_prior_mean,
                       M1_prior_sd = em$data_list$M1_prior_sd),
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

em$quantities$M1[,,1]/om$quantities$M1[,,1]

check <- data.frame(Parname = names(om$estimated_params), Off = length(names(om$estimated_params)))

for(i in 1:length(om$estimated_params)){
  check[i,2] <- sum(om$estimated_params[[i]] != em$estimated_params[[i]], na.rm = TRUE)
}

check2 <- data.frame(Parname = names(om$data_list), Off = length(names(om$data_list)))

for(i in 1:length(om$data_list)){
  check2[i,2] <- sum(om$data_list[[i]] != em$data_list[[i]], na.rm = TRUE)
}


# Maybe init mode is the problem
#FIXME at initialF to mapped out paramters in MSE function


# Run test MSE and check M
mse <- mse_run_parallel(om = om, em = em, nsim = 1, assessment_period = 1, sampling_period = 1, simulate_data = FALSE, sample_rec = FALSE, dir = NULL, file = NULL)


mod_list <- c(mse[[1]]$EM,list(mse[[1]]$OM))
plot_biomass(mod_list)
Mort <- sapply(mod_list, function(x) x$quantities$M[,1,1,1])

Year = 2017:2050
species = c("Pollock", "ATF", "Cod")
par(mfrow = c(3,1))
for(k in 1:3){
  plot(y = Mort[k,-ncol(Mort)], x = Year, type = "l", main = species[k], ylab = "Mortality", ylim = range(Mort[k,]))
  abline(h = Mort[k,ncol(Mort)], lty = 2)
  legend("topright", c("EM", "OM"), lty = c(1,2), bty = "n")
}

em$quantities$M1[,1,1]/om$quantities$M1[,1,1]
mod_list[[33]]$quantities$M1[,1,1]/om$quantities$M1[,1,1]

# Profile
M_vec <- seq(0.98,1.05, 0.00025)
em_list <- list()
for(i in 1:length(M_vec)){
  em$data_list$M1_model <- rep(0,3)
  inits <- om$estimated_params
  inits$ln_M1 <- log(exp(inits$ln_M1) * M_vec[i])
  
  # Restimate
  em_list[[i]] <- fit_mod(
    data_list = em$data_list,
    inits = inits,
    map =  NULL,
    bounds = NULL,
    file = NULL,
    estimateMode = 0, # Run hindcast and projection, otherwise debug
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
    loopnum = 3,
    getsd = FALSE,
    verbose = 0)
}


jnll_comp <- sapply(em_list, function(x) rowSums(x$quantities$jnll_comp))
jnll_comp[5,] <- jnll_comp[5,] + jnll_comp[8,]
jnll_comp <- rbind(jnll_comp , sapply(em_list, function(x) (x$quantities$jnll)))
jnll_comp <- rbind(jnll_comp , colSums(jnll_comp[1:3,]))
jnll_comp_standard <- jnll_comp

# Standardize to 0-1
for(j  in 1:nrow(jnll_comp)){
  jnll_comp_standard[j,] <- (jnll_comp[j,] - min(jnll_comp[j,])) / (range(jnll_comp[j,])[2] - range(jnll_comp[j,])[1])
}

# Plot
care_rows <- c(1,2,3,5,11:13,20,21)
plot(NA, NA, ylim = c(0,1), xlim = range(M_vec))
for(j  in 1:length(care_rows)){
  lines(y = jnll_comp_standard[care_rows[j],], x = M_vec, col = j, lwd = 3)
}
legend("topright", rownames(jnll_comp_standard)[care_rows], col = 1:length(care_rows), bty = "n", lty = 1, lwd = 3)
abline(v = 1, lty = 2, lwd = 2)


M1 <- sapply(em_list, function(x) (x$quantities$M1[,1,1]))



