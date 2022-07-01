################################################
# Set-up
################################################
source("R/GOA_condition_models_1977.R")

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
# Management strategy evaluation
################################################
### OMS
# 1. Single-species estimate M
# 2. Multi-species type II
om_list <- list(ss_run_Tier3, ss_run_M_Tier3, ms_run_f25)
om_names = c("SS_OM", "SSM_OM", "MS_OM")

## Rec scenarios
# 1. Constant
# 2. Linear increase 1.5
# 3. Linear decrease
rec_scen <- list(0)

### Management strategies
## EM
# 1. Single-species fix M
# 2. Single-species estimate M

# HCR
# 1, NPFMC Tier 3 HCR
# 1b. NPFMC Tier 3 dynamic HCR 
# 2. PFMC Category 1 40-10 HCR
# 2b. PFMC Category 1 40-10 dynamic HCR
# 3. SESSF Tier 1 HCR
# 3b. SESSF Tier 1 dynamic HCR
# 4. Average F
# 5. Equilibrium F_(40%)
em_hcr_list <- list(ss_run_Tier3, ss_run_dynamicTier3, ss_run_Cat1, ss_run_dynamicCat1, ss_run_Tier1, ss_run_dynamicTier1, ss_run_Fspr, ss_run_AvgF, # Fixed M
                    ss_run_M_Tier3, ss_run_M_dynamicTier3, ss_run_M_Cat1, ss_run_M_dynamicCat1, ss_run_M_Tier1, ss_run_M_dynamicTier1, ss_run_M_Fspr, ss_run_M_AvgF # Estimate M
)

em_hcr_names <- c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", "SS_fixM_AvgF_EM", # Fixed M
                  "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM", "SS_estM_AvgF_EM")


### Run MSEs
## Loop across OMs,

rec = 1
om = 3
em = 16
recname = list("ConstantR")
system = "GOA1977"

# Update avg F given model fit to regenerated data
if(em_hcr_list[[em]]$data_list$HCR == 2){
  # - Simulate index and comp data and updatae EM
  sim_dat <- sim_mod(om_list[[om]], simulate = TRUE)
  
  em_hcr_list[[em]]$data_list$srv_biom <- sim_dat$srv_biom
  em_hcr_list[[em]]$data_list$comp_data <- sim_dat$comp_data
  
  # - Restimate
  em_hcr_list[[em]] <- fit_mod(
    data_list = em_hcr_list[[em]]$data_list,
    inits = em_hcr_list[[em]]$estimated_params,
    map =  NULL,
    bounds = NULL,
    file = NULL,
    estimateMode = 0,
    random_rec = em_hcr_list[[em]]$data_list$random_rec,
    niter = em_hcr_list[[em]]$data_list$niter,
    msmMode = em_hcr_list[[em]]$data_list$msmMode,
    avgnMode = em_hcr_list[[em]]$data_list$avgnMode,
    minNByage = em_hcr_list[[em]]$data_list$minNByage,
    suitMode = em_hcr_list[[em]]$data_list$suitMode,
    phase = "default",
    updateM1 = FALSE,
    loopnum = 3,
    getsd = FALSE,
    verbose = 0)
  
  # - Get avg F
  avg_F <- (exp(em_hcr_list[[em]]$estimated_params$ln_mean_F+em_hcr_list[[em]]$estimated_params$F_dev)) # Average F from last 5 years
  avg_F <- rowMeans(avg_F[,(ncol(avg_F)-4) : ncol(avg_F)])
  avg_F <- data.frame(avg_F = avg_F, spp = em_hcr_list[[em]]$data_list$fleet_control$Species)
  avg_F <- avg_F %>% 
    group_by(spp) %>%
    summarise(avg_F = sum(avg_F)) %>%
    arrange(spp)
  
  # - Update model
  em_hcr_list[[em]] <- Rceattle::fit_mod(data_list = em_hcr_list[[em]]$data_list,
                                         inits = em_hcr_list[[em]]$estimated_params,
                                         estimateMode = 2, # Estimate nothing
                                         HCR = build_hcr(HCR = 2, # Input F
                                                         FsprTarget = avg_F$avg_F 
                                         ),
                                         phase = "default",
                                         msmMode = 0, # Single species mode
                                         verbose = 1, updateM1 = FALSE)
}

# Update OM if single-species to have save HCR as EM to calculate performance metrics
if(om_list[[om]]$data_list$msmMode == 0){
  om_list[[om]] <- Rceattle::fit_mod(data_list = om_list[[om]]$data_list,
                                     inits = om_list[[om]]$estimated_params,
                                     estimateMode = 2, # Run projection only
                                     HCR = build_hcr(HCR = em_hcr_list[[em]]$data_list$HCR, # Tier3 HCR
                                                     DynamicHCR = em_hcr_list[[em]]$data_list$DynamicHCR,
                                                     FsprTarget = em_hcr_list[[em]]$data_list$FsprTarget,
                                                     FsprLimit = em_hcr_list[[em]]$data_list$FsprLimit,
                                                     Ptarget = em_hcr_list[[em]]$data_list$Ptarget,
                                                     Plimit = em_hcr_list[[em]]$data_list$Plimit,
                                                     Alpha = em_hcr_list[[em]]$data_list$Alpha,
                                                     Pstar = em_hcr_list[[em]]$data_list$Pstar,
                                                     Sigma = em_hcr_list[[em]]$data_list$Sigma
                                     ),
                                     msmMode = 0, # Single species mode
                                     verbose = 1, updateM1 = FALSE)
}

# Run MSE
### Set up parallel processing
library(foreach)
library(doParallel)

cores = detectCores() - 2
registerDoParallel(cores)

stime <- system.time({
  mse <- foreach(seed = 666:(666+200)) %dopar% { # Rec trends
    
    
    # Load libraries
    library(Rceattle)
    library(dplyr)
    mse <- mse_run(om = om_list[[om]], em = em_hcr_list[[em]], 
                   nsim = 1, 
                   assessment_period = 1, sampling_period = sampling_period, 
                   simulate_data = TRUE, sample_rec = TRUE, 
                   rec_trend = rec_scen[[rec]],
                   seed = seed,
                   cap = NULL, 
                   dir = paste0("Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/",recname[rec],"/No cap"), 
                   file = NULL,
                   regenerate_past = TRUE)
  }
})


# When you're done, clean up the cluster
stopImplicitCluster()
