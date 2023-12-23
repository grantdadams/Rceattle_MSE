################################################
# Set-up
################################################
#source("R/GOA_condition_models_1977.R")
#source("R/GOA_condition_ricker_models_1977.R")
library(Rceattle)
library(tidyr)

ms_run$quantities$depletionSSB <- ms_run$quantities$biomassSSB/ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)]
ms_run_ricker$quantities$depletionSSB <- ms_run_ricker$quantities$biomassSSB/ms_run_ricker$quantities$biomassSSB[,ncol(ms_run_ricker$quantities$biomassSSB)]


################################################
# Management strategy evaluation
################################################
### OMS
# 1. Single-species fix M
# 2. Single-species estimate M
# 3. Multi-species type II
om_list <- list(ss_run_Tier3, ss_run_M_Tier3, ms_run_f25, ss_run_ricker_Tier3, ss_run_ricker_M_Tier3, ms_run_ricker_f25)
projected_OM_no_F <- list(ss_run, ss_run_M, ms_run, ss_run_ricker, ss_run_ricker_M, ms_run_ricker)
om_names = c("SS_OM", "SSM_OM", "MS_OM", "SS_Ricker_OM", "SSM_Ricker_OM", "MS_Ricker_OM")
om_names_print = c("SS fix M", "SS est M", "MS", "SS fix M Ricker", "SS est M Ricker", "MS Ricker")

# Lists to update reference points in OM (can remove later)
# - No ricker
om_hcr_list_fixM <- list(ss_run_Tier3, ss_run_dynamicTier3, ss_run_Cat1, ss_run_dynamicCat1, ss_run_Tier1, ss_run_dynamicTier1, ss_run_Fspr, ss_run_AvgF) # Fixed M
om_hcr_list_fixM = c(om_hcr_list_fixM, om_hcr_list_fixM)

om_hcr_list_estM <- list(ss_run_M_Tier3, ss_run_M_dynamicTier3, ss_run_M_Cat1, ss_run_M_dynamicCat1, ss_run_M_Tier1, ss_run_M_dynamicTier1, ss_run_M_Fspr, ss_run_M_AvgF) # Estimate M
om_hcr_list_estM = c(om_hcr_list_estM, om_hcr_list_estM)

# - Ricker
om_hcr_list_ricker_fixM <- list(ss_run_ricker_Tier3, ss_run_ricker_dynamicTier3, ss_run_ricker_Cat1, ss_run_ricker_dynamicCat1, ss_run_ricker_Tier1, ss_run_ricker_dynamicTier1, ss_run_ricker_Fspr, ss_run_ricker_AvgF) # Fixed M
om_hcr_list_ricker_fixM = c(om_hcr_list_ricker_fixM, om_hcr_list_ricker_fixM)

om_hcr_list_ricker_estM <- list(ss_run_ricker_M_Tier3, ss_run_ricker_M_dynamicTier3, ss_run_ricker_M_Cat1, ss_run_ricker_M_dynamicCat1, ss_run_ricker_M_Tier1, ss_run_ricker_M_dynamicTier1, ss_run_ricker_M_Fspr, ss_run_ricker_M_AvgF) # Estimate M
om_hcr_list_ricker_estM = c(om_hcr_list_ricker_estM, om_hcr_list_ricker_estM)


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


om = 2
em = 9
rec = 1
species = c(1,3,2)
recname = "ConstantR"

system = "GOA1977"
recname = "ConstantR"
om_list_no_F = projected_OM_no_F[1:3]
om_names = om_names[1:3]
om_hcr_list_fixM = om_hcr_list_fixM
om_hcr_list_estM = om_hcr_list_estM
em_hcr_names = em_hcr_names



################################################
# SUMMARY
################################################

print(paste0("OM ", om, ": EM ", em))

# STEP 1 -- Load MSE
if(!dir.exists(paste0("Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/",recname[rec],"/No cap"))){
  stop(paste0("Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/",recname[rec],"/No cap DOES NOT EXIST"))
}
mse3 <- load_mse(dir = paste0("Runs/", system,"/", om_names[om],"/", em_hcr_names[em],"/",recname[rec],"/No cap"), file = NULL)
MSE_names <- paste0(om_names[om],"__", em_hcr_names[em], "_", recname[rec])


# STEP 2 -- Update Ftarget, Flimit, and depletion for OMs
for(j in 1:length(mse3)){
  
  # - SINGLE-SPECIES
  if(mse3[[j]]$OM$data_list$msmMode == 0){
    
    # - Update depletion (proj mean rec is turned off in the MSE function)
    mse3[[j]]$OM$quantities$SB0 <- om_list_no_F[[om]]$quantities$SB0[,ncol(om_list_no_F[[om]]$quantities$SB0)]
    mse3[[j]]$OM$quantities$depletionSSB <- mse3[[j]]$OM$quantities$biomassSSB/mse3[[j]]$OM$quantities$SB0 
    
    # -- Dynamic BRPs
    if(mse3[[j]]$EM[[1]]$data_list$DynamicHCR == 1){
      mse3[[j]]$OM$quantities$depletionSSB = mse3[[j]]$OM$quantities$biomassSSB/mse3[[j]]$OM$quantities$DynamicSB0
      mse3[[j]]$OM$quantities$depletion = mse3[[j]]$OM$quantities$biomass/mse3[[j]]$OM$quantities$DynamicB0
    }
    
    # -- Fix M
    if(sum(mse3[[j]]$OM$data_list$M1_model) == 0){
      mse3[[j]]$OM$quantities$SBF <- om_hcr_list_fixM[[em]]$quantities$SBF[,ncol(om_hcr_list_fixM[[em]]$quantities$SBF)]
      
      # mse3[[j]]$OM$data_list$Plimit <- om_hcr_list_fixM[[em]]$data_list$Plimit # Update Target
      # mse3[[j]]$OM$data_list$Ptarget <- om_hcr_list_fixM[[em]]$data_list$Ptarget # Update Limit
      # 
      # mse3[[j]]$OM$quantities$Flimit <- om_hcr_list_fixM[[em]]$quantities$Flimit # Update Flimit from Ftarget that was optimized
      # mse3[[j]]$OM$quantities$Ftarget <- om_hcr_list_fixM[[em]]$quantities$Ftarget # Update Flimit from Ftarget that was optimized
    }
    
    # -- Estimate M
    if(sum(mse3[[j]]$OM$data_list$M1_model) > 0){
      mse3[[j]]$OM$quantities$SBF <- om_hcr_list_estM[[em]]$quantities$SBF[,ncol(om_hcr_list_estM[[em]]$quantities$SBF)]
      # mse3[[j]]$OM$data_list$Plimit <- om_hcr_list_estM[[em]]$data_list$Plimit # Update Target
      # mse3[[j]]$OM$data_list$Ptarget <- om_hcr_list_estM[[em]]$data_list$Ptarget # Update Limit
      # 
      # mse3[[j]]$OM$quantities$Flimit <- om_hcr_list_estM[[em]]$quantities$Flimit # Update Flimit from Ftarget that was optimized
      # mse3[[j]]$OM$quantities$Ftarget <- om_hcr_list_estM[[em]]$quantities$Ftarget # Update Flimit from Ftarget that was optimized
    }
  }
  
  # - MULTI-SPECIES
  # - Calculate depletion for multi-species models
  if(mse3[[j]]$OM$data_list$msmMode == 1){
    mse3[[j]]$OM$quantities$depletionSSB <- mse3[[j]]$OM$quantities$biomassSSB / om_list_no_F[[om]]$quantities$biomassSSB[,ncol(om_list_no_F[[om]]$quantities$biomassSSB)] # Divide ssb by SSB in 2060 under no fishing
    
    mse3[[j]]$OM$quantities$SB0 <- om_list_no_F[[om]]$quantities$biomassSSB[,ncol(om_list_no_F[[om]]$quantities$biomassSSB)] # Update SB0
    
    mse3[[j]]$OM$data_list$Plimit[1:3] <- 0.25 # Update Target
    mse3[[j]]$OM$data_list$Ptarget[1:3] <- 0.40 # Update Limit
    
    mse3[[j]]$OM$quantities$Flimit <- ms_run_f25$quantities$Ftarget # Update Flimit from Ftarget that was optimized
    mse3[[j]]$OM$quantities$Ftarget <- ms_run_f25$quantities$Ftarget # Update Flimit from Ftarget that was optimized
  }
}


# STEP 3 - Performance metrics
mse_metrics <- mse_summary(mse3)
mse_metrics <- mse_metrics[1:3,-c(2:3)]
mse_metrics <- tidyr::pivot_longer(mse_metrics, cols = 2:ncol(mse_metrics))
colnames(mse_metrics) <- c("Species", "Performance metric", MSE_names)
mse_metrics


# - Max year for plots
maxyr <- mse3$Sim_1$EM$EM$data_list$projyr


plot_depletionSSB(mse3, mse = TRUE, OM = TRUE, #file = paste0("Results/Figures/Depletion/", system, "/", recname[rec], "/True/", system, "_", recname[rec], " true ", MSE_names), 
                  line_col  = "#04395E", reference = om_list_no_F[[om]], top_adj = 1, species = species, width = 4.3, height = 4, maxyr = maxyr)

plot_depletionSSB(mse3, mse = TRUE, OM = FALSE, #file = paste0("Results/Figures/Depletion/", system, "/", recname[rec], "/Perceived/", system, "_", recname[rec], " Perceived ", MSE_names), 
                  line_col = "#5F0F40", top_adj = 1, species = species, width = 4.3, height = 4, maxyr = maxyr)

source("R/Functions/plot_m_through_time.R")
plot_m_at_age_mse(mse3, #file = paste0("Results/Figures/M/", system, "/", recname[rec], "/", system, "_", recname[rec], " Perceived ", MSE_names), 
                  line_col = "#5F0F40", top_adj = 1, species = species, width = 4.3, height = 4, age = 1)

# plot_ssb(c(list(mse3$Sim_1$OM), mse3$Sim_1$EM), mse = FALSE, incl_proj = TRUE, #file = paste0("Results/Figures/SSB/", system, "_", recname[rec], " 1 Sim/", system, " Perceived 1-Sim ", MSE_names), species = species, width = 4.3, height = 4)

plot_ssb(mse3, mse = TRUE, OM = TRUE, #file = paste0("Results/Figures/SSB/", system, "/", recname[rec], "/True/", system, "_", recname[rec], " true ", MSE_names), 
         line_col  = "#04395E", reference = om_list_no_F[[om]], species = species, width = 4.3, height = 4, maxyr = maxyr)
plot_ssb(mse3, mse = TRUE, OM = FALSE, #file = paste0("Results/Figures/SSB/", system, "/", recname[rec], "/Perceived/",  system, "_", recname[rec], " Perceived ", MSE_names), 
         line_col = "#5F0F40", species = species, maxyr = maxyr)

plot_biomass(mse3, mse = TRUE, OM = TRUE, #file = paste0("Results/Figures/B/", system, "/", recname[rec], "/True/", system, "_", recname[rec], " true ", MSE_names), 
             line_col  = "#04395E", reference = om_list_no_F[[om]], species = species, width = 4.3, height = 4, maxyr = maxyr)
plot_biomass(mse3, mse = TRUE, OM = FALSE, #file = paste0("Results/Figures/B/", system, "/", recname[rec], "/Perceived/", system, "_", recname[rec], " Perceived ", MSE_names), 
             line_col = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)

plot_recruitment(mse3, mse = TRUE, OM = TRUE, #file = paste0("Results/Figures/R/", system, "/", recname[rec], "/True/", system, "_", recname[rec], " true ", MSE_names), 
                 line_col  = "#04395E", species = species, width = 4.3, height = 4, maxyr = maxyr)
plot_recruitment(mse3, mse = TRUE, OM = FALSE, #file = paste0("Results/Figures/R/", system, "/", recname[rec], "/Perceived/",  system, "_", recname[rec], " Perceived ", MSE_names), 
                 line_col = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)

plot_f(mse3, mse = TRUE, OM = TRUE, #file = paste0("Results/Figures/F/",system, "/", recname[rec], "/True/", system, "_", recname[rec], " true ", MSE_names), 
       line_col  = "#04395E", species = species, width = 4.3, height = 4, maxyr = maxyr)
plot_f(mse3, mse = TRUE, OM = FALSE, #file = paste0("Results/Figures/F/",system, "/", recname[rec], "/Perceived/", system, "_", recname[rec], " Perceived ", MSE_names), 
       line_col  = "#5F0F40", species = species, width = 4.3, height = 4, maxyr = maxyr)
#
plot_catch(mse3, mse = TRUE, #file = paste0("Results/Figures/Catch/",system, "_", recname[rec], " true ", MSE_names), 
           line_col  = "#04395E", width = 4.3, height = 4)
# #