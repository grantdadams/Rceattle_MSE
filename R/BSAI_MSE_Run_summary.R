################################################
# Set-up
################################################
source("R/BSAI_condition_models.R")
library(Rceattle)
library(tidyr)

ms_run$quantities$depletionSSB <- ms_run$quantities$biomassSSB/ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)]


################################################
# Management strategy evaluation
################################################
### OMS
# 1. Single-species fix M
# 2. Single-species estimate M
# 3. Multi-species type II
om_list <- list(ss_run_Tier3, ss_run_M_Tier3, ms_run_f25)
projected_OM_no_F = list(ss_run, ss_run_M, ms_run)
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

# Lists to update reference points in OM (can remove later)
em_hcr_list_fixM <- list(ss_run_Tier3, ss_run_dynamicTier3, ss_run_Cat1, ss_run_dynamicCat1, ss_run_Tier1, ss_run_dynamicTier1, ss_run_Fspr, ss_run_AvgF) # Fixed M
em_hcr_list_fixM = c(em_hcr_list_fixM, em_hcr_list_fixM)

em_hcr_list_estM <- list(ss_run_M_Tier3, ss_run_M_dynamicTier3, ss_run_M_Cat1, ss_run_M_dynamicCat1, ss_run_M_Tier1, ss_run_M_dynamicTier1, ss_run_M_Fspr, ss_run_M_AvgF) # Estimate M
em_hcr_list_estM = c(em_hcr_list_estM, em_hcr_list_estM)

em_hcr_names <- c("SS_fixM_Tier3_EM", "SS_fixM_dynamicTier3_EM", "SS_fixM_Cat1_EM", "SS_fixM_dynamicCat1_EM", "SS_fixM_Tier1_EM", "SS_fixM_dynamicTier1_EM", "SS_fixM_Fspr_EM", "SS_fixM_AvgF_EM", # Fixed M
                  "SS_estM_Tier3_EM", "SS_estM_dynamicTier3_EM", "SS_estM_Cat1_EM", "SS_estM_dynamicCat1_EM", "SS_estM_Tier1_EM", "SS_estM_dynamicTier1_EM", "SS_estM_Fspr_EM", "SS_estM_AvgF_EM")

plot_biomass(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names, width = 6, height = 5)
plot_ssb(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names, width = 6, height = 5)
plot_recruitment(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names, width = 6, height = 5)
plot_b_eaten_prop(projected_OM_no_F, file = "Results/Figures/EBS_OM_", model_names = om_names, width = 6, height = 5)

################################################
# Load and run summary
################################################
## Loop across OMs,
for(om in 1:length(om_list)){  # OM model
  for(em in 1:length(em_hcr_list)){ # EM and HCR
    
    # STEP 1 -- Load MSE
    mse3 <- load_mse(dir = paste0("Runs/EBS/", om_names[om],"/", em_hcr_names[em],"/ConstantR/No cap"), file = NULL)
    MSE_names <- paste0(om_names[om],"__", em_hcr_names[em])
    
    
    # STEP 2 -- Update Ftarget and Flimit for OMs
    for(j in 1:length(mse3)){
      
      # - Ftarget and Flimit for single-species models
      if(mse3[[j]]$OM$data_list$msmMode == 0){
        
        # -- Fix M
        if(unique(mse3[[j]]$OM$data_list$est_M1) == 0){
          mse3[[j]]$OM$data_list$Plimit <- em_hcr_list_fixM[[em]]$data_list$Plimit # Update Target
          mse3[[j]]$OM$data_list$Ptarget <- em_hcr_list_fixM[[em]]$data_list$Ptarget # Update Limit
          
          mse3[[j]]$OM$quantities$Flimit <- em_hcr_list_fixM[[em]]$quantities$Flimit # Update Flimit from Ftarget that was optimized
          mse3[[j]]$OM$quantities$Ftarget <- em_hcr_list_fixM[[em]]$quantities$Ftarget # Update Flimit from Ftarget that was optimized
        }
        
        # -- Estimate M
        if(unique(mse3[[j]]$OM$data_list$est_M1) == 1){
          # - Minus 8 because fix m is the first om in the list and there are 8 hcrs
          mse3[[j]]$OM$data_list$Plimit <- em_hcr_list_estM[[em]]$data_list$Plimit # Update Target
          mse3[[j]]$OM$data_list$Ptarget <- em_hcr_list_estM[[em]]$data_list$Ptarget # Update Limit
          
          mse3[[j]]$OM$quantities$Flimit <- em_hcr_list_estM[[em]]$quantities$Flimit # Update Flimit from Ftarget that was optimized
          mse3[[j]]$OM$quantities$Ftarget <- em_hcr_list_estM[[em]]$quantities$Ftarget # Update Flimit from Ftarget that was optimized
        }
      }
      
      
      # - Calculate depletion for multi-species models
      if(mse3[[j]]$OM$data_list$msmMode == 1){
        mse3[[j]]$OM$quantities$depletionSSB <- mse3[[j]]$OM$quantities$biomassSSB / ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)] # Divide ssb by SSB in 2060 under no fishing
        
        mse3[[j]]$OM$quantities$SB0 <- ms_run$quantities$biomassSSB[,ncol(ms_run$quantities$biomassSSB)] # Update SB0
        
        mse3[[j]]$OM$data_list$Plimit <- 0.25 # Update Target
        mse3[[j]]$OM$data_list$Ptarget <- 0.40 # Update Limit
        
        mse3[[j]]$OM$quantities$Flimit <- ms_run_f25$quantities$Ftarget # Update Flimit from Ftarget that was optimized
        mse3[[j]]$OM$quantities$Ftarget <- ms_run_f25$quantities$Ftarget # Update Flimit from Ftarget that was optimized
      }
    }
    
    # STEP 3 - Performance metrics
    mse_metrics <- mse_summary(mse3)
    mse_metrics <- mse_metrics[1:3,-c(2:3)]
    mse_metrics <- pivot_longer(mse_metrics, cols = 2:ncol(mse_metrics))
    colnames(mse_metrics) <- c("Species", "Performance metric", MSE_names)

    if(om == 1 & em == 1){mse_metrics_complete = mse_metrics}
    if(om != 1 | em != 1){mse_metrics_complete = cbind(mse_metrics_complete, mse_metrics[,-c(1,2)])}
    write.csv(mse_metrics, file = paste0("Results/Tables/EBS/EBS_table_", MSE_names,".csv"))


    # STEP 4 - Plot
    plot_depletionSSB(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/Depletion/EBS true ", MSE_names), line_col  = "#04395E", reference = projected_OM_no_F[[om]], top_adj = 1, species = c(1, 2, 3), width = 4.3, height = 4)
    plot_depletionSSB(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/Depletion/EBS Perceived ", MSE_names), line_col = "#5F0F40", top_adj = 1, species = c(1, 2, 3), width = 4.3, height = 4)

    plot_depletionSSB(mse3$Sim_1$EM, mse = FALSE, incl_proj = TRUE, file = paste0("Results/Figures/Depletion/EBS 1 Sim/EBS Perceived 1-Sim ", MSE_names), species = c(1, 2, 3), width = 4.3, height = 4)

    plot_ssb(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/SSB/EBS true ", MSE_names), line_col  = "#04395E", reference = projected_OM_no_F[[om]], species = c(1, 2, 3), width = 4.3, height = 4)
    plot_ssb(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/SSB/EBS Perceived ", MSE_names), line_col = "#5F0F40", species = c(1, 2, 3))

    plot_biomass(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/B/EBS true ", MSE_names), line_col  = "#04395E", reference = projected_OM_no_F[[om]], species = c(1, 2, 3), width = 4.3, height = 4)
    plot_biomass(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/B/EBS Perceived ", MSE_names), line_col = "#5F0F40", species = c(1, 2, 3), width = 4.3, height = 4)

    plot_recruitment(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/R/EBS true ", MSE_names), line_col  = "#04395E", species = c(1, 2, 3), width = 4.3, height = 4)
    plot_recruitment(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/R/EBS Perceived ", MSE_names), line_col = "#5F0F40", species = c(1, 2, 3), width = 4.3, height = 4)

    plot_f(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/F/EBS true ", MSE_names), line_col  = "#04395E", species = c(1, 2, 3), width = 4.3, height = 4)
    plot_f(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/F/EBS Perceived ", MSE_names), line_col  = "#5F0F40", species = c(1, 2, 3), width = 4.3, height = 4)
    
    plot_catch(mse3, mse = TRUE, file = paste0("Results/Figures/Catch/EBS true ", MSE_names), line_col  = "#04395E", ymax = c(1e7, 280000, 1e5), width = 4.3, height = 4)
    
    # - Unload for memory
    rm(mse3)
  }
}

write.csv(mse_metrics_complete, file = paste0("Results/Tables/EBS_table_full.csv"))
