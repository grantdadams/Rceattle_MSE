library(Rceattle)

## File names# -- NPFMC Tier 3 HCRs No cap
dir_no_cap_names <- c("Runs/EBS/MS_OM/SS_Tier3_EM/ConstantR/No cap", "Runs/EBS/MS_OM/SS_M_Tier3_EM/ConstantR/No cap", "Runs/EBS/SS_OM/SS_Tier3_EM/ConstantR/No cap",  "Runs/EBS/SS_OM/SS_M_Tier3_EM/ConstantR/No cap")

dir_no_cap_names_GOA <- c("Runs/GOA1977/MS_OM/SS_Tier3_EM/ConstantR/No cap", "Runs/GOA1977/MS_OM/SS_M_Tier3_EM/ConstantR/No cap", "Runs/GOA1977/SS_OM/SS_Tier3_EM/ConstantR/No cap",  "Runs/GOA1977/SS_OM/SS_M_Tier3_EM/ConstantR/No cap")

MSE_names <- c("MS-OM, Fix M-No cap", "MS-OM, Est M-No cap", "SS-OM, Fix M-No cap", "SS-OM, Est M-No cap")

## Load and run summary
# mse_list <- lapply(dir_names, function(x) load_mse(dir = x, file = NULL))
# mse_metrics <- lapply(mse_list, function(x) mse_summary(x))

for(i in 1:length(dir_no_cap_names)){
  mse3 <- load_mse(dir = dir_no_cap_names_GOA[i], file = NULL)
  mse_metrics <- mse_summary(mse3)
  write.csv(mse_metrics, file = paste0("Results/GOA_table", MSE_names[i]))
  plot_depletionSSB(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/Depletion/GOA true", MSE_names[i]), line_col  = "#04395E")
  plot_depletionSSB(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/Depletion/GOA Perceived", MSE_names[i]), line_col = "#5F0F40")
  plot_ssb(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/SSB/GOA true", MSE_names[i]), line_col  = "#04395E")
  plot_ssb(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/SSB/GOA Perceived", MSE_names[i]), line_col = "#5F0F40")
  plot_catch(mse3, mse = TRUE, OM = TRUE, file = paste0("Results/Figures/SSB/GOA true", MSE_names[i]), line_col  = "#04395E")
  plot_catch(mse3, mse = TRUE, OM = FALSE, file = paste0("Results/Figures/SSB/GOA Perceived", MSE_names[i]), line_col = "#5F0F40")
  rm(mse3)
}



plot_depletionSSB(list(mse3_goa$Sim_1$EM$`OM_Sim_1. EM_yr_2060`, mse3_goa$Sim_1$OM), mse = FALSE, model_names = c("EM", "OM"))
plot_biomass(list(mse3_goa$Sim_1$EM$`OM_Sim_1. EM_yr_2060`, mse3_goa$Sim_1$OM), mse = FALSE, model_names = c("EM", "OM"))


plot_depletionSSB(c(mse3_goa$Sim_1$EM, list(mse3_goa$Sim_1$OM)), mse = FALSE, model_names = c("EM", "OM"))
plot_biomass(c(mse3_goa$Sim_1$EM, list(mse3_goa$Sim_1$OM)), mse = FALSE, model_names = c("EM", "OM"))
plot_recruitment(c(mse3_goa$Sim_1$EM, list(mse3_goa$Sim_1$OM)), model_names = c("EM", "OM"))



avg_catch <- data.frame(matrix(NA, 3, ncol = length(mse_list)))
colnames(avg_catch) <- MSE_names
rownames(avg_catch) <- c("Pollock", "Cod", "ATF")
ssb_mse <- catch_iav <- prob_overfished <- terminal_status <- avg_catch

mod_avg_list <- list()
# - Get summary
for(i in 1:length(summary_list)){
  avg_catch[,i] <- summary_list[[i]]$catch_summary_stats$`Average Catch`[1:3]
  catch_iav[,i] <- summary_list[[i]]$catch_summary_stats$`Catch IAV`[1:3]
  terminal_status[,i] <- summary_list[[i]]$biomass_summary_stats$`EM: Terminal SSB/SSB40`
  prob_overfished[,i] <- summary_list[[i]]$biomass_summary_stats$`EM: P(SSB < SSB20)`
  ssb_mse[,i] <- summary_list[[i]]$biomass_summary_stats$`Avg terminal SSB MSE`
  
  mod_avg_list[[i]] <- model_average(mse_list[[i]]$OM_list)
  
  plot_ssb(c(mse_list[[i]]$OM_list, list(mod_avg_list[[i]])), line_col = c(rep("grey70",50),1), file = paste0("R/Results/Figures/SSB/", MSE_names[i]), minyr = 2015)
  plot_recruitment(c(mse_list[[i]]$OM_list, list(mod_avg_list[[i]])), line_col = c(rep("grey70",50),1), file = paste0("R/Results/Figures/R/", MSE_names[i]), minyr = 2015)
  plot_biomass(c(mse_list[[i]]$OM_list, list(mod_avg_list[[i]])), line_col = c(rep("grey70",50),1), file = paste0("R/Results/Figures/B/", MSE_names[i]), minyr = 2015)
}

library(writexl)
write_xlsx(list(avg_catch = avg_catch, catch_iav = catch_iav, terminal_status = terminal_status, prob_overfished = prob_overfished, ssb_mse = ssb_mse), path = "R/Results/BSAI_mse_results.xlsx")



plot_ssb(mod_avg_list[7:11], file = paste0("R/Results/Figures/SSB/model_average_ss_om"), model_names = MSE_names[7:11], minyr = 2015)
plot_ssb(mod_avg_list[1:6], file = paste0("R/Results/Figures/SSB/model_average_ms_om"), model_names = MSE_names[1:6], minyr = 2015)


plot_recruitment(mod_avg_list, file = paste0("R/Results/Figures/SSB/model_average"), model_names = MSE_names, minyr = 2015)
plot_biomass(mod_avg_list, file = paste0("R/Results/Figures/SSB/model_average"), model_names = MSE_names, minyr = 2015)




