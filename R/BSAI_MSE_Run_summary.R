library(Rceattle)

## File names
# -- NPFMC Tier 3 HCRs with cap
dir_names <- c("Runs/EBS/MS_OM/SS_Tier3_EM/ConstantR/Cap1", "Runs/EBS/MS_OM/SS_M_Tier3_EM/ConstantR/Cap1",  "Runs/EBS/SS_M_OM/SS_Tier3_EM/ConstantR/Cap1",  "Runs/EBS/SS_M_OM/SS_M_Tier3_EM/ConstantR/Cap1", "Runs/EBS/SS_OM/SS_Tier3_EM/ConstantR/Cap1",  "Runs/EBS/SS_OM/SS_M_Tier3_EM/ConstantR/Cap1")

MSE_names <- c("MS-OM, Fix M-Cap", "MS-OM, Est M-Cap", "SS-M-OM, Fix M-Cap", "SS-M-OM, Est M-Cap", "SS-OM, Fix M-Cap", "SS-OM, Est M-Cap")


# -- NPFMC Tier 3 HCRs No cap
dir_no_cap_names <- c("Runs/EBS/MS_OM/SS_Tier3_EM/ConstantR/No cap", "Runs/EBS/MS_OM/SS_M_Tier3_EM/ConstantR/No cap",  "Runs/EBS/SS_M_OM/SS_Tier3_EM/ConstantR/No cap",  "Runs/EBS/SS_M_OM/SS_M_Tier3_EM/ConstantR/No cap", "Runs/EBS/SS_OM/SS_Tier3_EM/ConstantR/No cap",  "Runs/EBS/SS_OM/SS_M_Tier3_EM/ConstantR/No cap")

MSE_names <- c("MS-OM, Fix M-No cap", "MS-OM, Est M-No cap", "SS-M-OM, Fix M-No cap", "SS-M-OM, Est M-No cap", "SS-OM, Fix M-No cap", "SS-OM, Est M-No cap")

## Load and run summary
mse_list <- lapply(dir_names, function(x) load_mse(dir = x, file = NULL))
mse_metrics <- lapply(mse_list, function(x) mse_summary(x))

mse4 <- load_mse(dir = dir_names[4], file = NULL)
mse4_summ <- mse_summary(mse4)


plot_recruitment(c(lapply(mse4, function(x) x$OM) ), line_col = c(rep("grey", 200)))
plot_biomass(c(lapply(mse4, function(x) x$EM[[length(x$EM)]]), lapply(mse4, function(x) x$OM) ), line_col = c(rep(1, 200), rep("grey", 200)))













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




