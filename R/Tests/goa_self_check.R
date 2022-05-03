
### Self check
# - SS-OM: SSM-EM Tier 3 HCR
mse <- mse_run(om = ss_run_Tier3, em = ss_run_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = TRUE, cap = NULL, dir = NULL, file = NULL)

mod_list <- list(ss_run_Tier3, mse[[2]], mse[[1]][[43]])
mod_names <- c("OM projected", "OM updated", "EM")
plot_biomass(Rceattle = mod_list, model_names = mod_names, incl_proj = TRUE)
plot_depletionSSB(Rceattle = mod_list, model_names = mod_names, incl_proj = TRUE)
plot_recruitment(Rceattle = mod_list, model_names = mod_names, incl_proj = TRUE)
plot_catch(Rceattle = mod_list, model_names = mod_names, incl_proj = TRUE)

sapply(mod_list, function(x) x$quantities$SB0)
sapply(mod_list, function(x) x$quantities$mean_rec)
sapply(mod_list, function(x) rowMeans(x$quantities$R[,1:42]))
sapply(mod_list, function(x) rowMeans(x$quantities$R[,43:84]))


mse_no_rec <- mse_run(om = ss_run_Tier3, em = ss_run_Tier3, nsim = 1, assessment_period = 1, sampling_period = sampling_period, simulate_data = TRUE, sample_rec = FALSE, cap = NULL, dir = NULL, file = NULL)

mod_list <- list(ss_run_Tier3, mse_no_rec[[2]], mse_no_rec[[1]][[43]])
mod_names <- c("OM projected", "OM updated", "EM")
plot_biomass(Rceattle = mod_list, model_names = mod_names, incl_proj = TRUE)
plot_depletionSSB(Rceattle = mod_list, model_names = mod_names, incl_proj = TRUE)
plot_catch(Rceattle = mod_list, model_names = mod_names, incl_proj = TRUE)

sapply(mod_list, function(x) x$quantities$SB0)
sapply(mod_list, function(x) x$quantities$mean_rec)
sapply(mod_list, function(x) rowMeans(x$quantities$R[,1:42]))
sapply(mod_list, function(x) rowMeans(x$quantities$R[,43:84]))