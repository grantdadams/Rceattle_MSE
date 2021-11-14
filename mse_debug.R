
# Set models
Rceattle_EM_list[[sim]] <- list()
Rceattle_EM_list[[sim]][[1]] <- estimation_model
estimation_model_use <- estimation_model
operating_model_use <- operating_model



# Replace future rec_devs with numbers
if(simulate){
  for(sp in 1:operating_model_use$data_list$nspp){
    operating_model_use$estimated_params$rec_dev[sp,proj_yrs - operating_model_use$data_list$styr + 1] <- replace(
      operating_model_use$estimated_params$rec_dev[sp,proj_yrs - operating_model_use$data_list$styr + 1],
      values = rnorm( length(operating_model_use$estimated_params$rec_dev[sp,proj_yrs - operating_model_use$data_list$styr + 1]),
                      mean = 0,
                      sd = exp(operating_model_use$estimated_params$ln_rec_sigma[sp])) # Assumed value from penalized likelihood
    )
  }
}



# Run through model
k=1

# ------------------------------------------------------------
# 1. OBSERVATION MODEL
# ------------------------------------------------------------
new_years <- proj_yrs[which(proj_yrs <= assess_yrs[k] & proj_yrs > operating_model_use$data_list$endyr)]

# - Get projected catch data from EM
new_catch_data <- estimation_model_use$data_list$fsh_biom
dat_fill_ind <- which(new_catch_data$Year %in% new_years & is.na(new_catch_data$Catch))
new_catch_data$Catch[dat_fill_ind] <- estimation_model_use$quantities$fsh_bio_hat[dat_fill_ind]
if(!is.null(cap)){
  new_catch_data$Catch[dat_fill_ind] <- ifelse(new_catch_data$Catch[dat_fill_ind] > cap[new_catch_data$Species[dat_fill_ind]], cap[new_catch_data$Species[dat_fill_ind]], new_catch_data$Catch[dat_fill_ind])
}

# - Update catch data in OM and EM
operating_model_use$data_list$fsh_biom <- new_catch_data
estimation_model_use$data_list$fsh_biom <- new_catch_data

# - Update endyr of OM
nyrs_hind <- operating_model_use$data_list$endyr - operating_model_use$data_list$styr + 1
operating_model_use$data_list$endyr <- assess_yrs[k]

# - Update parameters
# -- F_dev
operating_model_use$estimated_params$F_dev <- cbind(operating_model_use$estimated_params$F_dev, matrix(0, nrow= nrow(operating_model_use$estimated_params$F_dev), ncol = length(new_years)))

# -- Time-varing survey catachbilitiy - Assume last year - filled by columns
operating_model_use$estimated_params$ln_srv_q_dev <- cbind(operating_model_use$estimated_params$ln_srv_q_dev, matrix(operating_model_use$estimated_params$ln_srv_q_dev[,ncol(operating_model_use$estimated_params$ln_srv_q_dev)], nrow= nrow(operating_model_use$estimated_params$ln_srv_q_dev), ncol = length(new_years)))

#FIXME: update random effects q if used again
# operating_model_use$estimated_params$ln_srv_q_dev_re <- cbind(operating_model_use$estimated_params$ln_srv_q_dev_re, matrix(operating_model_use$estimated_params$ln_srv_q_dev_re[,ncol(operating_model_use$estimated_params$ln_srv_q_dev_re)], nrow= nrow(operating_model_use$estimated_params$ln_srv_q_dev_re), ncol = length(new_years)))

# -- Time-varing selectivity - Assume last year - filled by columns
n_selectivities <- nrow(operating_model_use$data_list$fleet_control)

ln_sel_slp_dev = array(0, dim = c(2, n_selectivities, 2, nyrs_hind + length(new_years)))  # selectivity deviations paramaters for logistic; n = [2, nspp]
sel_inf_dev = array(0, dim = c(2, n_selectivities, 2, nyrs_hind + length(new_years)))  # selectivity deviations paramaters for logistic; n = [2, nspp]

#FIXME: update random effects sel if used again
# sel_slp_dev_re = array(0, dim = c(2, n_selectivities, 2, nyrs_hind + length(new_years)))  # selectivity random effect deviations paramaters for logistic; n = [2, nspp]
# sel_inf_dev_re = array(0, dim = c(2, n_selectivities, 2, nyrs_hind + length(new_years)))  # selectivity random effectdeviations paramaters for logistic; n = [2, nspp]

ln_sel_slp_dev[,,,1:nyrs_hind] <- operating_model_use$estimated_params$ln_sel_slp_dev
sel_inf_dev[,,,1:nyrs_hind] <- operating_model_use$estimated_params$sel_inf_dev

#FIXME: update random effects sel if used again
# sel_slp_dev_re[,,,1:nyrs_hind] <- operating_model_use$estimated_params$sel_slp_dev_re
# sel_inf_dev_re[,,,1:nyrs_hind] <- operating_model_use$estimated_params$sel_inf_dev_re

ln_sel_slp_dev[,,,(nyrs_hind + 1):(nyrs_hind + length(new_years))] <- ln_sel_slp_dev[,,,nyrs_hind]
sel_inf_dev[,,,(nyrs_hind + 1):(nyrs_hind + length(new_years))] <- sel_inf_dev[,,,nyrs_hind]
#FIXME: update random effects sel if used again
# sel_slp_dev_re[,,,(nyrs_hind + 1):(nyrs_hind + length(new_years))] <- sel_slp_dev_re[,,,nyrs_hind]
# sel_inf_dev_re[,,,(nyrs_hind + 1):(nyrs_hind + length(new_years))] <- sel_inf_dev_re[,,,nyrs_hind]

operating_model_use$estimated_params$ln_sel_slp_dev <- ln_sel_slp_dev
operating_model_use$estimated_params$sel_inf_dev <- sel_inf_dev
#FIXME: update random effects sel if used again
# operating_model_use$estimated_params$sel_slp_dev_re <- sel_slp_dev_re
# operating_model_use$estimated_params$sel_inf_dev_re <- sel_inf_dev_re


# - Update map (Only new parameter we are estimating in OM is the F_dev of the new years)
operating_model_use$map <- build_map(
  data_list = operating_model_use$data_list,
  params = operating_model_use$estimated_params,
  debug = operating_model_use$data_list$debug,
  random_rec = operating_model_use$data_list$random_rec)

# -- Fill in with NA's
for (i in 1:length(operating_model_use$map[[2]])) {
  operating_model_use$map[[2]][[i]] <- replace(operating_model_use$map[[2]][[i]], values = rep(NA, length(operating_model_use$map[[2]][[i]])))
}

# -- Estimate terminal F for catch
new_f_ind <- (ncol(operating_model_use$map[[2]]$F_dev) - length(new_years) + 1) : ncol(operating_model_use$map[[2]]$F_dev)
operating_model_use$map[[2]]$F_dev[,new_f_ind] <- replace(operating_model_use$map[[2]]$F_dev[,new_f_ind], values = 1:length(operating_model_use$map[[2]]$F_dev[,new_f_ind]))


# --- Turn off F for surveys
for (i in 1:nrow(operating_model_use$data_list$fleet_control)) {
  # Turn of F and F dev if not estimating of it is a Survey
  if (operating_model_use$data_list$fleet_control$Fleet_type[i] %in% c(0, 2)) {
    operating_model_use$map[[2]]$F_dev[i, ] <- NA
  }
}

# -- Map out Fdev for years with 0 catch to very low number
fsh_biom <- operating_model_use$data_list$fsh_biom
fsh_ind <- fsh_biom$Fleet_code[which(fsh_biom$Catch == 0)]
yr_ind <- fsh_biom$Year[which(fsh_biom$Catch == 0)] - operating_model_use$data_list$styr + 1
operating_model_use$map[[2]]$F_dev[fsh_ind, yr_ind] <- NA

for (i in 1:length( operating_model_use$map[[2]])) {
  operating_model_use$map[[1]][[i]] <- factor( operating_model_use$map[[2]][[i]])
}

# operating_model_use$estimated_params$ln_FSPR <- replace(operating_model_use$estimated_params$ln_FSPR, values = rep(-10, length(operating_model_use$estimated_params$ln_FSPR)))


# - Fit OM with new catch data
operating_model_use <- fit_mod(
  data_list = operating_model_use$data_list,
  inits = operating_model_use$estimated_params,
  map =  operating_model_use$map,
  bounds = NULL,
  file = NULL,
  debug = 1,
  random_rec = operating_model_use$data_list$random_rec,
  niter = operating_model_use$data_list$niter,
  msmMode = operating_model_use$data_list$msmMode,
  avgnMode = operating_model_use$data_list$avgnMode,
  minNByage = operating_model_use$data_list$minNByage,
  suitMode = operating_model_use$data_list$suitMode,
  suityr = operating_model$data_list$endyr,
  phase = NULL,
  verbose = 1,
  getsd = FALSE)

plot_biomass(list(operating_model_use, operating_model), model_names = c(1,2))
plot_logindex(operating_model_use, model_names = c(1,2))
plot_catch(list(operating_model_use, operating_model), model_names = c(1,2))
plot_recruitment(list(operating_model_use, operating_model), model_names = c(1,2))
plot_selectivity(operating_model_use, file = "use")
plot_selectivity(operating_model, file = "base")

# Rec is good
sum(operating_model_use$quantities$sel[,,,1:39] != operating_model$quantities$sel[,,,1:39], na.rm = TRUE)
# Selectivity is good
sum(operating_model_use$quantities$srv_q[,1:39] != operating_model$quantities$srv_q[,1:39], na.rm = TRUE)
# Q is good
sum(operating_model_use$quantities$M1 != operating_model$quantities$M1, na.rm = TRUE)
# M1 is good

sum(operating_model_use$quantities$F[,,,1:39] != operating_model$quantities$F[,,,1:39], na.rm = TRUE)
# F is good

sum(operating_model_use$quantities$ration2Age[,,,1:39] != operating_model$quantities$ration2Age[,,,1:39], na.rm = TRUE)
# F is good

sum(operating_model_use$quantities$M2[,,,1:39] != operating_model$quantities$M2[,,,1:39], na.rm = TRUE)
# M2 is not good

sum(operating_model_use$quantities$stomKirWt[,,,,1:39] != operating_model$quantities$stomKirWt[,,,,1:39], na.rm = TRUE)
# M2 is not good

sum(operating_model_use$quantities$stom_div_bio2[,,,,1:39] != operating_model$quantities$stom_div_bio2[,,,,1:39], na.rm = TRUE)
# Stomdivbio is good


sum(operating_model_use$quantities$AvgN[,,,1:39] != operating_model$quantities$AvgN[,,,1:39], na.rm = TRUE)
# AvgN is good

sum(operating_model_use$quantities$suma_suit[,,,1:39] != operating_model$quantities$suma_suit[,,,1:39], na.rm = TRUE)
# suma_suit is good

sum(operating_model_use$quantities$of_stomKir[,,,1:39] != operating_model$quantities$of_stomKir[,,,1:39], na.rm = TRUE)
# of_stomKir is good

sum(operating_model_use$quantities$suit_main[,,,,1:39] != operating_model$quantities$suit_main[,,,,1:39], na.rm = TRUE)
# suitability is not good

sum(operating_model_use$quantities$suit_other != operating_model$quantities$suit_other, na.rm = TRUE)
# suit_other is not good
