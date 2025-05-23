## Load packages and data ----
pacman::p_load(Rceattle, readxl, dplyr, tidyr, writexl)
combined_data <- read_data(file = "~/GitHub/Rceattle_MSE/Data/GOA_18_5_1_data_1977-2018_no_halibut.xlsx")
combined_data$projyr <- 2100
alpha = exp(c(3.143, 1.975, 1.44))


## Climate data ----
summer_bt_data <- read.csv("Data/goa_temp_610_to_630_summer_300M.csv") %>%
  filter(depthclass == "Bottom", hind == "yes")  %>%
  mutate(varname = "summer bt") %>%
  rename(value = mean_value_dc_610_to_630)

fall_sst_data <- read.csv("Data/goa_temp_610_to_630_winter_300M.csv") %>%
  filter(depthclass == "Surface", hind == "yes")  %>%
  mutate(varname = "winter sst") %>%
  rename(value = mean_value_dc_610_to_630)

zoo_data <- read.csv("Data/goa_large_zoo_610_to_630_fall_300M.csv") %>%
  filter(depthclass == "Surface", hind == "yes") %>%
  mutate(varname = "mzl") %>%
  rename(value = mean_value_dc_610_to_630)

climate_data <- rbind(summer_bt_data, fall_sst_data, zoo_data) %>%
  mutate(value_squared = value^2) %>%
  pivot_wider(names_from = c(simulation), values_from = c(value, value_squared)) %>%
  select(-depthclass, -hind) %>%
  rename(Year = year) %>%
  group_by(varname) %>%
  mutate(value_ssp126z = scale(value_ssp126 ),
         value_ssp245z = scale(value_ssp245 ),
         value_ssp585z = scale(value_ssp585 ),
         value_squared_ssp126z = scale(value_squared_ssp126 ),
         value_squared_ssp245z = scale(value_squared_ssp245 ),
         value_squared_ssp585z = scale(value_squared_ssp585 )
  ) %>%
  ungroup() %>%
  as.data.frame()

# - Add missing years
# -- Fall SST
summer_bt_data <- climate_data %>%
  filter(varname == "summer bt") %>%
  select(-varname)

temp_sub <- data.frame(Year = 1977:1979,
                       value_ssp126 = mean(summer_bt_data$value_ssp126[1:10]),
                       value_ssp245 = mean(summer_bt_data$value_ssp245[1:10]),
                       value_ssp585 = mean(summer_bt_data$value_ssp585[1:10]),
                       value_squared_ssp126 = mean(summer_bt_data$value_squared_ssp126[1:10]),
                       value_squared_ssp245 = mean(summer_bt_data$value_squared_ssp245[1:10]),
                       value_squared_ssp585 = mean(summer_bt_data$value_squared_ssp585[1:10]),
                       value_ssp126z = 0,
                       value_ssp245z = 0,
                       value_ssp585z = 0,
                       value_squared_ssp126z = 0,
                       value_squared_ssp245z = 0,
                       value_squared_ssp585z = 0)

summer_bt_data <- rbind(temp_sub, summer_bt_data)
colnames(summer_bt_data) <- c("Year", paste0("BT_", colnames(summer_bt_data)[2:ncol(summer_bt_data)]))


# -- Summer BT
fall_sst_data <- climate_data %>%
  filter(varname == "winter sst") %>%
  select(-varname)

temp_sub <- data.frame(Year = 1977:1979,
                       value_ssp126 = mean(fall_sst_data$value_ssp126[1:10]),
                       value_ssp245 = mean(fall_sst_data$value_ssp245[1:10]),
                       value_ssp585 = mean(fall_sst_data$value_ssp585[1:10]),
                       value_squared_ssp126 = mean(fall_sst_data$value_squared_ssp126[1:10]),
                       value_squared_ssp245 = mean(fall_sst_data$value_squared_ssp245[1:10]),
                       value_squared_ssp585 = mean(fall_sst_data$value_squared_ssp585[1:10]),
                       value_ssp126z = 0,
                       value_ssp245z = 0,
                       value_ssp585z = 0,
                       value_squared_ssp126z = 0,
                       value_squared_ssp245z = 0,
                       value_squared_ssp585z = 0)

fall_sst_data <- rbind(temp_sub, fall_sst_data)
colnames(fall_sst_data) <- c("Year", paste0("SST_", colnames(fall_sst_data)[2:ncol(fall_sst_data)]))


# -- Zooplankton
zoo_data <- climate_data %>%
  filter(varname == "mzl") %>%
  select(-varname)

mzl_sub <- data.frame(Year = 1977:1979,
                      value_ssp126 = mean(zoo_data$value_ssp126[1:10]),
                      value_ssp245 = mean(zoo_data$value_ssp245[1:10]),
                      value_ssp585 = mean(zoo_data$value_ssp585[1:10]),
                      value_squared_ssp126 = mean(zoo_data$value_squared_ssp126[1:10]),
                      value_squared_ssp245 = mean(zoo_data$value_squared_ssp245[1:10]),
                      value_squared_ssp585 = mean(zoo_data$value_squared_ssp585[1:10]),
                      value_ssp126z = 0,
                      value_ssp245z = 0,
                      value_ssp585z = 0,
                      value_squared_ssp126z = 0,
                      value_squared_ssp245z = 0,
                      value_squared_ssp585z = 0)
zoo_data <- rbind(mzl_sub, zoo_data)
colnames(zoo_data) <- c("Year", paste0("MZL_", colnames(zoo_data)[2:ncol(zoo_data)]))

# - Combine
climate_data <- fall_sst_data %>%
  inner_join(zoo_data, by = "Year") %>%
  inner_join(summer_bt_data, by = "Year") %>%
  arrange(Year)


# - add to Rceattle object
ssp_dat_126 <- ssp_dat_245 <- ssp_dat_585 <- combined_data

ssp_dat_126$env_data <- climate_data %>%
  select(Year, BT_value_ssp126, SST_value_ssp126z, SST_value_squared_ssp126z, MZL_value_ssp126z ) %>%
  filter(Year < 2019)

ssp_dat_245$env_data <- climate_data %>%
  select(Year, BT_value_ssp245, SST_value_ssp245z, SST_value_squared_ssp245z, MZL_value_ssp245z ) %>%
  filter(Year < 2019)

ssp_dat_585$env_data <- climate_data %>%
  select(Year, BT_value_ssp585, SST_value_ssp585z, SST_value_squared_ssp585z, MZL_value_ssp585z ) %>%
  filter(Year < 2019)




#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Debugging section ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
data_list = NULL;
inits = NULL;
map = NULL;
bounds = NULL;
file = NULL;
estimateMode = 0;
random_rec = FALSE;
random_q = FALSE;
random_sel = FALSE;
HCR = build_hcr();
niter = 3;
msmMode = 0;
avgnMode = 0;
initMode = 1
minNByage = 0;
suitMode = 0;
suit_meanyr = NULL;
phase = NULL;
getsd = TRUE;
use_gradient = TRUE;
rel_tol = 1;
control = list(eval.max = 1e+09,
               iter.max = 1e+09, trace = 0);
getJointPrecision = TRUE;
loopnum = 5;
verbose = 1;
newtonsteps = 0
recFun = build_srr()
M1Fun = build_M1()
projection_uncertainty = TRUE
catch_hcr = FALSE


data_list = ssp_dat_585;
# inits = ss_mod$estimated_params; # Initial parameters = 0
file = NULL; # Don't save
estimateMode = 0; # Estimate
random_rec = FALSE; # No random recruitment
msmMode = 0; # Single species mode
verbose = 1;
phase = NULL;
recFun = build_srr(srr_fun = 0,
                   srr_pred_fun = 5,
                   srr_env_indices = c(2,3,4),
                   proj_mean_rec = FALSE,
                   srr_est_mode = 1,
                   srr_prior_mean = alpha,
                   srr_prior_sd = 0.2
);
initMode = 2

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# STEP 0 - Start ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
start_time <- Sys.time()

extend_length <- function(x){
  if(length(x) == data_list$nspp){ return(x)}
  else {return(rep(x, data_list$nspp))}
}

setwd(getwd())


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# STEP 1 - Load data ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
if (is.null(data_list)) {
  stop("Missing data_list object")
}

data_list <- Rceattle::clean_data(data_list)

# Add switches from function call
data_list$random_rec <- as.numeric(random_rec)
data_list$estimateMode <- estimateMode
data_list$niter <- niter
data_list$avgnMode <- avgnMode
if(is.null(data_list$initMode)){
  data_list$initMode <- initMode
}
data_list$loopnum <- loopnum
data_list$msmMode <- msmMode
data_list$suitMode <- as.numeric(suitMode)
data_list$minNByage <- as.numeric(minNByage)


if(is.null(suit_meanyr) & is.null(data_list$suit_meanyr)){ # If no meanyear is provided in data or function, use end year
  data_list$suit_meanyr <- data_list$endyr
}
if(!is.null(suit_meanyr)){ # If mean year is provided in function, override data
  data_list$suit_meanyr <- suit_meanyr
}

# - Recruitment switches
data_list$srr_fun <- recFun$srr_fun
data_list$srr_pred_fun <- recFun$srr_pred_fun
data_list$proj_mean_rec <- recFun$proj_mean_rec
if(is.null(recFun$srr_meanyr) & is.null(data_list$srr_meanyr)){ # If no meanyear is provided in data or function, use end year
  data_list$srr_meanyr <- data_list$endyr
}
if(!is.null(recFun$srr_meanyr)){ # If mean year is provided in function, override data
  data_list$srr_meanyr <- recFun$srr_meanyr
}
data_list$srr_est_mode <- recFun$srr_est_mode
data_list$srr_prior_mean <- extend_length(recFun$srr_prior_mean)
data_list$srr_prior_sd <- extend_length(recFun$srr_prior_sd)
data_list$srr_env_indices <- recFun$srr_env_indices
data_list$Bmsy_lim <- extend_length(recFun$Bmsy_lim)

# - M1 switches
if(!is.null(data_list$M1_model)){
  if(sum(data_list$M1_model != extend_length(M1Fun$M1_model))){
    warning("M1_model in data is different than in call `fit_mod`")
  }
}

# FIXME: may want to pull from data here too
data_list$M1_model= extend_length(M1Fun$M1_model)
updateM1 = M1Fun$updateM1
data_list$M1_use_prior = extend_length(M1Fun$M1_use_prior) * (data_list$M1_model > 0) # Sets to 0 if M1 is fixed
data_list$M2_use_prior = extend_length(M1Fun$M2_use_prior) * (msmMode > 0) # Sets to 0 if single-species
data_list$M1_prior_mean = extend_length(M1Fun$M1_prior_mean)
data_list$M1_prior_sd = extend_length(M1Fun$M1_prior_sd)


# - HCR Switches (make length of nspp if not)
data_list$HCR = HCR$HCR
data_list$DynamicHCR = HCR$DynamicHCR
if(HCR$HCR != 2){ # FsprTarget is also used for fixed F (so may be of length nflts)
  data_list$FsprTarget = extend_length(HCR$FsprTarget)
} else {
  data_list$FsprTarget = HCR$FsprTarget
}
data_list$FsprLimit = extend_length(HCR$FsprLimit)
data_list$Ptarget = extend_length(HCR$Ptarget)
data_list$Plimit = extend_length(HCR$Plimit)
data_list$Alpha = extend_length(HCR$Alpha)
data_list$Pstar = extend_length(HCR$Pstar)
data_list$Sigma = extend_length(HCR$Sigma)
data_list$Fmult = extend_length(HCR$Fmult)
data_list$HCRorder = extend_length(HCR$HCRorder)
data_list$QnormHCR = qnorm(data_list$Pstar, 0, data_list$Sigma)

if(data_list$HCR == 2 & estimateMode == 2){estimateMode = 4} # If projecting under constant F, run parmeters through obj only
if(data_list$msmMode > 0 & !data_list$HCR %in% c(0, 1, 2, 3, 6)){
  warning("WARNING:: Only HCRs 1, 2, 3, and 6 work in multi-species mode currently")
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# STEP 2: Load/build parameters ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
if (is.character(inits) | is.null(inits)) {
  start_par <- suppressWarnings(Rceattle::build_params(data_list = data_list))
} else{
  start_par <- inits
}
if(verbose > 0) {message("Step 1: Parameter build complete")}

# Set Fdev for years with 0 catch to very low number
fsh_biom_sub <- data_list$fsh_biom %>%
  filter(Year <= data_list$endyr)
fsh_ind <- fsh_biom_sub$Fleet_code[which(fsh_biom_sub$Catch == 0)]
yr_ind <- fsh_biom_sub$Year[which(fsh_biom_sub$Catch == 0)] - data_list$styr + 1
for(i in 1:length(fsh_ind)){
  start_par$F_dev[fsh_ind[i], yr_ind[i]] <- -999
}
rm(fsh_biom_sub)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# STEP 3: Load/build map ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
if (is.null(map)) {
  map <- suppressWarnings(build_map(data_list, start_par, debug = estimateMode == 4, random_rec = random_rec, random_sel = random_sel))
} else{
  map <- map
}
if(verbose > 0) {message("Step 2: Map build complete")}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# STEP 4: Get bounds ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
if (is.null(bounds)) {
  bounds <- Rceattle::build_bounds(param_list = start_par, data_list)
} else {
  bounds = bounds
}
if(verbose > 0) {message("Step 3: Param bounds complete")}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# STEP 5: Setup random effects ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
random_vars <- c()
if (random_rec) {
  if(initMode > 0){
    random_vars <- c(random_vars , "rec_dev", "init_dev")
  } else{
    random_vars <- c(random_vars , "rec_dev")
  }
}
if(random_q){
  random_vars <- c(random_vars , "ln_srv_q_dev")
}
if(random_sel){
  random_vars <- c(random_vars , "ln_sel_slp_dev", "sel_inf_dev", "sel_coff_dev")
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# STEP 6: Reorganize data ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
TMBfilename <- "ceattle_v01_10"

Rceattle:::data_check(data_list)

data_list_reorganized <- Rceattle::rearrange_dat(data_list)
data_list_reorganized = c(list(model = TMBfilename), data_list_reorganized)
if(msmMode > 0 & !catch_hcr){
  data_list_reorganized$HCR = 0 # Estimate model with F = 0 for the projection if multispecies
}
data_list_reorganized$forecast <- FALSE # Don't include BRPs in likelihood of hindcast

# - Update comp weights, future F (if input) and F_prop from data
if(!is.null(data_list$fleet_control$Comp_weights)){
  start_par$comp_weights = data_list$fleet_control$Comp_weights
}
start_par$proj_F_prop = data_list$fleet_control$proj_F_prop

nyrs_proj <- data_list$projyr - data_list$styr + 1
if(!is.null(HCR$FsprTarget) & HCR$HCR == 2){
  start_par$ln_Ftarget = log(HCR$FsprTarget) # Fixed fishing mortality for projections for each species
}

# - Update M1 parameter object from data if initial parameter values input
if(updateM1){
  m1 <- array(0, dim = c(data_list$nspp, 2, max(data_list$nages, na.rm = T))) # Set up array

  # Initialize from inputs
  for (i in 1:nrow(data_list$M1_base)) {
    sp <- as.numeric(as.character(data_list$M1_base$Species[i]))
    sex <- as.numeric(as.character(data_list$M1_base$Sex[i]))

    # Fill in M1 array from fixed values for each sex
    if(sex == 0){ sex = c(1, 2)} # If sex = combined/both males and females, fill in both dimensions
    for(j in 1:length(sex)){
      m1[sp, sex[j], 1:max(data_list$nages, na.rm = T)] <- as.numeric(data_list$M1_base[i,(1:max(data_list$nages, na.rm = T)) + 2])
    }
  }
  start_par$ln_M1 <- log(m1)
}

# - Update alpha for stock-recruit if fixed/prior and initial parameter values input
if(data_list$srr_est_mode %in% c(0,2)){
  start_par$rec_pars[,2] <- log(data_list$srr_prior_mean)
}

if(verbose > 0) {message("Step 4: Data rearranged complete")}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# STEP 7: Set up parameter bounds ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
L <- c()
U <- c()
for(i in 1:length(map$mapFactor)){
  if(names(map$mapFactor)[i] %!in% random_vars){ # Dont have bounds for random effects
    L = c(L, unlist(bounds$lower[[i]])[which(!is.na(unlist(map$mapFactor[[i]])) & !duplicated(unlist(map$mapFactor[[i]])))])
    U = c(U, unlist(bounds$upper[[i]])[which(!is.na(unlist(map$mapFactor[[i]])) & !duplicated(unlist(map$mapFactor[[i]])))])
  }
}

# Dimension check
dim_check <- sapply(start_par, unlist(length)) == sapply(map$mapFactor, unlist(length))
if(sum(dim_check) != length(dim_check)){
  stop(print(paste0("Map and parameter objects are not the same size for: ", names(dim_check)[which(dim_check == FALSE)])))
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# STEP 8: Phase hindcast ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Set default phasing
if(!is.null(phase)){
  if(class(phase) == "character"){
    if(tolower(phase) == "default"){
      phase = list(
        dummy = 1,
        ln_pop_scalar = 4, # Scalar for input numbers-at-age
        rec_pars = 1, # Stock-recruit parameters or log(mean rec) if no stock-recruit relationship
        beta_rec_pars = 3,
        ln_rec_sigma = 2, # Variance for annual recruitment deviats
        rec_dev = 2, # Annual recruitment deviats
        init_dev = 2, # Age specific initial age-structure deviates or parameters
        ln_sex_ratio_sigma = 3, # Variance of sex ratio (usually fixed)
        ln_M1 = 4, #  Estimated natural or residual mortality
        ln_mean_F = 1, # Mean fleet-specific fishing mortality
        ln_Flimit = 3, # Estimated F limit
        ln_Ftarget = 3, # Estimated F target
        ln_Finit = 3, # Estimated fishing mortality for non-equilibrium initial age-structure
        proj_F_prop = 1, # Fixed fleet-specific proportion of Flimit and Ftarget apportioned within each species
        F_dev = 1, # Annual fleet specific fishing mortality deviates
        ln_srv_q = 3, # Survey catchability
        ln_srv_q_dev = 5, # Annual survey catchability deviates (if time-varying)
        ln_sigma_srv_q = 4, # Prior SD for survey catchability deviates
        ln_sigma_time_varying_srv_q = 4, # SD for annual survey catchability deviates (if time-varying)
        sel_coff = 3, # Non-parametric selectivity coefficients
        sel_coff_dev = 4, # Annual deviates for non-parametric selectivity coefficients
        ln_sel_slp = 3, # Slope parameters for logistic forms of selectivity
        sel_inf = 3, # Asymptote parameters for logistic forms of selectivity
        ln_sel_slp_dev = 5, # Annual deviates for slope parameters for logistic forms of selectivity (if time-varying)
        sel_inf_dev = 5, # Annual deviates for asymptote parameters for logistic forms of selectivity (if time-varying)
        ln_sigma_sel = 4, # SD for annual selectivity deviates (if time-varying)
        sel_curve_pen = 4, # Penalty for non-parametric selectivity
        ln_sigma_srv_index = 2, # Log SD for survey lognormal index likelihood (usually input)
        ln_sigma_fsh_catch = 2, # Log SD for lognormal catch likelihood (usually input)
        comp_weights = 4, # Weights for multinomial comp likelihood
        logH_1 = 6,  # Functional form parameter (not used in MSVPA functional form)
        logH_1a = 6, # Functional form parameter (not used in MSVPA functional form)
        logH_1b = 6, # Functional form parameter (not used in MSVPA functional form)
        logH_2 = 6, # Functional form parameter (not used in MSVPA functional form)
        logH_3 = 6, # Functional form parameter (not used in MSVPA functional form)
        H_4 = 6, # Functional form parameter (not used in MSVPA functional form)
        log_gam_a = 5, # Suitability parameter (not used in MSVPA style)
        log_gam_b = 5, # Suitability parameter (not used in MSVPA style)
        log_phi = 5 # Suitability parameter (not used in MSVPA style)
      )


      # debugphase = list(
      #   dummy = 1,
      #   ln_pop_scalar = 5, # Scalar for input numbers-at-age
      #   rec_pars = 1, # Stock-recruit parameters or log(mean rec) if no stock-recruit relationship
      #   ln_rec_sigma = 4, # Variance for annual recruitment deviats
      #   rec_dev = 2, # Annual recruitment deviats
      #   init_dev = 3, # Age specific initial age-structure deviates or parameters
      #   ln_sex_ratio_sigma = 3, # Variance of sex ratio (usually fixed)
      #   ln_M1 = 4, #  Estimated natural or residual mortality
      #   ln_mean_F = 6, # Mean fleet-specific fishing mortality
      #   ln_Flimit = 15, # Estimated F limit
      #   ln_Ftarget = 15, # Estimated F target
      #   ln_Finit = 7, # Estimated fishing mortality for non-equilibrium initial age-structure
      #   proj_F_prop = 14, # Fixed fleet-specific proportion of Flimit and Ftarget apportioned within each species
      #   F_dev = 7, # Annual fleet specific fishing mortality deviates
      #   ln_srv_q = 10, # Survey catchability
      #   ln_srv_q_dev = 11, # Annual survey catchability deviates (if time-varying)
      #   ln_sigma_srv_q = 15, # Prior SD for survey catchability deviates
      #   ln_sigma_time_varying_srv_q = 15, # SD for annual survey catchability deviates (if time-varying)
      #   sel_coff = 8, # Non-parametric selectivity coefficients
      #   sel_coff_dev = 11, # Annual deviates for non-parametric selectivity coefficients
      #   ln_sel_slp = 9, # Slope parameters for logistic forms of selectivity
      #   sel_inf = 9, # Asymptote parameters for logistic forms of selectivity
      #   ln_sel_slp_dev = 11, # Annual deviates for slope parameters for logistic forms of selectivity (if time-varying)
      #   sel_inf_dev = 11, # Annual deviates for asymptote parameters for logistic forms of selectivity (if time-varying)
      #   ln_sigma_sel = 12, # SD for annual selectivity deviates (if time-varying)
      #   sel_curve_pen = 13, # Penalty for non-parametric selectivity
      #   ln_sigma_srv_index = 14, # Log SD for survey lognormal index likelihood (usually input)
      #   ln_sigma_fsh_catch = 14, # Log SD for lognormal catch likelihood (usually input)
      #   comp_weights = 15, # Weights for multinomial comp likelihood
      #   logH_1 = 15,  # Functional form parameter (not used in MSVPA functional form)
      #   logH_1a = 15, # Functional form parameter (not used in MSVPA functional form)
      #   logH_1b = 15, # Functional form parameter (not used in MSVPA functional form)
      #   logH_2 = 15, # Functional form parameter (not used in MSVPA functional form)
      #   logH_3 = 15, # Functional form parameter (not used in MSVPA functional form)
      #   H_4 = 15, # Functional form parameter (not used in MSVPA functional form)
      #   log_gam_a = 15, # Suitability parameter (not used in MSVPA style)
      #   log_gam_b = 15, # Suitability parameter (not used in MSVPA style)
      #   log_phi = 15 # Suitability parameter (not used in MSVPA style)
      # )
    }
  }

  if(class(phase) == "character"){
    if(tolower(phase) != "default"){
      warning("phase misspecified: please set to 'default' or list with the same order as parameters.")
    }
  }
}

step = 5
if(!is.null(phase) & estimateMode %in% c(0,1) ){
  if(verbose > 0) {message(paste0("Step ", step,": Phasing begin"))}
  phase_pars <- Rceattle::TMBphase(
    data = data_list_reorganized,
    parameters = start_par,
    map = map$mapFactor,
    random = random_vars,
    phases = phase,
    model_name = TMBfilename,
    silent = verbose != 2,
    use_gradient = use_gradient,
    control = control
  )

  start_par <- phase_pars

  if(verbose > 0) {message(paste0("Step ", step,": Phasing complete - getting final estimates"))}
  step = step + 1
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# STEP 9: Fit hindcast ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
if(estimateMode != 2){ # dont build if projection and estimating HCR parameters
  if(sum(as.numeric(unlist(map$mapFactor)), na.rm = TRUE) == 0){stop("Map of length 0: all NAs")}
  obj = TMB::MakeADFun(
    data_list_reorganized,
    parameters = start_par,
    DLL = TMBfilename,
    map = map$mapFactor,
    random = random_vars,
    silent = verbose != 2
  )
}

# -- Save objects
mod_objects <-
  list(
    TMBfilename = TMBfilename,
    initial_params = start_par,
    bounds = bounds,
    map = map
  )

if(verbose > 0) {message(paste0("Step ",step, ": final build complete. Optimizing."))}
step = step + 1


# -- Optimize hindcast
if(estimateMode %in% c(0,1,4)){
  opt = Rceattle::fit_tmb(obj = obj,
                           fn=obj$fn,
                           gr=obj$gr,
                           startpar=obj$par,
                           lower = L,
                           upper = U,
                           loopnum = loopnum,
                           getsd = getsd,
                           control = control,
                           getJointPrecision = getJointPrecision,
                           quiet = verbose < 2,
  )
  if(verbose > 0) {message("Step ",step, ": Final optimization complete")
    step = step + 1
  }

  # -- Convergence warnings
  if(estimateMode %in% c(0,1)){
    # Bad parameter identification
    if(is.null(opt$SD) & getsd){
      identified <- suppressMessages(TMBhelper::check_estimability(obj))

      # Make into list
      identified_param_list <- obj$env$parList(identified$BadParams$Param_check)
      identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==0,"Not estimated",x), how = "replace")
      identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==1,"OK",x), how = "replace")
      identified_param_list <- rapply(identified_param_list,function(x) ifelse(x==2,"BAD",x), how = "replace")
      identified$param_list <- identified_param_list
      mod_objects$identified <- identified
    }
  }
}

# -- Get MLEs
if (estimateMode > 1) { # Debugging and projection only: use initial parameters
  last_par <- start_par
} else{
  if(!random_rec){
    last_par = try(obj$env$parList(obj$env$last.par.best)) # FIXME: maybe add obj$env$last.par.best inside?
  } else {
    last_par = try(obj$env$parList())
  }
}

