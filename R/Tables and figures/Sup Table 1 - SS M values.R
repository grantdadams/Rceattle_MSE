
library(Rceattle)

# - Run models
fit_all = FALSE
source("R/BSAI_condition_models.R")
source("R/BSAI_condition_ricker_models.R")
mod_list_EBS <- list(ss_run, ss_run_M, ss_run_ricker_M)
plot_biomass(mod_list_EBS)


source("R/GOA_condition_models_1977.R")
source("R/GOA_condition_ricker_models_1977.R")
mod_list_GOA <- list(ss_run, ss_run_M, ss_run_ricker_M)
plot_biomass(mod_list_GOA)

mod_list <- c(mod_list_EBS, mod_list_GOA)

################################################
# SET UP TABLE
################################################


supp_table_1 <- data.frame(matrix(NA, ncol = length(mod_list)*4, nrow = 21))


mod = c(1:6, # Pollock
        1:6, # Cod
        1:3, # EBS ATF
        4:6, # GOA ATF F
        4:6  # GOA ATF M
        )
species = c(rep(1,6), # Pollock
            rep(2,3), # EBS Cod
            rep(3,3), # GOA Cod
            rep(3,3), # EBS ATF
            rep(2,6) # GOA ATF F & M
)

sex = c(rep(1,18), 
        rep(2,3))


for(i in 1:length(mod)){
  ages <- 1:mod_list[[mod[i]]]$data_list$nages[species[i]]
  supp_table_1[ages,i] <- (mod_list[[mod[i]]]$quantities$M1[species[i],sex[i], ages])
}

write.csv(supp_table_1, file = "Results/Tables/Supp_table_1_M1.csv")

