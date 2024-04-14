pacman::p_load(Rceattle, readxl, dplyr, tidyr, writexl)
load("Models/GOA_23_1_1_mod_list.RData")
combined_data <- read_data(file = "Data/GOA_23_1_1_data_1977_2023_edited.xlsx")
combined_data$projyr <- 2100
combined_data$endyr <- 2023
alpha = exp(c(3.143, 1.975, 1.44))

# Biomass-at-age
ms_mod <- mod_list_all[[3]]
biomass <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr, 
                      JuvPollock_Biomass = ms_mod$quantities$biomassByage[1,1,1,],
                      AdultPollock_Biomass = colSums(ms_mod$quantities$biomassByage[1,1,2:10,]),
                      JuvATF_Biomass = NA,
                      AdultATF_Biomass = NA,
                      JuvPollock_BiomassConsBy_JuvATF = NA,
                      JuvPollock_BiomassConsBy_AdultATF = NA,
                      AdultPollock_BiomassConsBy_JuvATF = NA,
                      AdultPollock_BiomassConsBy_AdultATF = NA
                      )

for(i in 1:nrow(biomass)){
  biomass$JuvATF_Biomass[i] <- sum(ms_mod$quantities$biomassByage[2,,1,i], na.rm = TRUE)
  biomass$AdultATF_Biomass[i] <- sum(ms_mod$quantities$biomassByage[2,,2:21,i], na.rm = TRUE)
  
  
  biomass$JuvPollock_BiomassConsBy_JuvATF[i] = sum(ms_mod$quantities$B_eaten[c(2,5),1,1,1,i])
  biomass$JuvPollock_BiomassConsBy_AdultATF[i] = sum(ms_mod$quantities$B_eaten[c(2,5),1,2:21,1,i])
  
  biomass$AdultPollock_BiomassConsBy_JuvATF[i] = sum(ms_mod$quantities$B_eaten[c(2,5),1,1,2:10,i])
  biomass$AdultPollock_BiomassConsBy_AdultATF[i] = sum(ms_mod$quantities$B_eaten[c(2,5),1,2:21,2:10,i])
}

write.csv(biomass, "2023 CEATTLE biomass and biomass eaten estimates age 1 juvenile.csv")


## Numbers
numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr)
numbers_at_age$Species <- "Pollock" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$NByage[1,1,,])))
numbers_at_age_pollock <- numbers_at_age

numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr)
numbers_at_age$Species <- "ATF_Females" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$NByage[2,1,,])))
numbers_at_age_atf <- numbers_at_age

numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr)
numbers_at_age$Species <- "ATF_Males" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$NByage[2,2,,])))

numbers_at_age_final <- rbind(numbers_at_age_pollock, numbers_at_age_atf, numbers_at_age)

## M1
numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr)
numbers_at_age$Species <- "Pollock" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$M1[1,1,])))
numbers_at_age_pollock <- numbers_at_age

numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr)
numbers_at_age$Species <- "ATF_Females" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$M1[2,1,])))
numbers_at_age_atf <- numbers_at_age

numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr)
numbers_at_age$Species <- "ATF_Males" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$M1[2,2,])))

m1_at_age <- rbind(numbers_at_age_pollock, numbers_at_age_atf, numbers_at_age)

## M2
numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr)
numbers_at_age$Species <- "Pollock" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$M2[1,1,,])))
numbers_at_age_pollock <- numbers_at_age

numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr)
numbers_at_age$Species <- "ATF_Females" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$M2[2,1,,])))
numbers_at_age_atf <- numbers_at_age

numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr)
numbers_at_age$Species <- "ATF_Males" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$M2[2,2,,])))

m2_at_age <- rbind(numbers_at_age_pollock, numbers_at_age_atf, numbers_at_age)

## M
numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr)
numbers_at_age$Species <- "Pollock" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$M[1,1,,])))
numbers_at_age_pollock <- numbers_at_age

numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr)
numbers_at_age$Species <- "ATF_Females" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$M[2,1,,])))
numbers_at_age_atf <- numbers_at_age

numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr)
numbers_at_age$Species <- "ATF_Males" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$M[2,2,,])))

m_at_age <- rbind(numbers_at_age_pollock, numbers_at_age_atf, numbers_at_age)

## F
numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr,
                             "FfullSel" = ms_mod$quantities$F_spp[1,])
numbers_at_age$Species <- "Pollock" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$F_spp_age[1,1,,1:74])))
numbers_at_age_pollock <- numbers_at_age

numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr,
                             "FfullSel" = ms_mod$quantities$F_spp[2,])
numbers_at_age$Species <- "ATF_Females" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$F_spp_age[2,1,,1:74])))
numbers_at_age_atf <- numbers_at_age

numbers_at_age <- data.frame(Year = ms_mod$data_list$styr:ms_mod$data_list$projyr,
                             "FfullSel" = ms_mod$quantities$F_spp[2,])
numbers_at_age$Species <- "ATF_Males" 
numbers_at_age <- cbind(numbers_at_age, as.data.frame(t(ms_mod$quantities$F_spp_age[2,2,,1:74])))

f_at_age <- rbind(numbers_at_age_pollock, numbers_at_age_atf, numbers_at_age)


write_xlsx(list(N = numbers_at_age_final, M = m_at_age, M1 = m1_at_age, M2 = m2_at_age, "F" = f_at_age), path = "2023 CEATTLE outputs.xlsx")
