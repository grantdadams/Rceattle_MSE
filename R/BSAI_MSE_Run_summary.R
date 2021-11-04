library(Rceattle)

load("R/Runs/mse1.RData")
load("R/Runs/mse2.RData")
load("R/Runs/mse3.RData")
load("R/Runs/mse4.RData")
load("R/Runs/mse1b.RData")
load("R/Runs/mse2b.RData")
load("R/Runs/mse3b.RData")
load("R/Runs/mse4b.RData")
load("R/Runs/mse5.RData")
load("R/Runs/mse6.RData")


# mse1 MS-OM, SS-Est M Tier 3 EM
# mse2 SS-OM, SS-Est M Tier 3 EM
# mse4 MS-OM, SS-Fixed M Tier 3 EM
# mse3 SS-OM, SS-Fixed M Tier 3 EM
# mse5 MS-OM, SS-Fixed M No Catch EM
# mse6 SS-OM, SS-Fixed M No Catch EM
# mse1b MS-OM, SS-Est M Tier 3 EM - no cap
# mse2b SS-OM, SS-Est M Tier 3 EM - no cap
# mse4b MS-OM, SS-Fixed M Tier 3 EM - no cap
# mse3b SS-OM, SS-Fixed M Tier 3 EM - no cap


# Average catch
# Interannual catch variation
# SSB/SSB40% from single species model
# Probability of being lower that SSB20% from single species model


mse_summary <- function(mse){
  
  # Model dimensions
  nspp <- mse$OM_list$OM_Sim_1$data_list$nspp
  nflts <- nrow(mse$OM_list$OM_Sim_1$data_list$fleet_control)
  flt_type <- mse$OM_list$OM_Sim_1$data_list$fleet_control$Fleet_type
  styr <- mse$EM_list$OM_Sim_1$EM$data_list$styr
  endyr <- mse$EM_list$OM_Sim_1$EM$data_list$endyr
  projyr <- mse$EM_list$OM_Sim_1$EM$data_list$projyr
  projyrs <- (endyr+1):projyr
  projyrs_ind <- projyrs - endyr
  
  # MSE specifications
  nsim <- length(mse$OM_list)
  
  # MSE Output
  catch_summary_stats <- data.frame(matrix(0, nrow = nflts, ncol = 5))
  colnames(catch_summary_stats) <- c("Fleet_name", "Fleet_code","Average Catch", "Catch IAV", "EM: P(Fy > FOFL)")
  catch_summary_stats$Fleet_code <- mse$OM_list$OM_Sim_1$data_list$fleet_control$Fleet_code
  catch_summary_stats$Fleet_name <- mse$OM_list$OM_Sim_1$data_list$fleet_control$Fleet_name
  
  biomass_summary_stats <-  data.frame(matrix(0, nrow = nspp, ncol = 4))
  colnames(biomass_summary_stats) <-   c("Species", "EM: P(SSB < SSB20)", "EM: Terminal SSB/SSB40", "Avg SSB MSE")
  biomass_summary_stats$Species <- mse$OM_list$OM_Sim_1$data_list$spnames
  
  
  # Catch performance metrics
  # - Average catch
  # - Catch IAV
  # - EM: P(Fy > FOFL)
  for(flt in 1:nflts){
    if(flt_type[flt] == 1){ 
      # - Mean catch
      catch_summary_stats$`Average Catch`[flt] <- mean(sapply(mse$OM_list, function(x) 
        x$data_list$fsh_biom$Catch[which(x$data_list$fsh_biom$Fleet_code == flt &
                                           x$data_list$fsh_biom$Year %in% projyrs)]), na.rm = TRUE)
      
      # - Catch IAV
      catch_list_tmp <- lapply(mse$OM_list, function(x) 
        x$data_list$fsh_biom$Catch[which(x$data_list$fsh_biom$Fleet_code == flt &
                                           x$data_list$fsh_biom$Year %in% projyrs)])
      # -- Average across simulations
      for(sim in 1:nsim){
        catch_summary_stats$`Catch IAV`[flt] <- catch_summary_stats$`Catch IAV`[flt] + (sum((catch_list_tmp[[sim]][projyrs_ind[-1]] - catch_list_tmp[[sim]][projyrs_ind[-length(projyrs_ind)]])^2, na.rm = TRUE)/(length(projyrs_ind) - 1) / (sum(catch_list_tmp[[sim]][projyrs_ind], na.rm = TRUE)/ length(projyrs_ind)))/nsim
      }
      
      # - EM: P(Fy > FOFL)
      #TODO
      # sb_sb40_tmp <- lapply(mse$EM_list, function(x) x[[length(x)]]$quantities$F[sp, (projyrs - styr + 1)]/ (x[[length(x)]]$quantities$SB40[sp]))
    }
  }
  
  # Biomass performance metrics
  #FIXME currently based on EM rather than OM
  # - EM: P(SSB < SSB20)
  # - EM: Terminal SSB/SSB40
  for(sp in 1:nspp){
    sb_sb20_tmp <- lapply(mse$EM_list, function(x) x[[length(x)]]$quantities$biomassSSB[sp, (projyrs - styr + 1)]/ (x[[length(x)]]$quantities$SB0[sp] * 0.2))
    sb_sb40_tmp <- lapply(mse$EM_list, function(x) x[[length(x)]]$quantities$biomassSSB[sp, (projyrs - styr + 1)]/ (x[[length(x)]]$quantities$SB40[sp]))
    
    biomass_summary_stats$`EM: P(SSB < SSB20)`[sp] <- sum(unlist(sb_sb20_tmp) < 1) / length(unlist(sb_sb20_tmp))
    biomass_summary_stats$`EM: Terminal SSB/SSB40`[sp] <- mean(sapply(sb_sb40_tmp, function(x) x[length(x)]))
    
    # - Bias in SSB
    sb_em_tmp <- lapply(mse$EM_list, function(x) x[[length(x)]]$quantities$biomassSSB[sp, (projyrs - styr + 1)])
    sb_om_tmp <- lapply(mse$OM_list, function(x) x$quantities$biomassSSB[sp, (projyrs - styr + 1)])
    
    biomass_summary_stats$`Avg SSB MSE`[sp] = mean((unlist(sb_em_tmp) -  unlist(sb_om_tmp))^2, na.rm = TRUE)
  }
  
  biomass_trajectory <- lapply(mse$EM_list, function(x) x[[length(x)]]$quantities$biomass[, (projyrs - styr + 1)])
  biomassSSB_trajectory <- lapply(mse$EM_list, function(x) x[[length(x)]]$quantities$biomassSSB[, (projyrs - styr + 1)])
  
  return(list(biomass_summary_stats = biomass_summary_stats, catch_summary_stats = catch_summary_stats, biomass_trajectory = biomass_trajectory, biomassSSB_trajectory = biomassSSB_trajectory))
  
}



mse_list <- list(mse1,
                 mse3,
                 mse1b,
                 mse3b,
                 mse5,
                 mse2,
                 mse4,
                 mse2b,
                 mse4b,
                 mse6)


summary_list <- list()
for(i in 1:length(mse_list)){
  summary_list[[i]] <- mse_summary(mse_list[[i]])
}

MSE_names <- c("mse1 MS-OM, SS-Est M Tier 3 EM"
               , "mse3 MS-OM, SS-Fixed M Tier 3 EM"
               , "mse1b MS-OM, SS-Est M Tier 3 EM - no cap"
               , "mse3b MS-OM, SS-Fixed M Tier 3 EM - no cap"
               , "mse5 MS-OM, SS-Fixed M No Catch EM"
               , "mse2 SS-OM, SS-Est M Tier 3 EM"
               , "mse4 SS-OM, SS-Fixed M Tier 3 EM"
               , "mse2b SS-OM, SS-Est M Tier 3 EM - no cap"
               , "mse4b SS-OM, SS-Fixed M Tier 3 EM - no cap"
               , "mse6 SS-OM, SS-Fixed M No Catch EM")


avg_catch <- data.frame(matrix(NA, 3, ncol = length(mse_list)))
colnames(avg_catch) <- MSE_names
rownames(avg_catch) <- c("Pollock", "Cod", "ATF")
ssb_mse <- catch_iav <- prob_overfished <- terminal_status <- avg_catch


for(i in 1:length(summary_list)){
  avg_catch[,i] <- summary_list[[i]]$catch_summary_stats$`Average Catch`[1:3]
  catch_iav[,i] <- summary_list[[i]]$catch_summary_stats$`Catch IAV`[1:3]
  terminal_status[,i] <- summary_list[[i]]$biomass_summary_stats$`EM: Terminal SSB/SSB40`
  prob_overfished[,i] <- summary_list[[i]]$biomass_summary_stats$`EM: P(SSB < SSB20)`
  ssb_mse[,i] <- summary_list[[i]]$biomass_summary_stats$`Avg SSB MSE`
}

library(writexl)
write_xlsx(list(avg_catch = avg_catch, catch_iav = catch_iav, terminal_status = terminal_status, prob_overfished = prob_overfished, ssb_mse = ssb_mse), path = "R/Results/BSAI_mse_results.xlsx")




