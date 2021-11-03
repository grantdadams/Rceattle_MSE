load("R/Runs/mse1.RData")
load("R/Runs/mse2.RData")
load("R/Runs/mse3.RData")
load("R/Runs/mse4.RData")
load("R/Runs/mse5.RData")
load("R/Runs/mse6.RData")


# mse1 MS-OM, SS-Est M Tier 3 EM
# mse2 SS-OM, SS-Est M Tier 3 EM
# mse3 SS-OM, SS-Fixed M Tier 3 EM
# mse4 MS-OM, SS-Fixed M Tier 3 EM
# mse5 MS-OM, SS-Fixed M No Catch EM
# mse6 SS-OM, SS-Fixed M No Catch EM


Average catch
Interannual catch variation
SSB/SSB40% from single species model
Probability of being lower that SSB20% from single species model


mse_summary <- function(mse){
  
  # Model dimensions
  nspp <- mse1$OM_list$OM_Sim_1$data_list$nspp
  nflts <- nrow(mse1$OM_list$OM_Sim_1$data_list$fleet_control)
  flt_type <- mse1$OM_list$OM_Sim_1$data_list$fleet_control$Fleet_type
  styr <- mse1$EM_list$OM_Sim_1$EM$data_list$styr
  endyr <- mse1$EM_list$OM_Sim_1$EM$data_list$endyr
  projyr <- mse1$EM_list$OM_Sim_1$EM$data_list$projyr
  projyrs <- (endyr+1):projyr
  projyrs_ind <- projyrs - endyr
  
  # MSE specifications
  nsim <- length(mse1$OM_list)
  
  catch_summary_stats <- data.frame(matrix(0, nrow = nflts, ncol = 5))
  colnames(catch_summary_stats) <- c("Fleet_name", "Fleet_code","Average Catch", "Catch IAV", "EM: P(Fy > FOFL)")
  catch_summary_stats$Fleet_code <- mse1$OM_list$OM_Sim_1$data_list$fleet_control$Fleet_code
  catch_summary_stats$Fleet_name <- mse1$OM_list$OM_Sim_1$data_list$fleet_control$Fleet_name
  
  biomass_summary_stats <-  data.frame(matrix(0, nrow = nspp, ncol = 4))
  colnames(biomass_summary_stats) <-   c("Species", "EM: P(SSB < SSB20)", "EM: Terminal SSB/SSB40", "Bias")
  biomass_summary_stats$Species <- mse1$OM_list$OM_Sim_1$data_list$spnames
  
  
  # Catch performance metrics
  # - Average catch
  # - Catch IAV
  # - EM: P(Fy > FOFL)
  for(flt in 1:nflts){
    if(flt_type[flt] == 1){ 
      # - Mean catch
      catch_summary_stats$`Average Catch`[flt] <- mean(sapply(mse1$OM_list, function(x) 
        x$data_list$fsh_biom$Catch[which(x$data_list$fsh_biom$Fleet_code == flt &
                                           x$data_list$fsh_biom$Year %in% projyrs)]), na.rm = TRUE)
      
      # - Catch IAV
      catch_list_tmp <- lapply(mse1$OM_list, function(x) 
        x$data_list$fsh_biom$Catch[which(x$data_list$fsh_biom$Fleet_code == flt &
                                           x$data_list$fsh_biom$Year %in% projyrs)])
      # -- Average across simulations
      for(sim in 1:nsim){
        catch_summary_stats$`Catch IAV`[flt] <- catch_summary_stats$`Catch IAV`[flt] + (sum((catch_list_tmp[[sim]][projyrs_ind[-1]] - catch_list_tmp[[sim]][projyrs_ind[-length(projyrs_ind)]])^2, na.rm = TRUE)/(length(projyrs_ind) - 1) / (sum(catch_list_tmp[[sim]][projyrs_ind], na.rm = TRUE)/ length(projyrs_ind)))/nsim
      }
      
      # - EM: P(Fy > FOFL)
      
      sb_sb40_tmp <- lapply(mse1$EM_list, function(x) x[[length(x)]]$quantities$F[sp, (projyrs - styr + 1)]/ (x[[length(x)]]$quantities$SB40[sp]))
    }
  }
  
  # Biomass performance metrics
  #FIXME currently based on EM rather than OM
  # - EM: P(SSB < SSB20)
  # - EM: Terminal SSB/SSB40
  for(sp in 1:nspp){
    sb_sb20_tmp <- lapply(mse1$EM_list, function(x) x[[length(x)]]$quantities$biomassSSB[sp, (projyrs - styr + 1)]/ (x[[length(x)]]$quantities$SB0[sp] * 0.2))
    sb_sb40_tmp <- lapply(mse1$EM_list, function(x) x[[length(x)]]$quantities$biomassSSB[sp, (projyrs - styr + 1)]/ (x[[length(x)]]$quantities$SB40[sp]))
    
    biomass_summary_stats$`EM: P(SSB < SSB20)`[sp] <- sum(unlist(sb_sb20_tmp) < 1) / length(unlist(sb_sb20_tmp))
    biomass_summary_stats$`EM: Terminal SSB/SSB40`[sp] <- mean(sapply(sb_sb40_tmp, function(x) x[length(x)]))
  }
  
  biomass_trajectory <- data.frame(matrix(NA, nrow = 5, ncol = 6))
  
}