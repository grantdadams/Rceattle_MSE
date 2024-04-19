pacman::p_load(Rceattle, readxl, dplyr, tidyr, writexl)
load("Models/GOA_20_1_1_mod_list.RData")
combined_data <- read_data(file = "Data/GOA_23_1_1_data_1977_2023_edited.xlsx")
combined_data$projyr <- 2100
combined_data$endyr <- 2020
alpha = exp(c(3.143, 1.975, 1.44))



## MSE Summary ----
# * Load MSE
model_names <- c("Climate naive", "SSP-126", "SSP-245", "SSP-585")
om_names <- paste0(rep(c("SS-", "MS-"), each = 4), model_names)
em_hcr_names <- c("SS_fixM_Tier3_EM") # , "SS_estM_Tier3_EM")

mse_list <- list()
ind <- 1
for(om in 1:length(om_names)){
  for(em in 1:length(em_hcr_names)){
    mse_list[[ind]] <- load_mse(dir = paste0("Runs/GOA2020_Climate_Projections/", om_names[om],"/", em_hcr_names[em]), file = NULL)
    names(mse_list)[ind] <- paste0(om_names[om]," OM - ", em_hcr_names[em])
    ind = ind+1
  }
}


# * Get catch ----
OMs <- list()
TEMs <- list()
catch_list <- list()

# -- Loop through scenarios
for(i in 1:length(mse_list)){
  OMs[[i]] <- list()
  TEMs[[i]] <- list()
  catch_list[[i]] <- list()
  
  # -- Rename
  names(OMs)[i] <- names(mse_list)[i]
  names(TEMs)[i] <- names(mse_list)[i]
  names(catch_list)[i] <- names(mse_list)[i]
  
  # -- Loop through sims
  for(j in 1:length(mse_list[[i]])){
    OMs[[i]][[j]] <- mse_list[[i]][[j]]$OM
    OMs[[i]][[j]]$MSE = names(mse_list)[i]
    OMs[[i]][[j]]$SIM = j
    
    TEMs[[i]][[j]] <- mse_list[[i]][[j]]$EM[[length(mse_list[[i]][[j]]$EM)]]
    
    
    # -- Get catch
    catch_list[[i]][[j]] <- OMs[[i]][[j]]$data_list$fsh_biom
    OMs[[i]][[j]]$data_list$fsh_biom$MSE = names(mse_list)[i]
    OMs[[i]][[j]]$data_list$fsh_biom$SIM = j
    
    catch_list[[i]][[j]]$Catch[which(catch_list[[i]][[j]]$Year > 2023)] <- OMs[[i]][[j]]$quantities$fsh_bio_hat[which(catch_list[[i]][[j]]$Year > 2023)]
    
    catch_list[[i]][[j]] <- catch_list[[i]][[j]] %>%
      select(-Fleet_code, - Species, - Month, - Selectivity_block, -Log_sd) %>%
      mutate(MSE = names(mse_list)[i],
             SIM = j) %>%
      pivot_wider(names_from = Fleet_name, values_from = c(Catch)) %>%
      as.data.frame() %>%
      relocate(MSE, .before = Year)
  }
  
  # - Combine
  catch_list[[i]] <- do.call("rbind", catch_list[[i]])
}
writexl::write_xlsx(catch_list, path = "Results/Projections/MSE_catch_timeseries.xlsx")



# Plot settings ----
ss_col <- nmfspalette::nmfs_palette("seagrass")(7)[1:4]
ms_col <- nmfspalette::nmfs_palette("oceans")(7)[1:4]

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
## END


source("~/GitHub/Rceattle_MSE/plot catch.R")

# Plot catch ----
# * FIX-M -----
# - Get max catch
fixm_catch_mods <- c(OMs[[1]], OMs[[5]], OMs[[2]], OMs[[6]], OMs[[3]], OMs[[7]], OMs[[4]], OMs[[8]])
fixm_catch_mods <- do.call("rbind", lapply(fixm_catch_mods, function(x) x$data_list$fsh_biom))
max_catch <- fixm_catch_mods %>%
  group_by(Fleet_name, Fleet_code) %>%
  summarise(MaxCatch = max(Catch)) %>%
  arrange(Fleet_code)

# - Climate naive
plot_catch_mse_proj(list(OMs[[5]], OMs[[1]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5), top_adj = 1.05, file = "Results/Projections/Catch/FixM_climate_naive", lwd = 0.5, width = 10, ymax = max_catch$MaxCatch)

# - SSP126
plot_catch_mse_proj(list(OMs[[6]], OMs[[2]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5), top_adj = 1.05, file = "Results/Projections/Catch/FixM_ssp126", lwd = 0.5, width = 10, ymax = max_catch$MaxCatch)

# - SSP245
plot_catch_mse_proj(list(OMs[[7]], OMs[[3]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5), top_adj = 1.05, file = "Results/Projections/Catch/FixM_ssp245", lwd = 0.5, width = 10, ymax = max_catch$MaxCatch)

# - SPP585
plot_catch_mse_proj(list(OMs[[8]], OMs[[4]]), line_col = (adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5)), top_adj = 1.05, file = "Results/Projections/Catch/FixM_ssp585", lwd = 0.5, width = 10, ymax = max_catch$MaxCatch)


mean_catch <- fixm_catch_mods %>%
  filter(Year > 2090) %>%
  group_by(Fleet_name, Fleet_code, MSE, Year) %>%
  summarise(Catch = mean(Catch)) %>%
  arrange(Fleet_code, MSE, Year)

write.csv(mean_catch, "mean_catch.csv")



fixm_catch_mods <- c(OMs[[1]], OMs[[5]], OMs[[2]], OMs[[6]], OMs[[3]], OMs[[7]], OMs[[4]], OMs[[8]])
mse_names <- sapply(OMs, function(x) x$MSE)
sim_names <- sapply(OMs, function(x) x$SIM)

recs <- lapply(OMs, function(x) x$quantities$R[1,])
for(i in 1:length(recs)){
  recs[[i]] <- data.frame(Year = names(recs[[i]]), R = recs[[i]], MSE = mse_names[i], SIM = sim_names[i])
}

recs <- do.call("rbind", recs)

mean_rec <- recs %>%
  filter(Year > 2090) %>%
  group_by(MSE, Year) %>%
  summarise(MeanR = mean(R)) %>%
  arrange(Year, MSE)

write.csv(mean_rec, "mean_rec.csv")




# # * Est-M ----
# # - Climate naive
# plot_catch(c(OMs[[2]], OMs[[10]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5), top_adj = 1.05, file = "Results/Projections/Catch/EstM_climate_naive", lwd = 0.5, width = 10)
# 
# # - SSP126
# plot_catch(c(OMs[[4]], OMs[[12]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5), top_adj = 1.05, file = "Results/Projections/Catch/EstM_ssp126", lwd = 0.5, width = 10)
# 
# # - SSP245
# plot_catch(c(OMs[[6]], OMs[[14]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5), top_adj = 1.05, file = "Results/Projections/Catch/EstM_ssp245", lwd = 0.5, width = 10)
# 
# # - SPP585
# plot_catch(c(OMs[[8]], OMs[[16]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5), top_adj = 1.05, file = "Results/Projections/Catch/EstM_ssp585", lwd = 0.5, width = 10)


# Plot SSB ----
# * FIX-M ----
# - Climate naive
plot_ssb_mse_proj(list(OMs[[1]], OMs[[5]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.7), file = "Results/Projections/SSB/FixM_climate_naive", lwd = 1, ymax = c(3, 1.2, 0.5))

# - SSP126
plot_ssb_mse_proj(list(OMs[[2]], OMs[[6]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.7), file = "Results/Projections/SSB/FixM_ssp126", lwd = 1, ymax = c(3, 1.2, 0.5))

# - SSP245
plot_ssb_mse_proj(list(OMs[[3]], OMs[[7]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.7), file = "Results/Projections/SSB/FixM_ssp245", lwd = 1, ymax = c(3, 1.2, 0.5))

# - SPP585
plot_ssb_mse_proj(list(OMs[[4]], OMs[[8]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.7), file = "Results/Projections/SSB/FixM_ssp585", lwd = 1, ymax = c(3, 1.2, 0.5))


# # * Est-M ----
# # - Climate naive
# plot_ssb(list(OMs[[2]], OMs[[10]]), line_col = (c(rep(ms_col[3], 10), rep(ss_col[3], 10))), file = "Results/Projections/SSB/EstM_climate_naive", lwd = 1, ymax = c(4, 1.2, 0.5))
# 
# # - SSP126
# plot_ssb(list(OMs[[4]], OMs[[12]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5), file = "Results/Projections/SSB/EstM_ssp126", lwd = 1, ymax = c(4, 1.2, 0.5))
# 
# # - SSP245
# plot_ssb(list(OMs[[6]], OMs[[14]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5), file = "Results/Projections/SSB/EstM_ssp245", lwd = 1, ymax = c(4, 1.2, 0.5))
# 
# # - SPP585
# plot_ssb(list(OMs[[8]], OMs[[16]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5), file = "Results/Projections/SSB/EstM_ssp585", lwd = 1, ymax = c(4, 1.2, 0.5))



# Plot R ----
# * FIX-M ----
# - Climate naive
plot_ssb_mse_proj(list(OMs[[1]], OMs[[5]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.7), file = "Results/Projections/SSB/FixM_climate_naive", lwd = 1, ymax =  c(80, 8, 0.7), rec = TRUE)

# - SSP126
plot_ssb_mse_proj(list(OMs[[2]], OMs[[6]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.7), file = "Results/Projections/SSB/FixM_ssp126", lwd = 1, ymax =  c(80, 8, 0.7), rec = TRUE)

# - SSP245
plot_ssb_mse_proj(list(OMs[[3]], OMs[[7]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.7), file = "Results/Projections/SSB/FixM_ssp245", lwd = 1, ymax =  c(80, 8, 0.7), rec = TRUE)

# - SPP585
plot_ssb_mse_proj(list(OMs[[4]], OMs[[8]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.7), file = "Results/Projections/SSB/FixM_ssp585", lwd = 1, ymax =  c(80, 8, 0.7), rec = TRUE)



# # * Est-M ----
# # - Climate naive
# plot_recruitment(list(OMs[[2]], OMs[[10]]), line_col = (c(rep(ms_col[3], 10), rep(ss_col[3], 10))), file = "Results/Projections/Recruitment/EstM_climate_naive", lwd = 1, ymax = c(80, 8, 0.7))
# 
# # - SSP126
# plot_recruitment(list(OMs[[4]], OMs[[12]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5), file = "Results/Projections/Recruitment/EstM_ssp126", lwd = 1, ymax = c(80, 8, 0.7))
# 
# # - SSP245
# plot_recruitment(list(OMs[[6]], OMs[[14]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5), file = "Results/Projections/Recruitment/EstM_ssp245", lwd = 1, ymax = c(80, 8, 0.7))
# 
# # - SPP585
# plot_recruitment(list(OMs[[8]], OMs[[16]]), line_col = adjustcolor(c(ms_col[3], ss_col[3]), alpha.f = 0.5), file = "Results/Projections/Recruitment/EstM_ssp585", lwd = 1, ymax = c(80, 8, 0.7))