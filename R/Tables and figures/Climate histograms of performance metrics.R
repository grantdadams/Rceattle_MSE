# Histogram and table of performance metrics (only NPFMC is used) rest is coommented out

## Libraries ----
library(fmsb)
library(gmRi) # devtools::install_github("https://github.com/gulfofmaine/gmRi")
library(scales)
library(tidyr)
library(dplyr)
source("R/Functions/Climate_PM_histogram_function.R")
source("R/Functions/Climate_PM_table_functions.R")


# - Combined
climate_mse_histogram(species = "Pollock", file = "Results/Climate MSE/Figures/Histograms by PM/Pollock/", height = 6, width = 12)
climate_mse_histogram(species = "Cod", file = "Results/Climate MSE/Figures/Histograms by PM/Cod/", height = 6, width = 12)
climate_mse_histogram(species = "Arrowtooth flounder", file = "Results/Climate MSE/Figures/Histograms by PM/ATF/", height = 6, width = 12)

climate_mse_histogram(species = "Pollock", file = "Results/Climate MSE/Figures/Histograms by PM/Pollock/", height = 6, width = 12, legend.pos = "bottomleft")
climate_mse_histogram(species = "Cod", file = "Results/Climate MSE/Figures/Histograms by PM/Cod/", height = 6, width = 12, legend.pos = "bottomleft")
climate_mse_histogram(species = "Arrowtooth flounder", file = "Results/Climate MSE/Figures/Histograms by PM/ATF/", height = 6, width = 12, legend.pos = "bottomleft")

climate_mse_histogram(species = "Pollock", file = "Results/Climate MSE/Figures/Histograms by PM/Pollock/", height = 6, width = 12, legend.pos = "topright")
climate_mse_histogram(species = "Cod", file = "Results/Climate MSE/Figures/Histograms by PM/Cod/", height = 6, width = 12, legend.pos = "topright")
climate_mse_histogram(species = "Arrowtooth flounder", file = "Results/Climate MSE/Figures/Histograms by PM/ATF/", height = 6, width = 12, legend.pos = "topright")

climate_mse_histogram(species = "Pollock", file = "Results/Climate MSE/Figures/Histograms by PM/Pollock/", height = 6, width = 12, legend.pos = "bottomright")
climate_mse_histogram(species = "Cod", file = "Results/Climate MSE/Figures/Histograms by PM/Cod/", height = 6, width = 12, legend.pos = "bottomright")
climate_mse_histogram(species = "Arrowtooth flounder", file = "Results/Climate MSE/Figures/Histograms by PM/ATF/", height = 6, width = 12, legend.pos = "bottomright")