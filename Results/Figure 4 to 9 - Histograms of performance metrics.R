# Histogram and table of performance metrics 

## Libraries ----
library(fmsb)
library(gmRi) # devtools::install_github("https://github.com/gulfofmaine/gmRi")
library(scales)
library(tidyr)
library(dplyr)
source("R/PM_table_functions.R")
source("R/PM_histogram_function.R")


# - Combined (difference is where legend is)
mse_histogram_two_system(species = "Pollock", file = "Results/Figures/Performance metric histograms/Pollock/", allHCR = TRUE, height = 6, width = 12)
mse_histogram_two_system(species = "Cod", file = "Results/Figures/Performance metric histograms/Cod/", allHCR = TRUE, height = 6, width = 12)
mse_histogram_two_system(species = "Arrowtooth flounder", file = "Results/Figures/Performance metric histograms/ATF/", allHCR = TRUE, height = 6, width = 12)

mse_histogram_two_system(species = "Pollock", file = "Results/Figures/Performance metric histograms/Pollock/", allHCR = TRUE, height = 6, width = 12, legend.pos = "bottomleft")
mse_histogram_two_system(species = "Cod", file = "Results/Figures/Performance metric histograms/Cod/", allHCR = TRUE, height = 6, width = 12, legend.pos = "bottomleft")
mse_histogram_two_system(species = "Arrowtooth flounder", file = "Results/Figures/Performance metric histograms/ATF/", allHCR = TRUE, height = 6, width = 12, legend.pos = "bottomleft")

mse_histogram_two_system(species = "Pollock", file = "Results/Figures/Performance metric histograms/Pollock/", allHCR = TRUE, height = 6, width = 12, legend.pos = "topright")
mse_histogram_two_system(species = "Cod", file = "Results/Figures/Performance metric histograms/Cod/", allHCR = TRUE, height = 6, width = 12, legend.pos = "topright")
mse_histogram_two_system(species = "Arrowtooth flounder", file = "Results/Figures/Performance metric histograms/ATF/", allHCR = TRUE, height = 6, width = 12, legend.pos = "topright")

mse_histogram_two_system(species = "Pollock", file = "Results/Figures/Performance metric histograms/Pollock/", allHCR = TRUE, height = 6, width = 12, legend.pos = "bottomright")
mse_histogram_two_system(species = "Cod", file = "Results/Figures/Performance metric histograms/Cod/", allHCR = TRUE, height = 6, width = 12, legend.pos = "bottomright")
mse_histogram_two_system(species = "Arrowtooth flounder", file = "Results/Figures/Performance metric histograms/ATF/", allHCR = TRUE, height = 6, width = 12, legend.pos = "bottomright")
