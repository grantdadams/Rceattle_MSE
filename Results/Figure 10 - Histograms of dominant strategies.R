# Histogram of dominant strategies

## Libraries ----
library(fmsb)
library(gmRi) # devtools::install_github("https://github.com/gulfofmaine/gmRi")
library(scales)
library(tidyr)
library(dplyr)
source("R/PM_table_functions.R")
source("R/PM_dominant_strategy_histogram_function.R")


## Figures for all PMs and Species ----
# - Combined
dom_histogram_two_system(species = "Pollock", file = "Results/Figures/Dominant strategy histograms/Pollock/", type = 1, height = 7, width = 14, legend.pos = "bottomleft")
dom_histogram_two_system(species = "Cod", file = "Results/Figures/Dominant strategy histograms/Cod/", type = 1, height = 6, width = 12, legend.pos = NA)
dom_histogram_two_system(species = "Arrowtooth flounder", file = "Results/Figures/Dominant strategy histograms/ATF/", type = 1, height = 6, width = 12, legend.pos = NA)

# - Economic
dom_histogram_two_system(species = "Pollock", file = "Results/Figures/Dominant strategy histograms/Pollock/", type = 2, height = 6, width = 12, legend.pos = NA)
dom_histogram_two_system(species = "Cod", file = "Results/Figures/Dominant strategy histograms/Cod/", type = 2, height = 6, width = 12, legend.pos = NA)
dom_histogram_two_system(species = "Arrowtooth flounder", file = "Results/Figures/Dominant strategy histograms/ATF/", type = 2, height = 6, width = 12, legend.pos = NA)

# - Conservation
dom_histogram_two_system(species = "Pollock", file = "Results/Figures/Dominant strategy histograms/Pollock/", type = 3, height = 6, width = 12, legend.pos = NA)
dom_histogram_two_system(species = "Cod", file = "Results/Figures/Dominant strategy histograms/Cod/", type = 3, height = 6, width = 12, legend.pos = NA)
dom_histogram_two_system(species = "Arrowtooth flounder", file = "Results/Figures/Dominant strategy histograms/ATF/", type = 3, height = 6, width = 12, legend.pos = NA)
