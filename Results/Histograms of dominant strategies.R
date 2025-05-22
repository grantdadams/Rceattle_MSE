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
# - EBS
mse_histogram(system = "EBS", species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "EBS", species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "EBS", species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 4, width = 12)

# - GOA
mse_histogram(system = "GOA", species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "GOA", species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "GOA", species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 4, width = 12)


## Figures for economic PMs and Species ----
# - EBS
mse_histogram(system = "EBS", species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "EBS", species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "EBS", species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 4, width = 12)

# - GOA
mse_histogram(system = "GOA", species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "GOA", species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "GOA", species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 4, width = 12)


## Figures for conservation PMs and Species ----
# - EBS
mse_histogram(system = "EBS", species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "EBS", species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "EBS", species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 4, width = 12)

# - GOA
mse_histogram(system = "GOA", species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "GOA", species = "Cod", file = "Results/Figures/Histograms by PM/Cod/", allHCR = TRUE, height = 4, width = 12)
mse_histogram(system = "GOA", species = "Arrowtooth flounder", file = "Results/Figures/Histograms by PM/ATF/", allHCR = TRUE, height = 4, width = 12)
