# Histogram and table of performance metrics rest is coommented out

## Libraries ----
library(fmsb)
library(gmRi) # devtools::install_github("https://github.com/gulfofmaine/gmRi")
library(scales)
library(tidyr)
library(dplyr)
source("R/Functions/PM_table_functions.R")
source("R/Functions/PM_histogram_function_defense.R")

mse_histogram_defense(species = "Pollock", file = "Results/Figures/Histograms by PM/Pollock/", height = 6, width = 12)
