install.packages("pacman")
install.packages("TMB", type = "source")
install.packages("Matrix", type = "source")
pacman::p_load(dplyr,
               ggplot2,
               MASS,
               oce,
               readxl,
               TMB,
               devtools,
               writexl,
               reshape2,
               gplots,
               tidyr,
               testthat,
               foreach,
R.utils,
               knitr,
               doParallel)
devtools::install_github("kaskr/TMB_contrib_R/TMBhelper")
# install.packages("Rceattle_1.0.0.0000.tar.gz", repos = NULL, type = "source")
