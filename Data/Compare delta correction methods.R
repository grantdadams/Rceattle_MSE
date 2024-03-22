pacman::p_load(mgcv, dplyr, lubridate, ggplot2, tidyr, sf, tidyverse)


summer_bt_data <- read.csv("Data/goa_temp_610_to_630_summer_300M.csv") %>%
  filter(depthclass == "Bottom", hind == "yes")  %>%
  mutate(varname = "summer bt",
         sd = "yes") %>%
  rename(value = mean_value_dc_610_to_630)

winter_sst_data <- read.csv("Data/goa_temp_610_to_630_winter_300M.csv") %>%
  filter(depthclass == "Surface", hind == "yes")  %>%
  mutate(varname = "winter sst",
         sd = "yes") %>%
  rename(value = mean_value_dc_610_to_630)

zoo_data <- read.csv("Data/goa_large_zoo_610_to_630_fall_300M.csv") %>%
  filter(depthclass == "Surface", hind == "yes") %>%
  mutate(varname = "mzl",
         sd = "yes") %>%
  rename(value = mean_value_dc_610_to_630)


summer_bt_data_nv <- read.csv("Data/goa_temp_610_to_630_summer_300M_no_var_delta.csv") %>%
  filter(depthclass == "Bottom", hind == "yes")  %>%
  mutate(varname = "summer bt",
         sd = "no") %>%
  rename(value = mean_value_dc_610_to_630)

winter_sst_data_nv <- read.csv("Data/goa_temp_610_to_630_winter_300M_no_var_delta.csv") %>%
  filter(depthclass == "Surface", hind == "yes")  %>%
  mutate(varname = "winter sst",
         sd = "no") %>%
  rename(value = mean_value_dc_610_to_630)

zoo_data_nv <- read.csv("Data/goa_large_zoo_610_to_630_fall_300M_no_var_delta.csv") %>%
  filter(depthclass == "Surface", hind == "yes") %>%
  mutate(varname = "mzl",
         sd = "no") %>%
  rename(value = mean_value_dc_610_to_630)


# -- Combine and plot
# - Zooplankton
goa_zoo_610_to_630 <- rbind(zoo_data, zoo_data_nv)

ggplot(goa_zoo_610_to_630, aes(year, value, colour = sd)) + geom_line() +
  ylab("Large zooplankton biomass") + xlab("Year") + facet_wrap(~simulation)


# - Winter SST
goa_zoo_610_to_630 <- rbind(winter_sst_data, winter_sst_data_nv)

ggplot(goa_zoo_610_to_630, aes(year, value, colour = sd)) + geom_line() +
  ylab("Winter SST (Celsius)") + xlab("Year") + facet_wrap(~simulation)

# - Summer BT
goa_zoo_610_to_630 <- rbind(summer_bt_data, summer_bt_data_nv)

ggplot(goa_zoo_610_to_630, aes(year, value, colour = sd)) + geom_line() +
  ylab("Winter SST (Celsius)") + xlab("Year") + facet_wrap(~simulation)


