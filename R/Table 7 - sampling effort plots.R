library(Rceattle)
data("BS2017SS")

# Survey
par(mfrow = c(length(unique(BS2017SS$srv_biom$Fleet_code)),1))
for(flt in unique(BS2017SS$srv_biom$Fleet_code)){
  dat_sub <- BS2017SS$srv_biom[which(BS2017SS$srv_biom$Fleet_code == flt),]
  plot(y = dat_sub$Log_sd, x = dat_sub$Year, type = "l", main = dat_sub$Fleet_name[1], ylab = "survey sd")
}

# Catch
par(mfrow = c(length(unique(BS2017SS$fsh_biom$Fleet_code)),1))
for(flt in unique(BS2017SS$fsh_biom$Fleet_code)){
  dat_sub <- BS2017SS$fsh_biom[which(BS2017SS$fsh_biom$Fleet_code == flt),]
  plot(y = dat_sub$Log_sd, x = dat_sub$Year, type = "l", main = dat_sub$Fleet_name[1] , ylab = "catch sd")
}

# Comp
par(mfrow = c(ceiling(length(unique(BS2017SS$comp_data$Fleet_code))/2),2))
for(flt in unique(BS2017SS$comp_data$Fleet_code)){
  dat_sub <- BS2017SS$comp_data[which(BS2017SS$comp_data$Fleet_code == flt),]
  plot(y = dat_sub$Sample_size, x = dat_sub$Year, type = "l", main = dat_sub$Fleet_name[1], ylab = "N")
}
