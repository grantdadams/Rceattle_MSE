

plot_sr <- function(Rceattle = NULL){
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  
  yrs <- 1:length(Rceattle$data_list$styr:Rceattle$data_list$endyr)
  rec_df <- as.data.frame(t(Rceattle$quantities$R[,yrs]))
  ssb_df <- as.data.frame(t(Rceattle$quantities$biomassSSB[,yrs]))
  ssb_df$year <- rownames(ssb_df)
  rec_df$year <- rownames(rec_df)
  
  ssb_df <- pivot_longer(ssb_df, cols = 1:3) %>%
    rename(SSB = value)
  rec_df <- pivot_longer(rec_df, cols = 1:3) %>%
    rename(Age1R = value)
  
  sr_df <- merge(ssb_df, rec_df) %>%
    group_by(name) %>%
    arrange(year) %>%
    mutate(lagSSB = lag(SSB))
  
  print(ggplot(sr_df, aes(x = lagSSB, y = Age1R)) +
    geom_point() + 
    facet_wrap(~name, scales = "free"))

}