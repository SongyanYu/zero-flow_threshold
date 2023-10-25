#---
# this script calculate three hydro-signatures of flow intermittency
# 1) zero-flow duration
# 2) timing of first zero flow
# 3) dry-down period - from peak to no flow
# author: Songyan Yu
# date created: 20/10/2021
# date updated: 28/03/2023
#---

options(scipen = 999)  # no scientific notation

# read in CAMELS_AUS stream flow data set
flow.data <- read.csv("Data/streamflow_MLd_inclInfilled.csv")
head(flow.data)

library(lubridate)
library(dplyr)
library(tidyr)
source("F_peak2zero.R")  # function to calculate the dry-down period

col.name<-colnames(flow.data)

# for sensitivity analysis
zero.flow <- seq(from = 0, to = 0.1, by = 0.004)

for (z in zero.flow){
  for(i in 4:225){   # column number of stream gauges
    
    gauge.flow<-
      flow.data %>%
      mutate(date = ymd(paste(year,"-",month,"-",day))) %>%
      select(year, month, day, date, col.name[i])
    
    head(gauge.flow)
    
    # hydro-signatures of flow intermittency
    df <-
      gauge.flow %>%
      rename(Q = col.name[i]) %>%
      filter(Q >= 0) %>%
      mutate(Q_rounded = ifelse(Q/86.4 <= z, 0, Q/86.4),   # ML/d -> m3/s
             CurrentClimYear = ifelse(month >= 7, year, year-1),      # get climate year
             dayOfClimYear = date - ymd(paste(CurrentClimYear,"-06-30"))) %>%   # get day within climate year
      group_by(CurrentClimYear) %>% 
      summarize(noflowdays_r = sum(Q_rounded == 0),
                totaldays_r = sum(is.finite(Q_rounded)),
                annualfractionnoflow_r = noflowdays_r/totaldays_r,
                zeroflowfirst_r = min(dayOfClimYear[Q_rounded == 0])) %>% 
      ungroup() %>%
      mutate(gauge_ID = col.name[i])
    
    flow.peak2z.r <- 
      gauge.flow %>%
      select(date, col.name[i]) %>%
      rename(Q = col.name[i]) %>%
      filter(Q >= 0) %>%
      mutate(Q = ifelse(Q/86.4 <= z, 0, Q/86.4)) %>%   # ML/d -> m3/s
      peak2zero(.) %>%
      mutate(gauge_ID = col.name[i]) %>%
      select(-Q)
    
    # trend analysis
    if(sum(df$annualfractionnoflow_r > 0) >= 10){
      manken_frac_r <- rkt::rkt(df$CurrentClimYear, df$annualfractionnoflow_r)$tau
    }else{
      manken_frac_r <- NA
    }
    
    if (sum(is.finite(df$zeroflowfirst_r)) >= 10){
      manken_first_r <- rkt::rkt(df$CurrentClimYear[is.finite(df$zeroflowfirst_r)], as.numeric(df$zeroflowfirst_r[is.finite(df$zeroflowfirst_r)]))$tau
    } else {
      manken_first_r <- NA
    }
    
    df_trends <- data.frame(gauge_ID = col.name[i],
                            tau_annualfractionnoflow_rounded = manken_frac_r,
                            tau_zeroflowfirst_rounded = manken_first_r)
    
    # combine all output
    if(i == 4){
      df_all <- df
      df_trends_all <- df_trends
      df_peak2z_r <- flow.peak2z.r
    }else{
      df_all <- bind_rows(df_all, df)
      df_trends_all <- bind_rows(df_trends_all, df_trends)
      df_peak2z_r <- bind_rows(df_peak2z_r, flow.peak2z.r)
    }
    
    #print(paste0("Completed ", i, " ", col.name[i], " of ", length(c(4:225)), " at ", Sys.time()))
  }
  
  # save output
  write.csv(df_all, file = paste0("Results/sensitivityAnalysis_zeroFlow_v2/01_flowMetrics_AnnualHydroMetrics_zeroFlow-",z,".csv"),row.names = FALSE)
  write.csv(df_peak2z_r, file = paste0("Results/sensitivityAnalysis_zeroFlow_v2/01_flowMetrics_peak2zero_zeroFlow-",z,".csv"), row.names = FALSE)
  
  # trend analysis for peak2z
  df_trends_all <-
    df_peak2z_r %>%
    mutate(Date = ymd(Date),
           Year = lubridate::year(Date),
           DoY = lubridate::decimal_date(Date)) %>%
    group_by(gauge_ID) %>%
    summarise(tau_peak2z_rounded = ifelse(length(unique(Year)) >= 10, rkt::rkt(DoY, peak2z_length)$tau, NA)) %>%
    left_join(df_trends_all, ., by = "gauge_ID")
  
  
  write.csv(df_trends_all, file = paste0("Results/sensitivityAnalysis_zeroFlow_v2/01_flowMetrics_HydroMetricsTrends_zeroFlow-",z,".csv"),row.names = FALSE)
  
  cat(z,"\n")
}

