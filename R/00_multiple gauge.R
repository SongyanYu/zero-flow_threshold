#---
# identify intermittent stream gauges in CAMEL_AUS for multiple-gauge case study
# author: Songyan Yu
# date: 15/12/2022
#---


library(dplyr)
library(ggplot2)

hydro.files <- list.files("Results/sensitivityAnalysis_zeroFlow/",
                          pattern = "flowMetrics_AnnualHydroMetrics",
                          full.names = TRUE)

peak2z.files <- list.files("Results/sensitivityAnalysis_zeroFlow/",
                           pattern = "peak2zero",
                           full.names = TRUE)
peak2z.lst <-
  lapply(peak2z.files, FUN = function(x){
    zero.flow <- strsplit(strsplit(strsplit(strsplit(x, split = "/")[[1]][3],split = "_")[[1]][4],split = "-")[[1]][2], split = ".csv")[[1]]
    df <-
      read.csv(x) %>%
      filter(!is.na(peak2z_length)) %>%
      group_by(gauge_ID) %>%
      summarise(avg_ann = mean(peak2z_length, na.rm = TRUE)) %>%
      mutate(zero_flow_threshold = zero.flow)
  })


zero.flow.duration.lst <-
  lapply(hydro.files, FUN = function(x) {
    zero.flow <- strsplit(strsplit(strsplit(strsplit(x, split = "/")[[1]][3],split = "_")[[1]][4],split = "-")[[1]][2], split = ".csv")[[1]]
    df <- 
      read.csv(x) %>%
      group_by(gauge_ID) %>%
      summarise(avg_ann = mean(annualfractionnoflow_r, na.rm = TRUE)) %>%
      mutate(zero_flow_threshold = zero.flow)
  })

zeroflow.first.lst <-
  lapply(hydro.files, FUN = function(x) {
    zero.flow <- strsplit(strsplit(strsplit(strsplit(x, split = "/")[[1]][3],split = "_")[[1]][4],split = "-")[[1]][2], split = ".csv")[[1]]
    df <- 
      read.csv(x) %>%
      filter(!is.infinite(zeroflowfirst_r)) %>%
      group_by(gauge_ID) %>%
      summarise(avg_ann = mean(zeroflowfirst_r, na.rm = TRUE)) %>%
      mutate(zero_flow_threshold = zero.flow)
  })


# box plot of average annual fraction of no flow 
# for all gauges across all zero flow thresholds.
boxplot.df <- 
  do.call(rbind.data.frame, zero.flow.duration.lst) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  filter(avg_ann > 0)

temp <- boxplot.df %>%
  group_by(gauge_ID) %>%
  summarise(n = n()) %>%
  mutate(gauge_ID = gsub('X', '', gauge_ID))

read.csv('Data/CAMELS_AUS_Attributes&Indices_MasterTable.csv') %>%
  mutate(station_id = as.character(station_id)) %>%
  filter(station_id %in% temp$gauge_ID) %>%
  write.csv(., '../Zero-flow threshold/Data/multiple_gauge.csv', row.names = FALSE)