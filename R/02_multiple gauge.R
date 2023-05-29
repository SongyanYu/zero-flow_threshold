#---
# identify intermittent stream gauges in CAMEL_AUS for multiple-gauge case study
# author: Songyan Yu
# date: 15/12/2022
# date updated: 08/04/2023
#---
library(dplyr)
library(ggplot2)

hydro.files <- list.files("../../Zero-flow threshold/sensitivityAnalysis_zeroFlow/",
                          pattern = "flowMetrics_AnnualHydroMetrics",
                          full.names = TRUE)

peak2z.files <- list.files("../../Zero-flow threshold/sensitivityAnalysis_zeroFlow/",
                           pattern = "peak2zero",
                           full.names = TRUE)
peak2z.lst <-
  lapply(peak2z.files, FUN = function(x){
    zero.flow <- strsplit(strsplit(strsplit(strsplit(x, split = "/")[[1]][5],split = "_")[[1]][4],split = "-")[[1]][1], split = ".csv")[[1]]
    df <-
      read.csv(x) %>%
      filter(!is.na(peak2z_length)) %>%
      group_by(gauge_ID) %>%
      summarise(avg_ann = mean(peak2z_length, na.rm = TRUE)) %>%
      mutate(zero_flow_threshold = zero.flow)
  })

zero.flow.duration.lst <-
  lapply(hydro.files, FUN = function(x) {
    zero.flow <- strsplit(strsplit(strsplit(strsplit(x, split = "/")[[1]][5],split = "_")[[1]][4],split = "-")[[1]][1], split = ".csv")[[1]]
    df <- 
      read.csv(x) %>%
      group_by(gauge_ID) %>%
      summarise(avg_ann = mean(annualfractionnoflow_r, na.rm = TRUE)) %>%
      mutate(zero_flow_threshold = zero.flow)
  })

zeroflow.first.lst <-
  lapply(hydro.files, FUN = function(x) {
    zero.flow <- strsplit(strsplit(strsplit(strsplit(x, split = "/")[[1]][5],split = "_")[[1]][4],split = "-")[[1]][1], split = ".csv")[[1]]
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

## box plot of annual fraction of no flows
p.noflow<- ggplot(data =boxplot.df) +
  geom_boxplot(aes(x = zero_flow_threshold, y = avg_ann, group = zero_flow_threshold)) +
  theme_classic() +
  ylab("Dry period fraction") +
  xlab(element_blank())
#  xlab(expression(paste("Zero flow threshold (",m^3/s,")")))

lineplot.df <- 
  do.call(rbind.data.frame, zero.flow.duration.lst) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  filter(avg_ann == 0) %>%
  group_by(zero_flow_threshold) %>%
  summarise(perennial_case = n())

## plot of proportion of intermittent gauges
p.intermittent <- ggplot(data = lineplot.df) +
  geom_line(aes(x = zero_flow_threshold, y = 179- perennial_case), colour = "red") +
  theme_bw() +
  ylab("No. of intermittent gauges") +
  #  xlab(expression(paste("Zero flow threshold (",m^3/s,")"))) +
  xlab(element_blank()) +
  # ylim(50,150) +
  geom_vline(xintercept = 0.032, linetype ='dashed')+
  xlab(expression(paste("Zero flow threshold (",m^3/s,")")))


# box plot of average Julian date of first zero flow
p.firstzeroflow <- do.call(rbind.data.frame, zeroflow.first.lst) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  ggplot()+
  geom_boxplot(aes(x = zero_flow_threshold, y = avg_ann, group = zero_flow_threshold)) +
  theme_classic() +
  ylab("First zero flow day") +
  xlab(element_blank())
#  xlab(expression(paste("Zero flow threshold (",m^3/s,")")))

# box plot of dry-down period
p.drydown <- do.call(rbind.data.frame, peak2z.lst) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  ggplot()+
  geom_boxplot(aes(x = zero_flow_threshold, y = avg_ann, group = zero_flow_threshold)) +
  theme_classic() +
  ylab("Dry-down period (days)") +
  ylim(0,300) +
  xlab(element_blank())


ggpubr::ggarrange(p.noflow, p.firstzeroflow, p.drydown, p.intermittent,
                  ncol = 1,
                  label.x = 0.93,
                  label.y = 1,
                  labels = c('(a)','(b)','(c)', '(d)'))

ggsave(filename = "Figures/02_multi-gauge_zeroFlowThreshold_4metrics.png",
       width = 7, height = 10)

## change of flow metric values from using the lowest to the highest zero flow threshold
library(tidyr)
boxplot.df %>%
  filter(zero_flow_threshold == 0.0003 | zero_flow_threshold == 0.0099) %>%
  ggplot() +
  geom_path(aes(x = gauge_ID, y = avg_ann), arrow=arrow(angle=30,length=unit(0.1,"inches"),
                                                        type="closed")) +
  theme_classic()


#  find gauges in intermittent streams
do.call(rbind.data.frame, zero.flow.duration.lst) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  filter(avg_ann > 0) %>%
  mutate(gauge_ID = gsub('X', '', gauge_ID))

# identify the 179 intermittent stream gauges for multi-gauge case study.  
# box plot of average annual fraction of no flow 
# for all gauges across all zero flow thresholds.
do.call(rbind.data.frame, zero.flow.duration.lst) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  filter(avg_ann > 0) %>%
  ggplot() +
  geom_boxplot(aes(x = zero_flow_threshold, y = avg_ann, group = zero_flow_threshold)) +
  theme_bw()

read.csv('../../FlowIntermittency_AUS/Data/CAMELS_AUS_Attributes&Indices_MasterTable.csv') %>%
  mutate(station_id = as.character(station_id)) %>%
  filter(station_id %in% temp$gauge_ID) %>%
  write.csv(., '../Zero-flow threshold/Data/multiple_gauge.csv', row.names = FALSE)


# removed gauges with long dry-down period
do.call(rbind.data.frame, peak2z.lst) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  filter(zero_flow_threshold<0.032) %>%
  filter(avg_ann >200) # dry-down period >200 days

