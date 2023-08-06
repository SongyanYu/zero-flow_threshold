#---
# identify intermittent stream gauges in CAMEL_AUS for multiple-gauge case study
# author: Songyan Yu
# date: 15/12/2022
# date updated: 31/05/2023
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
  geom_line(aes(x = zero_flow_threshold, y = 222- perennial_case), colour = "red") +
  theme_bw() +
  ylab("No. of intermittent gauges") +
  #  xlab(expression(paste("Zero flow threshold (",m^3/s,")"))) +
  xlab(element_blank()) +
  # ylim(50,150) +
#  geom_vline(xintercept = 0.032, linetype ='dashed')+
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
  geom_path(aes(x = gauge_ID, y = avg_ann), 
            arrow=arrow(angle=30,length=unit(0.1,"inches"), type="closed")) +
  theme_classic()

# removed gauges with long dry-down period
do.call(rbind.data.frame, peak2z.lst) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  filter(zero_flow_threshold<0.032) %>%
  filter(avg_ann >200) # dry-down period >200 days

# CAMEL_AUS gauges
camel.gauge <- read.csv('../../FlowIntermittency_AUS/Data/CAMELS_AUS_Attributes&Indices_MasterTable.csv') %>%
  select(station_id, long_centroid, lat_centroid)

read.csv('../../FlowIntermittency_AUS/Data/CAMELS_AUS_Attributes&Indices_MasterTable.csv') %>%
  select(station_id, start_date, end_date) %>%
  mutate(start_date = lubridate::ymd(start_date),
         end_date = lubridate::ymd(end_date),
         duration = (end_date - start_date)/365) %>%
  ggplot() +
  geom_boxplot(aes(y = duration))

kennard.class <- readxl::read_xls('Data/fwb_2307_sm_appendix s1.xls',
                                  sheet = 'Stream gauge characteristics') %>%
  select('Gauge Number', 'Flow regime class (C1)') %>%
  rename(gauge_id = 'Gauge Number',
         regime_class = 'Flow regime class (C1)') %>%
  rowwise() %>%
  mutate(regime_class = strsplit(regime_class, split = ' - ')[[1]][1],
         gauge_id = as.character(gauge_id))

# identify which gauge became intermittent at threshold of 0 m3/s
do.call(rbind.data.frame, zero.flow.duration.lst) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  filter(zero_flow_threshold == 0 & avg_ann == 0) %>% # 160
  mutate(gauge_ID = gsub('X', '', gauge_ID)) %>%
  left_join(., camel.gauge, by = c('gauge_ID' = 'station_id')) %>%
  write.csv(., 'Data/Perennial_gauge_0cms.csv', row.names = FALSE)

perennial_gauge_0 <- read.csv('Data/Perennial_gauge_0cms.csv')
  
# identify which gauge became intermittent at threshold of 0.032 m3/s
do.call(rbind.data.frame, zero.flow.duration.lst) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  filter(zero_flow_threshold == 0.032 & avg_ann >0) %>%
  mutate(gauge_ID = gsub('X', '', gauge_ID)) %>%
  left_join(., camel.gauge, by = c('gauge_ID' = 'station_id')) %>%
  write.csv(., 'Data/Intermittent_gauge_032cms.csv', row.names = FALSE)

do.call(rbind.data.frame, zero.flow.duration.lst) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  filter(zero_flow_threshold == 0.032 & avg_ann ==0) %>%
  mutate(gauge_ID = gsub('X', '', gauge_ID)) %>%
  left_join(., camel.gauge, by = c('gauge_ID' = 'station_id')) %>%
  write.csv(., 'Data/Perennial_gauge_032cms.csv', row.names = FALSE)

perennial_gauge_32 <- read.csv('Data/Perennial_gauge_032cms.csv')

# which gauges changed to be non-perennial when zero-flow
# threshold is set to 0.032 m3/s
perennial_gauge_0 %>%
  filter(!(gauge_ID %in% perennial_gauge_32$gauge_ID)) %>%
  write.csv(., 'Data/Changed_gauge_032cms.csv', row.names = FALSE)


