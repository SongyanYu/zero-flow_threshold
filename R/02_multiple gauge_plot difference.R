#---
# uncertainty analysis for the multiple-gauge case study
# author: Songyan Yu
# date: 06/08/2023
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

## plot dry period fraction
p.noflow <- do.call(rbind.data.frame, zero.flow.duration.lst) %>%
  group_by(gauge_ID) %>%
  mutate(base = avg_ann[zero_flow_threshold == '0'],
         diff = avg_ann - base) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  ggplot() +
  geom_boxplot(aes(x = zero_flow_threshold, y = diff, group = zero_flow_threshold)) +
  theme_classic() +
  ylab("Diff. in dry period fraction") +
  xlab(element_blank())

## plot of proportion of intermittent gauges
lineplot.df <- 
  do.call(rbind.data.frame, zero.flow.duration.lst) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  filter(avg_ann == 0) %>%
  group_by(zero_flow_threshold) %>%
  summarise(perennial_case = n())

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
  group_by(gauge_ID) %>%
  mutate(base = ifelse('0' %in% zero_flow_threshold, avg_ann[zero_flow_threshold == '0'], avg_ann[zero_flow_threshold == '0.004']),
         diff = avg_ann - base) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  ggplot()+
  geom_boxplot(aes(x = zero_flow_threshold, y = diff, group = zero_flow_threshold)) +
  theme_classic() +
  ylab("Diff. in first zero flow day") +
  xlab(element_blank())
  

# box plot of dry-down period
p.drydown <- do.call(rbind.data.frame, peak2z.lst) %>%
  group_by(gauge_ID) %>%
  mutate(base = ifelse('0' %in% zero_flow_threshold, avg_ann[zero_flow_threshold == '0'], avg_ann[zero_flow_threshold == '0.004']),
         diff = avg_ann - base) %>%
  mutate(zero_flow_threshold = as.numeric(zero_flow_threshold)) %>%
  ggplot()+
  geom_boxplot(aes(x = zero_flow_threshold, y = diff, group = zero_flow_threshold)) +
  theme_classic() +
  ylab("Diff. in dry-down period (days)") +
  ylim(-200, 100) +
  xlab(element_blank())

ggpubr::ggarrange(p.noflow, p.firstzeroflow, p.drydown,
                  ncol = 1,
                  label.x = 0.93,
                  label.y = 1,
                  labels = c('(a)','(b)','(c)', '(d)'))

ggsave(filename = "Figures/02_multi-gauge_zeroFlowThreshold_4metrics_diff.png",
       width = 7, height = 7.25)


