#---
# individual gauge scenario - sensitivity analysis of zero flow threshold
# author: Songyan Yu
# date created: 12/12/2022
# date updated: 29/03/2023
#---

library(dplyr)
library(ggplot2)

hydro.files <- list.files("../sensitivityAnalysis_zeroFlow/",
                          pattern = "flowMetrics_AnnualHydroMetrics",
                          full.names = TRUE)

peak2z.files <- list.files("../sensitivityAnalysis_zeroFlow/",
                           pattern = "peak2zero",
                           full.names = TRUE)

peak2z.lst <-
  lapply(peak2z.files, FUN = function(x){
    zero.flow <- strsplit(strsplit(strsplit(strsplit(x, split = "/")[[1]][3],split = "_")[[1]][4],split = "-")[[1]][1], split = ".csv")[[1]]
    df <-
      read.csv(x) %>%
      filter(!is.na(peak2z_length)) %>%
      group_by(gauge_ID) %>%
      summarise(avg_ann = mean(peak2z_length, na.rm = TRUE)) %>%
      mutate(zero_flow_threshold = zero.flow)
  })


zero.flow.duration.lst <-
  lapply(hydro.files, FUN = function(x) {
    zero.flow <- strsplit(strsplit(strsplit(strsplit(x, split = "/")[[1]][3],split = "_")[[1]][4],split = "-")[[1]][1], split = ".csv")[[1]]
    df <- 
      read.csv(x) %>%
      group_by(gauge_ID) %>%
      summarise(avg_ann = mean(annualfractionnoflow_r, na.rm = TRUE)) %>%
      mutate(zero_flow_threshold = zero.flow)
  })

zeroflow.first.lst <-
  lapply(hydro.files, FUN = function(x) {
    zero.flow <- strsplit(strsplit(strsplit(strsplit(x, split = "/")[[1]][3],split = "_")[[1]][4],split = "-")[[1]][1], split = ".csv")[[1]]
    df <- 
      read.csv(x) %>%
      filter(!is.infinite(zeroflowfirst_r)) %>%
      group_by(gauge_ID) %>%
      summarise(avg_ann = mean(zeroflowfirst_r, na.rm = TRUE)) %>%
      mutate(zero_flow_threshold = zero.flow)
  })


peak2z.df <- do.call(rbind.data.frame, peak2z.lst)
zeroflow.first.df <- do.call(rbind.data.frame, zeroflow.first.lst)
zero.flow.duration.df <- do.call(rbind.data.frame, zero.flow.duration.lst)

single.gauge <- c('X227226', # 5
                  'X403217', # 6 
                  'X204034', # 7
                  'X230210', # 8
                  'X614044', # 9
                  'X803003', # 10
                  'X424002', # 11
                  'X406224')  # 12

classes <- c('Class 5', 'Class 6', 'Class 7', 'Class 8',
             'Class 9', 'Class 10', 'Class 11', 'Class 12')

data_vline <- data.frame(gauge_ID = classes,
                         vline = c(0.056, 0.072, 0.016, 0.076,
                                   0.050, 0.050, 0.05, 0.046)) %>%
  mutate(gauge_ID = factor(gauge_ID, levels = classes))

p.noflow <- zero.flow.duration.df %>%
  filter(gauge_ID %in% single.gauge) %>%
  mutate(gauge_ID = factor(gauge_ID, levels = single.gauge,
                           labels = classes)) %>%
  ggplot() +
  geom_line(aes(x = as.numeric(zero_flow_threshold), y = avg_ann)) +
  theme_bw() +
  facet_wrap(~gauge_ID, scales = 'free_y', ncol = 4) +
  xlab("") +
  ylab("Dry period fraction") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#  geom_vline(data = data_vline, aes(xintercept = vline), linetype = 'dashed', colour = 'red')

  
p.firstzeroflow <- zeroflow.first.df %>%
  filter(gauge_ID %in% single.gauge) %>%
  mutate(gauge_ID = factor(gauge_ID, levels = single.gauge,
                           labels = classes)) %>%
  ggplot() +
  geom_line(aes(x = as.numeric(zero_flow_threshold), y = avg_ann)) +
  theme_bw() +
  facet_wrap(~gauge_ID, scales = 'free_y', ncol = 4) +
  xlab("") +
  ylab("First zero flow day") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#  geom_vline(data = data_vline, aes(xintercept = vline), linetype = 'dashed', colour = 'red')


p.drydown <- peak2z.df %>%
  filter(gauge_ID %in% single.gauge) %>%
  mutate(gauge_ID = factor(gauge_ID, levels = single.gauge,
                           labels = classes)) %>%
  ggplot() +
  geom_line(aes(x = as.numeric(zero_flow_threshold), y = avg_ann)) +
  theme_bw() +
  facet_wrap(~gauge_ID, scales = 'free_y', ncol = 4) +
  xlab(expression(paste("Zero flow threshold (",m^3/s,")"))) +
  ylab("Dry-down period (days)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#  geom_vline(data = data_vline, aes(xintercept = vline), linetype = 'dashed', colour = 'red')

ggpubr::ggarrange(p.noflow, p.firstzeroflow, p.drydown,
                  ncol = 1,
                  label.x = 0.93,
                  label.y = 1)

ggsave(filename = "Figures/02_visualise_zeroFlowThreshold_3metrics.png",
       width = 6, height = 8)

