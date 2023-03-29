#---
# single gauge case study
# 1) select one gauge for each of the 8 intermittent flow-regime classes (Kennard 2010)
# author: Songyan Yu
# date created: 27 Mar 2023
#---

# kennard flow regime classification
kennard.df <- readxl::read_xls('../../Zero-flow threshold/Data/fwb_2307_sm_appendix s1.xls',
                               sheet = 'Stream gauge characteristics')

# CAMEL_AUS gauges
camel.df <- read.csv('../../Zero-flow threshold/Data/multiple_gauge.csv')

library(dplyr)

gauge.df <- kennard.df %>%
  mutate(`Gauge Number` = as.character(`Gauge Number`)) %>%
  right_join(., camel.df, by = c('Gauge Number' = 'station_id'))

# based on the 'temp' data frame, we selected eight gauges for each of the 8
# intermittent flow regime classes. The selected gauges are as below:
# 'X204034' - Class 7
# 'X227226' - Class 5
# 'X230210' - Class 8
# 'X403217' - Class 6 
# 'X406224' - Class 12
# 'X424002' - Class 11
# 'X614044' - Class 9
# 'X803003' - Class 10

gauge.df %>%
  filter(`Gauge Number` %in% c('204034', '227226', '230210', '403217', '406224',
                               '424002', '614044', '803003')) %>%
  write.csv('../../Zero-flow threshold/Data/01a_single_gauge_selection.csv',
            row.names = FALSE)



