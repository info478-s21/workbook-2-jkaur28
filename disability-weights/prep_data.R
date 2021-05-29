# Set up
setwd('~/Documents/info-498c/demos/gbd/disability-weights/')
library(dplyr)
library(tidyr)

# Read in cause hierarchy
cause.hierarchy <- read.csv('./data/raw/gbd-cause-hierarchy.csv', stringsAsFactors = FALSE) %>% 
                        select(cause_id, cause_name, cause_short, level)
            
# Read in burden data
burden <- read.csv('./data/raw/gbd-2015-global-dalys.csv', stringsAsFactors = FALSE)

# Merge cause hierarchy with burden data
all.data <- left_join(burden, cause.hierarchy, by='cause_id')

# Keep only level 2 causes (a certain level of granularity) and save
filtered <- all.data %>% 
                filter(level == 3) %>% 
                select(cause = cause_name.x, metric = measure_name, value = val)

# Rename measure
filtered$metric <- tolower(filtered$metric)
filtered[grepl('daly', filtered$metric),'metric'] <- 'dalys'
filtered[grepl('yll', filtered$metric),'metric'] <- 'ylls'
filtered[grepl('yld', filtered$metric),'metric'] <- 'ylds'
filtered[grepl('Prevalence', filtered$metric),'metric'] <- 'prevalence'

# Reshape wide
wide.data <- spread(filtered, metric, value)
write.csv(wide.data, './data/prepped/global_burden.csv', row.names = FALSE)
