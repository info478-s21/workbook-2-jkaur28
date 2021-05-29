# Prep DALY data

# Set up
library(dplyr)
setwd('~/Documents/info-498c/demos/gbd/dalys')

# Function to load/prep data
LoadData <- function(metric.name) {
  data <- read.csv(paste0('data/raw/', metric.name, '.csv')) %>% 
              mutate(metric = metric.name, cause = Cause.of.death.or.injury) %>% 
              filter(Year == 2015, Location == "Colombia") %>% 
              select(cause, Value, metric)
  return(data)
}

# Load each data type
deaths <- LoadData('deaths')
ylls <- LoadData('ylls')
ylds <- LoadData('ylds')
dalys <- LoadData('dalys')

# Append data together
prepped.data <- rbind(deaths, ylls, ylds, dalys)

# Save data
write.csv(prepped.data, 'data/prepped/overview.csv', row.names = FALSE)