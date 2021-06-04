# Exploring Disability Weights

# Set up -- make sure to set your working directory
library(dplyr)
library(tidyr)
library(ggplot2)

# Load global data (disease burden in 2015, both sexes, all ages)
global_data <- read.csv(
  "./data/prepped/global_burden.csv",
  stringsAsFactors = FALSE
)

# Replace NA as 0 for deaths, ylls, ylds
global_data[is.na(global_data)] <- 0

# What disease was responsible for the most burden (for each metric)?
# Store the name of each *cause* in a variable
most_burden_dalys <- global_data %>% 
  arrange(desc(dalys)) %>% 
  top_n(1,dalys) %>%  
  select(cause)

most_burden_deaths <- global_data %>% 
  arrange(desc(deaths)) %>% 
  top_n(1,deaths) %>%  
  select(cause)

most_burden_ylds <- global_data %>% 
  arrange(desc(ylds)) %>% 
  top_n(1,ylds) %>%  
  select(cause)

most_burden_ylls <- global_data %>% 
  arrange(desc(ylls)) %>% 
  top_n(1,ylls) %>%  
  select(cause)

most_burden_prevelance <- global_data %>% 
  arrange(desc(prevalence)) %>% 
  top_n(1,prevalence) %>%  
  select(cause)


# Using prevalence and YLDs, calculate inferred disability weights
# Note: these are not actual weights used in the study
global_data$disability_weights <- global_data$ylds / global_data$prevalence

# Identify any "unreasonable" values and replace them as NA
global_data <-  subset(global_data, is.na(disability_weights) == FALSE)

# Create a histogram of the disability weights
hist(global_data$disability_weights, xlim = c(0,2), breaks=10000)


# What are the ten highest disability weights? Store these in a variable
ten_highest_weights <-  global_data %>%  
  arrange(disability_weights) %>%  
  top_n(10,disability_weights) %>% 
  select(cause, disability_weights)

# Using prevalence and YLDs, calculate inferred disability weights
# Note: these are not actual weights used in the study
global_data$disability_weights <- global_data$ylds / global_data$prevalence

# Identify any "unreasonable" values and replace them as NA
global_data <-  subset(global_data, is.na(disability_weights) == FALSE)

# Create a histogram of the disability weights
hist(global_data$disability_weights, xlim = c(0,2), breaks=10000)

# What are the ten highest disability weights? Store these in a variable
ten_highest_weights <-  global_data %>%  
  arrange(disability_weights) %>%  
  top_n(10,disability_weights) %>% 
  select(cause, disability_weights)

# Which diseases have more YLDs than YLLs (and ylls > 0)?
greater_yld <-  global_data[global_data$ylds > global_data$ylls,]

# How many times higher is the prevalence than the number of deaths?
# Show the ratio of prevalence to deaths (for these diseases) in a histogram
hist(greater_yld$prevalence, breaks = 150)
hist(greater_yld$deaths, breaks = 150)

# Which disease has the most similar burden of YLLs and YLDs (where ylls > 0)?
greater_yld$diff <-  abs(greater_yld$ylls - greater_yld$ylds)

most_similar <-  greater_yld %>%  
  filter(ylls > 0) %>% 
  top_n(-1,diff) %>%  
  select(cause)
