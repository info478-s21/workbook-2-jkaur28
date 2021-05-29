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

# Using prevalence and YLDs, calculate inferred disability weights
# Note: these are not actual weights used in the study


# Identify any "unreasonable" values and replace them as NA

# Create a histogram of the disability weights


# What are the ten highest disability weights? Store these in a variable


# Which diseases have more YLDs than YLLs (and ylls > 0)?

# How many times higher is the prevalence than the number of deaths?
# Show the ratio of prevalence to deaths (for these diseases) in a histogram

# Which disease has the most similar burden of YLLs and YLDs (where ylls > 0)?

# For each cause, compute how many cases would have to have to be avoided
# to equal 65 YLLs (the equivalent of one death of a ~25 year old)
