
 
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(stringr)
library(readxl)
library(openxlsx)
library(tidyverse)
library(dplyr)
library(broom)
library(lubridate)
library(car)
library("writexl")
library(ggplot2)
library(kableExtra)
library(glmnet)
library(stargazer)
library(janitor)
library(data.table)
library(stringr)

# Clear the console and environment
rm(list=ls())

# Set project path based on the environment (RStudio or command line)
args = commandArgs(trailingOnly = TRUE)
if (Sys.getenv("RSTUDIO") == 1) {
  sys_info <- Sys.info()
  if (sys_info["sysname"] == "Windows") {
    project_root <- paste(Sys.getenv("USERPROFILE"), "/Dropbox/interconnections_data/", sep="")
  } else if (sys_info["sysname"] == "Darwin") {
    project_root <- paste(Sys.getenv("HOME"), "/Dropbox/interconnections_data/", sep="")
  }
} else {
  project_root <- args[1]
}


phase_1_cost_data <- read.csv(paste0(project_root, "data/ic_studies/clean/costs_phase_1_final_data.csv"))






## 1. Phase I POI Cost Histogram by Cluster 
phase_1_POI_cost_histogram_cluster <- ggplot(phase_1_cost_data, 
                                             aes(x = PTO_cost_own/(1000*capacity))) + 
  geom_histogram(binwidth = 50, fill = "blue") + 
  labs(title = "CAISO Scraped Phase I POI Cost per kW Histogram (Descalated Costs)", 
       x = "Phase 1 POI Cost", y = "Count") +
  facet_wrap(~ cluster, ncol = 7) +
  xlim(c(0, 15000)) +
  coord_flip() +
  theme_light()

phase_1_POI_cost_histogram_cluster

## 2. Phase I Network Cost Histogram by Cluster
phase_1_network_cost_histogram_cluster <- ggplot(phase_1_cost_data, 
                                                 aes(x = Network_cost_own/(1000*capacity))) + 
  geom_histogram(binwidth = 50, fill = "blue") + 
  labs(title = "CAISO Scraped Phase I Network Cost per kW Histogram (Descalated Costs)", 
       x = "Phase 1 Network Cost", y = "Count") +
  facet_wrap(~ cluster, ncol = 7) +
  xlim(c(0, 1500)) +
  coord_flip() +
  theme_light()

phase_1_network_cost_histogram_cluster