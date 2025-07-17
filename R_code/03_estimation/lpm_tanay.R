cat("\014")
rm(list=ls())

# ========== PROJECT PATH =========
# Set up for direct execution or through an Rscript call from the shell
args = commandArgs(trailingOnly = TRUE)
if (Sys.getenv("RSTUDIO") == 1)
{
  sys_info <- Sys.info()
  if (sys_info["sysname"] == "Windows")
  {
    project_root<-paste(Sys.getenv("USERPROFILE"),"/Dropbox/interconnections_data", sep="")
  } else if (sys_info["sysname"] == "Darwin")
  {
    project_root<-paste(Sys.getenv("HOME"),"/Dropbox/interconnections_data", sep="")
  }
} else {
  project_root<-args[1]
}

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

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
library(vtable)
library(sandwich)
library(lmtest)
library(jtools)
library(broom.mixed)

## ===== IMPORTING DATA =====
CAISO_RIMS <- read.csv(paste0(project_root, 
                              "/data/app_and_study/RIMS 5_App & Study-Project Info (8-28-2024).csv"))

cluster_14_cost <- read.csv(paste0(project_root, "/data/working/cluster_14_cost_data.csv"))
cluster_13_cost <- read.csv(paste0(project_root, "/data/working/cluster_13_cost_data.csv"))
cluster_12_cost <- read.csv(paste0(project_root, "/data/working/cluster_12_cost_data.csv"))
cluster_11_cost <- read.csv(paste0(project_root, "/data/working/cluster_11_cost_data.csv"))
cluster_10_cost <- read.csv(paste0(project_root, "/data/working/cluster_10_cost_data.csv"))
cluster_9_cost <- read.csv(paste0(project_root, "/data/working/cluster_9_cost_data.csv"))
cluster_8_cost <- read.csv(paste0(project_root, "/data/working/cluster_8_cost_data.csv"))
cluster_7_cost <- read.csv(paste0(project_root, "/data/working/cluster_7_cost_data.csv"))

cost_data <- read.csv(paste0(project_root, "/data/working/CAISO_cost_data.csv"))

checklist_data <- read.csv(paste0(project_root,
                                  "/data/working/clean_RIMS_checklist_cluster_updated.csv"))

## ===== CLEANING DATA =====
## Removing Projects without a Capacity Value or with a Capacity of 0
clean_cost_data <- subset(cost_data, !is.na(capacity) & capacity != 0 &
                            !is.na(req_deliverability))

## Removing NA Status Dates from Checklist Data
checklist_data <- subset(checklist_data, !is.na(checklist_data$status_date))

## Formatting Checklist Data
checklist_data$q_date <- ymd(checklist_data$q_date)
checklist_data$status_date <- ymd(checklist_data$status_date)

## Calculating the Aggregate Costs
clean_cost_data$phase_1_total_network_cost <- ifelse(is.na(clean_cost_data$phase_1_network_MCR_cost), 
                                                     clean_cost_data$phase_1_total_RNU_cost + 
                                                       clean_cost_data$phase_1_total_LDNU_cost,
                                                     clean_cost_data$phase_1_network_MCR_cost)

clean_cost_data$phase_2_total_network_cost <- ifelse(is.na(clean_cost_data$phase_2_network_MCR_cost), 
                                                     clean_cost_data$phase_2_total_RNU_cost + 
                                                       clean_cost_data$phase_2_total_LDNU_cost,
                                                     clean_cost_data$phase_2_network_MCR_cost)

clean_cost_data$phase_1_total_cost <- clean_cost_data$phase_1_total_POI_cost + 
  clean_cost_data$phase_1_total_network_cost

clean_cost_data$phase_2_total_cost <- clean_cost_data$phase_2_total_POI_cost + 
  clean_cost_data$phase_2_total_network_cost

## Calculating the per kW Costs
clean_cost_data$phase_1_total_network_cost_per_kW <- clean_cost_data$phase_1_total_network_cost/(1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_network_cost_per_kW <- clean_cost_data$phase_2_total_network_cost/(1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_total_POI_cost_per_kW <- clean_cost_data$phase_1_total_POI_cost/(1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_POI_cost_per_kW <- clean_cost_data$phase_2_total_POI_cost/(1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_total_cost_per_kW <- clean_cost_data$phase_1_total_cost/(1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_cost_per_kW <- clean_cost_data$phase_2_total_cost/(1000 * clean_cost_data$capacity)

## Phase 2 Costs that Projects Actually Face According to Interconnection Process
clean_cost_data$phase_2_total_POI_cost_per_kW_realized <- ifelse(is.na(clean_cost_data$phase_1_total_POI_cost_per_kW),
                                                                 NA, clean_cost_data$phase_2_total_POI_cost_per_kW)

clean_cost_data$phase_2_total_network_cost_per_kW_realized <- pmin(clean_cost_data$phase_1_total_network_cost_per_kW,
                                                                   clean_cost_data$phase_2_total_network_cost_per_kW,
                                                                   na.rm = FALSE)

clean_cost_data$phase_2_total_cost_per_kW_realized <- clean_cost_data$phase_2_total_network_cost_per_kW_realized +
  clean_cost_data$phase_2_total_POI_cost_per_kW

## Winzorising Cost Data to the 99th Percentile
clean_cost_data$phase_1_total_POI_cost_per_kW <- ifelse(clean_cost_data$phase_1_total_POI_cost_per_kW > quantile(clean_cost_data$phase_1_total_POI_cost_per_kW, 
                                                                                                                 0.99, na.rm = TRUE),
                                                        quantile(clean_cost_data$phase_1_total_POI_cost_per_kW, 
                                                                 0.99, na.rm = TRUE), 
                                                        clean_cost_data$phase_1_total_POI_cost_per_kW)
clean_cost_data$phase_1_total_network_cost_per_kW <- ifelse(clean_cost_data$phase_1_total_network_cost_per_kW > quantile(clean_cost_data$phase_1_total_network_cost_per_kW, 
                                                                                                                         0.99, na.rm = TRUE),
                                                            quantile(clean_cost_data$phase_1_total_network_cost_per_kW, 
                                                                     0.99, na.rm = TRUE), 
                                                            clean_cost_data$phase_1_total_network_cost_per_kW)
clean_cost_data$phase_1_total_cost_per_kW <- ifelse(clean_cost_data$phase_1_total_cost_per_kW > quantile(clean_cost_data$phase_1_total_cost_per_kW, 
                                                                                                         0.99, na.rm = TRUE),
                                                    quantile(clean_cost_data$phase_1_total_cost_per_kW, 
                                                             0.99, na.rm = TRUE), 
                                                    clean_cost_data$phase_1_total_cost_per_kW)

clean_cost_data$phase_2_total_POI_cost_per_kW <- ifelse(clean_cost_data$phase_2_total_POI_cost_per_kW > quantile(clean_cost_data$phase_2_total_POI_cost_per_kW, 
                                                                                                                 0.99, na.rm = TRUE),
                                                        quantile(clean_cost_data$phase_2_total_POI_cost_per_kW, 
                                                                 0.99, na.rm = TRUE), 
                                                        clean_cost_data$phase_2_total_POI_cost_per_kW)
clean_cost_data$phase_2_total_network_cost_per_kW <- ifelse(clean_cost_data$phase_2_total_network_cost_per_kW > quantile(clean_cost_data$phase_2_total_network_cost_per_kW, 
                                                                                                                         0.99, na.rm = TRUE),
                                                            quantile(clean_cost_data$phase_2_total_network_cost_per_kW, 
                                                                     0.99, na.rm = TRUE), 
                                                            clean_cost_data$phase_2_total_network_cost_per_kW)
clean_cost_data$phase_2_total_network_cost_per_kW_realized <- ifelse(clean_cost_data$phase_2_total_network_cost_per_kW_realized > quantile(clean_cost_data$phase_2_total_network_cost_per_kW_realized, 
                                                                                                                                           0.99, na.rm = TRUE),
                                                                     quantile(clean_cost_data$phase_2_total_network_cost_per_kW_realized, 
                                                                              0.99, na.rm = TRUE), 
                                                                     clean_cost_data$phase_2_total_network_cost_per_kW_realized)
clean_cost_data$phase_2_total_cost_per_kW <- ifelse(clean_cost_data$phase_2_total_cost_per_kW > quantile(clean_cost_data$phase_2_total_cost_per_kW, 
                                                                                                         0.99, na.rm = TRUE),
                                                    quantile(clean_cost_data$phase_2_total_cost_per_kW, 
                                                             0.99, na.rm = TRUE), 
                                                    clean_cost_data$phase_2_total_cost_per_kW)
clean_cost_data$phase_2_total_cost_per_kW_realized <- ifelse(clean_cost_data$phase_2_total_cost_per_kW_realized > quantile(clean_cost_data$phase_2_total_cost_per_kW_realized, 
                                                                                                                           0.99, na.rm = TRUE),
                                                             quantile(clean_cost_data$phase_2_total_cost_per_kW_realized, 
                                                                      0.99, na.rm = TRUE), 
                                                             clean_cost_data$phase_2_total_cost_per_kW_realized)


## Prices per MW
clean_cost_data$phase_1_total_network_cost_per_MW <- clean_cost_data$phase_1_total_network_cost/
  (clean_cost_data$capacity)

clean_cost_data$phase_2_total_network_cost_per_MW <- clean_cost_data$phase_2_total_network_cost/
  (clean_cost_data$capacity)

clean_cost_data$phase_1_total_POI_cost_per_MW <- clean_cost_data$phase_1_total_POI_cost/
  (clean_cost_data$capacity)

clean_cost_data$phase_2_total_POI_cost_per_MW <- clean_cost_data$phase_2_total_POI_cost/
  (clean_cost_data$capacity)

clean_cost_data$phase_1_total_cost_per_MW <- clean_cost_data$phase_1_total_cost/
  (clean_cost_data$capacity)

clean_cost_data$phase_2_total_cost_per_MW <- clean_cost_data$phase_2_total_cost/
  (clean_cost_data$capacity)

## Phase 2 Costs that Projects Actually Face According to Interconnection Process
clean_cost_data$phase_2_total_POI_cost_per_MW_realized <- ifelse(is.na(clean_cost_data$phase_1_total_POI_cost_per_MW),
                                                                 NA, clean_cost_data$phase_2_total_POI_cost_per_MW)

clean_cost_data$phase_2_total_network_cost_per_MW_realized <- pmin(clean_cost_data$phase_1_total_network_cost_per_MW,
                                                                   clean_cost_data$phase_2_total_network_cost_per_MW,
                                                                   na.rm = FALSE)

clean_cost_data$phase_2_total_cost_per_MW_realized <- clean_cost_data$phase_2_total_network_cost_per_MW_realized +
  clean_cost_data$phase_2_total_POI_cost_per_MW

## Winzorising Cost Data to the 99th Percentile
clean_cost_data$phase_1_total_POI_cost_per_MW <- ifelse(clean_cost_data$phase_1_total_POI_cost_per_MW > quantile(clean_cost_data$phase_1_total_POI_cost_per_MW, 
                                                                                                                 0.99, na.rm = TRUE),
                                                        quantile(clean_cost_data$phase_1_total_POI_cost_per_MW, 
                                                                 0.99, na.rm = TRUE), 
                                                        clean_cost_data$phase_1_total_POI_cost_per_MW)
clean_cost_data$phase_1_total_network_cost_per_MW <- ifelse(clean_cost_data$phase_1_total_network_cost_per_MW > quantile(clean_cost_data$phase_1_total_network_cost_per_MW, 
                                                                                                                         0.99, na.rm = TRUE),
                                                            quantile(clean_cost_data$phase_1_total_network_cost_per_MW, 
                                                                     0.99, na.rm = TRUE), 
                                                            clean_cost_data$phase_1_total_network_cost_per_MW)
clean_cost_data$phase_1_total_cost_per_MW <- ifelse(clean_cost_data$phase_1_total_cost_per_MW > quantile(clean_cost_data$phase_1_total_cost_per_MW, 
                                                                                                         0.99, na.rm = TRUE),
                                                    quantile(clean_cost_data$phase_1_total_cost_per_MW, 
                                                             0.99, na.rm = TRUE), 
                                                    clean_cost_data$phase_1_total_cost_per_MW)

clean_cost_data$phase_2_total_POI_cost_per_MW <- ifelse(clean_cost_data$phase_2_total_POI_cost_per_MW > quantile(clean_cost_data$phase_2_total_POI_cost_per_MW, 
                                                                                                                 0.99, na.rm = TRUE),
                                                        quantile(clean_cost_data$phase_2_total_POI_cost_per_MW, 
                                                                 0.99, na.rm = TRUE), 
                                                        clean_cost_data$phase_2_total_POI_cost_per_MW)
clean_cost_data$phase_2_total_network_cost_per_MW <- ifelse(clean_cost_data$phase_2_total_network_cost_per_MW > quantile(clean_cost_data$phase_2_total_network_cost_per_MW, 
                                                                                                                         0.99, na.rm = TRUE),
                                                            quantile(clean_cost_data$phase_2_total_network_cost_per_MW, 
                                                                     0.99, na.rm = TRUE), 
                                                            clean_cost_data$phase_2_total_network_cost_per_MW)
clean_cost_data$phase_2_total_network_cost_per_MW_realized <- ifelse(clean_cost_data$phase_2_total_network_cost_per_MW_realized > quantile(clean_cost_data$phase_2_total_network_cost_per_MW_realized, 
                                                                                                                                           0.99, na.rm = TRUE),
                                                                     quantile(clean_cost_data$phase_2_total_network_cost_per_MW_realized, 
                                                                              0.99, na.rm = TRUE), 
                                                                     clean_cost_data$phase_2_total_network_cost_per_MW_realized)
clean_cost_data$phase_2_total_cost_per_MW <- ifelse(clean_cost_data$phase_2_total_cost_per_MW > quantile(clean_cost_data$phase_2_total_cost_per_MW, 
                                                                                                         0.99, na.rm = TRUE),
                                                    quantile(clean_cost_data$phase_2_total_cost_per_MW, 
                                                             0.99, na.rm = TRUE), 
                                                    clean_cost_data$phase_2_total_cost_per_MW)
clean_cost_data$phase_2_total_cost_per_MW_realized <- ifelse(clean_cost_data$phase_2_total_cost_per_MW_realized > quantile(clean_cost_data$phase_2_total_cost_per_MW_realized, 
                                                                                                                           0.99, na.rm = TRUE),
                                                             quantile(clean_cost_data$phase_2_total_cost_per_MW_realized, 
                                                                      0.99, na.rm = TRUE), 
                                                             clean_cost_data$phase_2_total_cost_per_MW_realized)

## Giving Aggregate Labels to the Fuel Type
clean_cost_data$fuel_category <- with(clean_cost_data,
                                      ifelse(fuel == "Wind" |
                                               fuel == "Water",
                                             "wind_water", 
                                             ifelse(fuel == "Battery" |
                                                      fuel == "Pumped-Storage hydro",
                                                    "battery_storage", 
                                                    ifelse(fuel == "Solar",
                                                           "solar","biofuel_natural_gas_other"))))

clean_cost_data <- clean_cost_data %>% 
  relocate(fuel_category, .after = fuel)

## Giving Aggregate Labels to the Generator Type
clean_cost_data$generator_category <- with(clean_cost_data,
                                           ifelse(generator_type == "Solar Thermal" |
                                                    generator_type == "Photovoltaic",
                                                  "Solar", generator_type))

clean_cost_data <- clean_cost_data %>% 
  relocate(generator_category, .after = generator_type)

## ===== INCLUDING INDICATORS =====
## Merging Project Status Data
clean_CAISO_RIMS_data <- CAISO_RIMS[, c("Queue.Number", "Project.Status")]
colnames(clean_CAISO_RIMS_data)[1] <- "q_id"
colnames(clean_CAISO_RIMS_data)[2] <- "status"

cost_status_data <- merge(clean_cost_data, clean_CAISO_RIMS_data, by = "q_id")

cost_status_data <- cost_status_data %>%
  select(-status.x) %>%
  relocate(status.y, .after = PTO)

colnames(cost_status_data)[4] <- "status"

## Creating Capacity Quartiles
cost_status_data$capacity_quartiles <- ntile(cost_status_data$capacity, 4)

## Creating Completion Indicator
cost_status_data$comp_ind <- ifelse(cost_status_data$status == "COMPLETED", 1, 0)

## Creating Withdrawn Indicator
cost_status_data$with_ind <- ifelse(cost_status_data$status == "WITHDRAWN", 1, 0)

## Creating Phase 2 Study Receive Indicator
cost_status_data$ph2_ind <- ifelse(!is.na(cost_status_data$phase_2_total_cost), 
                                   1, 0)

## Creating Indicator for Phase 2 POI > Phase 1 POI
cost_status_data$ph2_POI_greater <- ifelse(cost_status_data$phase_2_total_POI_cost_per_kW >
                                             cost_status_data$phase_1_total_POI_cost_per_kW,
                                           1, 0)

## Creating Indicator for Phase 2 POI < Phase 1 POI
cost_status_data$ph1_POI_greater <- ifelse(cost_status_data$phase_2_total_POI_cost_per_kW <
                                             cost_status_data$phase_1_total_POI_cost_per_kW,
                                           1, 0)

## Creating Indicator for Phase 2 Network Cost > Phase 1 Network Cost
cost_status_data$ph2_network_greater <- ifelse(cost_status_data$phase_2_total_network_cost_per_kW >
                                                 cost_status_data$phase_1_total_network_cost_per_kW,
                                               1, 0)

## Creating Indicator for Phase 2 Network Cost < Phase 1 Network Cost
cost_status_data$ph1_network_greater <- ifelse(cost_status_data$phase_2_total_network_cost_per_kW <
                                                 cost_status_data$phase_1_total_network_cost_per_kW,
                                               1, 0)

## Creating Indicator for Getting GIA within 2 Years
gia_earliest <- checklist_data %>%
  filter(checklist_phase == "GIA") %>%             # Only GIA steps
  group_by(q_id) %>%                               # Group by project
  summarize(earliest_gia_date = min(status_date, na.rm = TRUE), .groups = "drop") # Find earliest GIA date

gia_earliest <- subset(gia_earliest, !is.na(gia_earliest$earliest_gia_date))

checklist_data_GIA_indicator <- checklist_data %>%
  left_join(gia_earliest, by = "q_id") %>%          # Add earliest GIA date to each project
  mutate(
    q_date = as.Date(q_date),                      # Ensure dates are in Date format
    earliest_gia_date = as.Date(earliest_gia_date),
    GIA_ind = ifelse(!is.na(earliest_gia_date) &                  # Check if GIA date exists
                       earliest_gia_date <= q_date + months(24),    # Check if GIA date is within 24 months
                     1,                                           # If yes, assign 1
                     0                                            # Otherwise, assign 0
    ))

repeated_GIA_indicator <- checklist_data_GIA_indicator %>%
  select(q_id, GIA_ind)
unique_GIA_indicator <- unique(repeated_GIA_indicator)

cost_status_data <- merge(cost_status_data, unique_GIA_indicator, by = "q_id")

## New/Old Cluster Indicator
cost_status_data$project_age <- ifelse(cost_status_data$cluster <= 11, 0,
                                       1)




## ===== LPM (FOR CAISO SLIDES) =====
## Creating Quartiles for Capacity
cost_status_data$capacity_quartiles <- ntile(cost_status_data$capacity, 4)

cost_status_data$phase_1_network_cost_per_kW_bins <- ifelse(cost_status_data$phase_1_total_network_cost_per_kW < 20,
                                                            1, ifelse(cost_status_data$phase_1_total_network_cost_per_kW < 50,
                                                                      2, ifelse(cost_status_data$phase_1_total_network_cost_per_kW < 100,
                                                                                3, 4)))

cost_status_data$phase_1_POI_cost_per_kW_bins <- ifelse(cost_status_data$phase_1_total_POI_cost_per_kW < 20,
                                                        1, ifelse(cost_status_data$phase_1_total_POI_cost_per_kW < 50,
                                                                  2, ifelse(cost_status_data$phase_1_total_POI_cost_per_kW < 100,
                                                                            3, 4)))

cost_status_data$phase_2_network_cost_per_kW_bins <- ifelse(cost_status_data$phase_2_total_network_cost_per_kW < 20,
                                                            1, ifelse(cost_status_data$phase_2_total_network_cost_per_kW < 50,
                                                                      2, ifelse(cost_status_data$phase_2_total_network_cost_per_kW < 100,
                                                                                3, 4)))

cost_status_data$phase_2_POI_cost_per_kW_bins <- ifelse(cost_status_data$phase_2_total_POI_cost_per_kW < 20,
                                                        1, ifelse(cost_status_data$phase_2_total_POI_cost_per_kW < 50,
                                                                  2, ifelse(cost_status_data$phase_2_total_POI_cost_per_kW < 100,
                                                                            3, 4)))







cost_status_data$phase_1_POI_cost_per_kW_bins <- factor(cost_status_data$phase_1_POI_cost_per_kW_bins, levels = c(1, 2, 3, 4))
cost_status_data$phase_1_network_cost_per_kW_bins <- factor(cost_status_data$phase_1_network_cost_per_kW_bins, levels = c(1, 2, 3, 4))
cost_status_data$phase_2_POI_cost_per_kW_bins <- factor(cost_status_data$phase_2_POI_cost_per_kW_bins, levels = c(1, 2, 3, 4))
cost_status_data$phase_2_network_cost_per_kW_bins <- factor(cost_status_data$phase_2_network_cost_per_kW_bins, levels = c(1, 2, 3, 4))

cost_status_data <- cost_status_data %>%
  mutate(
    phase_1_POI_cost_per_kW_bins = factor(phase_1_POI_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
    phase_1_network_cost_per_kW_bins = factor(phase_1_network_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
    phase_2_POI_cost_per_kW_bins = factor(phase_2_POI_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
    phase_2_network_cost_per_kW_bins = factor(phase_2_network_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
    capacity_quartiles = factor(capacity_quartiles),
    cluster = factor(cluster),
    req_deliverability = factor(req_deliverability, levels = c("Energy Only", "Partial Capacity", "Full Capacity")),
    fuel_category = factor(fuel_category, levels = c("wind_water", "battery_storage", "solar", "biofuel_natural_gas_other"))
  )

# Model Fitting
lm_ph2_prob_phase_1_costs <- lm(ph2_ind ~ 0 + phase_1_POI_cost_per_kW_bins
                                + phase_1_network_cost_per_kW_bins +
                                  factor(capacity_quartiles) + 
                                  factor(cluster) + 
                                  factor(req_deliverability) + 
                                  factor(fuel_category),
                                data = cost_status_data)


 

lm_ph2_prob_phase_1_costs_corrected <- vcovHC(lm_ph2_prob_phase_1_costs, 
                                              type = "HC1")
phase_1_costs_robust <- coeftest(lm_ph2_prob_phase_1_costs, 
                                 vcov = lm_ph2_prob_phase_1_costs_corrected)
phase_1_costs_robust_se <- phase_1_costs_robust[, 2]

# Linear Probability Model for GIA Indicator
lm_GIA_prob_phase_2_costs <- lm(GIA_ind ~ 0 + phase_2_POI_cost_per_kW_bins
                                + phase_2_network_cost_per_kW_bins +
                                  factor(capacity_quartiles) + 
                                  factor(cluster) + 
                                  factor(req_deliverability) + 
                                  factor(fuel_category),
                                data = cost_status_data)

lm_GIA_prob_phase_2_costs_corrected <- vcovHC(lm_GIA_prob_phase_2_costs, 
                                              type = "HC1")
phase_2_costs_robust <- coeftest(lm_GIA_prob_phase_2_costs, 
                                 vcov = lm_GIA_prob_phase_2_costs_corrected)
phase_2_costs_robust_se <- phase_2_costs_robust[, 2]

# Collect robust standard errors
all_prob_models <- list(lm_ph2_prob_phase_1_costs, 
                        lm_GIA_prob_phase_2_costs)

robust_se <- c(phase_1_costs_robust_se, phase_2_costs_robust_se)
 
print(summary(lm_ph2_prob_phase_1_costs))



 covariate_labels <- c(
  "Phase 1 POI Cost per kW (0 - 20)",
  "Phase 1 POI Cost per kW (20 - 50)",
   "Phase 1 POI Cost per kW (50 - 100)",
   "Phase 1 POI Cost per kW (> 100)",
    
   "Phase 1 Network Cost per kW (20 - 50)",
   "Phase 1 Network Cost per kW (50 - 100)",
  "Phase 1 Network Cost per kW (> 100)",
   "Phase 2 POI Cost per kW (0 - 20)",
   "Phase 2 POI Cost per kW (20 - 50)",
   "Phase 2 POI Cost per kW (50 - 100)",
   "Phase 2 POI Cost per kW (> 100)",
 
  "Phase 2 Network Cost per kW (20 - 50)",
   "Phase 2 Network Cost per kW (50 - 100)",
  "Phase 2 Network Cost per kW (> 100)"
 )

 
stargazer(all_prob_models, type = "html", style = "qje", 
           se = list(robust_se, NULL),
           dep.var.labels = c("Ph2 Study Indicator", "GIA Indicator"),
           covariate.labels = covariate_labels,
           title = "LPM for Phase II Study and GIA within 2 Years",
           omit = c("capacity_quartiles", "cluster",
                    "req_deliverability", "fuel_category"),
           omit.labels = c("Capacity Quartile", "Cluster",
                           "Requested Deliverability", "Fuel"),
           omit.stat = c("f","ser","rsq"),
           notes.append = F, 
           intercept.bottom = F,
           out = paste0(project_root,
                        "/output/tables/lin_prob/regression_prob_phase_2_and_GIA_cost_bin_no_intercept.html"))
 
 stargazer(all_prob_models, type = "latex", style = "qje", 
           se = list(robust_se, NULL),
           dep.var.labels = c("Ph2 Study Indicator", "GIA Indicator"),
           covariate.labels = covariate_labels,
           title = "LPM for Phase II Study and GIA within 2 Years",
           omit = c("capacity_quartiles", "cluster",
                    "req_deliverability", "fuel_category"),
           omit.labels = c("Capacity Quartile", "Cluster",
                           "Requested Deliverability", "Fuel"),
           omit.stat = c("f","ser","rsq"),
           notes.append = F, 
           intercept.bottom = F,
           out = paste0(project_root,
                        "/output/tables/lin_prob/regression_prob_phase_2_and_GIA_cost_bin_no_intercept.tex"))




# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
 
 unique_capacity_quartiles <- levels(cost_status_data$capacity_quartiles)
 unique_clusters <- levels(cost_status_data$cluster)
 
 
 # Define Desired X-Axis Labels
 desired_labels <- c("0-20", "20-50", "50-100", ">100")
 
 # Define Representative Values for Prediction Grids
 unique_capacity_quartiles <- levels(cost_status_data$capacity_quartiles)
 unique_clusters <- levels(cost_status_data$cluster)
 
 representative_values <- list(
   capacity_quartiles = factor(4, levels = c(1, 2, 3, 4)),    # Highest quartile
   cluster = factor(7, levels = levels(cost_status_data$cluster)),  # Cluster 7
   req_deliverability = factor("Full Capacity", levels = levels(cost_status_data$req_deliverability)),
   fuel_category = factor("solar", levels = levels(cost_status_data$fuel_category))
 )
 
 # Create Prediction Grid for Phase II POI and Network Cost Bins
 ph2_POI_grid <- expand.grid(
   phase_1_POI_cost_per_kW_bins = factor(1:4, levels = c(1, 2, 3, 4)),
   phase_1_network_cost_per_kW_bins = factor(1, levels = c(1, 2, 3, 4)),
   phase_2_POI_cost_per_kW_bins = factor(1, levels = c(1, 2, 3, 4)),
   phase_2_network_cost_per_kW_bins = factor(1, levels = c(1, 2, 3, 4))
 ) %>%
   mutate(
     capacity_quartiles = representative_values$capacity_quartiles,
     cluster = representative_values$cluster,
     req_deliverability = representative_values$req_deliverability,
     fuel_category = representative_values$fuel_category,
     Cost_Type = "POI"
   )
 
 ph2_network_grid <- expand.grid(
   phase_1_POI_cost_per_kW_bins = factor(1, levels = c(1, 2, 3, 4)),
   phase_1_network_cost_per_kW_bins = factor(1:4, levels = c(1, 2, 3, 4)),
   phase_2_POI_cost_per_kW_bins = factor(1, levels = c(1, 2, 3, 4)),
   phase_2_network_cost_per_kW_bins = factor(1, levels = c(1, 2, 3, 4))
 ) %>%
   mutate(
     capacity_quartiles = representative_values$capacity_quartiles,
     cluster = representative_values$cluster,
     req_deliverability = representative_values$req_deliverability,
     fuel_category = representative_values$fuel_category,
     Cost_Type = "Network"
   )
 
 # Combine POI and Network Grids for Phase II
 ph2_pred_grid <- bind_rows(ph2_POI_grid, ph2_network_grid)
 
 # Predict Phase II Indicator
 ph2_predictions <- predict(lm_ph2_prob_phase_1_costs, newdata = ph2_pred_grid, se.fit = TRUE)
 
 # Add Predictions and Standard Errors to Phase II Grid
 ph2_pred_grid <- ph2_pred_grid %>%
   mutate(
     predicted_ph2_ind = ph2_predictions$fit,
     se_ph2_ind = ph2_predictions$se.fit,
     Cost_Bin = ifelse(Cost_Type == "POI",
                       as.character(phase_1_POI_cost_per_kW_bins),
                       as.character(phase_1_network_cost_per_kW_bins)),
     Cost_Bin = factor(Cost_Bin, levels = c("1", "2", "3", "4"),
                       labels = desired_labels)
   )
 
 # Create Prediction Grid for GIA POI and Network Cost Bins
 GIA_POI_grid <- expand.grid(
   phase_2_POI_cost_per_kW_bins = factor(1:4, levels = c(1, 2, 3, 4)),
   phase_2_network_cost_per_kW_bins = factor(1, levels = c(1, 2, 3, 4)),
   phase_1_POI_cost_per_kW_bins = factor(1, levels = c(1, 2, 3, 4)),
   phase_1_network_cost_per_kW_bins = factor(1, levels = c(1, 2, 3, 4))
 ) %>%
   mutate(
     capacity_quartiles = representative_values$capacity_quartiles,
     cluster = representative_values$cluster,
     req_deliverability = representative_values$req_deliverability,
     fuel_category = representative_values$fuel_category,
     Cost_Type = "POI"
   )
 
 GIA_network_grid <- expand.grid(
   phase_2_POI_cost_per_kW_bins = factor(1, levels = c(1, 2, 3, 4)),
   phase_2_network_cost_per_kW_bins = factor(1:4, levels = c(1, 2, 3, 4)),
   phase_1_POI_cost_per_kW_bins = factor(1, levels = c(1, 2, 3, 4)),
   phase_1_network_cost_per_kW_bins = factor(1, levels = c(1, 2, 3, 4))
 ) %>%
   mutate(
     capacity_quartiles = representative_values$capacity_quartiles,
     cluster = representative_values$cluster,
     req_deliverability = representative_values$req_deliverability,
     fuel_category = representative_values$fuel_category,
     Cost_Type = "Network"
   )
 
 # Combine POI and Network Grids for GIA
 GIA_pred_grid <- bind_rows(GIA_POI_grid, GIA_network_grid)
 
 # Predict GIA Indicator
 GIA_predictions <- predict(lm_GIA_prob_phase_2_costs, newdata = GIA_pred_grid, se.fit = TRUE)
 
 # Add Predictions and Standard Errors to GIA Grid
 GIA_pred_grid <- GIA_pred_grid %>%
   mutate(
     predicted_GIA_ind = GIA_predictions$fit,
     se_GIA_ind = GIA_predictions$se.fit,
     Cost_Bin = ifelse(Cost_Type == "POI",
                       as.character(phase_2_POI_cost_per_kW_bins),
                       as.character(phase_2_network_cost_per_kW_bins)),
     Cost_Bin = factor(Cost_Bin, levels = c("1", "2", "3", "4"),
                       labels = desired_labels)
   )
 
 # Prepare Phase II Predictions
 ph2_results <- ph2_pred_grid %>%
   select(Cost_Type, Cost_Bin, predicted_ph2_ind, se_ph2_ind) %>%
   mutate(Indicator = "Phase II") %>%
   rename(predicted = predicted_ph2_ind, se = se_ph2_ind)
 
 # Prepare GIA Predictions
 GIA_results <- GIA_pred_grid %>%
   select(Cost_Type, Cost_Bin, predicted_GIA_ind, se_GIA_ind) %>%
   mutate(Indicator = "GIA") %>%
   rename(predicted = predicted_GIA_ind, se = se_GIA_ind)
 
 # Combine All Predictions
 all_predictions <- bind_rows(ph2_results, GIA_results)
 
 # Plot for Phase II Indicator
 phaseII_data <- all_predictions %>%
   filter(Indicator == "Phase II")
 
 Ph2_plot <- ggplot(phaseII_data, aes(x = Cost_Bin, y = predicted, fill = Cost_Type)) +
   geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
   geom_errorbar(aes(ymin = predicted - se, ymax = predicted + se),
                 width = 0.2, position = position_dodge(width = 0.8)) +
   labs(
     title = "Effect of Phase 1 Cost per kW Bin on Receiving Phase 2 Study",
     x = "Phase 1 Cost per kW Bin",
     y = "Predicted Probability",
     fill = "Cost Type"
   ) + 
   scale_y_continuous(  limits = c(0, 1)) +
   scale_fill_manual(values = c("POI" = "skyblue", "Network" = "salmon")) +  # Custom Colors
   theme_minimal()+
   theme(
     axis.text.x = element_text(angle = 0, hjust = 0.5, size = 27),  # Horizontal x-axis labels
     axis.text.y = element_text(size = 27),  # Adjust y-axis text size
     strip.text = element_text(size = 30),  # Adjust facet label size
     plot.title = element_text(size = 30, hjust = 0.5),  # Centered title without bold
     axis.title.x = element_text(size = 25),  # Larger x-axis title
     axis.title.y = element_text(size = 25),  # Larger y-axis title
     legend.title = element_text(size = 20),
     legend.text = element_text(size = 20),
   ) 
 
 # Save the Phase II Plot
 ggsave(
   filename = paste0(project_root, "/output/figures/lin_prob/bar_graph_ph2_ind_no_intercept_cost_bins.png"),
   plot = Ph2_plot,
   width = 16, height = 9, dpi = 300
 )
 
 # Plot for GIA Indicator
 GIA_data <- all_predictions %>%
   filter(Indicator == "GIA")
 
 GIA_plot <- ggplot(GIA_data, aes(x = Cost_Bin, y = predicted, fill = Cost_Type)) +
   geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
   geom_errorbar(aes(ymin = predicted - se, ymax = predicted + se),
                 width = 0.2, position = position_dodge(width = 0.8)) +
   labs(
     title = "Effect of Phase 2 Cost per kW Bin on Completing GIA",
     x = "Phase 2 Cost per kW Bin",
     y = "Predicted Probability",
     fill = "Cost Type"
   ) +
   scale_y_continuous(limits = c(-0.04, 0.2)) +
   scale_fill_manual(values = c("POI" = "skyblue", "Network" = "salmon")) +  # Custom Colors
   theme_minimal() +
   theme(
     axis.text.x = element_text(angle = 0, hjust = 0.5, size = 27),  # Horizontal x-axis labels
     axis.text.y = element_text(size = 27),  # Adjust y-axis text size
     strip.text = element_text(size = 30),  # Adjust facet label size
     plot.title = element_text(size = 30, hjust = 0.5),  # Centered title without bold
     axis.title.x = element_text(size = 25),  # Larger x-axis title
     axis.title.y = element_text(size = 25),  # Larger y-axis title
     legend.title = element_text(size = 20),
     legend.text = element_text(size = 20),
   ) 
 
 # Save the GIA Plot
 ggsave(
   filename = paste0(project_root, "/output/figures/lin_prob/bar_graph_GIA_ind_no_intercept_cost_bins.png"),
   plot = GIA_plot,
   width = 16, height = 9, dpi = 300
 )
 
 # Optional: Print the plots to the R console
 print(Ph2_plot)
 print(GIA_plot)



######### for whole dataset

# # Load necessary libraries
# library(dplyr)
# library(ggplot2)
# library(tidyr)
# library(scales)     # Needed for percent_format
# library(sandwich)
# library(lmtest)
# 
# # Ensure cost bins are factors with levels 1-4
# cost_status_data <- cost_status_data %>%
#   mutate(
#     phase_1_POI_cost_per_kW_bins = factor(phase_1_POI_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
#     phase_1_network_cost_per_kW_bins = factor(phase_1_network_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
#     phase_2_POI_cost_per_kW_bins = factor(phase_2_POI_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
#     phase_2_network_cost_per_kW_bins = factor(phase_2_network_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
#     capacity_quartiles = factor(capacity_quartiles),
#     cluster = factor(cluster),
#     req_deliverability = factor(req_deliverability, levels = c("Energy Only", "Partial Capacity", "Full Capacity")),
#     fuel_category = factor(fuel_category, levels = c("wind_water", "battery_storage", "solar", "biofuel_natural_gas_other"))
#   )
# 
# # Define all unique levels for control variables
# unique_capacity_quartiles <- levels(cost_status_data$capacity_quartiles)
# unique_clusters <- levels(cost_status_data$cluster)
# unique_deliverability <- levels(cost_status_data$req_deliverability)
# unique_fuel_categories <- levels(cost_status_data$fuel_category)
# 
# # Create grid for Phase II Predictions (POI and Network)
# ph2_pred_grid <- expand.grid(
#   phase_1_POI_cost_per_kW_bins = factor(1:4, levels = c(1, 2, 3, 4)),
#   phase_1_network_cost_per_kW_bins = factor(1:4, levels = c(1, 2, 3, 4)),
#   phase_2_POI_cost_per_kW_bins = factor(1:4, levels = c(1, 2, 3, 4)),
#   phase_2_network_cost_per_kW_bins = factor(1:4, levels = c(1, 2, 3, 4)),
#   capacity_quartiles = unique_capacity_quartiles,
#   cluster = unique_clusters,
#   req_deliverability = unique_deliverability,
#   fuel_category = unique_fuel_categories
# )
# 
# # Predict Phase II indicator
# ph2_predictions <- predict(lm_ph2_prob_phase_1_costs, newdata = ph2_pred_grid, se.fit = TRUE)
# 
# # Add predictions and standard errors to the grid
# ph2_pred_grid$predicted_ph2_ind <- ph2_predictions$fit
# ph2_pred_grid$se_ph2_ind <- ph2_predictions$se.fit
# 
# # Predict GIA indicator
# GIA_predictions <- predict(lm_GIA_prob_phase_2_costs, newdata = ph2_pred_grid, se.fit = TRUE)
# 
# # Add predictions and standard errors to the grid
# ph2_pred_grid$predicted_GIA_ind <- GIA_predictions$fit
# ph2_pred_grid$se_GIA_ind <- GIA_predictions$se.fit
# 
# # Aggregate Phase II Predictions by POI Cost Bin
# ph2_POI_summary <- ph2_pred_grid %>%
#   group_by(phase_1_POI_cost_per_kW_bins) %>%
#   summarize(
#     mean_predicted = mean(predicted_ph2_ind, na.rm = TRUE),
#     se_predicted = sd(predicted_ph2_ind, na.rm = TRUE) / sqrt(n())
#   ) %>%
#   mutate(
#     Cost_Type = "POI",
#     Cost_Bin = as.numeric(as.character(phase_1_POI_cost_per_kW_bins)),
#     Indicator = "Phase II"
#   )
# 
# # Aggregate Phase II Predictions by Network Cost Bin
# ph2_network_summary <- ph2_pred_grid %>%
#   group_by(phase_1_network_cost_per_kW_bins) %>%
#   summarize(
#     mean_predicted = mean(predicted_ph2_ind, na.rm = TRUE),
#     se_predicted = sd(predicted_ph2_ind, na.rm = TRUE) / sqrt(n())
#   ) %>%
#   mutate(
#     Cost_Type = "Network",
#     Cost_Bin = as.numeric(as.character(phase_1_network_cost_per_kW_bins)),
#     Indicator = "Phase II"
#   )
# 
# # Aggregate GIA Predictions by POI Cost Bin
# GIA_POI_summary <- ph2_pred_grid %>%
#   group_by(phase_2_POI_cost_per_kW_bins) %>%
#   summarize(
#     mean_predicted = mean(predicted_GIA_ind, na.rm = TRUE),
#     se_predicted = sd(predicted_GIA_ind, na.rm = TRUE) / sqrt(n())
#   ) %>%
#   mutate(
#     Cost_Type = "POI",
#     Cost_Bin = as.numeric(as.character(phase_2_POI_cost_per_kW_bins)),
#     Indicator = "GIA"
#   )
# 
# # Aggregate GIA Predictions by Network Cost Bin
# GIA_network_summary <- ph2_pred_grid %>%
#   group_by(phase_2_network_cost_per_kW_bins) %>%
#   summarize(
#     mean_predicted = mean(predicted_GIA_ind, na.rm = TRUE),
#     se_predicted = sd(predicted_GIA_ind, na.rm = TRUE) / sqrt(n())
#   ) %>%
#   mutate(
#     Cost_Type = "Network",
#     Cost_Bin = as.numeric(as.character(phase_2_network_cost_per_kW_bins)),
#     Indicator = "GIA"
#   )
# 
# # Combine all summaries
# all_summaries <- bind_rows(ph2_POI_summary, ph2_network_summary, GIA_POI_summary, GIA_network_summary)
# 
# 
#  
# # Inspect the aggregated summaries
# head(all_summaries)
# 
# # Load necessary libraries
# library(ggplot2)
# library(scales)
# 
# # Plot for Phase II Indicator
# ggplot(all_predictions %>% filter(Indicator == "Phase II"), aes(x = factor(Cost_Bin), y = predicted, fill = Cost_Type)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   geom_errorbar(aes(ymin = predicted - se, ymax = predicted + se), position = position_dodge(0.9), width = 0.2) +
#   scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.2,0.6)) +
#   labs(title = "Predicted Probability of Phase II Indicator",
#        x = "Cost Bin",
#        y = "Predicted Probability",
#        fill = "Cost Type") +
#   theme_minimal()
# 
# # Plot for GIA Indicator
# ggplot(all_predictions %>% filter(Indicator == "GIA"), aes(x = factor(Cost_Bin), y = predicted, fill = Cost_Type)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   geom_errorbar(aes(ymin = predicted - se, ymax = predicted + se), position = position_dodge(0.9), width = 0.2) +
#   scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(-0.1,0.2)) +
#   labs(title = "Predicted Probability of GIA Indicator",
#        x = "Cost Bin",
#        y = "Predicted Probability",
#        fill = "Cost Type") +
#   theme_minimal()



#########################################################
## OLD vs NEW

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(scales)

# -------------------------------
# Load Necessary Libraries
# -------------------------------
# Install the packages if they are not already installed
# install.packages(c("dplyr", "lmtest", "sandwich", "stargazer"))

library(dplyr)       # For data manipulation
library(lmtest)      # For coeftest
library(sandwich)    # For robust standard errors
library(stargazer)   # For creating regression tables

# -------------------------------
# Define Project Root Directory
# -------------------------------
 

# -------------------------------
# Data Preparation
# -------------------------------
# Assuming 'cost_status_data' is your dataframe already loaded into the R environment

cost_status_data <- cost_status_data %>%
  # Convert relevant variables to factors with specified levels
  mutate(
    phase_1_POI_cost_per_kW_bins = factor(phase_1_POI_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
    phase_1_network_cost_per_kW_bins = factor(phase_1_network_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
    phase_2_POI_cost_per_kW_bins = factor(phase_2_POI_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
    phase_2_network_cost_per_kW_bins = factor(phase_2_network_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
    capacity_quartiles = factor(capacity_quartiles),
    cluster = factor(cluster),
    req_deliverability = factor(req_deliverability, levels = c("Energy Only", "Partial Capacity", "Full Capacity")),
    fuel_category = factor(fuel_category, levels = c("wind_water", "battery_storage", "solar", "biofuel_natural_gas_other"))
  ) %>%
  # Create a new variable 'cluster_group' to categorize clusters as "Old" or "New"
  mutate(
    cluster_group = case_when(
      cluster %in% 7:10 ~ "Old",
      cluster %in% 11:13 ~ "New",
      TRUE ~ NA_character_  # Assign NA to clusters outside 7-13 if any
    )
  ) %>%
  # Convert 'cluster_group' to a factor with specified levels
  mutate(cluster_group = factor(cluster_group, levels = c("Old", "New")))

# -------------------------------
# Optional: Verify Cluster Grouping
# -------------------------------
# This step is optional but recommended to ensure clusters are grouped correctly
print("Distribution of Cluster Groups:")
print(table(cost_status_data$cluster_group))

# -------------------------------
# Subset Data for Old and New Clusters
# -------------------------------
old_clusters_data <- cost_status_data %>%
  filter(cluster_group == "Old")

new_clusters_data <- cost_status_data %>%
  filter(cluster_group == "New")

# -------------------------------
# Define a Function to Run Regressions
# -------------------------------
# This function fits a linear model and computes robust standard errors
run_regression <- function(data, dependent_var, phase_POI_bins, phase_network_bins) {
  # Dynamically create the regression formula
  formula <- as.formula(
    paste(
      dependent_var, "~ 0 +",
      paste(c(phase_POI_bins, phase_network_bins), collapse = " + "),
      "+ capacity_quartiles + cluster + req_deliverability + fuel_category"
    )
  )
  
  # Fit the linear model
  lm_model <- lm(formula, data = data)
  
  # Calculate robust standard errors using HC1 estimator
  robust_se <- sqrt(diag(vcovHC(lm_model, type = "HC1")))
  
  return(list(model = lm_model, robust_se = robust_se))
}

# -------------------------------
# Run Regressions for Each Group and Dependent Variable
# -------------------------------

# 1. Old Clusters - Ph2 Indicator
reg_old_ph2 <- run_regression(
  data = old_clusters_data,
  dependent_var = "ph2_ind",
  phase_POI_bins = "phase_1_POI_cost_per_kW_bins",
  phase_network_bins = "phase_1_network_cost_per_kW_bins"
)

# 2. Old Clusters - GIA Indicator
reg_old_GIA <- run_regression(
  data = old_clusters_data,
  dependent_var = "GIA_ind",
  phase_POI_bins = "phase_2_POI_cost_per_kW_bins",
  phase_network_bins = "phase_2_network_cost_per_kW_bins"
)

# 3. New Clusters - Ph2 Indicator
reg_new_ph2 <- run_regression(
  data = new_clusters_data,
  dependent_var = "ph2_ind",
  phase_POI_bins = "phase_1_POI_cost_per_kW_bins",
  phase_network_bins = "phase_1_network_cost_per_kW_bins"
)

# 4. New Clusters - GIA Indicator
reg_new_GIA <- run_regression(
  data = new_clusters_data,
  dependent_var = "GIA_ind",
  phase_POI_bins = "phase_2_POI_cost_per_kW_bins",
  phase_network_bins = "phase_2_network_cost_per_kW_bins"
)

# -------------------------------
# Collect Models and Robust Standard Errors
# -------------------------------
all_prob_models <- list(
  reg_old_ph2$model,
  reg_old_GIA$model,
  reg_new_ph2$model,
  reg_new_GIA$model
)

robust_se <- list(
  reg_old_ph2$robust_se,
  reg_old_GIA$robust_se,
  reg_new_ph2$robust_se,
  reg_new_GIA$robust_se
)

# -------------------------------
# Define Covariate Labels
# -------------------------------
covariate_labels <- c(
 
  
  # Phase 1 Network Cost per kW
  "Phase 1 Network Cost per kW (20 - 50)",
  "Phase 1 Network Cost per kW (50 - 100)",
  "Phase 1 Network Cost per kW (> 100)",
  
 
  
  # Phase 2 Network Cost per kW
  "Phase 2 Network Cost per kW (20 - 50)",
  "Phase 2 Network Cost per kW (50 - 100)",
  "Phase 2 Network Cost per kW (> 100)"
)


 

# -------------------------------
# Generate Regression Tables with Stargazer
# -------------------------------
# Create output directories if they don't exist
# Define variables to omit (including POI cost bins)
# Define variables to omit (including all POI cost bin variables and control variables)
omit_vars <- c(
  "capacity_quartiles", 
  "cluster", 
  "req_deliverability", 
  "fuel_category", 
  "phase_1_POI_cost_per_kW_bins1",
  "phase_1_POI_cost_per_kW_bins2",
  "phase_1_POI_cost_per_kW_bins3",
  "phase_1_POI_cost_per_kW_bins4",
  "phase_2_POI_cost_per_kW_bins1",
  "phase_2_POI_cost_per_kW_bins2",
  "phase_2_POI_cost_per_kW_bins3",
  "phase_2_POI_cost_per_kW_bins4"
)

# Define corresponding labels for all omitted variables
omit_labels_full <- c(
  "Capacity Quartile", 
  "Cluster", 
  "Requested Deliverability", 
  "Fuel",
  "Phase 1 POI Cost Bins",
  "Phase 1 POI Cost Bins",
  "Phase 1 POI Cost Bins",
  "Phase 1 POI Cost Bins",
  "Phase 2 POI Cost Bins",
  "Phase 2 POI Cost Bins",
  "Phase 2 POI Cost Bins",
  "Phase 2 POI Cost Bins"
)

# Create output directories if they don't exist
dir.create(file.path(project_root, "output", "tables", "lin_prob"), recursive = TRUE, showWarnings = FALSE)

# Define custom lines indicating inclusion of control variables
add_lines_custom <- list(
  c("Capacity Quartile", "Yes", "Yes", "Yes", "Yes"),
  c("Cluster", "Yes", "Yes", "Yes", "Yes"),
  c("Requested Deliverability", "Yes", "Yes", "Yes", "Yes"),
  c("Fuel", "Yes", "Yes", "Yes", "Yes"),
  c("POI Cost Bins", "Yes", "Yes", "Yes", "Yes")
)

# 1. HTML Table
stargazer(
  all_prob_models,
  type = "html",
  style = "qje",
  se = robust_se,
  dep.var.labels = c(
    "Ph2 Indicator (7-10)", 
    "GIA Indicator (7-10)",
    "Ph2 Indicator (11-13)", 
    "GIA Indicator (11-13)"
  ),
  covariate.labels = covariate_labels,  # Include only network cost bins
  title = "Linear Probability Models for Phase II Study and GIA within 2 Years by Cluster Group",
  omit = omit_vars,  # Omit POI cost bins and control variables
     # Provided labels for all omitted variables
  omit.stat = c("f", "ser", "rsq"),
  add.lines = add_lines_custom,  # Add rows indicating inclusion of control variables
  notes = "",  # Remove default notes
  notes.append = FALSE,
  intercept.bottom = FALSE,
  out = file.path(project_root, "output", "tables", "lin_prob", "regression_prob_phase_2_and_GIA_cost_bin_grouped.html")
)

# 2. LaTeX Table
stargazer(
  all_prob_models,
  type = "latex",
  style = "qje",
  se = robust_se,
  dep.var.labels = c(
    "Ph2 Indicator (7-10)", 
    "GIA Indicator (7-10)",
    "Ph2 Indicator (11-13)", 
    "GIA Indicator (11-13)"
  ),
  covariate.labels = covariate_labels,  # Include only network cost bins
  title = "Linear Probability Models for Phase II Study and GIA within 2 Years by Cluster Group",
  omit = omit_vars,  # Omit POI cost bins and control variables
  # Provided labels for all omitted variables
  omit.stat = c("f", "ser", "rsq"),
  add.lines = add_lines_custom,  # Add rows indicating inclusion of control variables
  notes = "",  # Remove default notes
  notes.append = FALSE,
  intercept.bottom = FALSE,
  out = file.path(project_root, "output", "tables", "lin_prob", "regression_prob_phase_2_and_GIA_cost_bin_grouped.tex")
)
# -------------------------------
# Optional: Print Regression Summaries to Console
# -------------------------------
# Uncomment the lines below if you wish to see the regression summaries in the R console

# print(summary(reg_old_ph2$model))
# print(summary(reg_old_GIA$model))
# print(summary(reg_new_ph2$model))
# print(summary(reg_new_GIA$model))




#-----------------------------------
# Bar Plots
#-------------------------------------

# Define the cost bins and cluster groups
cost_bins <- c("20-50", "50-100", ">100")
cluster_groups <- c("Old", "New")

# Initialize empty data frames for Ph2 and GIA
ph2_data <- data.frame()
gia_data <- data.frame()

# Extract coefficients and SEs for Ph2 Indicator
for (i in 1:length(cost_bins)) {
  # Ph2 - Old Clusters
  var_name_old <- paste0("phase_1_network_cost_per_kW_bins", i + 1)
  ph2_coef_old <- coef(all_prob_models[[1]])[var_name_old]
  ph2_se_old <- robust_se[[1]][var_name_old]
  
  ph2_data <- rbind(ph2_data, data.frame(
    Cost_Bin = cost_bins[i],
    Cluster_Group = "Old",
    Coefficient = ph2_coef_old,
    SE = ph2_se_old
  ))
  
  # Ph2 - New Clusters
  var_name_new <- paste0("phase_1_network_cost_per_kW_bins", i + 1)
  ph2_coef_new <- coef(all_prob_models[[3]])[var_name_new]
  ph2_se_new <- robust_se[[3]][var_name_new]
  
  ph2_data <- rbind(ph2_data, data.frame(
    Cost_Bin = cost_bins[i],
    Cluster_Group = "New",
    Coefficient = ph2_coef_new,
    SE = ph2_se_new
  ))
}

# Extract coefficients and SEs for GIA Indicator
for (i in 1:length(cost_bins)) {
  # GIA - Old Clusters
  var_name_old <- paste0("phase_2_network_cost_per_kW_bins", i + 1)
  gia_coef_old <- coef(all_prob_models[[2]])[var_name_old]
  gia_se_old <- robust_se[[2]][var_name_old]
  
  gia_data <- rbind(gia_data, data.frame(
    Cost_Bin = cost_bins[i],
    Cluster_Group = "Old",
    Coefficient = gia_coef_old,
    SE = gia_se_old
  ))
  
  # GIA - New Clusters
  var_name_new <- paste0("phase_2_network_cost_per_kW_bins", i + 1)
  gia_coef_new <- coef(all_prob_models[[4]])[var_name_new]
  gia_se_new <- robust_se[[4]][var_name_new]
  
  gia_data <- rbind(gia_data, data.frame(
    Cost_Bin = cost_bins[i],
    Cluster_Group = "New",
    Coefficient = gia_coef_new,
    SE = gia_se_new
  ))
}

# Ensure Cost_Bin is a factor with the correct order
ph2_data$Cost_Bin <- factor(ph2_data$Cost_Bin, levels = cost_bins)
gia_data$Cost_Bin <- factor(gia_data$Cost_Bin, levels = cost_bins)


# -------------------------------
# Step 8: Create the Bar Plots
# -------------------------------
# Recode Cluster_Group to "Clusters 7-10" and "Clusters 11-13" and set factor levels
# Recode Cluster_Group to "Clusters 7-10" and "Clusters 11-13" and set factor levels
ph2_data <- ph2_data %>%
  mutate(
    Cluster_Group = case_when(
      Cluster_Group == "Old" ~ "Clusters 7-10",
      Cluster_Group == "New" ~ "Clusters 11-13",
      TRUE ~ Cluster_Group  # Retain original value if not "Old" or "New"
    ),
    # Set factor levels to control legend order
    Cluster_Group = factor(Cluster_Group, levels = c("Clusters 7-10", "Clusters 11-13"))
  )

gia_data <- gia_data %>%
  mutate(
    Cluster_Group = case_when(
      Cluster_Group == "Old" ~ "Clusters 7-10",
      Cluster_Group == "New" ~ "Clusters 11-13",
      TRUE ~ Cluster_Group  # Retain original value if not "Old" or "New"
    ),
    # Set factor levels to control legend order
    Cluster_Group = factor(Cluster_Group, levels = c("Clusters 7-10", "Clusters 11-13"))
  )
# Define custom colors for Cluster Groups
cluster_colors <- c("Clusters 7-10" = "skyblue", "Clusters 11-13" = "salmon")

# Plot for Ph2 Indicator
ph2_plot <- ggplot(ph2_data, aes(x = Cost_Bin, y = Coefficient, fill = Cluster_Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Coefficient - SE, ymax = Coefficient + SE),
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(title = "Affect of Phase 1 Cost per kW on Receiving Phase 2 study",
       x = "Phase 1 Network Cost per kW Bin",
       y = "Marginal Probability (Relative to Projects with cost < $20/kW)",
       fill = "Cluster Group") +
  scale_fill_manual(values = cluster_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 27),  # Horizontal x-axis labels
    axis.text.y = element_text(size = 27),  # Adjust y-axis text size
    strip.text = element_text(size = 30),  # Adjust facet label size
    plot.title = element_text(size = 30, hjust = 0.5),  # Centered title without bold
    axis.title.x = element_text(size = 22),  # Larger x-axis title
    axis.title.y = element_text(size = 22),  # Larger y-axis title
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
  ) 
# Plot for GIA Indicator
gia_plot <- ggplot(gia_data, aes(x = Cost_Bin, y = Coefficient, fill = Cluster_Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = Coefficient - SE, ymax = Coefficient + SE),
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(title = "Affect of Phase 2 Cost per kW on Completing GIA",
       x = "Phase 2 Network Cost per kW Bin",
       y = "Marginal Probability (Relative to Projects with cost < $20/kW)",
       fill = "Cluster Group") +
  scale_fill_manual(values = cluster_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 27),  # Horizontal x-axis labels
    axis.text.y = element_text(size = 27),  # Adjust y-axis text size
    strip.text = element_text(size = 30),  # Adjust facet label size
    plot.title = element_text(size = 30, hjust = 0.5),  # Centered title without bold
    axis.title.x = element_text(size = 22),  # Larger x-axis title
    axis.title.y = element_text(size = 22),  # Larger y-axis title
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
  ) 

# Display the Ph2 Indicator Plot
print(ph2_plot)

# Display the GIA Indicator Plot
print(gia_plot)

# -------------------------------
# Step 9: Optional - Combine Both Plots into a Single Figure
# -------------------------------

 

# -------------------------------
# Step 10: Save the Plots
# -------------------------------

# Save Ph2 plot

ggsave(filename = paste0(project_root, 
                         "/output/figures/lin_prob/bar_graph_ph2_ind_cost_bins_old_vs_new.png"),
       plot = ph2_plot,
       width = 16, height = 9, dpi = 300)

ggsave(filename = paste0(project_root, 
                         "/output/figures/lin_prob/bar_graph_gia_ind_cost_bins_old_vs_new.png"),
       plot = gia_plot,
       width = 16, height = 9, dpi = 300)

 


# -------------------------------
# End of Script
# -------------------------------

