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
library(jtools)
library(AER)

## ===== IMPORTING DATA =====
cost_data <- read.csv(paste0(project_root, "/data/working/CAISO_cost_data.csv"))

all_projects_summary <- read.csv(paste0(project_root, 
                                        "/data/Scraper/all_projects_summary.csv"))

CAISO_RIMS <- read.csv(paste0(project_root, 
                              "/data/app_and_study/RIMS 5_App & Study-Project Info (8-28-2024).csv"))

checklist_data <- read.csv(paste0(project_root,
                                  "/data/working/clean_RIMS_checklist_cluster_updated.csv"))


firm_names <- read.csv(paste0(project_root,
                              "/data/working/primary_contacts.csv"))


core_events_checklist <- read.csv(paste0(project_root, 
                                         "/data/working/clean_RIMS_checklist_cluster_core.csv")) 

## ===== CLEANING DATA =====
## Cleaning All Projects Summary
colnames(all_projects_summary) <- c("project_name", "q_id", "q_date",
                                    "project_cost_code", "study_type",
                                    "cluster", "PTO", "PTO_region",
                                    "affected_PTO", "affected_PTO_region",
                                    "POI", "voltage_level_kV", "project_status",
                                    "reason_for_withdraw", "status_date",
                                    "QM_project_standing", "req_deliverability",
                                    "project_description", "shared_gen_tie",
                                    "contact_first_name", "contact_last_name",
                                    "contact_title", "signature_date", 
                                    "AIM_org", "company_type", 
                                    "state_incorporated", "parent_company",
                                    "IC_legal_entity", "address", "city", 
                                    "county", "state", "zip_code", "latitude",
                                    "longitude", "path_26", "local_area",
                                    "generator_type", "fuel", "net_MW", 
                                    "MWh", "storage_duration_hours", 
                                    "hyrbid", "co_located", 
                                    "general_description",
                                    "description", "date_tendered", 
                                    "negotiation_status", 
                                    "negotiation_status_date", 
                                    "released_to_RC", 
                                    "exxecution_vers_sent_all_parties", 
                                    "GIA_execution_status", 
                                    "GIA_execution_status_date",
                                    "FERC_filliing_status", 
                                    "FERC_filling_status_date",
                                    "negotiation_time_elapsed", 
                                    "non_conforming", "EQR", 
                                    "ISO_attorney", 
                                    "execution_date_per_MMA", "project_file")

all_projects_summary <- subset(all_projects_summary, 
                               !all_projects_summary$cluster == "[None]")

all_projects_summary$net_MW <- as.numeric(all_projects_summary$net_MW)


## Removing NA Status Dates from Checklist Data
checklist_data <- subset(checklist_data, !is.na(checklist_data$status_date))

## Formatting Checklist Data
checklist_data$q_date <- ymd(checklist_data$q_date)
checklist_data$status_date <- ymd(checklist_data$status_date)



## Cleaning Cost Data
clean_cost_data <- subset(cost_data, !is.na(capacity) & capacity != 0 &
                            !is.na(fuel)) #this removes a few projects

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

## Prices per kW
clean_cost_data$phase_1_total_network_cost_per_kW <- clean_cost_data$phase_1_total_network_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_network_cost_per_kW <- clean_cost_data$phase_2_total_network_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_total_POI_cost_per_kW <- clean_cost_data$phase_1_total_POI_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_POI_cost_per_kW <- clean_cost_data$phase_2_total_POI_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_total_cost_per_kW <- clean_cost_data$phase_1_total_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_cost_per_kW <- clean_cost_data$phase_2_total_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_network_CCR_cost_per_kW <- clean_cost_data$phase_1_network_CCR_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_network_MCR_cost_per_kW <- clean_cost_data$phase_1_network_MCR_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_network_ADNU_cost_per_kW <- clean_cost_data$phase_1_network_ADNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_network_CCR_cost_per_kW <- clean_cost_data$phase_2_network_CCR_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_network_MCR_cost_per_kW <- clean_cost_data$phase_2_network_MCR_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_network_ADNU_cost_per_kW <- clean_cost_data$phase_2_network_ADNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_total_RNU_cost_per_kW <- clean_cost_data$phase_1_total_RNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_total_LDNU_cost_per_kW <- clean_cost_data$phase_1_total_LDNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_total_ADNU_cost_per_kW <- clean_cost_data$phase_1_total_ADNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_RNU_cost_per_kW <- clean_cost_data$phase_2_total_RNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_LDNU_cost_per_kW <- clean_cost_data$phase_2_total_LDNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_ADNU_cost_per_kW <- clean_cost_data$phase_2_total_ADNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_total_network_cost_per_kW <- clean_cost_data$phase_1_total_network_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_network_cost_per_kW <- clean_cost_data$phase_2_total_network_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_total_POI_cost_per_kW <- clean_cost_data$phase_1_total_POI_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_POI_cost_per_kW <- clean_cost_data$phase_2_total_POI_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_total_cost_per_kW <- clean_cost_data$phase_1_total_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_cost_per_kW <- clean_cost_data$phase_2_total_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_network_CCR_cost_per_kW <- clean_cost_data$phase_1_network_CCR_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_network_MCR_cost_per_kW <- clean_cost_data$phase_1_network_MCR_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_network_ADNU_cost_per_kW <- clean_cost_data$phase_1_network_ADNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_network_CCR_cost_per_kW <- clean_cost_data$phase_2_network_CCR_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_network_MCR_cost_per_kW <- clean_cost_data$phase_2_network_MCR_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_network_ADNU_cost_per_kW <- clean_cost_data$phase_2_network_ADNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_total_RNU_cost_per_kW <- clean_cost_data$phase_1_total_RNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_total_LDNU_cost_per_kW <- clean_cost_data$phase_1_total_LDNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_1_total_ADNU_cost_per_kW <- clean_cost_data$phase_1_total_ADNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_RNU_cost_per_kW <- clean_cost_data$phase_2_total_RNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_LDNU_cost_per_kW <- clean_cost_data$phase_2_total_LDNU_cost/
  (1000 * clean_cost_data$capacity)

clean_cost_data$phase_2_total_ADNU_cost_per_kW <- clean_cost_data$phase_2_total_ADNU_cost/
  (1000 * clean_cost_data$capacity)


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
 
                                                                      
clean_cost_data$phase_2_total_cost_per_kW <- ifelse(clean_cost_data$phase_2_total_cost_per_kW > quantile(clean_cost_data$phase_2_total_cost_per_kW, 
                                                                                                         0.99, na.rm = TRUE),
                                                    quantile(clean_cost_data$phase_2_total_cost_per_kW, 
                                                             0.99, na.rm = TRUE), 
                                                    clean_cost_data$phase_2_total_cost_per_kW)
 


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



## Cleaning Core Events Checklist Data
core_events_checklist <- subset(core_events_checklist, 
                                !(core_events_checklist$q_id == 9999) &
                                  !(core_events_checklist$project_id == 1))

## Adding Fuel Type to Core Events Checklist Data
project_fuel <- clean_cost_data[c("q_id", "fuel")]
project_fuel$q_id <- as.character(project_fuel$q_id)

core_events_checklist <- core_events_checklist %>%
  left_join(project_fuel, by = "q_id")

## Adding Deliverability Type to Core Events Checklist
project_deliv <- clean_cost_data[c("q_id", "req_deliverability")]
project_deliv$q_id <- as.character(project_deliv$q_id)

core_events_checklist <- core_events_checklist %>%
  left_join(project_deliv, by = "q_id")


# ## ==== RANDOMLY ASSIGNING FIRM (FOR TESTING PURPOSES) ====
# ## Assigning 4 Firms at Random to Projects
# firms <- c("Firm1", "Firm2", "Firm3", "Firm4")
# set.seed(123)  # Set seed for reproducibility
# clean_cost_data$firm <- sample(firms, nrow(clean_cost_data), replace = TRUE)


## ===== CLEANING FIRM DATA =====
clean_firm_names <- subset(firm_names, 
                           !(is.na(firm_names$queue_id) | 
                               (firm_names$queue_id == "") | 
                               (firm_names$primary_contact_email == "")))

clean_firm_names$firm <- sub(".*@(.*?)\\..*", "\\1", 
                             clean_firm_names$primary_contact_email)

colnames(clean_firm_names)[2] <- "q_id"

clean_firm_names <- clean_firm_names %>%
  select(q_id, firm)

## Adding Firm Information to other Dataframes
clean_cost_data$q_id <- as.character(clean_cost_data$q_id)

clean_cost_data <- clean_cost_data %>%
  left_join(clean_firm_names, by = "q_id")

core_events_checklist <- core_events_checklist %>%
  left_join(clean_firm_names, by = "q_id",
            relationship = "many-to-many")



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


cost_status_data <- cost_status_data %>%
 relocate(firm, .after = PTO)

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











## Transition Probability Regressions #####


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




#cost_status_data <- cost_status_data %>%
 # mutate(firm = ifelse(firm == "edf-re", "edf-renewables", firm))

 
#cost_status_data <- cost_status_data %>%
 # mutate(firm = na_if(firm, NA)) %>%  # Converts NA values explicitly (optional)
 # filter(!is.na(firm))  # Removes rows where firm is NA






###### Frequency tables of most common firms#########


### IN general #########
 
 
freq_firm_gen <- clean_firm_names %>%
  count(firm) %>%
  arrange(desc(n))
colnames(freq_firm_gen)[2] <- "count"

 

# Top 10 most common firms in general
top_10_firms <- freq_firm_gen %>%
  slice_max(order_by = count, n = 10) 


stargazer(top_10_firms, summary = F, rownames = F, type = "latex",
          title = "Number of Projects by Firm (CAISO queue)", 
          digit.separate = 0,
          out = paste0(project_root,
                       "/output/tables/firm_level/table_CAISO_general_projects_firm_freq_top_10_.tex"))


## Top 4/ 10 most common in kiteworks

freq_firm <- cost_status_data %>%
  filter(!is.na(firm)) %>%  # Exclude NA values
  count(firm) %>%
  arrange(desc(n))

colnames(freq_firm)[2] <- "count"

top_four_firms_k <- freq_firm %>%
  slice_max(count, n = 4)  # Select the top 4 firms

top_ten_firms_k<- freq_firm %>%
  slice_max(count, n = 20) 





stargazer(top_four_firms_k, summary = F, rownames = F, type = "latex",
          title = "Number of Projects by Firm (CAISO Cost Data Clusters 7 to 14)", 
          digit.separate = 0,
          out = paste0(project_root,
                       "/output/tables/firm_level/table_CAISO_kiteworks_projects_firm_freq_top_4.tex"))

stargazer(top_ten_firms_k, summary = F, rownames = F, type = "latex",
          title = "Number of Projects by Firm (CAISO Cost Data Clusters 7 to 14)", 
          digit.separate = 0,
          out = paste0(project_root,
                       "/output/tables/firm_level/table_CAISO_kiteworks_projects_firm_freq_top_10_.tex"))

print(top_ten_firms_k)
print(top_10_firms)









# ------------------------------
# 1. Create Frequency Tables for Projects by Firm
# ------------------------------

# Frequency table for general projects
freq_firm_gen <- clean_firm_names %>%
  count(firm) %>%
  arrange(desc(n))
colnames(freq_firm_gen)[2] <- "count"

# Top 10 most common firms in general
top_10_firms <- freq_firm_gen %>%
  slice_max(order_by = count, n = 10) 

# Frequency table for projects from kiteworks
freq_firm <- cost_status_data %>%
  filter(!is.na(firm)) %>%  # Exclude NA values
  count(firm) %>%
  arrange(desc(n))
colnames(freq_firm)[2] <- "count"

top_four_firms_k <- freq_firm %>%
  slice_max(count, n = 4)  # Select the top 4 firms

top_ten_firms_k <- freq_firm %>%
  slice_max(count, n = 20) # Select the top 20 firms (or adjust as needed)

# ------------------------------
# 2. Define a Mapping from Raw to Pretty Firm Names
# ------------------------------

pretty_names <- c(
  "firstsolar"      = "First Solar",
  "nexteraenergy"   = "NextEra",
  "recurrentenergy" = "Recurrent",
  "edf-re"          = "EDF Renewable",
  "terra-gen"       = "TerraGen",
  "aes"             = "AES",
  "rwe"             = "RWE",
  "clearwayenergy"  = "Clearway",
  "aypa"            = "AYPA",
  "calpine"         = "Calpine",
  "engie"           = "Engie",
  "revrenewables"   = "REV Renewables", 
  "longroadenergy" = "Long Road",
   "nee" = "NEE",
  "eon" = "EON" ,
  "hecateenergy" = "Hecate",
  "cimgroup" = "CIM Group",
  "sunpower" = "SunPower",
  "LeewardEnergy" = "Leeward",
  "arevonenergy" = "Arevon"
)

# ------------------------------
# 3. Helper Function: Recode Only Existing Keys
# ------------------------------
# This function uses a simple lookup to replace values in x with their pretty version if available.
recode_existing <- function(x, mapping) {
  x_char <- as.character(x)
  mapping_subset <- mapping[names(mapping) %in% unique(x_char)]
  # For each element, return the mapped value if available, else the original value.
  vapply(x_char, function(val) {
    if (val %in% names(mapping_subset)) {
      mapping_subset[[val]]
    } else {
      val
    }
  }, FUN.VALUE = character(1))
}

# ------------------------------
# 4. Update the Frequency Tables with Pretty Names for Display
# ------------------------------

top_10_firms_pretty <- top_10_firms %>%
  mutate(firm = recode_existing(firm, pretty_names))

top_four_firms_k_pretty <- top_four_firms_k %>%
  mutate(firm = recode_existing(firm, pretty_names))

top_ten_firms_k_pretty <- top_ten_firms_k %>%
  mutate(firm = recode_existing(firm, pretty_names))

# ------------------------------
# 5. Output Tables via Stargazer
# ------------------------------

# Table for general projects (Top 10)
stargazer(top_10_firms_pretty, summary = FALSE, rownames = FALSE, type = "latex",
          title = "Number of Projects by Firm (In general)", 
          digit.separate = 0,
          out = paste0(project_root,
                       "/output/tables/firm_level/table_CAISO_general_projects_firm_freq_top_10.tex"))

# Table for kiteworks projects (Top 4)
stargazer(top_four_firms_k_pretty, summary = FALSE, rownames = FALSE, type = "latex",
          title = "Number of Projects by Firm (for projects from kiteworks)", 
          digit.separate = 0,
          out = paste0(project_root,
                       "/output/tables/firm_level/table_CAISO_kiteworks_projects_firm_freq_top_4.tex"))

# Table for kiteworks projects (Top 10 or Top 20)
stargazer(top_ten_firms_k_pretty, summary = FALSE, rownames = FALSE, type = "latex",
          title = "Number of Projects by Firm (for projects from kiteworks)", 
          digit.separate = 0,
          out = paste0(project_root,
                       "/output/tables/firm_level/table_CAISO_kiteworks_projects_firm_freq_top_10.tex"))

# Optionally, print the updated data frames to verify the recoding
print(top_10_firms_pretty)
print(top_ten_firms_k_pretty)



# ------------------------------
# 1. Recode the Kiteworks Data for All Firms
# ------------------------------

freq_firm_pretty <- freq_firm %>%
  mutate(firm = recode_existing(firm, pretty_names))

# ------------------------------
# 2. Combine General Top 10 with Kiteworks Data, Sort, and Reorder Columns
# ------------------------------

combined_table <- top_10_firms_pretty %>%
  left_join(freq_firm_pretty %>% select(firm, kiteworks_count = count),
            by = "firm") %>%
  rename(`Firm` = firm,
         `Cost Data` = count,
         `Clusters 7 to 14` = kiteworks_count) %>%
  # Replace NA with blank for display purposes
  mutate(`Clusters 7 to 14` = ifelse(is.na(`Clusters 7 to 14`), "", `Clusters 7 to 14`)) %>%
  # Sort by the numeric value of "Clusters 7 to 14" (non-numeric values become NA and sorted last)
  arrange(desc(as.numeric(`Clusters 7 to 14`))) %>%
  # Reorder columns: Firm, Cost Data, Clusters 7 to 14
  select(`Firm`, `Cost Data`, `Clusters 7 to 14`)

# ------------------------------
# 3. Output the Combined Table via Stargazer with a Footnote
# ------------------------------

stargazer(combined_table,
          summary = FALSE,
          rownames = FALSE,
          type = "latex",
          title = "Projects by Firm",
          notes = "green indicates firms analyzed in the slides",
          out = paste0(project_root,
                       "/output/tables/firm_level/combined_table1.tex"))





## Four most common firms, firstsolar, nexteraenergy ,recurrentenergy , edf-re in general, but in our sample
## it is nextera, terra-gen, aes, clearwayenergy.

firm1_cost_status_data <- subset(cost_status_data, firm == 'nextera')
firm2_cost_status_data <- subset(cost_status_data, firm == 'terra-gen')
firm3_cost_status_data <- subset(cost_status_data, firm == 'aes')
firm4_cost_status_data <- subset(cost_status_data, firm == 'clearwayenergy')


 
#_________________________--------------------------------------------------------------------------------
#-----------------------------------------------Histograms-------------------
plot_histograms_phase1 <- function(data) {
  # 1) Identify the distinct firms in your data
  #firms <- unique(data$firm)
  firms <- c("nexteraenergy", "terra-gen", "aes", "clearwayenergy", "calpine")
  
  pretty_names <- c(
    "nexteraenergy" = "NextEra",
    "terra-gen"     = "TerraGen",
    "aes"           = "AES",
    "clearwayenergy"= "Clearway",
    "calpine" = "Calpine"
  )
  
  # 2) For each firm, build an overlaid histogram
  for (f in firms) {
    
    # Create a label: "FirmX" vs. "Other_Firms"
    data$FirmLabel <- factor(ifelse(is.na(data$firm) | data$firm != f, "Other Firms", f),
                             levels = c(f, "Other Firms"))  # Explicit ordering
    
    
    
    
    
    # Pick colors: dark for the target firm, lighter for others
    # Ensure the names match the factor values in `FirmLabel`
    fill_colors <- setNames(c("#0072B2", "#D55E00"), c(f, "Other Firms"))
    
    # Build the plot
    # Build histogram with densities
    p <- ggplot(data, aes(x=phase_1_total_cost_per_kW, fill=FirmLabel)) +
      geom_histogram(
        aes(y = ..density..), #aes(y=..count.. / sum(..count..)),
        position="identity", 
        alpha=0.5,
        bins = 40                  # choose number of bins
      ) +
      scale_fill_manual(
        values = fill_colors,
        labels = c(pretty_names[f], "Other Firms")
      ) +
      labs(
        title = paste("Histogram of Phase 1 Total Cost per kW -", pretty_names[f], "vs. Other firms"),
        x = "Phase 1 Total Cost ($/kW)",
        y = "Density"
      ) +
      theme_minimal() +     theme(
        panel.background       = element_blank(),       # no gray panel
        plot.background        = element_rect(fill = "white", color = NA),
        panel.border           = element_blank(),       # no box around panel
        legend.background      = element_blank(),       # no legend fill
        legend.box.background  = element_blank(),       # no box around legend
        axis.line              = element_line(color = "black"),  # keep axis lines
        #panel.grid.major       = element_blank(),       # remove major grid lines
        #panel.grid.minor       = element_blank()        # remove minor grid lines
      )
    
    # Save or print the single plot
    fig_path <- paste0(project_root,"/output/figures/firm_level/")
 
    
    # Save each histogram if desired
    ggsave(
      filename = paste0(fig_path, "hist_phase1_", tolower(f), ".png"),
      plot = p, width = 8, height = 5, dpi =300
    )
    
    # Print to the console/plots pane
    print(p)
  }
}

plot_histograms_phase2 <- function(data) {
  # 1) Identify the distinct firms in your data
  firms <- c("nexteraenergy", "terra-gen", "aes", "clearwayenergy", "calpine")
  
  pretty_names <- c(
    "nexteraenergy" = "NextEra",
    "terra-gen"     = "TerraGen",
    "aes"           = "AES",
    "clearwayenergy" = "Clearway",
    "calpine" = "Calpine"
  )
  
  # 2) For each firm, build an overlaid histogram
  for (f in firms) {
    
    # Create a label: "FirmX" vs. "Other_Firms"
    data$FirmLabel <- factor(ifelse(is.na(data$firm) | data$firm != f, "Other Firms", f),
                             levels = c(f, "Other Firms"))  # Explicit ordering
    
    
    
    
    
    # Pick colors: dark for the target firm, lighter for others
    # Ensure the names match the factor values in `FirmLabel`
    fill_colors <- setNames(c("#0072B2", "#D55E00"), c(f, "Other Firms"))
    
    # Build the plot
    # Build histogram with densities
    p <- ggplot(data, aes(x=phase_2_total_cost_per_kW, fill=FirmLabel)) +
      geom_histogram(
        aes(y = ..density..),#aes(y=..count.. / sum(..count..)),
        position="identity", 
        alpha=0.5,
        bins = 40                  # choose number of bins
      ) +
      scale_fill_manual(
        values = fill_colors,
        labels = c(pretty_names[f], "Other Firms")
      ) +
      labs(
        title = paste("Histogram of Phase 2 Total Cost per kW -", pretty_names[f], "vs. Other Firms"),
        x = "Phase 2 Total Cost ($/kW)",
        y = "Density"
      ) +
      theme_minimal() +     theme(
        panel.background       = element_blank(),       # no gray panel
        plot.background        = element_rect(fill = "white", color = NA),
        panel.border           = element_blank(),       # no box around panel
        legend.background      = element_blank(),       # no legend fill
        legend.box.background  = element_blank(),       # no box around legend
        axis.line              = element_line(color = "black"),  # keep axis lines
        #panel.grid.major       = element_blank(),       # remove major grid lines
        #panel.grid.minor       = element_blank()        # remove minor grid lines
      )
    
    # Save or print the single plot
    fig_path <- paste0(project_root,"/output/figures/firm_level/")
    
    
    # Save each histogram if desired
    ggsave(
      filename = paste0(fig_path, "hist_phase2_", tolower(f), ".png"),
      plot = p, width = 8, height = 5, dpi =300
    )
    
    # Print to the console/plots pane
    print(p)
  }
}
plot_histograms_phase1(cost_status_data)
plot_histograms_phase2(cost_status_data)


 



 



# ### For Phase 2 indicator with fixed effects #############
# 
# lm_f1_ph2_prob_phase_1_costs <- lm(ph2_ind ~ 0 + phase_1_POI_cost_per_kW_bins
#                                 + phase_1_network_cost_per_kW_bins +
#                                   capacity_quartiles + 
#                                   cluster + 
#                                   req_deliverability + 
#                                   fuel_category,
#                                 data = firm1_cost_status_data)
# 
# lm_f1_ph2_prob_phase_1_costs_corrected <- vcovHC(lm_f1_ph2_prob_phase_1_costs, 
#                                               type = "HC1")
# f1_phase_1_costs_robust <- coeftest(lm_f1_ph2_prob_phase_1_costs, 
#                                  vcov = lm_f1_ph2_prob_phase_1_costs_corrected)
# f1_phase_1_costs_robust_se <- f1_phase_1_costs_robust[, 2]
# 
# print(summary(lm_f1_ph2_prob_phase_1_costs))
# 
# 
# 
# # Create an indicator: 1 if firm == "Firm1", 0 otherwise.
# cost_status_data <- cost_status_data %>%
#   mutate(firm_group = ifelse(firm == "Firm1", "Firm1", "Others"))
# 
# 
# 
# 
# ######## For GIA simple###########
# 
# lm_f1_GIA_prob_phase_2_costs_simple <- lm(GIA_ind ~ 0 + firm_group:phase_2_total_POI_cost_per_kW
#                                 + firm_group:phase_2_total_network_cost_per_kW
#                                 +                                  capacity_quartiles + 
#                                   cluster + 
#                                   req_deliverability + 
#                                   fuel_category,
#                                   
#                                   
#                                 data = cost_status_data)
# 
# lm_f1_GIA_prob_phase_2_costs_simple_corrected <- vcovHC(lm_f1_GIA_prob_phase_2_costs_simple, 
#                                               type = "HC1")
# f1_phase_2_costs_simple_robust <- coeftest(lm_f1_GIA_prob_phase_2_costs_simple, 
#                                  vcov = lm_f1_GIA_prob_phase_2_costs_simple_corrected)
# f1_phase_2_costs_simple_robust_se <- f1_phase_2_costs_simple_robust[, 2]
# 
# 
# 
# 
# 
# print(summary(lm_f1_GIA_prob_phase_2_costs_simple))
# 
# 
# 
# 
# ############ For GIA with fixed effects###############
# lm_f1_GIA_prob_phase_2_costs <- lm(GIA_ind ~ 0 + phase_2_POI_cost_per_kW_bins
#                                 + phase_2_network_cost_per_kW_bins +
#                                   capacity_quartiles + 
#                                   cluster + 
#                                   req_deliverability + 
#                                   fuel_category,
#                                 data = firm1_cost_status_data)
# 
# lm_f1_GIA_prob_phase_2_costs_corrected <- vcovHC(lm_f1_GIA_prob_phase_2_costs, 
#                                               type = "HC1")
# f1_phase_2_costs_robust <- coeftest(lm_f1_GIA_prob_phase_2_costs, 
#                                  vcov = lm_f1_GIA_prob_phase_2_costs_corrected)
# f1_phase_2_costs_robust_se <- f1_phase_2_costs_robust[, 2]
# 
# 
# print(summary(lm_f1_GIA_prob_phase_2_costs))
# 
# 
# 
# 
# 
# covariate_labels <- c(
#   "Phase 1 POI Cost per kW (0 - 20)",
#   "Phase 1 POI Cost per kW (20 - 50)",
#   "Phase 1 POI Cost per kW (50 - 100)",
#   "Phase 1 POI Cost per kW (> 100)",
#   
#   "Phase 1 Network Cost per kW (20 - 50)",
#   "Phase 1 Network Cost per kW (50 - 100)",
#   "Phase 1 Network Cost per kW (> 100)",
#   
#   
#   "Phase 2 POI Cost per kW (0 - 20)",
#   "Phase 2 POI Cost per kW (20 - 50)",
#   "Phase 2 POI Cost per kW (50 - 100)",
#   "Phase 2 POI Cost per kW (> 100)",
#   
#   "Phase 2 Network Cost per kW (20 - 50)",
#   "Phase 2 Network Cost per kW (50 - 100)",
#   "Phase 2 Network Cost per kW (> 100)"
# )
# 
# 
# omit_vars <- c(
#   "capacity_quartiles", 
#   "cluster", 
#   "req_deliverability", 
#   "fuel_category" 
# )
# 
 add_lines_custom <- list(
   c("Capacity Quartile", "No", "No", "Yes", "Yes"),
   c("Cluster", "No", "No", "Yes", "Yes"),
   c("Requested Deliverability", "No", "No", "Yes", "Yes"),
   c("Fuel", "No", "No", "Yes", "Yes"),
  c("POI Cost Bins", "No", "No", "Yes", "Yes")
 )
# 
# 
# f1_all_prob_models <- list(lm_f1_ph2_prob_phase_1_costs_simple, lm_f1_ph2_prob_phase_1_costs,
#                         lm_f1_GIA_prob_phase_2_costs_simple, lm_f1_GIA_prob_phase_2_costs)
# 
# 
# f1_robust_se <- c(f1_phase_1_costs_simple_robust_se,f1_phase_1_costs_robust_se, 
#                   f1_phase_2_costs_simple_robust_se, f1_phase_2_costs_robust_se)
# 
# 
# 
# stargazer(f1_all_prob_models, type = "html", style = "qje", 
#           se = list(f1_robust_se, NULL),
#           dep.var.labels = c("Ph2 Study Indicator", "GIA Indicator"),
#           covariate.labels = covariate_labels,
#           title = "Firm 1-LPM for Phase II Study and GIA within 2 Years",
#           omit = omit_vars,  # Omit POI cost bins and control variables
#           # Provided labels for all omitted variables
#           omit.stat = c("f", "ser", "rsq"),
#           add.lines = add_lines_custom,  # Add rows indicating inclusion of control variables
#           notes = "",  # Remove default notes
#           notes.append = FALSE,
#           intercept.bottom = FALSE,
#           out = paste0(project_root,
#                        "/output/tables/firm_level/lin_prob/f1_regression_prob_phase_2_and_GIA_cost_bin_no_intercept.html"))
# 
# 
# 
# stargazer(f1_all_prob_models, type = "latex", style = "qje", 
#           se = list(f1_robust_se, NULL),
#           dep.var.labels = c("Ph2 Study Indicator", "GIA Indicator"),
#           covariate.labels = covariate_labels,
#           title = "Firm 1-LPM for Phase II Study and GIA within 2 Years",
#           omit = omit_vars,  # Omit POI cost bins and control variables
#           # Provided labels for all omitted variables
#           omit.stat = c("f", "ser", "rsq"),
#           add.lines = add_lines_custom,  # Add rows indicating inclusion of control variables
#           notes = "",  # Remove default notes
#           notes.append = FALSE,
#           intercept.bottom = FALSE,
#           out = paste0(project_root,
#                        "/output/tables/firm_level/lin_prob/f1_regression_prob_phase_2_and_GIA_cost_bin_no_intercept.tex"))
# 
#  
# 
# 

#------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------Code to run regressions in level/ linear costs--------------
# --------------------------------------------------------------------------
# ------------------------------
# 1. Load Required Libraries
omit_vars <- c("capacity_quartiles", "cluster", "req_deliverability", "fuel_category")
# ------------------------------
library(dplyr)
library(stargazer)
library(ggplot2)
library(sandwich)

# ------------------------------
# 2. Create Dummy Variables for Each Firm
# firms <- c("nexteraenergy", "terra-gen", "aes", "clearwayenergy")
# ------------------------------
# (This creates four new variables, each taking the target firm's name or "Other_Firms".)
cost_status_data <- cost_status_data %>%
  mutate(
    firm_group1 = ifelse(firm == "nexteraenergy", "nexteraenergy", "Other_Firms"),
    firm_group2 = ifelse(firm == "terra-gen", "terra-gen", "Other_Firms"),
    firm_group3 = ifelse(firm == "aes", "aes", "Other_Firms"),
    firm_group4 = ifelse(firm == "clearwayenergy", "clearwayenergy", "Other_Firms"),
    firm_group5 = ifelse(firm == "calpine", "calpine", "Other_Firms"),
    firm_group = if_else(firm %in% c("nexteraenergy", "terra-gen", "aes", "clearwayenergy", "calpine"),
                         "top_firms",
                         "Other_Firms")
   )
# (It’s important that "Other_Firms" has no spaces.)

# ------------------------------
# 3. Prepare Other Variables in cost_status_data
# we also scaled the kw cost by 1000, so going back to mW
# ------------------------------
cost_status_data <- cost_status_data %>%
  mutate(
    capacity_quartiles = factor(capacity_quartiles),
    cluster = factor(cluster),
    phase_1_total_POI_cost_per_kW = phase_1_total_POI_cost_per_kW/1000,
    phase_2_total_POI_cost_per_kW = phase_2_total_POI_cost_per_kW/1000,
    phase_1_total_network_cost_per_kW = phase_1_total_network_cost_per_kW/1000,
    phase_2_total_network_cost_per_kW = phase_2_total_network_cost_per_kW/1000,
   
    req_deliverability = factor(req_deliverability, 
                                levels = c("Energy Only", "Partial Capacity", "Full Capacity")),
    fuel_category = factor(fuel_category, 
                           levels = c("wind_water", "battery_storage", "solar", "biofuel_natural_gas_other")),
    cluster_group = case_when(
      cluster %in% 7:10 ~ "Old",
      cluster %in% 11:13 ~ "New",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Old", "New"))
  )

# ------------------------------
# 4. List of Firms to Iterate Over
# ------------------------------
#firms <- unique(cost_status_data$firm)
firms <- c("nexteraenergy", "terra-gen", "aes", "clearwayenergy", "calpine", "top_firms")

# ------------------------------
# 5. LaTeX Table Function (mkTexTable)
# ------------------------------
 

mkTexTable2 <- function(models, robust_se, pretty_firm, file) {
  tbl <- capture.output({
    stargazer(models, type = "latex", style = "qje",
              se = robust_se,  # Pass robust SE explicitly
              dep.var.labels = c("Ph2 Indicator", "GIA Indicator"),
              covariate.labels = covariate_labels,  # Use the global covariate_labels (set later)
              title = paste(pretty_firm, "- LPM for Phase II Study and GIA"),  # Use the pretty firm name here
              omit = omit_vars, omit.stat = c("f", "ser", "rsq"),
              add.lines = add_lines_custom,
              float = FALSE)  # Prevent floating tables
  })   
  
  # Fix `>` encoding for Beamer
  tbl <- gsub(">", "$>$", tbl, fixed = TRUE)  # Math mode version of >
  # Apply \scalebox{0.8}{...} around tabular environment for better scaling
  # Apply `\scalebox` AND center the table
  tbl <- gsub("\\begin{tabular}", "\\begin{center} \\scalebox{0.72}{\\begin{tabular}", tbl, fixed = TRUE)
  tbl <- gsub("\\end{tabular}", "\\end{tabular}} \\end{center}", tbl, fixed = TRUE)
  
  # Write to file
  print(paste("Saving LaTeX table to:", file))  # Debugging print statement
  writeLines(tbl, file)
}



# ------------------------------
# 6. Regression Function (with Bar Plots)
# ------------------------------
run_regressions <- function(firm_name, data) {
  # Select the appropriate dummy variable name for this firm.
  # (For Firm1 we use "firm_group1", for Firm2 "firm_group2", etc.)
  
  ## 1. Define a mapping from raw firm names to pretty labels:
  pretty_names <- c(
    "nexteraenergy"  = "NextEra",
    "terra-gen"      = "TerraGen",
    "aes"            = "AES",
    "clearwayenergy" = "Clearway",
    "calpine" = "Calpine",
    "top_firms" = "TopFirms"
  )
  pretty_label <- pretty_names[firm_name]
  
  
  dummy_var <- switch(firm_name,
                      
                      "nexteraenergy" = "firm_group1",
                      "terra-gen" = "firm_group2",
                      "aes" = "firm_group3",
                      "clearwayenergy" = "firm_group4",
                      "calpine" = "firm_group5",
                      "top_firms" = "firm_group")
  
  # No subsetting of the data is done here because we run the regression on the full dataset.
  # We will build the model formulas so that the interaction uses the chosen dummy variable.
  # Construct formulas as strings and then convert to formula objects.
  
  form_ph2_no_fe <- reformulate(
    paste0(
      "0 + ", dummy_var, ":phase_1_total_POI_cost_per_kW", 
      " + ", dummy_var, ":phase_1_total_network_cost_per_kW"
    ),
    response = "ph2_ind"
  )
  form_ph2_fe <- reformulate(
    c(
      paste0("0 + ", dummy_var, ":phase_1_total_POI_cost_per_kW"),
      paste0(dummy_var, ":phase_1_total_network_cost_per_kW"),
      "capacity_quartiles + cluster + req_deliverability + fuel_category"
    ),
    response = "ph2_ind"
  )
  
  form_gia_no_fe <- reformulate(
    paste0(
      "0 + ", dummy_var, ":phase_2_total_POI_cost_per_kW",
      " + ", dummy_var, ":phase_2_total_network_cost_per_kW"
    ),
    response = "GIA_ind"
  )
  form_gia_fe <- reformulate(
    c(
      paste0("0 + ", dummy_var, ":phase_2_total_POI_cost_per_kW"),
      paste0(dummy_var, ":phase_2_total_network_cost_per_kW"),
      "capacity_quartiles + cluster + req_deliverability + fuel_category"
    ),
    response = "GIA_ind"
  )
  
  # 3) Fit models
  ph2_no_fe <- lm(form_ph2_no_fe, data = data)
  ph2_fe    <- lm(form_ph2_fe,    data = data)
  gia_no_fe <- lm(form_gia_no_fe, data = data)
  gia_fe    <- lm(form_gia_fe,    data = data)
  models    <- list(ph2_no_fe, ph2_fe, gia_no_fe, gia_fe)
  
  # 4) Pull robust SE
  robust_se <- lapply(models, function(m) sqrt(diag(vcovHC(m, type = "HC1", dof = Inf))))
  
  # Compute robust standard errors (HC1)
  robust_se <- lapply(models, function(model) 
    sqrt(diag(vcovHC(model, type = "HC1", dof = Inf)))
  )
  
  # --- Define Coefficient Labels for stargazer ---
  # (We expect 8 cost coefficients in total: 2 for Phase 1, 2 for Phase 2,
  #  each split into the target firm and Other Firms.)
  ## 6. Define the covariate labels using the pretty firm name:
  covariate_labels <<- c(
    paste0(pretty_label, " X Phase 1 POI cost"),
    "Other Firms X Phase 1 POI cost",
    paste0(pretty_label, " X Phase 1 Network cost"),
    "Other Firms X Phase 1 Network cost",
    paste0(pretty_label, " X Phase 2 POI cost"),
    "Other Firms X Phase 2 POI cost",
    paste0(pretty_label, " X Phase 2 Network cost"),
    "Other Firms X Phase 2 Network cost"
  )
  omit_vars <- c("capacity_quartiles", "cluster", "req_deliverability", "fuel_category")
  add_lines_custom <- list(
    c("Capacity Quartile", "No", "Yes", "No", "Yes"),
    c("Cluster", "No", "Yes", "No", "Yes"),
    c("Requested Deliverability", "No", "Yes", "No", "Yes"),
    c("Fuel", "No", "Yes", "No", "Yes")
  )
  
  # --- Output File Names (adjust project_root as needed) ---
  output_path <- paste0(project_root, "/output/tables/firm_level/lin_prob/")
  output_file_html <- paste0(output_path, tolower(firm_name), "_regression_prob_ph2_gia_continuous_no_intercept.html")
  output_file_tex  <- paste0(output_path, tolower(firm_name), "_regression_prob_ph2_gia_continuous_no_intercept.tex")
  
  # Debug: print robust SEs
  print(paste("Standard Errors for", firm_name, "(Before Stargazer):"))
  print(formatC(unlist(robust_se), digits = 3, format = "f"))
  
  ## 9. Generate the HTML stargazer output, using the pretty label in the title:
  stargazer(models, type = "html", style = "qje", digits = 3,
            se = lapply(models, function(model) sqrt(diag(vcovHC(model, type = "HC1", dof = Inf)))),
            dep.var.labels = c("Ph2 Indicator", "GIA Indicator"),
            covariate.labels = covariate_labels,
            title = paste(pretty_label, "- LPM for Phase II Study and GIA"),
            omit = omit_vars, omit.stat = c("f", "ser", "rsq"),
            add.lines = add_lines_custom,
            notes.append = FALSE, intercept.bottom = FALSE,
            out = output_file_html)
  
  tex_file <- paste0(project_root, "/output/tables/firm_level/lin_prob/", 
                     tolower(firm_name), "_regression_prob_ph2_gia_continuous_no_intercept.tex")
  
  ## 10. Generate the LaTeX table output, passing the pretty label:
  mkTexTable2(models, robust_se, pretty_label, tex_file)
  

  
  # Generate the LaTeX table with \resizebox (80% width & height)
  mkTexTable2(models, robust_se, firm_name, tex_file)
  
  # Return the FE models (ph2_fe & gia_fe) + robust SE if we want to plot later
  return(list(
    ph2_fe   = ph2_fe,
    gia_fe   = gia_fe,
    robust_se= robust_se
  ))
  
  # Define your list of firms (raw names) as before:
  firms <- c("nexteraenergy", "terra-gen", "aes", "clearwayenergy", "calpine")
  
}
  results <- lapply(firms, run_regressions, data = cost_status_data)
  
  # --- Generate LaTeX Table ---
  #mkTexTable(models, robust_se, firm_name, output_file_tex)
  
  # ------------------------------
  # 7. Generate Bar Plots (using fixed effects models: ph2_fe and gia_fe)
  # ------------------------------
  # ---------------------------
  # 5) Extract Coeffs & SE (FE models only)
  #    ph2_fe => Phase 1 => ph2_ind
  #    gia_fe => Phase 2 => GIA_ind
  # ---------------------------
  
  
#   c_ph2_fe <- coef(ph2_fe)
#   se_ph2_fe <- sqrt(diag(vcovHC(ph2_fe, type="HC1", dof=Inf)))
#   
#   c_gia_fe <- coef(gia_fe)
#   se_gia_fe <- sqrt(diag(vcovHC(gia_fe, type="HC1", dof=Inf)))
#   
#   get_c <- function(x, n) x[n]
#   
#   # Phase 1
#   nm_ph1_poi_t   <- paste0(dummy_var, firm_name, ":phase_1_total_POI_cost_per_kW")
#   nm_ph1_poi_o   <- paste0(dummy_var, "Other_Firms:phase_1_total_POI_cost_per_kW")
#   nm_ph1_net_t   <- paste0(dummy_var, firm_name, ":phase_1_total_network_cost_per_kW")
#   nm_ph1_net_o   <- paste0(dummy_var, "Other_Firms:phase_1_total_network_cost_per_kW")
#   
#   ph1_poi_t   <- get_c(c_ph2_fe, nm_ph1_poi_t)
#   ph1_poi_tSE <- get_c(se_ph2_fe, nm_ph1_poi_t)
#   ph1_poi_o   <- get_c(c_ph2_fe, nm_ph1_poi_o)
#   ph1_poi_oSE <- get_c(se_ph2_fe, nm_ph1_poi_o)
#   
#   ph1_net_t   <- get_c(c_ph2_fe, nm_ph1_net_t)
#   ph1_net_tSE <- get_c(se_ph2_fe, nm_ph1_net_t)
#   ph1_net_o   <- get_c(c_ph2_fe, nm_ph1_net_o)
#   ph1_net_oSE <- get_c(se_ph2_fe, nm_ph1_net_o)
#   
#   # Phase 2
#   nm_ph2_poi_t   <- paste0(dummy_var, firm_name, ":phase_2_total_POI_cost_per_kW")
#   nm_ph2_poi_o   <- paste0(dummy_var, "Other_Firms:phase_2_total_POI_cost_per_kW")
#   nm_ph2_net_t   <- paste0(dummy_var, firm_name, ":phase_2_total_network_cost_per_kW")
#   nm_ph2_net_o   <- paste0(dummy_var, "Other_Firms:phase_2_total_network_cost_per_kW")
#   
#   ph2_poi_t   <- get_c(c_gia_fe, nm_ph2_poi_t)
#   ph2_poi_tSE <- get_c(se_gia_fe, nm_ph2_poi_t)
#   ph2_poi_o   <- get_c(c_gia_fe, nm_ph2_poi_o)
#   ph2_poi_oSE <- get_c(se_gia_fe, nm_ph2_poi_o)
#   
#   ph2_net_t   <- get_c(c_gia_fe, nm_ph2_net_t)
#   ph2_net_tSE <- get_c(se_gia_fe, nm_ph2_net_t)
#   ph2_net_o   <- get_c(c_gia_fe, nm_ph2_net_o)
#   ph2_net_oSE <- get_c(se_gia_fe, nm_ph2_net_o)
#   
#   # ---------------------------
#   # 6) Single bar chart for $100 only
#   #    4 cost categories x 2 bars
#   # ---------------------------
#   inc <- 10
#   
#   df_100 <- data.frame(
#     CostType = rep(
#       c("Phase 1 POI", "Phase 1 Network", 
#         "Phase 2 POI", "Phase 2 Network"),
#       each=2
#     ),
#     Group = c(
#       firm_name, "Other_Firms",
#       firm_name, "Other_Firms",
#       firm_name, "Other_Firms",
#       firm_name, "Other_Firms"
#     ),
#     Effect = c(
#       ph1_poi_t*inc,   ph1_poi_o*inc,
#       ph1_net_t*inc,   ph1_net_o*inc,
#       ph2_poi_t*inc,   ph2_poi_o*inc,
#       ph2_net_t*inc,   ph2_net_o*inc
#     ),
#     Error = c(
#       ph1_poi_tSE*inc, ph1_poi_oSE*inc,
#       ph1_net_tSE*inc, ph1_net_oSE*inc,
#       ph2_poi_tSE*inc, ph2_poi_oSE*inc,
#       ph2_net_tSE*inc, ph2_net_oSE*inc
#     )
#   )
#   df_100$ymin <- df_100$Effect - df_100$Error
#   df_100$ymax <- df_100$Effect + df_100$Error
#   
#   df_100$CostType <- factor(df_100$CostType,
#                             levels = c("Phase 1 POI","Phase 1 Network","Phase 2 POI","Phase 2 Network")
#   )
#   
#   group_colors <- setNames(c("skyblue","salmon"), c(firm_name,"Other_Firms"))
#   
#   single_plot_100 <- ggplot(df_100, aes(x=CostType, y=Effect, fill=Group)) +
#     geom_bar(stat="identity", position=position_dodge(width=0.8)) +
#     geom_errorbar(aes(ymin=ymin, ymax=ymax),
#                   width=0.25, position=position_dodge(width=0.8)) +
#     labs(
#       title = paste("Effect of $10/mW Increase in POI and Network Cost on receiving Phase 2 and GIA -", firm_name),
#       x = NULL,
#       y = "Change in Probability"
#     ) +  theme(plot.title = element_text(size=5))+
#     scale_fill_manual(values=group_colors) +
#     theme_minimal() + theme(legend.position = "right",
#                             panel.background = element_rect(fill = "white"),
#                             plot.background = element_rect(fill = "white"))
#   
#   # Save or print the single plot
#   fig_path <- paste0(project_root,"/output/figures/firm_level/")
#   ggsave(paste0(fig_path, tolower(firm_name), "prob_ph2_gia_100_increments_all_costs.png"),
#          plot=single_plot_100, width=10, height=5, dpi =300)
#   
#   print(single_plot_100)
#   
#   # Return stuff if needed
#   return(list(
#     models       = models,
#     robust_se    = robust_se,
#     single_plot  = single_plot_100,
#     df_100       = df_100
#   ))
# }
# ------------------------------
# 7. Run the Regression Function for Each Firm
# ------------------------------




plot_cost_impacts <- function(firm_name, ph2_fe, gia_fe) {
  # 1) We need the dummy var to find the right coefficients
  
  pretty_names <- c(
    "nexteraenergy"  = "NextEra",
    "terra-gen"      = "TerraGen",
    "aes"            = "AES",
    "clearwayenergy" = "Clearway",
    "calpine" = "Calpine",
    "top_firms" = "TopFirms"
  )
  pretty_label <- pretty_names[firm_name]
  
  
  dummy_var <- switch(
    firm_name,
    "nexteraenergy" = "firm_group1",
    "terra-gen" = "firm_group2",
    "aes" = "firm_group3",
    "clearwayenergy" = "firm_group4",
    "calpine" = "firm_group5",
    "top_firms" = "firm_group"
  )
  
  
  
  # 2) Extract FE coefficients
  c_ph2_fe <- coef(ph2_fe)
  se_ph2_fe<- sqrt(diag(vcovHC(ph2_fe, type="HC1", dof=Inf)))
  
  c_gia_fe <- coef(gia_fe)
  se_gia_fe<- sqrt(diag(vcovHC(gia_fe, type="HC1", dof=Inf)))
  
  # Helper
  get_c <- function(x, n) x[n]
  
  # Phase 1 => ph2_ind
  nm_ph1_poi_t  <- paste0(dummy_var, firm_name, ":phase_1_total_POI_cost_per_kW")
  nm_ph1_poi_o  <- paste0(dummy_var, "Other_Firms:phase_1_total_POI_cost_per_kW")
  nm_ph1_net_t  <- paste0(dummy_var, firm_name, ":phase_1_total_network_cost_per_kW")
  nm_ph1_net_o  <- paste0(dummy_var, "Other_Firms:phase_1_total_network_cost_per_kW")
  
  ph1_poi_t   <- get_c(c_ph2_fe, nm_ph1_poi_t)
  ph1_poi_tSE <- get_c(se_ph2_fe, nm_ph1_poi_t)
  ph1_poi_o   <- get_c(c_ph2_fe, nm_ph1_poi_o)
  ph1_poi_oSE <- get_c(se_ph2_fe, nm_ph1_poi_o)
  
  ph1_net_t   <- get_c(c_ph2_fe, nm_ph1_net_t)
  ph1_net_tSE <- get_c(se_ph2_fe, nm_ph1_net_t)
  ph1_net_o   <- get_c(c_ph2_fe, nm_ph1_net_o)
  ph1_net_oSE <- get_c(se_ph2_fe, nm_ph1_net_o)
  
  # Phase 2 => GIA_ind
  nm_ph2_poi_t <- paste0(dummy_var, firm_name, ":phase_2_total_POI_cost_per_kW")
  nm_ph2_poi_o <- paste0(dummy_var, "Other_Firms:phase_2_total_POI_cost_per_kW")
  nm_ph2_net_t <- paste0(dummy_var, firm_name, ":phase_2_total_network_cost_per_kW")
  nm_ph2_net_o <- paste0(dummy_var, "Other_Firms:phase_2_total_network_cost_per_kW")
  
  ph2_poi_t   <- get_c(c_gia_fe, nm_ph2_poi_t)
  ph2_poi_tSE <- get_c(se_gia_fe, nm_ph2_poi_t)
  ph2_poi_o   <- get_c(c_gia_fe, nm_ph2_poi_o)
  ph2_poi_oSE <- get_c(se_gia_fe, nm_ph2_poi_o)
  
  ph2_net_t   <- get_c(c_gia_fe, nm_ph2_net_t)
  ph2_net_tSE <- get_c(se_gia_fe, nm_ph2_net_t)
  ph2_net_o   <- get_c(c_gia_fe, nm_ph2_net_o)
  ph2_net_oSE <- get_c(se_gia_fe, nm_ph2_net_o)
  
  # 3) Build data frame for, say, $10 increments
  inc <- 10
  df_10 <- data.frame(
    CostType = rep(c("Phase 1 POI","Phase 1 Network","Phase 2 POI","Phase 2 Network"), each=2),
    Group    = rep(c(firm_name,"Other_Firms"), times=4),
    Effect   = c(
      ph1_poi_t*inc, ph1_poi_o*inc,
      ph1_net_t*inc, ph1_net_o*inc,
      ph2_poi_t*inc, ph2_poi_o*inc,
      ph2_net_t*inc, ph2_net_o*inc
    ),
    Error = c(
      ph1_poi_tSE*inc, ph1_poi_oSE*inc,
      ph1_net_tSE*inc, ph1_net_oSE*inc,
      ph2_poi_tSE*inc, ph2_poi_oSE*inc,
      ph2_net_tSE*inc, ph2_net_oSE*inc
    )
  )
  df_10$ymin <- df_10$Effect - df_10$Error
  df_10$ymax <- df_10$Effect + df_10$Error
  df_10$Group <- factor(df_10$Group, 
                        levels = c(firm_name, "Other_Firms"),
                        labels = c(pretty_label, "Other Firms"))
  
  df_10$CostType <- factor(df_10$CostType,
                           levels = c("Phase 1 POI","Phase 1 Network","Phase 2 POI","Phase 2 Network")
  )
  
  group_colors <- setNames(c("skyblue", "salmon"), c(pretty_label, "Other Firms"))
  
 
  single_plot_100 <- ggplot(df_10, aes(x=CostType, y=Effect, fill=Group)) +
    geom_bar(stat="identity", position=position_dodge(width=0.8)) +
    geom_errorbar(aes(ymin=ymin, ymax=ymax),
                  width=0.25, position=position_dodge(width=0.8)) +
    labs(
      title = paste("Effect of $10/mW Increase in POI and Network Cost on receiving Phase 2 and GIA -", pretty_label),
      x = NULL,
      y = "Change in Probability"
    ) +  theme(plot.title = element_text(size=5))+
    scale_fill_manual(values=group_colors) +
    theme_minimal() + theme(
      panel.background       = element_blank(),       # no gray panel
      plot.background        = element_rect(fill = "white", color = NA),
      panel.border           = element_blank(),       # no box around panel
      legend.background      = element_blank(),       # no legend fill
      legend.box.background  = element_blank(),       # no box around legend
      axis.line              = element_line(color = "black"),  # keep axis lines
      #panel.grid.major       = element_blank(),       # remove major grid lines
      #panel.grid.minor       = element_blank()        # remove minor grid lines
    )
  
  # Save or print the single plot
  fig_path <- paste0(project_root,"/output/figures/firm_level/")
  ggsave(paste0(fig_path, tolower(firm_name), "prob_ph2_gia_100_increments_all_costs.png"),
         plot=single_plot_100, width=10, height=5, dpi =300)
  
  print(single_plot_100)
  
  return(single_plot_100)
}
  # 1) For each firm, run the regression + produce tables
  results_list <- lapply(firms, run_regressions, data=cost_status_data)
  # 'results_list' is a list (one entry per firm), each entry has
  #   $ph2_fe, $gia_fe, $robust_se
  
  # 2) Then, for each firm, call plot_cost_impacts()
  for(i in seq_along(firms)) {
    f <- firms[i]          # e.g. "Firm1"
    out <- results_list[[i]]
    
    plot_cost_impacts(
      firm_name = f,
      ph2_fe    = out$ph2_fe,
      gia_fe    = out$gia_fe
    )
  }
  

#--------------------------------------------------------------------------------------------------------------


output_path <- paste0(project_root, "/output/tables/firm_level/lin_prob/")
########################################
### FIRM 1: Regressions
########################################

# -- Ph2 Indicator, No FE --
ph2_no_fe_f1 <- lm(
  ph2_ind ~ 0 
  + firm_group1:phase_1_total_POI_cost_per_kW 
  + firm_group1:phase_1_total_network_cost_per_kW,
  data = cost_status_data
)

# -- Ph2 Indicator, With FE --
ph2_fe_f1 <- lm(
  ph2_ind ~ 0 
  + firm_group1:phase_1_total_POI_cost_per_kW 
  + firm_group1:phase_1_total_network_cost_per_kW
  + capacity_quartiles + cluster + req_deliverability + fuel_category,
  data = cost_status_data
)

# -- GIA Indicator, No FE --
gia_no_fe_f1 <- lm(
  GIA_ind ~ 0
  + firm_group1:phase_2_total_POI_cost_per_kW
  + firm_group1:phase_2_total_network_cost_per_kW,
  data = cost_status_data
)

# -- GIA Indicator, With FE --
gia_fe_f1 <- lm(
  GIA_ind ~ 0
  + firm_group1:phase_2_total_POI_cost_per_kW
  + firm_group1:phase_2_total_network_cost_per_kW
  + capacity_quartiles + cluster + req_deliverability + fuel_category,
  data = cost_status_data
)

########################################
### FIRM 1: Stargazer Output
########################################

model_list_f1 <- list(ph2_no_fe_f1, ph2_fe_f1, gia_no_fe_f1, gia_fe_f1)
robust_se_f1  <- list(
  sqrt(diag(vcovHC(ph2_no_fe_f1, type="HC1"))),
  sqrt(diag(vcovHC(ph2_fe_f1,    type="HC1"))),
  sqrt(diag(vcovHC(gia_no_fe_f1, type="HC1"))),
  sqrt(diag(vcovHC(gia_fe_f1,    type="HC1")))
)

cov_labels_f1 <- c(
  "Firm1 X Phase 1 POI cost",
  "Other Firms X Phase 1 POI cost",
  "Firm1 X Phase 1 Network cost",
  "Other Firms X Phase 1 Network cost",
  "Firm1 X Phase 2 POI cost",
  "Other Firms X Phase 2 POI cost",
  "Firm1 X Phase 2 Network cost",
  "Other Firms X Phase 2 Network cost"
)

omit_vars <- c("capacity_quartiles", "cluster", "req_deliverability", "fuel_category")
add_lines_custom <- list(
  c("Capacity Quartile",          "No", "No", "Yes", "Yes"),
  c("Cluster",                    "No", "No", "Yes", "Yes"),
  c("Requested Deliverability",   "No", "No", "Yes", "Yes"),
  c("Fuel",                       "No", "No", "Yes", "Yes")
)

# Example: HTML output
stargazer(
  model_list_f1,
  type            = "html",
  style           = "qje",
  se              = robust_se_f1,
  dep.var.labels  = c("Ph2 Indicator", "GIA Indicator"),
  covariate.labels = cov_labels_f1,
  omit            = omit_vars,
  omit.stat       = c("f","ser","rsq"),
  add.lines       = add_lines_custom,
  digits          = 3,
  intercept.bottom= FALSE,
  out             = paste0(project_root,"/output/tables/firm_level/lin_prob/firm1_regression_prob_ph2_gia_continuous_no_intercept.html")
)

# Example: LaTeX output
stargazer(
  model_list_f1,
  type            = "latex",
  style           = "qje",
  se              = robust_se_f1,
  dep.var.labels  = c("Ph2 Indicator", "GIA Indicator"),
  covariate.labels = cov_labels_f1,
  omit            = omit_vars,
  omit.stat       = c("f","ser","rsq"),
  add.lines       = add_lines_custom,
  digits          = 3,
  intercept.bottom= FALSE,
  out             = paste0(project_root,"/output/tables/firm_level/lin_prob/firm1_regression_prob_ph2_gia_continuous_no_intercept.tex")
)

########################################
### FIRM 1: Bar Plots (Focus on FE models)
########################################

coef_ph2_fe_f1 <- coef(ph2_fe_f1)  # For ph2_ind (using Phase 1 costs)
coef_gia_fe_f1 <- coef(gia_fe_f1)  # For GIA_ind (using Phase 2 costs)

# Extract the relevant coefficients for ph2_fe_f1 (Ph2 outcome)
Ph2_poi_f1_target <- coef_ph2_fe_f1["firm_group1Firm1:phase_1_total_POI_cost_per_kW"]
Ph2_poi_f1_others <- coef_ph2_fe_f1["firm_group1Other_Firms:phase_1_total_POI_cost_per_kW"]
Ph2_net_f1_target <- coef_ph2_fe_f1["firm_group1Firm1:phase_1_total_network_cost_per_kW"]
Ph2_net_f1_others <- coef_ph2_fe_f1["firm_group1Other_Firms:phase_1_total_network_cost_per_kW"]

print(Ph2_poi_f1_target)
summary(ph2_fe_f1)

# Extract the relevant coefficients for gia_fe_f1 (GIA outcome)
GIA_poi_f1_target <- coef_gia_fe_f1["firm_group1Firm1:phase_2_total_POI_cost_per_kW"]
GIA_poi_f1_others <- coef_gia_fe_f1["firm_group1Other_Firms:phase_2_total_POI_cost_per_kW"]
GIA_net_f1_target <- coef_gia_fe_f1["firm_group1Firm1:phase_2_total_network_cost_per_kW"]
GIA_net_f1_others <- coef_gia_fe_f1["firm_group1Other_Firms:phase_2_total_network_cost_per_kW"]

# Define cost increments
increases <- c(10, 50, 100)

########################################
# 1) Data frames for Ph2 Indicator
########################################

Ph2_poi_data_f1 <- data.frame(
  Increase = rep(increases, each=2),
  Group    = rep(c("Firm1", "Other_Firms"), times=3),
  Effect   = c(Ph2_poi_f1_target * increases, Ph2_poi_f1_others * increases)
)

Ph2_poi_data_f1 <- data.frame(
  Increase = rep(c(10, 50, 100), each = 2),         # (10,10,50,50,100,100)
  Group    = rep(c("Firm1", "Other_Firms"), times = 3),
  Effect   = c(
    Ph2_poi_f1_target * 10,   Ph2_poi_f1_others * 10,    # rows 1,2
    Ph2_poi_f1_target * 50,   Ph2_poi_f1_others * 50,    # rows 3,4
    Ph2_poi_f1_target * 100,  Ph2_poi_f1_others * 100    # rows 5,6
  )
)

Ph2_net_data_f1 <- data.frame(
  Increase = rep(increases, each=2),
  Group    = rep(c("Firm1", "Other_Firms"), times=3),
  Effect   = c(Ph2_net_f1_target * increases, Ph2_net_f1_others * increases)
)

########################################
# 2) Data frames for GIA Indicator
########################################

GIA_poi_data_f1 <- data.frame(
  Increase = rep(increases, each=2),
  Group    = rep(c("Firm1", "Other_Firms"), times=3),
  Effect   = c(GIA_poi_f1_target * increases, GIA_poi_f1_others * increases)
)
GIA_net_data_f1 <- data.frame(
  Increase = rep(increases, each=2),
  Group    = rep(c("Firm1", "Other_Firms"), times=3),
  Effect   = c(GIA_net_f1_target * increases, GIA_net_f1_others * increases)
)

########################################
# 3) Define color mapping
########################################
group_colors_f1 <- c("Firm1"="skyblue", "Other_Firms"="salmon")

########################################
# 4) Create the plots
########################################

Ph2_poi_plot_f1 <- ggplot(Ph2_poi_data_f1, aes(x=factor(Increase), y=Effect, fill=Group)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8)) +
  labs(
    title = "POI Cost Effect on Ph2 Indicator (Firm1)",
    x = "Cost Increase ($/kW)",
    y = "Change in Probability"
  ) +
  scale_fill_manual(values=group_colors_f1) +
  theme_minimal()

Ph2_net_plot_f1 <- ggplot(Ph2_net_data_f1, aes(x=factor(Increase), y=Effect, fill=Group)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8)) +
  labs(
    title = "Network Cost Effect on Ph2 Indicator (Firm1)",
    x = "Cost Increase ($/kW)",
    y = "Change in Probability"
  ) +
  scale_fill_manual(values=group_colors_f1) +
  theme_minimal()

GIA_poi_plot_f1 <- ggplot(GIA_poi_data_f1, aes(x=factor(Increase), y=Effect, fill=Group)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8)) +
  labs(
    title = "POI Cost Effect on GIA Indicator (Firm1)",
    x = "Cost Increase ($/kW)",
    y = "Change in Probability"
  ) +
  scale_fill_manual(values=group_colors_f1) +
  theme_minimal()

GIA_net_plot_f1 <- ggplot(GIA_net_data_f1, aes(x=factor(Increase), y=Effect, fill=Group)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8)) +
  labs(
    title = "Network Cost Effect on GIA Indicator (Firm1)",
    x = "Cost Increase ($/kW)",
    y = "Change in Probability"
  ) +
  scale_fill_manual(values=group_colors_f1) +
  theme_minimal()

########################################
# 5) Print (or save) the plots
########################################

print(Ph2_poi_plot_f1)
print(Ph2_net_plot_f1)
print(GIA_poi_plot_f1)
print(GIA_net_plot_f1)

# Example of saving each to file:
# ggsave("firm1_Ph2_poi_plot.png", Ph2_poi_plot_f1, width=8, height=5)
# ggsave("firm1_Ph2_net_plot.png", Ph2_net_plot_f1, width=8, height=5)
# ggsave("firm1_GIA_poi_plot.png", GIA_poi_plot_f1, width=8, height=5)
# ggsave("firm1_GIA_net_plot.png", GIA_net_plot_f1, width=8, height=5)


# If you want to save to disk:
# ggsave("Firm1_ph1_poi.png", ph1_poi_plot_f1, width=8, height=5)
# ... etc.




summary(ph2_fe_f1)
names(coef(gia_fe_f1))

coef_val <- coef(ph2_fe_f1)["firm_group1Firm1:phase_1_total_POI_cost_per_kW"]
# For example: 0.0003

coef_val * 10
coef_val * 50
coef_val * 100

 

#------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------MOdels with cost bins-------------------------------------------------------------------

 
# Define a function to create LaTeX tables with \resizebox (80% width & height)
# mkTexTable <- function(models, robust_se, file) {
#   # Capture the stargazer output
#   tbl <- capture.output({
#     stargazer(models, type = "latex", style = "qje",
#               se = robust_se,  # Pass robust SE explicitly
#               dep.var.labels = c("Ph2 Indicator", "GIA Indicator"),
#               covariate.labels = covariate_labels,
#               title = "Firm-Level LPM for Phase II Study and GIA",
#               omit = omit_vars, omit.stat = c("f", "ser", "rsq"),
#               add.lines = add_lines_custom,
#               float = FALSE)  # Prevent floating tables
#   })    
#   
#   # Apply \resizebox{0.8\textwidth}{0.8\textheight} around tabular environment
#   tbl <- gsub("\\begin{tabular}", "\\resizebox{0.7\\textwidth}{0.6\\textheight}{\\begin{tabular}", tbl, fixed = TRUE)
#   tbl <- gsub("\\end{tabular}", "\\end{tabular}}", tbl, fixed = TRUE)
#   
#   # Write to file
#   print(paste("Saving LaTeX table to:", file))  # Debugging print statement
#   writeLines(tbl, file)
# }


mkTexTable <- function(models, robust_se, firm_name, file) {
  # Capture the stargazer output
  tbl <- capture.output({
    stargazer(models, type = "latex", style = "qje",
              se = robust_se,  # Pass robust SE explicitly
              dep.var.labels = c("Ph2 Indicator", "GIA Indicator"),
              covariate.labels = covariate_labels,  # Ensuring Covariate Labels Are Included
              title = paste(firm_name, "- LPM for Phase II Study and GIA"),  # Dynamic Title
              omit = omit_vars, omit.stat = c("f", "ser", "rsq"),
              add.lines = add_lines_custom,
              float = FALSE)  # Prevent floating tables
  })    
  
  # Fix `>` encoding for Beamer
  tbl <- gsub(">", "$>$", tbl, fixed = TRUE)  # Math mode version of >
  # Apply \scalebox{0.8}{...} around tabular environment for better scaling
  # Apply `\scalebox` AND center the table
  tbl <- gsub("\\begin{tabular}", "\\begin{center} \\scalebox{0.58}{\\begin{tabular}", tbl, fixed = TRUE)
  tbl <- gsub("\\end{tabular}", "\\end{tabular}} \\end{center}", tbl, fixed = TRUE)
  
  # Write to file
  print(paste("Saving LaTeX table to:", file))  # Debugging print statement
  writeLines(tbl, file)
}

# Prepare cost_status_data
cost_status_data <- cost_status_data %>%
  mutate(
    phase_1_POI_cost_per_kW_bins = factor(phase_1_POI_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
    phase_1_network_cost_per_kW_bins = factor(phase_1_network_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
    phase_2_POI_cost_per_kW_bins = factor(phase_2_POI_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
    phase_2_network_cost_per_kW_bins = factor(phase_2_network_cost_per_kW_bins, levels = c(1, 2, 3, 4)),
    capacity_quartiles = factor(capacity_quartiles),
    cluster = factor(cluster),
    req_deliverability = factor(req_deliverability, levels = c("Energy Only", "Partial Capacity", "Full Capacity")),
    fuel_category = factor(fuel_category, levels = c("wind_water", "battery_storage", "solar", "biofuel_natural_gas_other")),
    cluster_group = case_when(
      cluster %in% 7:10 ~ "Old",
      cluster %in% 11:13 ~ "New",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Old", "New"))
  )

# List of firms to iterate over
firms <- unique(cost_status_data$firm)

# Define regression function
run_regressions <- function(firm_name, data) {
  firm_data <- subset(data, firm == firm_name)
  
  # Run regressions
  models <- list(
    lm(ph2_ind ~ 0 + phase_1_POI_cost_per_kW_bins + phase_1_network_cost_per_kW_bins, data = firm_data),
    lm(ph2_ind ~ 0 + phase_1_POI_cost_per_kW_bins + phase_1_network_cost_per_kW_bins + 
         capacity_quartiles + cluster + req_deliverability + fuel_category, data = firm_data),
    lm(GIA_ind ~ 0 + phase_2_POI_cost_per_kW_bins + phase_2_network_cost_per_kW_bins, data = firm_data),
    lm(GIA_ind ~ 0 + phase_2_POI_cost_per_kW_bins + phase_2_network_cost_per_kW_bins + 
         capacity_quartiles + cluster + req_deliverability + fuel_category, data = firm_data)
  )
  
  # Compute robust standard errors
  #robust_se <- lapply(models, function(model) coeftest(model, vcovHC(model, type = "HC1"))[, 2])
 robust_se <- lapply(models, function(model) sqrt(diag(vcovHC(model, type = "HC1", dof = Inf))))
 
 
  
  
  # Define labels
  covariate_labels <- c(
    "Phase 1 POI Cost per kW (0 - 20)", "Phase 1 POI Cost per kW (20 - 50)", "Phase 1 POI Cost per kW (50 - 100)", "Phase 1 POI Cost per kW (> 100)",
    "Phase 1 Network Cost per kW (20 - 50)", "Phase 1 Network Cost per kW (50 - 100)", "Phase 1 Network Cost per kW (> 100)",
    "Phase 2 POI Cost per kW (0 - 20)", "Phase 2 POI Cost per kW (20 - 50)", "Phase 2 POI Cost per kW (50 - 100)", "Phase 2 POI Cost per kW (> 100)",
    "Phase 2 Network Cost per kW (20 - 50)", "Phase 2 Network Cost per kW (50 - 100)", "Phase 2 Network Cost per kW (> 100)"
  )
  
  omit_vars <- c("capacity_quartiles", "cluster", "req_deliverability", "fuel_category")
  
  add_lines_custom <- list(
    c("Capacity Quartile", "No", "No", "Yes", "Yes"),
    c("Cluster", "No", "No", "Yes", "Yes"),
    c("Requested Deliverability", "No", "No", "Yes", "Yes"),
    c("Fuel", "No", "No", "Yes", "Yes")
  )
  
  # Generate output
  output_path <- paste0(project_root, "/output/tables/firm_level/lin_prob/")
  output_file_html <- paste0(output_path, tolower(firm_name), "_regression_prob_phase_2_and_GIA_cost_bin_no_intercept.html")
  output_file_tex <- paste0(output_path, tolower(firm_name), "_regression_prob_phase_2_and_GIA_cost_bin_no_intercept.tex")
  
  
 
  
  # Print SEs for debugging
  print(paste("Standard Errors for", firm_name, "(Before Stargazer)"))
  print(formatC(unlist(robust_se), digits = 3, format = "f"))
  
  # Stargazer output with correct SE structure
  stargazer(models, type = "html", style = "qje", digits = 3,
            se = list(unlist(robust_se), NULL),  # Flatten list of SEs
            dep.var.labels = c("Ph2 Study Indicator", "GIA Indicator"),
            covariate.labels = covariate_labels, 
            omit = omit_vars, omit.stat = c("f", "ser", "rsq"),
            add.lines = add_lines_custom,
            notes.append = FALSE, intercept.bottom = FALSE,
            out = output_file_html)
  
 
  
  
  
  tex_file <- paste0(project_root, "/output/tables/firm_level/lin_prob/", 
                     tolower(firm_name), "_regression_prob_phase_2_and_GIA_cost_bin_no_intercept.tex")
  
  # Generate the LaTeX table with \resizebox (80% width & height)
  mkTexTable(models, robust_se, firm_name, tex_file)
  
  return(list(models = models, robust_se = robust_se))
}

# Run for each firm
results <- lapply(firms, run_regressions, data = cost_status_data)

 


 


#-------------------------------------------OLD VS NEW CLUSTER ANALYSIS -------------------------------------------------------------------



 
# Load necessary libraries
 

# Ensure cost_status_data is prepared
cost_status_data <- cost_status_data %>%
  mutate(
    cluster_group = case_when(
      cluster %in% 7:10 ~ "Old",
      cluster %in% 11:13 ~ "New",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Old", "New"))
  )

# Get unique firms
firms <- unique(cost_status_data$firm)

# -------------------------------
# Function to Run Regressions
# -------------------------------
run_regression <- function(data, dependent_var, phase_POI_bins, phase_network_bins) {
  formula <- as.formula(
    paste(
      dependent_var, "~ 0 +",
      paste(c(phase_POI_bins, phase_network_bins), collapse = " + "),
      "+ capacity_quartiles + cluster + req_deliverability + fuel_category"
    )
  )
  
  lm_model <- lm(formula, data = data)
  robust_se <- sqrt(diag(vcovHC(lm_model, type = "HC1")))
  
  return(list(model = lm_model, robust_se = robust_se))
}

# -------------------------------
# Function to Run Firm-Level Analysis
# -------------------------------
run_firm_analysis <- function(firm_name, data) {
  firm_data <- subset(data, firm == firm_name)
  
  # Subset Data for Old and New Clusters
  old_clusters_data <- firm_data %>% filter(cluster_group == "Old")
  new_clusters_data <- firm_data %>% filter(cluster_group == "New")
  
  # Run regressions
  reg_old_ph2 <- run_regression(old_clusters_data, "ph2_ind", "phase_1_POI_cost_per_kW_bins", "phase_1_network_cost_per_kW_bins")
  reg_old_GIA <- run_regression(old_clusters_data, "GIA_ind", "phase_2_POI_cost_per_kW_bins", "phase_2_network_cost_per_kW_bins")
  reg_new_ph2 <- run_regression(new_clusters_data, "ph2_ind", "phase_1_POI_cost_per_kW_bins", "phase_1_network_cost_per_kW_bins")
  reg_new_GIA <- run_regression(new_clusters_data, "GIA_ind", "phase_2_POI_cost_per_kW_bins", "phase_2_network_cost_per_kW_bins")
  
  # Store models and SEs
  all_prob_models <- list(reg_old_ph2$model, reg_old_GIA$model, reg_new_ph2$model, reg_new_GIA$model)
  robust_se <- list(reg_old_ph2$robust_se, reg_old_GIA$robust_se, reg_new_ph2$robust_se, reg_new_GIA$robust_se)
  
  # Define paths
  output_path <- paste0(project_root, "/output/tables/firm_level/lin_prob/")
  figure_path <- paste0(project_root, "/output/figures/firm_level/lin_prob/")
  dir.create(figure_path, recursive = TRUE, showWarnings = FALSE)  # Ensure directory exists
  
  output_file_html <- paste0(output_path, tolower(firm_name), "_regression_prob_phase_2_and_GIA_cost_bin_grouped.html")
  output_file_tex <- paste0(output_path, tolower(firm_name), "_regression_prob_phase_2_and_GIA_cost_bin_grouped.tex")
  
  
  # Define Covariate Labels
  covariate_labels <- c(
    "Phase 1 Network Cost per kW (20 - 50)", "Phase 1 Network Cost per kW (50 - 100)", "Phase 1 Network Cost per kW (> 100)",
    "Phase 2 Network Cost per kW (20 - 50)", "Phase 2 Network Cost per kW (50 - 100)", "Phase 2 Network Cost per kW (> 100)"
  )
  
  # Define Omitted Variables (Excluding POI Cost Bins)
  omit_vars <- c(
    "capacity_quartiles", "cluster", "req_deliverability", "fuel_category",
    "phase_1_POI_cost_per_kW_bins1", "phase_1_POI_cost_per_kW_bins2",
    "phase_1_POI_cost_per_kW_bins3", "phase_1_POI_cost_per_kW_bins4",
    "phase_2_POI_cost_per_kW_bins1", "phase_2_POI_cost_per_kW_bins2",
    "phase_2_POI_cost_per_kW_bins3", "phase_2_POI_cost_per_kW_bins4"
  )
  
  # Define Custom Add Lines for Fixed Effects
  add_lines_custom <- list(
    c("Capacity Quartile", "Yes", "Yes", "Yes", "Yes"),
    c("Cluster", "Yes", "Yes", "Yes", "Yes"),
    c("Requested Deliverability", "Yes", "Yes", "Yes", "Yes"),
    c("Fuel", "Yes", "Yes", "Yes", "Yes"),
    c("POI Cost Bins", "Yes", "Yes", "Yes", "Yes")
  )
  
  # Stargazer Output with Fixed Effects Labels
  stargazer(all_prob_models, type = "html", style = "qje",
            se = robust_se, covariate.labels = covariate_labels,
            dep.var.labels = c("Ph2 Indicator (7-10)", "GIA Indicator (7-10)", "Ph2 Indicator (11-13)", "GIA Indicator (11-13)"),
            title = paste(firm_name, "- LPM for Phase II Study and GIA by Cluster Group"),
            omit = omit_vars, omit.stat = c("f", "ser", "rsq"),
            add.lines = add_lines_custom,
            out = output_file_html)
  
  stargazer(all_prob_models, type = "latex", style = "qje",
            se = robust_se, covariate.labels = covariate_labels,
            dep.var.labels = c("Ph2 Indicator (7-10)", "GIA Indicator (7-10)", "Ph2 Indicator (11-13)", "GIA Indicator (11-13)"),
            title = paste(firm_name, "- LPM for Phase II Study and GIA by Cluster Group"),
            omit = omit_vars, omit.stat = c("f", "ser", "rsq"),
            add.lines = add_lines_custom,
            out = output_file_tex)


  
 
  
  # ---------------------------------
  # Generate Bar Plots
  # ---------------------------------
  cost_bins <- c("20-50", "50-100", ">100")
  cluster_groups <- c("Old", "New")
  
  ph2_data <- data.frame()
  gia_data <- data.frame()
  
  for (i in 1:length(cost_bins)) {
    var_name_old <- paste0("phase_1_network_cost_per_kW_bins", i + 1)
    var_name_new <- paste0("phase_1_network_cost_per_kW_bins", i + 1)
    
    # Extract Ph2 data
    ph2_data <- rbind(ph2_data, data.frame(
      Cost_Bin = cost_bins[i],
      Cluster_Group = "Old",
      Coefficient = coef(all_prob_models[[1]])[var_name_old],
      SE = robust_se[[1]][var_name_old]
    ))
    ph2_data <- rbind(ph2_data, data.frame(
      Cost_Bin = cost_bins[i],
      Cluster_Group = "New",
      Coefficient = coef(all_prob_models[[3]])[var_name_new],
      SE = robust_se[[3]][var_name_new]
    ))
    
    # Extract GIA data
    var_name_old <- paste0("phase_2_network_cost_per_kW_bins", i + 1)
    var_name_new <- paste0("phase_2_network_cost_per_kW_bins", i + 1)
    
    gia_data <- rbind(gia_data, data.frame(
      Cost_Bin = cost_bins[i],
      Cluster_Group = "Old",
      Coefficient = coef(all_prob_models[[2]])[var_name_old],
      SE = robust_se[[2]][var_name_old]
    ))
    gia_data <- rbind(gia_data, data.frame(
      Cost_Bin = cost_bins[i],
      Cluster_Group = "New",
      Coefficient = coef(all_prob_models[[4]])[var_name_new],
      SE = robust_se[[4]][var_name_new]
    ))
  }
  
  # Factor levels
  ph2_data$Cost_Bin <- factor(ph2_data$Cost_Bin, levels = cost_bins)
  gia_data$Cost_Bin <- factor(gia_data$Cost_Bin, levels = cost_bins)
  
  # Define colors
  cluster_colors <- c("Old" = "skyblue", "New" = "salmon")
  
  # Generate Ph2 Plot
  ph2_plot <- ggplot(ph2_data, aes(x = Cost_Bin, y = Coefficient, fill = Cluster_Group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = Coefficient - SE, ymax = Coefficient + SE),
                  position = position_dodge(width = 0.8), width = 0.2) +
    labs(title = paste("Effect of Phase 1 Cost per kW on Receiving Phase 2 study -", firm_name),
         x = "Phase 1 Network Cost per kW Bin",
         y = "Marginal Probability",
         fill = "Cluster Group") +
    scale_fill_manual(values = cluster_colors) +
    theme_minimal()
  
  # Generate GIA Plot
  gia_plot <- ggplot(gia_data, aes(x = Cost_Bin, y = Coefficient, fill = Cluster_Group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = Coefficient - SE, ymax = Coefficient + SE),
                  position = position_dodge(width = 0.8), width = 0.2) +
    labs(title = paste("Effect of Phase 2 Cost per kW on Completing GIA -", firm_name),
         x = "Phase 2 Network Cost per kW Bin",
         y = "Marginal Probability",
         fill = "Cluster Group") +
    scale_fill_manual(values = cluster_colors) +
    theme_minimal()
  
  # Save plots to firm-level directory
  ggsave(paste0(figure_path, tolower(firm_name), "_ph2_plot_old_vs_new_clusters.png"), plot = ph2_plot, width = 10, height = 6)
  ggsave(paste0(figure_path, tolower(firm_name), "_gia_plot_old_vs_new_clusters.png"), plot = gia_plot, width = 10, height = 6)
  
  # Display the Ph2 Indicator Plot
  print(ph2_plot)
  
  # Display the GIA Indicator Plot
  print(gia_plot)
}

# Run for each firm
lapply(firms, run_firm_analysis, data = cost_status_data)
































 






