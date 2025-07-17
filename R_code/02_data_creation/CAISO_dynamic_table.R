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

library(readxl)
library(tidyverse)
library(dplyr)
library(broom)
library(lubridate)
library(car)
library("writexl")
library(openxlsx)
library(ggplot2)
library(kableExtra)
library(glmnet)
library(stargazer)
library(reshape2)

## ===== IMPORTING DATA =====
## Importing the Cleaned, Improved Checklist Data
RIMS_checklist_updated <- read_csv(paste0(project_root, 
                                          "/data/working/clean_RIMS_checklist_cluster_updated.csv"))

## Importing the Core Checklist
core_checklist <- read_csv(paste0(project_root, 
                                  "/data/working/clean_RIMS_checklist_cluster_core.csv"))
## Importing Cost Data
kiteworks_cost_data <- read_csv(paste0(project_root, 
                                   "/data/working/CAISO_cost_data.csv"))


## Importing Scraped data

Phase1_cost_data <-read_csv(paste0(project_root, "/data/ic_studies/clean/costs_phase_1_final_data.csv"))
Phase2_cost_data <-read_csv(paste0(project_root, "/data/ic_studies/clean/costs_phase_2_final_data.csv"))

### 


 

 
## ===== CLEANING DATA =====
## Consolidating the CAISO Phase 1 and 2 Cost Data
kiteworks_cost_data$kiteworks_phase_1_total_network_cost <- ifelse(is.na(kiteworks_cost_data$phase_1_network_MCR_cost), 
                                                     kiteworks_cost_data$phase_1_total_RNU_cost + 
                                                     kiteworks_cost_data$phase_1_total_LDNU_cost,
                                                     kiteworks_cost_data$phase_1_network_MCR_cost)

kiteworks_cost_data$kiteworks_phase_2_total_network_cost <- ifelse(is.na(kiteworks_cost_data$phase_2_network_MCR_cost), 
                                                     kiteworks_cost_data$phase_2_total_RNU_cost + 
                                                       kiteworks_cost_data$phase_2_total_LDNU_cost,
                                                     kiteworks_cost_data$phase_2_network_MCR_cost)
kiteworks_cost_data$kiteworks_phase_1_total_POI_cost <-  kiteworks_cost_data$phase_1_total_POI_cost

kiteworks_cost_data$kiteworks_phase_2_total_POI_cost <-  kiteworks_cost_data$phase_2_total_POI_cost

kiteworks_cost_data$kiteworks_phase_1_total_cost <- kiteworks_cost_data$phase_1_total_POI_cost + 
  kiteworks_cost_data$kiteworks_phase_1_total_network_cost

kiteworks_cost_data$kiteworks_phase_2_total_cost <- kiteworks_cost_data$kiteworks_phase_2_total_POI_cost + 
  kiteworks_cost_data$kiteworks_phase_2_total_network_cost





 
# 1) Take only those kiteworks rows that *do* have Phase 2 POI data
phase2_kiteworks_data <- kiteworks_cost_data %>%
  filter(!is.na(kiteworks_phase_2_total_POI_cost) | !is.na(kiteworks_phase_2_total_network_cost)) %>%
  mutate(q_id = as.numeric(q_id))

# 2) Make sure your Phase2 scraped IDs are numeric too
phase2_ids <- Phase2_cost_data %>%
  mutate(q_id = as.numeric(q_id)) %>%
  pull(q_id) %>%
  unique()

kite_ids <- phase2_kiteworks_data$q_id %>% unique()

# 3) Compute the three sets
common_ids     <- intersect(kite_ids, phase2_ids)
only_in_kite   <- setdiff(kite_ids, phase2_ids)
only_in_phase2 <- setdiff(phase2_ids, kite_ids)

# 4) Summary table
id_summary <- tibble(
  source    = c("Only in kiteworks", "Only in phase2", "In both"),
  n_qids    = c(length(only_in_kite), length(only_in_phase2), length(common_ids))
)

print(id_summary)

 
 




## Consolidating the Phase 1 and Phase 2 Scraped cost_data

 

# 2. Merge everything by q_id
library(stringr)
library(dplyr)


# --- Base columns you trust from Phase1 ---
base_cols <- c(
  "q_id", "original", "cluster", "PTO", "status",
  "req_deliverability", "capacity", "latitude",
  "longitude", "point_of_interconnection"
)

# --- 1) All q_ids from Phase1 scrape and Kiteworks ---
all_qids <- tibble(
  q_id = unique(c(Phase1_cost_data$q_id,
                  kiteworks_cost_data$q_id))
) %>%
  mutate(q_id = as.character(q_id))

# --- 2) Pull in whatever base columns exist, suffixing _scr or _kite ---
base_phase1 <- Phase1_cost_data %>%
  mutate(q_id = as.character(q_id)) %>%
  select(q_id, any_of(base_cols[-1])) %>%
  rename_with(~ paste0(.x, "_scr"), -q_id)

base_kite <- kiteworks_cost_data %>%
  mutate(q_id = as.character(q_id)) %>%
  select(q_id, any_of(base_cols[-1])) %>%
  rename_with(~ paste0(.x, "_kite"), -q_id)

# --- 3) Full join both sets of base columns onto all_qids ---
base_merged <- all_qids %>%
  left_join(base_phase1, by = "q_id") %>%
  left_join(base_kite,   by = "q_id")

# --- 4) Identify which base columns survived in at least one source ---
base_vars <- intersect(
  base_cols[-1],
  sub("_(scr|kite)$", "", names(base_merged)[-1])
)

# --- 5) Coalesce each base column, preferring scraped then kite if present ---
base_final <- reduce(
  base_vars,
  function(df, var) {
    scr_var  <- paste0(var, "_scr")
    kite_var <- paste0(var, "_kite")
    df %>%
      mutate(
        !!var := coalesce(
          .data[[scr_var]],
          if (kite_var %in% names(df)) .data[[kite_var]] else NA
        )
      )
  },
  .init = base_merged
) %>%
  select(q_id, all_of(base_vars))

# --- 6) Build your master dataset by joining in all cost sources ---
master <- base_final %>%
  # Kiteworks totals
  left_join(
    kiteworks_cost_data %>%
      mutate(q_id = as.character(q_id)) %>%
      select(
        q_id,
        everything(),
        -any_of(setdiff(base_cols, "q_id"))
      ),
    by = "q_id"
  ) %>%
  # Phase1 scraped costs (_1)
  # Phase1 scraped costs (_1)
  left_join(
    Phase1_cost_data %>%
      mutate(q_id = as.character(q_id)) %>%
      select(q_id, starts_with("cost_")) %>%
      # ONLY multiply the cost_ columns *except* our four flags/shares
      mutate(
        across(
          .cols = starts_with("cost_") &
            !any_of(c(
              "cost_network_shared",
              "cost_network_leverage",
              "cost_total_inf_flag",
              "cost_shared_flag"
            )),
          ~ .x * 1000
        )
      ) %>%
      rename_with(~ paste0(., "_1"), -q_id),
    by = "q_id"
  ) %>%
  # Phase2 scraped costs (_2)
  left_join(
    Phase2_cost_data %>%
      mutate(q_id = as.character(q_id)) %>%
      select(q_id, starts_with("cost_")) %>%
      mutate(
        across(
          .cols = starts_with("cost_") &
            !any_of(c(
              "cost_network_shared",
              "cost_network_leverage",
              "cost_total_inf_flag",
              "cost_shared_flag"
            )),
          ~ .x * 1000
        )
      ) %>%
      rename_with(~ paste0(., "_2"), -q_id),
    by = "q_id"
  ) %>%
  # 7) Coalesce phase‐level totals
  # 7) Instead of coalesce, do manual if/else and set flags:
  mutate(
    # Phase 1: pick scraped if present, else kiteworks, else NA
    phase_1_total_POI_cost     = ifelse(!is.na(cost_poi_own_1),
                                        cost_poi_own_1,
                                        kiteworks_phase_1_total_POI_cost),
    phase_1_total_network_cost = ifelse(!is.na(cost_network_own_1),
                                        cost_network_own_1,
                                        kiteworks_phase_1_total_network_cost),
    phase_1_total_cost         = ifelse(
      !is.na(phase_1_total_POI_cost) & !is.na(phase_1_total_network_cost),
      phase_1_total_POI_cost + phase_1_total_network_cost,
      NA_real_
    ),
    flag_phase1 = case_when(
      !is.na(cost_poi_own_1) | !is.na(cost_network_own_1)           ~ "scraped",
      !is.na(kiteworks_phase_1_total_POI_cost) |
        !is.na(kiteworks_phase_1_total_network_cost)                 ~ "kiteworks",
      TRUE                                                          ~ NA_character_
    ),
    
    # Phase 2: same pattern
    phase_2_total_POI_cost     = ifelse(!is.na(cost_poi_own_2),
                                        cost_poi_own_2,
                                        kiteworks_phase_2_total_POI_cost),
    phase_2_total_network_cost = ifelse(!is.na(cost_network_own_2),
                                        cost_network_own_2,
                                        kiteworks_phase_2_total_network_cost),
    phase_2_total_cost         = ifelse(
      !is.na(phase_2_total_POI_cost) & !is.na(phase_2_total_network_cost),
      phase_2_total_POI_cost + phase_2_total_network_cost,
      NA_real_
    ),
    flag_phase2 = case_when(
      !is.na(cost_poi_own_2) | !is.na(cost_network_own_2)           ~ "scraped",
      !is.na(kiteworks_phase_2_total_POI_cost) |
        !is.na(kiteworks_phase_2_total_network_cost)                 ~ "kiteworks",
      TRUE                                                          ~ NA_character_
    )
  )


#mutate(
#    phase_1_total_POI_cost     = coalesce(cost_poi_own_1,     kiteworks_phase_1_total_POI_cost),
#    phase_1_total_network_cost = coalesce(cost_network_own_1, kiteworks_phase_1_total_network_cost),
#    phase_1_total_cost         = coalesce(cost_poi_own_1 + cost_network_own_1, kiteworks_phase_1_total_cost),
#    
#    phase_2_total_POI_cost     = coalesce(cost_poi_own_2,     kiteworks_phase_2_total_POI_cost),
#    phase_2_total_network_cost = coalesce(cost_network_own_2, kiteworks_phase_2_total_network_cost),
#   phase_2_total_cost         = coalesce(cost_poi_own_2 + cost_network_own_2, kiteworks_phase_2_total_cost)
#  )
#


# Pre-compute unique q_id counts for each source
n_phase1_qids <- master %>%
  filter(!is.na(cost_poi_own_1) | !is.na(cost_network_own_1)) %>%
  distinct(q_id) %>%
  nrow()

n_phase2_qids <- master %>%
  filter(!is.na(cost_poi_own_2) | !is.na(cost_network_own_2)) %>%
  distinct(q_id) %>%
  nrow()

n_kiteworks1_qids <- master %>%
  filter(!is.na(kiteworks_phase_1_total_POI_cost) | !is.na(kiteworks_phase_1_total_network_cost)) %>%
  distinct(q_id) %>%
  nrow()

n_kiteworks2_qids <- master %>%
  filter(!is.na(kiteworks_phase_2_total_POI_cost) | !is.na(kiteworks_phase_2_total_network_cost)) %>%
  distinct(q_id) %>%
  nrow()

# Build summary_counts
summary_counts <- master %>%
  summarise(
    # Phase 1 POI
    phase1_POI_cost_scrape    = sum(!is.na(cost_poi_own_1)),
    phase1_POI_cost_kiteworks = sum(is.na(cost_poi_own_1) & !is.na(kiteworks_phase_1_total_POI_cost)),
    
    # Phase 1 network
    phase1_network_cost_scrape    = sum(!is.na(cost_network_own_1)),
    phase1_network_cost_kiteworks = sum(is.na(cost_network_own_1) & !is.na(kiteworks_phase_1_total_network_cost)),
    
    # Phase 1 total
    phase1_total_cost_scrape    = sum(!is.na(cost_poi_own_1) & !is.na(cost_network_own_1)),
    phase1_total_cost_kiteworks = sum(is.na(cost_poi_own_1) & is.na(cost_network_own_1)),
    
    # Phase 2 POI
    phase2_POI_cost_scrape    = sum(!is.na(cost_poi_own_2)),
    phase2_POI_cost_kiteworks = sum(is.na(cost_poi_own_2) & !is.na(kiteworks_phase_2_total_POI_cost)),
    
    # Phase 2 network
    phase2_network_cost_scrape    = sum(!is.na(cost_network_own_2)),
    phase2_network_cost_kiteworks = sum(is.na(cost_network_own_2) & !is.na(kiteworks_phase_2_total_network_cost)),
    
    # Phase 2 total
    phase2_total_cost_scrape    = sum(!is.na(cost_poi_own_2) & !is.na(cost_network_own_2)),
    phase2_total_cost_kiteworks = sum(is.na(cost_poi_own_2) & !is.na(kiteworks_phase_2_total_cost))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to      = c("metric", "source"),
    names_pattern = "(.*)_(scrape|kiteworks)$",
    values_to     = "count"
  ) %>%
  pivot_wider(names_from = source, values_from = count) %>%
  # Add raw q_id counts by phase
  mutate(
    raw_phase_qids       = if_else(str_detect(metric, "^phase1_"), n_phase1_qids, n_phase2_qids),
    raw_kiteworks_qids   = if_else(str_detect(metric, "^phase1_"), n_kiteworks1_qids, n_kiteworks2_qids)
  )

print(summary_counts)


summary_counts <- master %>%
  summarise(
    phase1_scraped    = sum(flag_phase1 == "scraped",    na.rm = TRUE),
    phase1_kiteworks  = sum(flag_phase1 == "kiteworks",  na.rm = TRUE),
    phase2_scraped    = sum(flag_phase2 == "scraped",    na.rm = TRUE),
    phase2_kiteworks  = sum(flag_phase2 == "kiteworks",  na.rm = TRUE)
  ) %>%
  pivot_longer(everything(),
               names_to = c("phase","source"),
               names_sep = "_",
               values_to = "count")

print(summary_counts)

# turn your long summary_counts back into wide form
summary_counts2 <- summary_counts %>%
  pivot_wider(
    id_cols   = phase,
    names_from = source,
    values_from = count
  ) %>%
  rename(
    `No from Scraped`     = scraped,
    `No from Kiteworks`   = kiteworks
  )

# 2) Export with stargazer
# (adjust the filenames/path as you like)
stargazer(
  summary_counts2,
  summary   = FALSE,
  rownames  = FALSE,
  style     = "qje",
  type      = "latex",
  out       = paste0(project_root, "/output/tables/project_dynamic/kiteworks_scrape_merge_summary_counts.tex")
)

stargazer(
  summary_counts2,
  summary   = FALSE,
  rownames  = FALSE,
  style     = "qje",
  type      = "html",
  out       = paste0(project_root, "/output/tables/project_dynamic/kiteworks_scrape_merge_summary_counts.html")
)

 


 


CAISO_cost_data <- master



CAISO_cost_data$phase_1_total_network_cost_per_kW <- CAISO_cost_data$phase_1_total_network_cost/
  (1000 * CAISO_cost_data$capacity)

CAISO_cost_data$phase_2_total_network_cost_per_kW <- CAISO_cost_data$phase_2_total_network_cost/
  (1000 * CAISO_cost_data$capacity)

CAISO_cost_data$phase_1_total_POI_cost_per_kW <- CAISO_cost_data$phase_1_total_POI_cost/
  (1000 * CAISO_cost_data$capacity)

CAISO_cost_data$phase_2_total_POI_cost_per_kW <- CAISO_cost_data$phase_2_total_POI_cost/
  (1000 * CAISO_cost_data$capacity)

CAISO_cost_data$phase_1_total_cost_per_kW <- CAISO_cost_data$phase_1_total_cost/
  (1000 * CAISO_cost_data$capacity)

CAISO_cost_data$phase_2_total_cost_per_kW <- CAISO_cost_data$phase_2_total_cost/
  (1000 * CAISO_cost_data$capacity)


 CAISO_cost_data <- CAISO_cost_data %>% 
   mutate(
     phase_1_total_network_own_cost_shared_per_kW = cost_network_own_shared_1/(1000 * capacity),
     phase_2_total_network_own_cost_shared_per_kW = cost_network_own_shared_2/(1000 * capacity),
     phase_1_cost_network_shared = cost_network_shared_1,
     phase_2_cost_network_shared = cost_network_shared_2,
     phase_1_cost_network_leverage = cost_network_leverage_1,
     phase_2_cost_network_leverage = cost_network_leverage_2,
   ) %>% 

   # 1) Relocate all Phase 1 cols together, right after `fuel`
   relocate(
     # final cost variables
     phase_1_total_POI_cost,           phase_1_total_POI_cost_per_kW,
     phase_1_total_network_cost,       phase_1_total_network_cost_per_kW,
     phase_1_total_cost,               phase_1_total_cost_per_kW,
     # network‐shared & leverage
     phase_1_cost_network_shared,      phase_1_cost_network_leverage,
     # your data‐source flag for Phase 1
     flag_phase1,
     .after = fuel
   ) %>%
   
   # 2) Then relocate all Phase 2 cols immediately after the last Phase 1 column
   relocate(
     phase_2_total_POI_cost,           phase_2_total_POI_cost_per_kW,
     phase_2_total_network_cost,       phase_2_total_network_cost_per_kW,
     phase_2_total_cost,               phase_2_total_cost_per_kW,
     phase_2_cost_network_shared,      phase_2_cost_network_leverage,
     flag_phase2,
     .after = flag_phase1
   )   
 


 
#--------------------------------------------------------------------------------------------------------------------------------------
 #---------------------------------------------------- Data Description : Missing and Zero Costs-------------------------------------
 
 library(dplyr)
 library(knitr)
 library(stargazer)
 
 # 1) Compute all counts
 n_total <- nrow(CAISO_cost_data)
 
 # Phase 1 raw & per-kW
 p1_valid           <- sum(!is.na(CAISO_cost_data$flag_phase1))
 p1_missing         <- sum( is.na(CAISO_cost_data$flag_phase1))
 p1_zero_POI        <- sum(!is.na(CAISO_cost_data$flag_phase1) &
                             near(CAISO_cost_data$phase_1_total_POI_cost, 0),
                           na.rm = TRUE)
 p1_zero_net        <- sum(!is.na(CAISO_cost_data$flag_phase1) &
                             near(CAISO_cost_data$phase_1_total_network_cost, 0),
                           na.rm = TRUE)
 p1_inf_POI         <- sum(!is.na(CAISO_cost_data$flag_phase1) &
                             is.infinite(CAISO_cost_data$phase_1_total_POI_cost),
                           na.rm = TRUE)
 p1_inf_net         <- sum(!is.na(CAISO_cost_data$flag_phase1) &
                             is.infinite(CAISO_cost_data$phase_1_total_network_cost),
                           na.rm = TRUE)
 
 p1_zero_POI_perkw  <- sum(!is.na(CAISO_cost_data$flag_phase1) &
                             near(CAISO_cost_data$phase_1_total_POI_cost_per_kW, 0),
                           na.rm = TRUE)
 p1_zero_net_perkw  <- sum(!is.na(CAISO_cost_data$flag_phase1) &
                             near(CAISO_cost_data$phase_1_total_network_cost_per_kW, 0),
                           na.rm = TRUE)
 p1_inf_POI_perkw   <- sum(!is.na(CAISO_cost_data$flag_phase1) &
                             is.infinite(CAISO_cost_data$phase_1_total_POI_cost_per_kW),
                           na.rm = TRUE)
 p1_inf_net_perkw   <- sum(!is.na(CAISO_cost_data$flag_phase1) &
                             is.infinite(CAISO_cost_data$phase_1_total_network_cost_per_kW),
                           na.rm = TRUE)
 
 # Phase 2 raw & per-kW
 p2_valid           <- sum(!is.na(CAISO_cost_data$flag_phase2))
 p2_missing         <- sum( is.na(CAISO_cost_data$flag_phase2))
 p2_zero_POI        <- sum(!is.na(CAISO_cost_data$flag_phase2) &
                             near(CAISO_cost_data$phase_2_total_POI_cost, 0),
                           na.rm = TRUE)
 p2_zero_net        <- sum(!is.na(CAISO_cost_data$flag_phase2) &
                             near(CAISO_cost_data$phase_2_total_network_cost, 0),
                           na.rm = TRUE)
 p2_inf_POI         <- sum(!is.na(CAISO_cost_data$flag_phase2) &
                             is.infinite(CAISO_cost_data$phase_2_total_POI_cost),
                           na.rm = TRUE)
 p2_inf_net         <- sum(!is.na(CAISO_cost_data$flag_phase2) &
                             is.infinite(CAISO_cost_data$phase_2_total_network_cost),
                           na.rm = TRUE)
 
 p2_zero_POI_perkw  <- sum(!is.na(CAISO_cost_data$flag_phase2) &
                             near(CAISO_cost_data$phase_2_total_POI_cost_per_kW, 0),
                           na.rm = TRUE)
 p2_zero_net_perkw  <- sum(!is.na(CAISO_cost_data$flag_phase2) &
                             near(CAISO_cost_data$phase_2_total_network_cost_per_kW, 0),
                           na.rm = TRUE)
 p2_inf_POI_perkw   <- sum(!is.na(CAISO_cost_data$flag_phase2) &
                             is.infinite(CAISO_cost_data$phase_2_total_POI_cost_per_kW),
                           na.rm = TRUE)
 p2_inf_net_perkw   <- sum(!is.na(CAISO_cost_data$flag_phase2) &
                             is.infinite(CAISO_cost_data$phase_2_total_network_cost_per_kW),
                           na.rm = TRUE)
 
 # 2) Build a summary tibble
 summary_table <- tibble(
   Metric = c(
     "Total observations",
     "Valid observations",
     "Missing observations",
     "Zero-cost POI observations",
     "Zero-cost network observations",
     "Inf-cost POI observations",
     "Inf-cost network observations"
   ),
   `Phase 1 (raw)`     = c(n_total, p1_valid,   p1_missing,   p1_zero_POI,    p1_zero_net,    p1_inf_POI,    p1_inf_net),
   `Phase 1 (per kW)`  = c(n_total, p1_valid,   p1_missing,   p1_zero_POI_perkw, p1_zero_net_perkw, p1_inf_POI_perkw, p1_inf_net_perkw),
   `Phase 2 (raw)`     = c(n_total, p2_valid,   p2_missing,   p2_zero_POI,    p2_zero_net,    p2_inf_POI,    p2_inf_net),
   `Phase 2 (per kW)`  = c(n_total, p2_valid,   p2_missing,   p2_zero_POI_perkw, p2_zero_net_perkw, p2_inf_POI_perkw, p2_inf_net_perkw)
 )
 
 # 3) Display with kable (optional)
 kable(
   summary_table,
   col.names = names(summary_table),
   digits   = 0,
   align    = c("l", rep("r", 4))
 )
 
 # 4) Export to LaTeX via stargazer
 stargazer(
   summary_table,
   summary   = FALSE,
   rownames  = FALSE,
   style     = "qje",
   type      = "latex",
   title = "CAISO Cost Data Summary - Scraped vs Kiteworks",
   out       = paste0(project_root, "/output/tables/project_dynamic/caiso_merged_cost_data_summary.tex")
 )
 
 # 5) Export to HTML
 stargazer(
   summary_table,
   summary   = FALSE,
   rownames  = FALSE,
   style     = "qje",
   type      = "html",
   out       = paste0(project_root, "/output/tables/project_dynamic/caiso_merged_cost_data_summary.html")
 )

#-------------------------------------------
# Investigating Source of Zero Costs--------
#-------------------------------------------
 
 #–– 1) Compute zero‐cost counts by phase, cost‐type, and data‐source
 p1_zero_POI_scraped   <- sum(
   CAISO_cost_data$flag_phase1 == "scraped" &
     near(CAISO_cost_data$phase_1_total_POI_cost, 0),
   na.rm = TRUE
 )
 p1_zero_POI_kiteworks <- sum(
   CAISO_cost_data$flag_phase1 == "kiteworks" &
     near(CAISO_cost_data$phase_1_total_POI_cost, 0),
   na.rm = TRUE
 )
 
 p1_zero_net_scraped   <- sum(
   CAISO_cost_data$flag_phase1 == "scraped" &
     near(CAISO_cost_data$phase_1_total_network_cost, 0),
   na.rm = TRUE
 )
 p1_zero_net_kiteworks <- sum(
   CAISO_cost_data$flag_phase1 == "kiteworks" &
     near(CAISO_cost_data$phase_1_total_network_cost, 0),
   na.rm = TRUE
 )
 
 p2_zero_POI_scraped   <- sum(
   CAISO_cost_data$flag_phase2 == "scraped" &
     near(CAISO_cost_data$phase_2_total_POI_cost, 0),
   na.rm = TRUE
 )
 p2_zero_POI_kiteworks <- sum(
   CAISO_cost_data$flag_phase2 == "kiteworks" &
     near(CAISO_cost_data$phase_2_total_POI_cost, 0),
   na.rm = TRUE
 )
 
 p2_zero_net_scraped   <- sum(
   CAISO_cost_data$flag_phase2 == "scraped" &
     near(CAISO_cost_data$phase_2_total_network_cost, 0),
   na.rm = TRUE
 )
 p2_zero_net_kiteworks <- sum(
   CAISO_cost_data$flag_phase2 == "kiteworks" &
     near(CAISO_cost_data$phase_2_total_network_cost, 0),
   na.rm = TRUE
 )
 
 #–– 2) Build the diagnostic table
 diag_table <- tibble(
   Metric = c(
     "Phase 1 Zero–cost POI",
     "Phase 1 Zero–cost Network",
     "Phase 2 Zero–cost POI",
     "Phase 2 Zero–cost Network"
   ),
   Scraped   = c(p1_zero_POI_scraped, p1_zero_net_scraped, p2_zero_POI_scraped, p2_zero_net_scraped),
   Kiteworks = c(p1_zero_POI_kiteworks, p1_zero_net_kiteworks, p2_zero_POI_kiteworks, p2_zero_net_kiteworks)
 )
 
 #–– 3) Print with kable()
 kable(
   diag_table,
   col.names = c("","From scraped","From kiteworks"),
   digits   = 0,
   align    = c("l","r","r")
 )
 
 #–– 4) Export via stargazer
 stargazer(
   diag_table,
   summary   = FALSE,
   rownames  = FALSE,
   style     = "qje",
   type      = "latex",
   out       = paste0(project_root, "/output/tables/project_dynamic/caiso_zero_cost_diagnostics.tex")
 )
 stargazer(
   diag_table,
   summary   = FALSE,
   rownames  = FALSE,
   style     = "qje",
   type      = "html",
   out       = paste0(project_root, "/output/tables/project_dynamic/caiso_zero_cost_diagnostics.html")
 )
 
 



# 1. Get distinct (q_id, cluster) from each source
kite_clusters <- kiteworks_cost_data %>%
  filter(!is.na(kiteworks_phase_1_total_POI_cost) | 
           !is.na(kiteworks_phase_1_total_network_cost)) %>%
  mutate(q_id = as.character(q_id),
         cluster_kite = as.character(cluster)) %>%
  distinct(q_id, cluster_kite)

phase1_clusters <- Phase1_cost_data %>%
  mutate(q_id = as.character(q_id),
         cluster_scr  = as.character(cluster)) %>%
  distinct(q_id, cluster_scr)

# 2. Join them and keep only those where the clusters disagree
cluster_mismatches <- inner_join(
  kite_clusters,
  phase1_clusters,
  by = "q_id"
) %>%
  filter(cluster_kite != cluster_scr)

# 3. Print them out
print(cluster_mismatches)


## Consolidating the CAISO Reassessment Cost Data
#selected_columns <- colnames(CAISO_cost_data)[grep("_[0-9]+$", 
#                                                  colnames(CAISO_cost_data))]

#list(selected_columns)
#reassessment_years <- unique(as.numeric(gsub(".*_([0-9]+)$", "\\1", 
#                                       selected_columns)))

# select only columns ending in underscore + exactly two digits
selected_columns <- grep("_[0-9]{2}$",
                         colnames(CAISO_cost_data),
                         value = TRUE)
list(selected_columns)
# extract the two-digit years and turn into numeric
reassessment_years <- unique(
  as.numeric(sub(".*_([0-9]{2})$", "\\1", selected_columns))
)

reassessment_years
for (year in reassessment_years)
{
  reassessment_POI_cost <- paste0("reassessment_", year, "_total_POI_cost_per_kW")
  reassessment_network_cost <- paste0("reassessment_", year, "_total_network_cost_per_kW")
  reassessment_total_cost <- paste0("reassessment_", year, "_total_cost_per_kW")
  
  POI_column <- paste0("POI_cost_", year)
  
  CAISO_cost_data[, reassessment_POI_cost] <- ifelse(POI_column %in% selected_columns, 
                                                     CAISO_cost_data[,POI_column]/(1000*CAISO_cost_data$capacity),
                                                     NA)
  
  network_MCR_column <- paste0("allocated_MCR_", year)
  network_RNU_column <- paste0("RNU_cost_", year)
  network_LDNU_column <- paste0("LDNU_cost_", year)
  
  CAISO_cost_data[, reassessment_network_cost] <- ifelse(network_MCR_column %in% selected_columns,
                                                         CAISO_cost_data[, network_MCR_column]/(1000*CAISO_cost_data$capacity),
                                                         ifelse(all(c(network_RNU_column, 
                                                                      network_LDNU_column) %in% selected_columns),
                                                                (CAISO_cost_data[, network_RNU_column] + 
                                                                   CAISO_cost_data[, network_LDNU_column])/(1000*CAISO_cost_data$capacity),
                                                                NA))
  
  CAISO_cost_data[, reassessment_total_cost] <- CAISO_cost_data[, reassessment_POI_cost] + 
    CAISO_cost_data[, reassessment_network_cost]
  
}






## ===== CHECKING FOR ANOMOLIES =====
## Number of Projects Completed Without GIA
core_checklist %>%
    group_by(q_id) %>%
    summarize(ever_completed = any(project_status == "COMPLETED"),
              ever_gia = any(checklist_phase == "GIA" & 
                             checklist_status == "Executed")) %>%
    filter(ever_completed, !ever_gia) %>%
    summarise(n_unique_projects = n())

## Number of Projects with GIA After WITHDRAWN or COMPLETED

## ===== BASIC ANALYSIS TO BETTER MAKE THE DYNAMIC TABLE =====
## Distribution of Days Between Ph2 and GIA
unique_q_ids <- unique(core_checklist$q_id) 

ph2_to_GIA <- data.frame(project_id = numeric(), 
                               days = numeric(), 
                               cluster = character())

for(id in unique_q_ids)
{
  project <- subset(core_checklist, core_checklist$project_id == id)
  ph2_date <- as.Date(project[project$checklist_item == "Phase II Study Report", ]$status_date)
  GIA_date <- as.Date(project[project$checklist_item == "GIA Execution", ]$status_date)
  GIA_date <- GIA_date[1]
  clusters <- project$cluster_number[1]
  days <- as.numeric(GIA_date - ph2_date)
  
  if (length(days) == 0)
  {
    next
  }
  if (is.na(days))
  {
    next
  }
  if (days < 0)  ## There is one project (project_id = 752) who's phase I study report date is before the interconnection request form date
  {
    next
  }
  
  data <- data.frame(project_id = id, days = days, cluster = clusters)
  ph2_to_GIA <- rbind(ph2_to_GIA, data)
}

ph2_to_GIA_hist <- ggplot(ph2_to_GIA[!(ph2_to_GIA$cluster == "TC" |
                                       ph2_to_GIA$cluster == "SGIP-TC"),], 
                                aes(x = days, after_stat(density))) + 
  geom_histogram(boundary = 365, binwidth = 7,
                 fill = "blue") + 
  coord_flip() + 
  # geom_density(alpha=.2, fill="yellow") +  
  labs(x = "Days", 
       y = "Density",
       title = "Days from Ph2 Study to GIA") + 
  expand_limits(x = 0, y = 0) +
  facet_wrap(~cluster, nrow = 1) +
  theme_light()

# Wide range of days between Ph2 and GIA, but majority for most clusters
# signed before 500 days

## Number of Projects that Withdraw Before Phase 1 Study
withdrawn_projects <- subset(core_checklist, 
                             core_checklist$project_status == "WITHDRAWN") 

no_studies <- withdrawn_projects %>%
              group_by(q_id) %>%
              mutate(no_phase_1_study = ifelse(any(checklist_phase == "Phase I Study"), 
                                               0, 1))


# Tanay - it throws the following error for me -Error in quo_as_label(quo) : argument "expr" is missing, with no default
# how to make it so that only get some for unique ids
#clusters_no_study <- no_studies %>% group_by(cluster_number) %>%
#                     summarize(project = q_id,
#                               withdraw_before_study = )
#
number_of_such_projects <- length(unique(no_studies$q_id[no_studies$no_phase_1_study == 1]))
number_of_such_projects

length(unique(core_checklist$q_id))

## ===== CREATING FULL DYNAMIC TABLE =====
## Organizing POI Cost Data by Checklist Event
simple_POI_cost_data <- CAISO_cost_data[,c("q_id", "capacity",
                                           "phase_1_total_POI_cost_per_kW",
                                           "phase_2_total_POI_cost_per_kW",
                                           "reassessment_20_total_POI_cost_per_kW",
                                           "reassessment_19_total_POI_cost_per_kW",
                                           "reassessment_18_total_POI_cost_per_kW",
                                           "reassessment_17_total_POI_cost_per_kW",
                                           "reassessment_16_total_POI_cost_per_kW",
                                           "reassessment_15_total_POI_cost_per_kW",
                                           "reassessment_14_total_POI_cost_per_kW")]


long_simple_POI_cost_data <- melt(simple_POI_cost_data, id.vars = c("q_id", "capacity"),
                                  variable_name = "checklist_phase",
                                  value.name = "POI_cost")

colnames(long_simple_POI_cost_data)[3] <- "checklist_phase"

long_simple_POI_cost_data <- long_simple_POI_cost_data %>%
  mutate(checklist_phase = case_when(checklist_phase == "phase_1_total_POI_cost_per_kW" ~ "Phase I Study",
                                     checklist_phase == "phase_2_total_POI_cost_per_kW" ~ "Phase II Study",
                                     checklist_phase %in% paste0("reassessment_", 14:20, "_total_POI_cost_per_kW") ~ 
                                     gsub("_total_POI_cost_per_kW", "", 
                                          gsub("reassessment_", "Reassessment ", checklist_phase)),
                                     TRUE ~ "Reassessment"))

## Organizing Network Cost Data
simple_network_cost_data <- CAISO_cost_data[,c("q_id", "capacity",
                                               "phase_1_total_network_cost_per_kW",
                                               "phase_2_total_network_cost_per_kW",
                                               "phase_1_total_network_own_cost_shared_per_kW",
                                               "phase_2_total_network_own_cost_shared_per_kW",
                                               "phase_1_cost_network_shared",
                                               "phase_2_cost_network_shared",
                                               "phase_1_cost_network_leverage",
                                               "phase_2_cost_network_leverage",
                                               "reassessment_20_total_network_cost_per_kW",
                                               "reassessment_19_total_network_cost_per_kW",
                                               "reassessment_18_total_network_cost_per_kW",
                                               "reassessment_17_total_network_cost_per_kW",
                                               "reassessment_16_total_network_cost_per_kW",
                                               "reassessment_15_total_network_cost_per_kW",
                                               "reassessment_14_total_network_cost_per_kW")]

long_simple_network_cost_data <- melt(simple_network_cost_data, id.vars = c("q_id", "capacity"),
                                      variable_name = "checklist_phase",
                                      value.name = "network_cost")
colnames(long_simple_network_cost_data)[3] <- "checklist_phase"
long_simple_network_cost_data <- long_simple_network_cost_data %>%
  mutate(checklist_phase = case_when(checklist_phase == "phase_1_total_network_cost_per_kW" ~ "Phase I Study",
                                     checklist_phase == "phase_2_total_network_cost_per_kW" ~ "Phase II Study",
                                     checklist_phase %in% paste0("reassessment_", 14:20, "_total_network_cost_per_kW") ~ 
                                       gsub("_total_network_cost_per_kW", "", 
                                            gsub("reassessment_", "Reassessment ", checklist_phase)),
                                     TRUE ~ "Reassessment"))



 



# New code to include the shared and leverage terms
# define the exact order you want
phase_levels <- c(
  "Phase I Study",
  "Phase II Study",
  paste0("Reassessment ", 14:20),
  "Reassessment"           # if you still need a catch-all
)

long_simple_network_cost_data <- simple_network_cost_data %>%
  pivot_longer(
    cols          = -c(q_id, capacity),
    names_to      = c("checklist_phase", "metric"),
    names_pattern = "^(phase_[12]|reassessment_\\d{2})_(.*)$",
    values_to     = "value"
  ) %>%
  pivot_wider(
    names_from   = metric,
    values_from  = value
  ) %>%
  rename(
    network_cost            = total_network_cost_per_kW,
    cost_network_own_shared = total_network_own_cost_shared_per_kW,
    cost_network_shared     = cost_network_shared,
    cost_network_leverage   = cost_network_leverage
  ) %>%
  # recode into your human-readable labels
  mutate(
    checklist_phase = case_when(
      checklist_phase == "phase_1"                     ~ "Phase I Study",
      checklist_phase == "phase_2"                     ~ "Phase II Study",
      str_detect(checklist_phase, "^reassessment_")     ~ 
        str_replace(checklist_phase, "reassessment_", "Reassessment "),
      TRUE                                              ~ "Reassessment"
    ),
    # turn into an *ordered* factor
    checklist_phase = factor(checklist_phase, levels = phase_levels, ordered = TRUE)
  ) %>%
  # finally, sort so within each q_id you get I → II → all reassessments
  arrange(q_id, checklist_phase) %>%
  select(
    q_id, capacity, checklist_phase,
    network_cost,
    cost_network_own_shared,
    cost_network_shared,
    cost_network_leverage
  )





## Merging Organized Cost Data
long_simple_cost_data <- merge(long_simple_POI_cost_data, 
                               long_simple_network_cost_data,
                               by = c("q_id", "capacity", "checklist_phase"))

# Bringing over the scraped vs kiteworks data flag for diagnostics
long_simple_cost_data <- long_simple_cost_data %>%
  # bring in your two flags
  left_join(
    CAISO_cost_data %>%
      select(q_id, flag_phase1, flag_phase2),
    by = "q_id"
  ) %>%
  # pick the right one for each row
  mutate(
    data_source = case_when(
      checklist_phase == "Phase I Study"   ~ flag_phase1,
      checklist_phase == "Phase II Study"  ~ flag_phase2,
      TRUE                                  ~ NA_character_
    )
  ) %>%
  select(-flag_phase1, -flag_phase2)  # drop the originals if you like


## Getting Data from Core Checklist
core_checklist_thin <- core_checklist %>% group_by(q_id) %>%
                       mutate(last_date = max(status_date),
                              first_date = min(status_date)) %>%
                       ungroup() %>%
                       select(q_id, checklist_phase, checklist_item,
                              checklist_status, status_date)

core_checklist_thin$checklist_phase <- ifelse(core_checklist_thin$checklist_phase == "Exit",
                                              core_checklist_thin$checklist_item,
                                              core_checklist_thin$checklist_phase)

core_checklist_thin <- core_checklist_thin %>%
                       filter(!(checklist_phase == "GIA" & 
                                checklist_status != "Executed"))

core_checklist_thin <- core_checklist_thin %>%
                       mutate(year = substr(year(status_date), 3, 4),
                              checklist_phase = ifelse(checklist_phase == "Reassessment",
                                                       paste0(checklist_phase, " ", year),
                                                       checklist_phase)) %>%
                       select(-c(year, checklist_item, checklist_status))

## Including the Costs
core_checklist_thin_with_cost <- merge(core_checklist_thin, 
                                       long_simple_cost_data, 
                                       by = c("q_id", "checklist_phase"),
                                       all.x = TRUE)

core_checklist_thin_with_cost <- core_checklist_thin_with_cost %>%
                                 select(-capacity)

core_checklist_thin_with_cost <- core_checklist_thin_with_cost %>% 
                                 group_by(q_id) %>%
                                 arrange(status_date, .by_group = TRUE) %>%
                                 ungroup()

## Forcing GIA to come after Phase II
core_checklist_thin_with_cost <- core_checklist_thin_with_cost %>%
                                 group_by(q_id) %>%
                                 mutate(is_gia = checklist_phase == "GIA",
                                        is_phase_ii = checklist_phase == "Phase II Study",
                                        
                                        # Get dates (only works safely when there is one GIA and one Phase II row per project)
                                        gia_date = if_else(is_gia, status_date, as.Date(NA)),
                                        phase_ii_date = if_else(is_phase_ii, status_date, as.Date(NA)),
                                        
                                        # Propagate dates across the group
                                        gia_date = max(gia_date, na.rm = TRUE),
                                        phase_ii_date = max(phase_ii_date, na.rm = TRUE),
                                        
                                        # Should GIA be moved?
                                        move_gia = is_gia & gia_date < phase_ii_date,
                                        
                                        # Save original date
                                        original_status_date = if_else(move_gia, status_date, as.Date(NA)),
                                        
                                        # Adjust status_date
                                        status_date = if_else(move_gia, phase_ii_date + years(1), status_date)) %>%
                                ungroup() %>%
                                select(-is_gia, -is_phase_ii, -gia_date, -phase_ii_date, -move_gia)

## Making it so that GIA is Equivalent to Being Completed 
core_checklist_thin_with_cost <- core_checklist_thin_with_cost %>%
                                 arrange(q_id, status_date) %>%  # Ensure correct order in time
                                 group_by(q_id) %>%
                                 mutate(gia_occurred = checklist_phase == "GIA",
                                        gia_before = cumany(gia_occurred),  # TRUE from first GIA onward
                                        # Identify rows to be excluded:
                                        to_remove = gia_before & 
                                                    checklist_phase %in% c("WITHDRAWN", 
                                                                            "COMPLETED")) %>%
                                 filter(!to_remove) %>%  # Remove rows where GIA came before withdrawal/completion
                                 select(-gia_occurred, -gia_before, 
                                        -to_remove) %>%
                                 ungroup()

## Including an End Date of 12-31-2024 for ACTIVE Projects 
ids_needing_extra_row <- core_checklist_thin_with_cost %>%
                         group_by(q_id) %>%
                         summarize(has_gia = any(checklist_phase == "GIA"),
                                   has_withdrawn = any(checklist_phase == "WITHDRAWN"),
                                   has_cancelled = any(checklist_phase == "CANCELLED")) %>%
                         filter(!has_gia & !has_withdrawn & !has_cancelled) %>%
                         pull(q_id)

new_rows <- tibble(q_id = ids_needing_extra_row,
                   checklist_phase = NA,  # or a label like "ACTIVE_PLACEHOLDER"
                   status_date = as.Date("2024-12-31"),
                   POI_cost = NA,
                   network_cost = NA,
                   original_status_date = NA)

core_checklist_thin_with_cost <- bind_rows(core_checklist_thin_with_cost, 
                                           new_rows)

core_checklist_thin_with_cost <- core_checklist_thin_with_cost %>%
                                 arrange(q_id, status_date)

## Adding Additional Rows if the Number of Years Between First and Last Event 
## is More than the Number of Events
core_checklist_thin_with_cost <- core_checklist_thin_with_cost %>% 
                                 arrange(q_id, status_date) %>%
                                 group_by(q_id) %>%
                                 mutate(years_between = c(1, 
                                                          floor(as.numeric(diff(status_date)/365.25))),
                                        days_between = c(as.numeric(diff(status_date)),
                                                         NA)) %>%
                                 ungroup()


core_checklist_thin_with_cost <- core_checklist_thin_with_cost %>%
  left_join(
    CAISO_cost_data %>% select(q_id, flag_phase1, flag_phase2),
    by = "q_id"
  ) %>%
  mutate(
    data_source = case_when(
      checklist_phase == "Phase I Study"  ~ flag_phase1,
      checklist_phase == "Phase II Study" ~ flag_phase2,
      TRUE                                 ~ NA_character_
    )
  ) %>%
  select(-flag_phase1, -flag_phase2)



## collapse all "Reassessment 14", "Reassessment 20", etc. into "Reassessment"
core_checklist_thin_with_cost <- core_checklist_thin_with_cost %>%
  # 2) collapse any "Reassessment XX" into just "Reassessment"
  mutate(
    checklist_phase = if_else(
      str_detect(checklist_phase, "^Reassessment"),
      "Reassessment",
      checklist_phase
    )
  )

q_id_list <- unique(core_checklist_thin_with_cost$q_id)

dynamic_table <- data.frame()

for (id in q_id_list)
{
  project <- subset(core_checklist_thin_with_cost, 
                    core_checklist_thin_with_cost$q_id == id)
  number_rows <- nrow(project)
  
  
  
  additional_row <- data.frame(q_id = id,
                               checklist_phase = NA,
                               status_date = NA,
                               POI_cost = NA,
                               network_cost = NA,
                               original_status_date = NA,
                               years_between = 1)
  
  rows_to_insert <- list()  # Use a list to collect new rows efficiently
  insert_positions <- c()   # Track positions where rows should be inserted
  
  for(i in 1:number_rows)
  {
    years_in_between <- as.numeric(project[i, "years_between"])
    
    if (!is.na(years_in_between) & !(years_in_between == 1 | 
                                     years_in_between == 0)) 
    {
      # Create multiple empty rows efficiently
      new_rows <- additional_row[rep(1, years_in_between), ]
      rows_to_insert[[length(rows_to_insert) + 1]] <- new_rows
      insert_positions <- c(insert_positions, i)
    }
  }
  
  if (length(rows_to_insert) > 0) 
  {
    rows_to_insert <- bind_rows(rows_to_insert)  # Combine all new rows
    insert_positions <- unique(insert_positions) # Avoid duplicates
    
    # Use `bind_rows()` and `slice()` for insertion
    project <- bind_rows(slice(project, 
                               1:(insert_positions[1] - 1)),
                         rows_to_insert,
                         slice(project, insert_positions[1]:n()))
  }
  
  dynamic_table <- rbind(dynamic_table, project)
}
 


## Cleaning Row Numbers
rownames(dynamic_table) <- NULL

## Deleting Rows After GIA has Occurred 
clean_dynamic_table <- dynamic_table %>% group_by(q_id) %>%
                       mutate(gia_row = case_when(any(checklist_phase == "GIA") ~ min(which(checklist_phase == "GIA")),
                                                  TRUE ~ Inf)) %>%
                       filter(row_number() <= gia_row) %>%
                       select(-gia_row) %>%
                       ungroup()

## Creating Column for Decision After Receiving Information
clean_dynamic_table <- clean_dynamic_table %>% group_by(q_id) %>%
                       mutate(next_status = lead(checklist_phase),
                              is_last_row = row_number() == n(),
                              action = case_when(next_status == "GIA" ~ "contract",
                                                 next_status == "WITHDRAWN" ~ "exit",
                                                 next_status == "CANCELLED" ~ "exit",
                                                 is.na(next_status) & 
                                                 is_last_row & 
                                                 (!(checklist_phase == "GIA") | 
                                                    is.na(checklist_phase)) ~ "active",
                                                 checklist_phase == "GIA" & 
                                                 is_last_row ~ "",
                                                 !is.na(next_status) & !is_last_row ~ "continue",
                                                 TRUE ~ "continue")) %>% 
                      select(-c(next_status, is_last_row)) %>%
                      ungroup()

## Deleting Rows that Indicate Exit Status
clean_dynamic_table <- clean_dynamic_table %>%
                       filter(is.na(checklist_phase) | 
                              !(checklist_phase == "WITHDRAWN" |
                                checklist_phase == "COMPLETED" |
                                checklist_phase == "CANCELLED"))

## Deleting Rows that Indicate GIA if GIA Occurred within 1 Year of Ph2 Study
# clean_dynamic_table <- clean_dynamic_table %>%
#                        group_by(q_id) %>%
#                        mutate(previous_phase = lag(checklist_phase),
#                               previous_action = lag(action),
#                               GIA_within_1_year = ifelse(previous_phase == "Phase II Study" &
#                                                          previous_action == "GIA",
#                                                          1, 0)) %>%
#                        select(-c(previous_phase, previous_action))
# 
# clean_dynamic_table <- clean_dynamic_table %>%
#                        filter(!(checklist_phase == "GIA" & GIA_within_1_year == 1)) %>%
#                        select(-GIA_within_1_year)

## Forward Filling POI and Network Costs
clean_dynamic_table <- clean_dynamic_table %>%
                       group_by(q_id) %>%
                       fill(POI_cost, network_cost, .direction = "down") %>%
                       ungroup()

## Adding Column for Indicating if Cost is Forward Filled or Observed
clean_dynamic_table <- clean_dynamic_table %>% 
                       group_by(q_id) %>%
                       mutate(POI_costs_carried_forward = ifelse(!(POI_cost == lag(POI_cost)) | 
                                                                 is.na(lag(POI_cost)),
                                                                 0, 1),
                              network_costs_carried_forward = ifelse(!(network_cost == lag(network_cost)) | 
                                                                      (is.na(lag(network_cost))),
                                                                     0, 1))

## Adding Column for Censoring Indicator for Active Projects
clean_dynamic_table <- clean_dynamic_table %>%
                       group_by(q_id) %>%
                       mutate(active_project = ifelse(action == "active", 1, 0))

## Removing "active" Action and Replacing with "continue"
clean_dynamic_table$action <- ifelse(clean_dynamic_table$action == "active",
                                     "continue", clean_dynamic_table$action)

## Incorporating Model Year
clean_dynamic_table <- clean_dynamic_table %>% group_by(q_id) %>%
                       mutate(model_year = row_number() - 1) %>%
                       relocate(model_year, .before = "q_id")

## Removing Extraneous Columns
final_dynamic_table <- clean_dynamic_table %>%
                       select(-c(years_between, days_between))

final_dynamic_table <- final_dynamic_table %>% 
  rename(
  cost_poi     = POI_cost,
  cost_network = network_cost
) 


###############################################
#### Some core checklist rows do not have a Phase 1 or 2 study but we have cost data, so creating fake rows

# A) Precompute the “previous” model_years for Phase I and Phase II

library(dplyr)
library(knitr)

# A) Precompute the “previous” model_years
prev_ir <- final_dynamic_table %>%
  filter(checklist_phase == "Interconnection Request") %>%
  select(q_id, model_year) %>%
  rename(prev_year = model_year)

prev_p1 <- final_dynamic_table %>%
  filter(checklist_phase == "Phase I Study") %>%
  select(q_id, model_year) %>%
  rename(prev_year = model_year)


# B) Which q_ids need fake Phase I or Phase II?
p1_missing <- CAISO_cost_data %>%
  filter(!is.na(phase_1_total_POI_cost_per_kW) |
           !is.na(phase_1_total_network_cost_per_kW)) %>%
  distinct(q_id) %>%
  anti_join(final_dynamic_table %>% filter(checklist_phase == "Phase I Study"),
            by = "q_id") %>%
  pull(q_id)

p2_missing <- CAISO_cost_data %>%
  filter(!is.na(phase_2_total_POI_cost_per_kW) |
           !is.na(phase_2_total_network_cost_per_kW)) %>%
  distinct(q_id) %>%
  anti_join(final_dynamic_table %>% filter(checklist_phase == "Phase II Study"),
            by = "q_id") %>%
  pull(q_id)


# C) Fake IR rows for any p1_missing without an IR
fake_ir <- tibble(q_id = p1_missing) %>%
  anti_join(prev_ir, by = "q_id") %>%
  # assign data_source = NA (no cost) and fake_row=1
  transmute(
    q_id,
    model_year      = 0L,
    checklist_phase = "Interconnection Request",
    cost_poi        = NA_real_,
    cost_network    = NA_real_,
    data_source     = NA_character_,
    fake_row        = 1L
  )


# D) Fake Phase I rows
fake_p1 <- CAISO_cost_data %>%
  filter(q_id %in% p1_missing) %>%
  # bring in whichever previous year we have (IR or none)
  left_join(prev_ir, by = "q_id") %>%
  transmute(
    q_id,
    model_year      = if_else(!is.na(prev_year), prev_year + 1L, 1L),
    checklist_phase = "Phase I Study",
    cost_poi        = phase_1_total_POI_cost_per_kW,
    cost_network    = phase_1_total_network_cost_per_kW,
    data_source     = flag_phase1,
    fake_row        = 1L
  )


# E) Fake Phase II rows (default to 2 if no P1)
fake_p2 <- CAISO_cost_data %>%
  filter(q_id %in% p2_missing) %>%
  left_join(prev_p1, by = "q_id") %>%
  transmute(
    q_id,
    model_year      = if_else(!is.na(prev_year), prev_year + 1L, 2L),
    checklist_phase = "Phase II Study",
    cost_poi        = phase_2_total_POI_cost_per_kW,
    cost_network    = phase_2_total_network_cost_per_kW,
    data_source     = flag_phase2,
    fake_row        = 1L
  )


# F) Augment the table
final_dynamic_table <- final_dynamic_table %>%
  mutate(fake_row = 0L) %>%
  bind_rows(fake_ir, fake_p1, fake_p2)


# G) Report how many fake rows we added, by phase & source
fake_summary <- final_dynamic_table %>%
  filter(fake_row == 1L) %>%
  # total per phase & data_source
  count(checklist_phase, data_source, name = "Fake Rows Added") %>%
  rename(
    `Checklist Phase` = checklist_phase,
    `Data Source`     = data_source
  )

# H) Print
kable(
  fake_summary,
  digits = 0,
  align  = c("l","l","r")
)



 
## Exporting the Table
# write_csv(final_dynamic_table,
#           paste0(project_root, "/data/working/dynamic_table.csv"))

 

# 1) Extract only the fake rows
fakes <- final_dynamic_table  %>% 
  filter(fake_row == 1L)

# 2) Build an aggregated summary:
summary_fake <- fakes %>%
  # bucket phases into just the three groups
  mutate(
    phase_group = case_when(
      checklist_phase == "Phase I Study"  ~ "Phase I Study",
      checklist_phase == "Phase II Study" ~ "Phase II Study",
      TRUE                                ~ "All Phases"
    )
  ) %>%
  # **aggregate** counts per phase_group & source
  group_by(phase_group, data_source) %>%
  summarise(n = n(), .groups = "drop") %>%
  # reshape wide so scraped/kiteworks are columns
  pivot_wider(
    names_from  = data_source,
    values_from = n,
    values_fill = 0
  ) %>%
  # put the rows in the desired order
  mutate(
    phase_group = factor(
      phase_group,
      levels = c("All Phases", "Phase I Study", "Phase II Study"),
      ordered = TRUE
    )
  ) %>%
  arrange(phase_group) %>%
  rename(
    `Checklist Phase` = phase_group,
    Scraped           = scraped,
    Kiteworks         = kiteworks
  )

# 3) Print your clean frequency table
kable(
  summary_fake,
  digits = 0,
  align  = c("l","r","r")
)




## Creating Codebook
codebook <- data.frame(value = c("model_year", "q_id", "checklist_phase",
                                 "status_date", "cost_poi", "cost_network",
                                 "original_status_date", "action",
                                 "POI_costs_carried_forward", 
                                 "network_costs_carried_forward",
                                 "active_project"),
                       description = c("number of years project has been in queue in model years",
                                       "unique queue identification for project",
                                       "the phase that a project is in during the interconnection process",
                                       "the date that a particular phase was reached",
                                       "the cost of a project's facility/POI upgrades",
                                       "the cost of a project's network upgrades",
                                       "the status date may have been incorrect, so this is the original status date as it appeared in the data",
                                       "the action that a project takes at a particular step in the interconnection process",
                                       "1 if the costs are not newly observed in the data; 0 otherwise",
                                       "1 if the costs are not newly observed in the data; 0 otherwise",
                                       "1 if a project remains in the queue after this point but we can no longer see it in the data; 0 otherwise"))

## Exporting Data as Workbook
wb <- createWorkbook()

addWorksheet(wb, "data")
addWorksheet(wb, "codebook")

writeData(wb, "data", final_dynamic_table)
writeData(wb, "codebook", codebook)

saveWorkbook(wb, paste0(project_root, "/data/input/dynamic_table.xlsx"), 
             overwrite = TRUE)


data_1837c <- CAISO_cost_data[ CAISO_cost_data$q_id == "1837", ]
data_1837 <- kiteworks_cost_data[ kiteworks_cost_data$q_id == "1837", ]

 










# 1) Pull out the grouping values
phases <- final_dynamic_table %>% 
  pull(checklist_phase) %>% 
  unique()

# Restrict to Year 0, 1, 2
years <- c(0, 1, 2)

# 2) Helper to compute all your metrics on any slice
compute_metrics <- function(df) {
  tibble(
    Metric              = c(
      "No. of observations",
      "No. of projects",
      "Valid q_ids",
      "Missing q_ids",
      "Scraped q_ids",
      "Kiteworks q_ids",
      "POI: Valid",
      "POI: Missing",
      "POI: Zero",
      "POI: Inf",
      "Network: Valid",
      "Network: Missing",
      "Network: Zero",
      "Network: Inf"
    ),
    Value = c(
      nrow(df),
      n_distinct(df$q_id),
      sum(!is.na(df$data_source)),
      sum(is.na(df$data_source)),
      sum(df$data_source == "scraped",   na.rm = TRUE),
      sum(df$data_source == "kiteworks", na.rm = TRUE),
      sum(!is.na(df$cost_poi)),
      sum(is.na(df$cost_poi)),
      sum(near(df$cost_poi,  0), na.rm = TRUE),
      sum(is.infinite(df$cost_poi), na.rm = TRUE),
      sum(!is.na(df$cost_network)),
      sum(is.na(df$cost_network)),
      sum(near(df$cost_network,  0), na.rm = TRUE),
      sum(is.infinite(df$cost_network), na.rm = TRUE)
    )
  )
}

# 3) Compute for the full dataset
all_stats <- compute_metrics(final_dynamic_table) %>%
  rename(`Dataset` = Value)

# 4) Compute for each phase
phase_stats <- map_dfc(phases, function(ph) {
  compute_metrics(filter(final_dynamic_table, checklist_phase == ph)) %>%
    pull(Value)
})
colnames(phase_stats) <- phases

# 5) Compute for Years 0,1,2
year_stats <- map_dfc(years, function(y) {
  compute_metrics(filter(final_dynamic_table, model_year == y)) %>%
    pull(Value)
})
colnames(year_stats) <- paste0("Year ", years)

# 6) Stitch into one table
summary_table <- bind_cols(
  Metric   = all_stats$Metric,
  Dataset  = all_stats$Dataset,
  phase_stats,
  year_stats
)

# 7) Print with kable()
kable(
  summary_table,
  col.names = c("Metric", "dynamic panel", phases, paste0("Year ", years)),
  digits   = 0,
  align    = c("l", rep("r", length(phases) + length(years) + 1))
)

# 8) Export via stargazer
stargazer(
  summary_table %>% rename(dynamic_panel = Dataset),
  summary   = FALSE,
  rownames  = FALSE,
  style     = "qje",
  type      = "latex",
  title = "Cost Data Summary - Dynamic Panel",
  out       = paste0(project_root,
                     "/output/tables/project_dynamic/dynamic_panel_summary.tex")
)
stargazer(
  summary_table %>% rename(dynamic_panel = Dataset),
  summary   = FALSE,
  rownames  = FALSE,
  style     = "qje",
  type      = "html",
  out       = paste0(project_root,
                     "/output/tables/project_dynamic/dynamic_panel_summary.html")
)



library(dplyr)
library(purrr)
library(knitr)
library(stargazer)

# 1) Base datasets
base_datasets <- list(
  dynamic_panel   = final_dynamic_table,
  CAISO_cost_data = CAISO_cost_data,
  CAISO_scraped   = CAISO_cost_data %>% filter(flag_phase1 == "scraped"  | flag_phase2 == "scraped"),
  CAISO_kiteworks = CAISO_cost_data %>% filter(flag_phase1 == "kiteworks"| flag_phase2 == "kiteworks"),
  core_checklist  = core_checklist
)

# 2) Unique phases
core_phases <- core_checklist %>% pull(checklist_phase) %>% unique()

# 3) Build a list of phase‐filtered datasets named by the raw phase
phase_datasets <- map(core_phases, ~ filter(core_checklist, checklist_phase == .x))
names(phase_datasets) <- core_phases

# 4) Combine into one list
all_datasets <- c(base_datasets, phase_datasets)

# 5) Helper to compute the four metrics
get_metrics <- function(df, name) {
  valid_flag <- if (identical(name, "dynamic_panel")) {
    !is.na(df$data_source)
  } else if (name %in% c("CAISO_cost_data","CAISO_scraped","CAISO_kiteworks")) {
    !is.na(df$flag_phase1) | !is.na(df$flag_phase2)
  } else {
    # core_checklist and its phase slices
    !is.na(df$q_id)
  }
  tibble(
    Metric = c(
      "Total observations",
      "Total projects",
      "Valid observations",
      "Missing observations"
    ),
    !!name := c(
      nrow(df),
      n_distinct(df$q_id),
      sum(valid_flag,   na.rm = TRUE),
      sum(!valid_flag,  na.rm = TRUE)
    )
  )
}

# 6) Compute & merge
metric_tables <- imap(all_datasets, get_metrics)
summary_wide  <- reduce(metric_tables, left_join, by = "Metric")

# 7) Display
kable(
  summary_wide,
  col.names = c("Metric", names(all_datasets)),
  digits   = 0,
  align    = c("l", rep("r", length(all_datasets)))
)

# 8) Export via stargazer
stargazer(
  summary_wide,
  summary    = FALSE,
  rownames   = FALSE,
  style      = "qje",
  na.string  = "",
  type       = "latex",
  out        = paste0(project_root, "/output/tables/project_dynamic/data_source_comparison_with_phases.tex")
)
stargazer(
  summary_wide,
  summary    = FALSE,
  rownames   = FALSE,
  style      = "qje",
  na.string  = "",
  type       = "html",
  out        = paste0(project_root, "/output/tables/project_dynamic/data_source_comparison_with_phases.html")
)






library(dplyr)
library(purrr)
library(tidyr)
library(knitr)
library(stargazer)

# 1) Phases in order
phases <- final_dynamic_table %>% pull(checklist_phase) %>% unique()

# 2) Datasets
datasets <- list(
  dynamic_panel   = final_dynamic_table,
  CAISO_cost_data = CAISO_cost_data,
  CAISO_scraped   = CAISO_cost_data %>% filter(flag_phase1 == "scraped"  | flag_phase2 == "scraped"),
  CAISO_kiteworks = CAISO_cost_data %>% filter(flag_phase1 == "kiteworks"| flag_phase2 == "kiteworks"),
  core_checklist  = core_checklist
)

# 3) Metrics
metrics <- c(
  "No. observations",
  "No. missing observations",
  "No. projects",
  "No. missing projects",
  "No. Inf projects"
)

# 4) Fixed helper
compute_cell <- function(df, name, metric, phase) {
  # 4a) slice/shape into q_id + cost_poi + cost_network where appropriate
  df_phase <- if (name == "dynamic_panel") {
    df %>% 
      filter(checklist_phase == phase) %>%
      select(q_id, cost_poi, cost_network)
  } else if (name == "core_checklist") {
    df %>% filter(checklist_phase == phase)
  } else {
    # CAISO datasets
    if (!is.na(phase) && phase == "Phase I Study") {
      df %>% transmute(q_id,
                       cost_poi     = phase_1_total_POI_cost_per_kW,
                       cost_network = phase_1_total_network_cost_per_kW)
    } else if (!is.na(phase) && phase == "Phase II Study") {
      df %>% transmute(q_id,
                       cost_poi     = phase_2_total_POI_cost_per_kW,
                       cost_network = phase_2_total_network_cost_per_kW)
    } else {
      # nothing for other phases (or NA)
      tibble(q_id=character(), cost_poi=numeric(), cost_network=numeric())
    }
  }
  
  # 4b) compute the metric
  switch(metric,
         "No. observations" = nrow(df_phase),
         
         "No. missing observations" = if ("cost_poi" %in% names(df_phase)) {
           sum(is.na(df_phase$cost_poi) & is.na(df_phase$cost_network))
         } else NA_integer_,
         
         "No. projects" = n_distinct(df_phase$q_id),
         
         "No. missing projects" = if ("cost_poi" %in% names(df_phase)) {
           df_phase %>%
             filter(is.na(cost_poi) & is.na(cost_network)) %>%
             pull(q_id) %>%
             unique() %>%
             length()
         } else NA_integer_,
         
         "No. Inf projects" = if ("cost_poi" %in% names(df_phase)) {
           df_phase %>%
             filter(is.infinite(cost_poi) | is.infinite(cost_network)) %>%
             pull(q_id) %>%
             unique() %>%
             length()
         } else NA_integer_
  )
}

# 5) Build summary: one row per Dataset×Metric
summary_tbl <- expand_grid(
  Dataset = names(datasets),
  Metric  = metrics
) %>%
  rowwise() %>%
  mutate(
    vals = list(map_int(phases, ~ compute_cell(
      df     = datasets[[Dataset]],
      name   = Dataset,
      metric = Metric,
      phase  = .x
    )))
  ) %>%
  unnest_wider(vals, names_sep = "_") %>%
  ungroup()

# 6) Rename phase columns
colnames(summary_tbl)[-(1:2)] <- phases

# 7) Print
kable(
  summary_tbl,
  col.names = c("Dataset", "Metric", phases),
  digits   = 0,
  align    = c("l","l", rep("r", length(phases)))
)

# 8) Export via stargazer
stargazer(
  summary_tbl,
  summary    = FALSE,
  rownames   = FALSE,
  style      = "qje",
  title = "Cost Data Summary from different sources",
  na.string  = "",
  type       = "latex",
  out        = paste0(project_root, "/output/tables/project_dynamic/dataset_phase_metrics.tex")
)
stargazer(
  summary_tbl,
  summary    = FALSE,
  rownames   = FALSE,
  style      = "qje",
  title = "Cost Data Summary from different sources",
  na.string  = "",
  type       = "html",
  out        = paste0(project_root, "/output/tables/project_dynamic/dataset_phase_metrics.html")
)




#########################################################

# get the set of q_ids in the dynamic panel by phase
dyn_p1_ids <- final_dynamic_table %>%
  filter(checklist_phase == "Phase I Study") %>%
  distinct(q_id)

dyn_p2_ids <- final_dynamic_table %>%
  filter(checklist_phase == "Phase II Study") %>%
  distinct(q_id)

# 1) Phase I: scraped but missing in dynamic
p1_scraped_not_in_dyn <- CAISO_cost_data %>%
  filter(flag_phase1 == "scraped") %>%
  distinct(q_id) %>%
  anti_join(dyn_p1_ids, by = "q_id")

# 2) Phase I: kiteworks but missing in dynamic
p1_kiteworks_not_in_dyn <- CAISO_cost_data %>%
  filter(flag_phase1 == "kiteworks") %>%
  distinct(q_id) %>%
  anti_join(dyn_p1_ids, by = "q_id")

# 3) Phase II: scraped but missing in dynamic
p2_scraped_not_in_dyn <- CAISO_cost_data %>%
  filter(flag_phase2 == "scraped") %>%
  distinct(q_id) %>%
  anti_join(dyn_p2_ids, by = "q_id")

# 4) Phase II: kiteworks but missing in dynamic
p2_kiteworks_not_in_dyn <- CAISO_cost_data %>%
  filter(flag_phase2 == "kiteworks") %>%
  distinct(q_id) %>%
  anti_join(dyn_p2_ids, by = "q_id")



##
library(dplyr)

# 1) Helper: get the “missing” q_ids for any flag & phase
get_missing_ids <- function(phase, flag_col, flag_val) {
  dyn_ids <- final_dynamic_table %>%
    filter(checklist_phase == phase) %>%
    distinct(q_id)
  
  CAISO_cost_data %>%
    filter(.data[[flag_col]] == flag_val) %>%
    distinct(q_id) %>%
    anti_join(dyn_ids, by = "q_id") %>%
    pull(q_id)
}

# 2) Define the four cases
cases <- tribble(
  ~phase,           ~flag_col,      ~flag_val,       ~label,
  "Phase I Study",  "flag_phase1",  "scraped",       "P1-scraped",
  "Phase I Study",  "flag_phase1",  "kiteworks",     "P1-kiteworks",
  "Phase II Study", "flag_phase2",  "scraped",       "P2-scraped",
  "Phase II Study", "flag_phase2",  "kiteworks",     "P2-kiteworks"
)

# 3) Loop over them
for (i in seq_len(nrow(cases))) {
  ph    <- cases$phase[i]
  col   <- cases$flag_col[i]
  val   <- cases$flag_val[i]
  lbl   <- cases$label[i]
  
  cat("\n=== CASE:", lbl, "(", ph, "/", val, ") ===\n")
  
  missing_qids <- get_missing_ids(ph, col, val)
  cat("Number missing:", length(missing_qids), "\n")
  if (length(missing_qids) == 0) next
  
  cat("\n-- CAISO_cost_data details --\n")
  print(n = 100,
    CAISO_cost_data %>%
      filter(q_id %in% missing_qids) %>%
      select(q_id, capacity,
             phase_1_total_POI_cost, phase_1_total_network_cost,
             phase_2_total_POI_cost, phase_2_total_network_cost,
             !!sym(col))
  )
  
  cat("\n-- raw core_checklist phases seen --\n")
  print(n = 100,
    core_checklist %>%
      filter(q_id %in% missing_qids) %>%
      count(q_id, checklist_phase)
  )
  
  cat("\n-- pre-filtered core_checklist_thin_with_cost --\n")
  print( n = 100,
    core_checklist_thin_with_cost %>%
      filter(q_id %in% missing_qids) %>%
      select(q_id, checklist_phase, status_date)
  )
  
  cat("\n--------------------------------\n")
}






###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
#Old Code

# --------------------------------------------
#  Cluster‐level overlap tables for Phase 1 and Phase 2
# --------------------------------------------

## =====Cluster‐level overlap tables for Phase 1 and Phase 2 (cluster as character & numeric sort)=====
kite_a   <- kiteworks_cost_data %>%
  filter(is.na(kiteworks_phase_1_total_POI_cost) | is.na(kiteworks_phase_1_total_network_cost)) 
# Prepare dataframes: ensure cluster and q_id are characters
kite_all   <- kiteworks_cost_data %>%
  filter(!is.na(kiteworks_phase_1_total_POI_cost) | !is.na(kiteworks_phase_1_total_network_cost)) %>% 
  mutate(q_id = as.character(q_id),
         cluster = as.character(cluster))

kite2      <- phase2_kiteworks_data %>%          # your filtered Phase 2-only Kiteworks
  mutate(q_id = as.character(q_id),
         cluster = as.character(cluster))

scr1_df    <- Phase1_cost_data %>%
  mutate(q_id = as.character(q_id),
         cluster = as.character(cluster))

scr2_df    <- Phase2_cost_data %>%
  mutate(q_id = as.character(q_id),
         cluster = as.character(cluster))

build_cluster_overlap <- function(kite, scrape) {
  # distinct pairs
  kite_ids <- kite   %>% distinct(cluster, q_id)
  scr_ids  <- scrape %>% distinct(cluster, q_id)
  
  # compute counts
  kite_only   <- kite_ids %>% filter(!q_id %in% scr_ids$q_id) %>%
    count(cluster, name = "Only_in_kiteworks")
  scrape_only <- scr_ids  %>% filter(!q_id %in% kite_ids$q_id) %>%
    count(cluster, name = "Only_in_scrape")
  # In_both via intersect per cluster
  both <- kite_ids %>%
    group_by(cluster) %>%
    summarise(
      In_both = length(
        intersect(
          q_id,
          scr_ids$q_id[scr_ids$cluster == unique(cluster)]
        )
      )
    ) %>%
    ungroup()
  
  # full join, fill zeros, force cluster char
  tbl <- full_join(kite_only, scrape_only, by = "cluster") %>%
    full_join(both,         by = "cluster") %>%
    replace_na(list(
      Only_in_kiteworks = 0,
      Only_in_scrape    = 0,
      In_both           = 0
    )) %>%
    mutate(cluster = as.character(cluster))
  
  # total row
  total_row <- tibble(
    cluster            = "TOTAL",
    Only_in_kiteworks  = sum(tbl$Only_in_kiteworks),
    Only_in_scrape     = sum(tbl$Only_in_scrape),
    In_both            = sum(tbl$In_both)
  )
  
  # sort numeric clusters then append TOTAL
  body <- tbl %>%
    filter(cluster != "TOTAL") %>%
    arrange(as.numeric(cluster))
  
  bind_rows(body, total_row)
}

# Build tables
phase1_cluster_overlap <- build_cluster_overlap(kite_all, scr1_df)
phase2_cluster_overlap <- build_cluster_overlap(kite2,     scr2_df)

# Export with stargazer (LaTeX + HTML) in your existing folder
phase1_cluster_overlap <- phase1_cluster_overlap %>%
  mutate(Total = Only_in_kiteworks + Only_in_scrape + In_both)
phase2_cluster_overlap <- phase2_cluster_overlap %>%
  mutate(Total = Only_in_kiteworks + Only_in_scrape + In_both)

# Re‐export updated overlap tables
for (info in list(
  list(df = phase1_cluster_overlap, name = "phase1_cluster_overlap"),
  list(df = phase2_cluster_overlap, name = "phase2_cluster_overlap")
)) {
  
  # grab the single digit after “phase”
  ph <- substr(info$name, 6, 6)
  cap <- paste("Phase", ph, "Overlaps")
  stargazer(
    info$df,
    summary   = FALSE,
    rownames  = FALSE,
    style     = "qje",
    type      = "latex",
    title =    cap,
    out       = paste0(project_root, "/output/tables/project_dynamic/", info$name, ".tex")
  )
  stargazer(
    info$df,
    summary   = FALSE,
    rownames  = FALSE,
    style     = "qje",
    type      = "html",
    out       = paste0(project_root, "/output/tables/project_dynamic/", info$name, ".html")
  )
}


## ===== Zero‐cost by cluster, Kiteworks only (Phase 1 & Phase 2)=====
# Phase 1 Kiteworks: keep only those with any Phase 1 cost
kite1 <- kiteworks_cost_data %>%
  filter(
    !is.na(kiteworks_phase_1_total_POI_cost) |
      !is.na(kiteworks_phase_1_total_network_cost)
  ) %>%
  mutate(
    q_id    = as.character(q_id),
    cluster = as.character(cluster)
  )

# Phase 2 Kiteworks
kite2 <- phase2_kiteworks_data %>%
  mutate(
    q_id    = as.character(q_id),
    cluster = as.character(cluster)
  )

build_zero_by_cluster <- function(df, poi_col, net_col) {
  df %>%
    group_by(cluster) %>%
    summarise(
      Zero_POI     = sum(.data[[poi_col]] == 0, na.rm = TRUE),
      Zero_Network = sum(.data[[net_col]] == 0, na.rm = TRUE),
      Total_qids   = n_distinct(q_id),
      .groups      = "drop"
    ) %>%
    # sort clusters numerically
    arrange(as.numeric(cluster)) %>%
    # add total row
    bind_rows(
      tibble(
        cluster      = "TOTAL",
        Zero_POI     = sum(.$Zero_POI),
        Zero_Network = sum(.$Zero_Network),
        Total_qids   = sum(.$Total_qids)
      )
    )
}

# Build both tables
phase1_kite_zero <- build_zero_by_cluster(
  kite1,
  poi_col = "kiteworks_phase_1_total_POI_cost",
  net_col = "kiteworks_phase_1_total_network_cost"
)

phase2_kite_zero <- build_zero_by_cluster(
  kite2,
  poi_col = "kiteworks_phase_2_total_POI_cost",
  net_col = "kiteworks_phase_2_total_network_cost"
)

# Export via stargazer
for (info in list(
  list(df = phase1_kite_zero, name = "phase1_kite_zero"),
  list(df = phase2_kite_zero, name = "phase2_kite_zero")
)) {
  # grab the single digit after “phase”
  ph <- substr(info$name, 6, 6)
  cap <- paste("Phase", ph, "Zero Costs from Kiteworks")
  stargazer(
    info$df,
    summary   = FALSE,
    rownames  = FALSE,
    style     = "qje",
    type      = "latex",
    title =  cap,
    out       = paste0(project_root,
                       "/output/tables/project_dynamic/", info$name, ".tex")
  )
  stargazer(
    info$df,
    summary   = FALSE,
    rownames  = FALSE,
    style     = "qje",
    type      = "html",
    out       = paste0(project_root,
                       "/output/tables/project_dynamic/", info$name, ".html")
  )
}


## ===== Zero‐cost by cluster, Scraped‐only (not in Kiteworks) =====

# 1) Identify the scraped‐only q_ids for each phase
kite1_ids <- kiteworks_cost_data %>%
  filter(!is.na(kiteworks_phase_1_total_POI_cost) |
           !is.na(kiteworks_phase_1_total_network_cost)) %>%
  pull(q_id) %>% as.character()

scr1_only <- Phase1_cost_data %>%
  mutate(q_id = as.character(q_id),
         poi_1 = cost_poi_own     * 1000,
         net_1 = cost_network_own * 1000,
         cluster = as.character(cluster)) %>%
  filter(!q_id %in% kite1_ids)

kite2_ids <- phase2_kiteworks_data$q_id %>% as.character()

scr2_only <- Phase2_cost_data %>%
  mutate(q_id = as.character(q_id),
         poi_2 = cost_poi_own     * 1000,
         net_2 = cost_network_own * 1000,
         cluster = as.character(cluster)) %>%
  filter(!q_id %in% kite2_ids)

# 2) Helper to build the zero‐cost table for scraped‐only
build_zero_scraped_only <- function(df, poi_col, net_col) {
  df %>%
    group_by(cluster) %>%
    summarise(
      Zero_POI     = sum(.data[[poi_col]] == 0, na.rm = TRUE),
      Zero_Network = sum(.data[[net_col]] == 0, na.rm = TRUE),
      Total_qids   = n_distinct(q_id),
      .groups      = "drop"
    ) %>%
    arrange(as.numeric(cluster)) %>%
    bind_rows(
      tibble(
        cluster      = "TOTAL",
        Zero_POI     = sum(.$Zero_POI),
        Zero_Network = sum(.$Zero_Network),
        Total_qids   = sum(.$Total_qids)
      )
    )
}

# 3) Build the two tables
phase1_scr_only_zero <- build_zero_scraped_only(
  scr1_only,
  poi_col = "poi_1",
  net_col = "net_1"
)

phase2_scr_only_zero <- build_zero_scraped_only(
  scr2_only,
  poi_col = "poi_2",
  net_col = "net_2"
)

# 4) Export via stargazer (LaTeX + HTML)
for (info in list(
  list(df = phase1_scr_only_zero, name = "phase1_scrape_only_zero"),
  list(df = phase2_scr_only_zero, name = "phase2_scrape_only_zero")
)) {
  # grab the single digit after “phase”
  ph <- substr(info$name, 6, 6)
  cap <- paste("Phase", ph, "Zero Costs from Scraped Data Only (Not overlap with Kiteworks)")
  stargazer(
    info$df,
    summary   = FALSE,
    rownames  = FALSE,
    style     = "qje",
    type      = "latex",
    title = cap,
    out       = paste0(
      project_root,
      "/output/tables/project_dynamic/",
      info$name, ".tex"
    )
  )
  stargazer(
    info$df,
    summary   = FALSE,
    rownames  = FALSE,
    style     = "qje",
    type      = "html",
    out       = paste0(
      project_root,
      "/output/tables/project_dynamic/",
      info$name, ".html"
    )
  )
}

## =====Zero‐cost by cluster, Master data (CAISO_cost_data) =====
# Ensure q_id and cluster are character
master_df <- CAISO_cost_data %>%
  mutate(
    q_id    = as.character(q_id),
    cluster = as.character(cluster)
  )

# Helper to build zero‐cost tables from any df & cost columns
build_zero_master <- function(df, poi_col, net_col) {
  df %>%
    # only include rows where at least one cost is non‐missing
    filter(!is.na(.data[[poi_col]]) | !is.na(.data[[net_col]])) %>%
    group_by(cluster) %>%
    summarise(
      Zero_POI     = sum(.data[[poi_col]]     == 0, na.rm = TRUE),
      Zero_Network = sum(.data[[net_col]]     == 0, na.rm = TRUE),
      Total_qids   = n_distinct(q_id),
      .groups      = "drop"
    ) %>%
    arrange(as.numeric(cluster)) %>%
    bind_rows(
      tibble(
        cluster      = "TOTAL",
        Zero_POI     = sum(.$Zero_POI),
        Zero_Network = sum(.$Zero_Network),
        Total_qids   = sum(.$Total_qids)
      )
    )
}

# Phase 1 zero‐cost table on master
phase1_master_zero <- build_zero_master(
  master_df,
  poi_col = "phase_1_total_POI_cost",
  net_col = "phase_1_total_network_cost"
)

# Phase 2 zero‐cost table on master
phase2_master_zero <- build_zero_master(
  master_df,
  poi_col = "phase_2_total_POI_cost",
  net_col = "phase_2_total_network_cost"
)

# Export via stargazer
for (info in list(
  list(df = phase1_master_zero, name = "phase1_master_zero"),
  list(df = phase2_master_zero, name = "phase2_master_zero")
)) {
  # grab the single digit after “phase”
  ph <- substr(info$name, 6, 6)
  cap <- paste("Phase", ph, "Zero Costs from Merged Kiteworks and Scraped Data")
  stargazer(
    info$df,
    summary   = FALSE,
    rownames  = FALSE,
    style     = "qje",
    type      = "latex",
    title = cap,
    out       = paste0(
      project_root,
      "/output/tables/project_dynamic/",
      info$name, ".tex"
    )
  )
  stargazer(
    info$df,
    summary   = FALSE,
    rownames  = FALSE,
    style     = "qje",
    type      = "html",
    out       = paste0(
      project_root,
      "/output/tables/project_dynamic/",
      info$name, ".html"
    )
  )
}









## ======= Checking Kiteworks vs Scraped Data ========
# 1) THREE-COLUMN SUMMARY FOR POI & NETWORK
tbl_match3 <- CAISO_cost_data %>%
  summarise(
    # POI
    POI_KiteworksMatch = sum(!is.na(kiteworks_phase_1_total_POI_cost) &
                               kiteworks_phase_1_total_POI_cost == phase_1_total_POI_cost,
                             na.rm = TRUE),
    POI_Mismatch       = sum(!is.na(kiteworks_phase_1_total_POI_cost) &
                               kiteworks_phase_1_total_POI_cost != phase_1_total_POI_cost,
                             na.rm = TRUE),
    POI_PDFonly        = sum( is.na(kiteworks_phase_1_total_POI_cost) &
                                !is.na(phase_1_total_POI_cost),
                              na.rm = TRUE),
    # Network
    Network_KiteworksMatch = sum(!is.na(kiteworks_phase_1_total_network_cost) &
                                   kiteworks_phase_1_total_network_cost == phase_1_total_network_cost,
                                 na.rm = TRUE),
    Network_Mismatch       = sum(!is.na(kiteworks_phase_1_total_network_cost) &
                                   kiteworks_phase_1_total_network_cost != phase_1_total_network_cost,
                                 na.rm = TRUE),
    Network_PDFonly        = sum( is.na(kiteworks_phase_1_total_network_cost) &
                                    !is.na(phase_1_total_network_cost),
                                  na.rm = TRUE)
  ) %>%
  # reshape to 2×3
  pivot_longer(everything(),
               names_to = c("Measure","Status"),
               names_sep = "_") %>%
  pivot_wider(names_from = Status, values_from = value)



# export with stargazer
stargazer(tbl_match3,
          type     = "latex",
          title    = "POI & Network: Kiteworksvs IC studies",
          summary  = FALSE,
          rownames = FALSE,
          out      = paste0(project_root, "/output/tables/table_CAISO_ICstudies_mismatch_count.tex"))

stargazer(tbl_match3,
          type     = "html",
          title    = "POI & Network: Kiteworksvs IC studies",
          summary  = FALSE,
          rownames = FALSE,
          out      = paste0(project_root, "/output/tables/table_CAISO_ICstudies_mismatch_count.html"))


# 2) MISMATCH-SIZE FREQUENCY (TRUE MISMATCHES ONLY)
# define your new breaks and labels
bins <- c(0,
          1e3,      # 1 k
          1e4,      # 10 k
          1e5,      # 100 k
          2e5,      # 200 k
          3e5,      # 300 k
          4e5,      # 400 k
          5e5,      # 500 k
          Inf)
labs <- c(
  "<1k",
  "1k–10k",
  "10k–100k",
  "100k–200k",
  "200k–300k",
  "300k–400k",
  "400k–500k",
  ">500k"
)

tbl_mismatch2 <- CAISO_cost_data %>%
  # 1) stack the two series into one long tbl, keeping only true mismatches
  transmute(
    Measure = "POI",
    diff    = abs(kiteworks_phase_1_total_POI_cost - phase_1_total_POI_cost)
  ) %>%
  filter(!is.na(diff) & diff > 0) %>%
  bind_rows(
    CAISO_cost_data %>%
      transmute(
        Measure = "Network",
        diff    = abs(kiteworks_phase_1_total_network_cost - phase_1_total_network_cost)
      ) %>%
      filter(!is.na(diff) & diff > 0)
  ) %>%
  # 2) bin the differences
  mutate(
    Bin = cut(diff,
              breaks = bins,
              labels = labs,
              right = FALSE)
  ) %>%
  # 3) count per Bin × Measure
  count(Bin, Measure) %>%
  # 4) pivot so rows = Bin, cols = Measure
  pivot_wider(
    names_from   = Measure,
    values_from  = n,
    values_fill  = 0
  )


tbl_mismatch2 <- tbl_mismatch2 %>%
  column_to_rownames(var = "Bin")



# export it too
stargazer(tbl_mismatch2,
          type     = "latex",
          title    = "Kiteworksvs IC studies Mismatch‐Size Frequency (True Mismatches Only )",
          summary  = FALSE,
          rownames = TRUE,
          out      = paste0(project_root, "/output/tables/table_CAISO_ICstudies_mismatch_frequency.tex"))

stargazer(tbl_mismatch2,
          type     = "html",
          title    = " Kiteworksvs IC studies Mismatch‐Size Frequency (True Mismatches Only)",
          summary  = FALSE,
          rownames = TRUE,
          out      = paste0(project_root, "/output/tables/table_CAISO_ICstudies_mismatch_frequency.html"))

