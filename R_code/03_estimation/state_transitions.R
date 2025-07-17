#----------------------------------------------------------------------------------------------------------------
# This code estimates the Private and Public State transition regressions.

#----------------------------------------------------------------------------------------------------------------
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
library(lubridate)
library(sandwich)   
library(lmtest)      

# Clear the console and environment
rm(list=ls())

# Set project path based on the environment (RStudio or command line)
args = commandArgs(trailingOnly = TRUE)
if (Sys.getenv("RSTUDIO") == 1) {
  sys_info <- Sys.info()
  if (sys_info["sysname"] == "Windows") {
    project_root <- paste(Sys.getenv("USERPROFILE"), "/Dropbox/interconnections_data", sep="")
  } else if (sys_info["sysname"] == "Darwin") {
    project_root <- paste(Sys.getenv("HOME"), "/Dropbox/interconnections_data", sep="")
  }
} else {
  project_root <- args[1]
}


out_dir <- paste0(project_root,"/output/tables/project_static_dynamic/")
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)

#----------------------------------------------------------------------------------------------------------------
# Load Data
#----------------------------------------------------------------------------------------------------------------
project_static_dynamic<- read_csv(paste0(project_root, "/data/input/project_static_dynamic.csv"))

#project_static_dynamic<- project_static_dynamic %>% 
#  rename(PTO= area_code) %>% 
# rename(area_code=county)


# Voltage level clean up

levels(project_static_dynamic$co_located)
project_static_dynamic<-project_static_dynamic %>% 
mutate(
  voltage_level_kV = factor(voltage_level_kV,
                            levels = sort(unique(voltage_level_kV))),
  # and turn co_located "Y"/"N" into 1/0
  co_located       =  factor(co_located, 
                             levels= sort(unique(co_located)))
 
)

levels(project_static_dynamic$voltage_level_kV)

levels(project_static_dynamic$co_located)


project_static_dynamic <- project_static_dynamic %>%
  # coerce the old voltage into numeric so we can bucket it
  mutate(v_kv = as.numeric(as.character(voltage_level_kV))) %>%
  mutate(
    voltage_group = case_when(
      v_kv <= 100             ~ "<=100 kV",
      v_kv > 100  & v_kv <= 230 ~ "100–230 kV",
      v_kv >= 500             ~ "500+ kV",
      TRUE                    ~ NA_character_
    ),
    # now turn it into an ordered factor if you like
    voltage_group = factor(
      voltage_group,
      levels = c("<=100 kV","100–230 kV","500+ kV"),
      ordered = TRUE
    )
  ) %>%
  select(-v_kv)  # drop the old one if you no longer need it

voltage_by_type <- project_static_dynamic %>%
  filter(!is.na(Type), !is.na(voltage_group)) %>%
  distinct(q_id, Type, voltage_group) %>%
  count(Type, voltage_group) %>%
  pivot_wider(
    names_from  = voltage_group,
    values_from = n,
    values_fill = 0
  ) %>%
  # if you want Types in a specific order:
  arrange(match(Type, c("Battery","Solar","Wind","Others")))

# 2. Write it out with stargazer
stargazer(
  voltage_by_type,
  type             = "html",
  out              = paste0(out_dir, "voltage_by_type.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Voltage Level by Type",
  covariate.labels = names(voltage_by_type)[-1],  # the voltage_group columns
  digits           = 0
)

stargazer(
  voltage_by_type,
  type             = "latex",
  out              = paste0(out_dir, "voltage_by_type.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Voltage Level by Type",
  covariate.labels = names(voltage_by_type)[-1],
  digits           = 0
)



library(dplyr)
library(tidyr)
library(forcats)

make_voltage_table <- function(df) {
  df %>%
    filter(!is.na(Type)) %>%
    distinct(q_id, Type, voltage_group) %>%
    count(Type, voltage_group) %>%
    # explicit NA factor level
    mutate(voltage_group = fct_explicit_na(voltage_group, na_level = "Missing")) %>%
    pivot_wider(
      names_from  = voltage_group,
      values_from = n,
      values_fill = 0
    ) %>%
    bind_rows(
      summarise(
        .,
        Type = "Total",
        across(where(is.numeric), sum)
      )
    ) %>%
    arrange(match(Type, c("Battery", "Solar", "Wind", "Others", "Total")))
}

# Full-sample:
voltage_by_type_all <- make_voltage_table(project_static_dynamic)

# Phase I only:
voltage_by_type_phase1 <- project_static_dynamic %>%
  filter(checklist_phase == "Phase I Study") %>%
  make_voltage_table()

voltage_by_type_all
voltage_by_type_phase1






# 3) Tabulate project‐type frequencies by voltage_group
type_by_voltage <- project_static_dynamic %>%
  distinct(q_id, Type, voltage_group) %>%   # one row per project
  count(voltage_group, Type, name = "n_projects") %>%
  pivot_wider(
    names_from  = Type,
    values_from = n_projects,
    values_fill = 0
  ) %>%
  arrange(voltage_group)

# 2) Compute the totals row
total_row <- type_by_voltage %>%
  summarise(
    voltage_group = "Total",
    across(-voltage_group, sum)
  )

# 3) Bind it on
type_by_voltage_totals <- bind_rows(type_by_voltage, total_row)


# 4) Export that table via stargazer
stargazer(
  type_by_voltage,
  type       = "html",
  title      = "Project Type by Voltage Group",
  summary    = FALSE,
  rownames   = FALSE,
  covariate.labels = c("Voltage Tier","Battery","Others","Solar","Wind"),
  out        = paste0(out_dir, "type_by_voltage.html")
)

stargazer(
  type_by_voltage,
  type       = "latex",
  title      = "Project Type by Voltage Group",
  summary    = FALSE,
  rownames   = FALSE,
  covariate.labels = c("Voltage Tier","Battery","Others","Solar","Wind"),
  out        = paste0(out_dir, "type_by_voltage.tex")
)





#--- 2) PRE‐TOPCODING SUMMARY STATS (2 decimals) ------------------------------
cost_poi_stats <- project_static_dynamic %>%
  summarise(
    Min     = round(min(cost_poi,     na.rm=TRUE), 2),
    Q1      = round(unname(quantile(cost_poi, .25, na.rm=TRUE)), 2),
    Median  = round(median(cost_poi,   na.rm=TRUE), 2),
    Q3      = round(unname(quantile(cost_poi, .75, na.rm=TRUE)), 2),
    Max     = round(max(cost_poi,      na.rm=TRUE), 2),
    Mean    = round(mean(cost_poi,     na.rm=TRUE), 2),
    SD      = round(sd(cost_poi,       na.rm=TRUE), 2),
    N       = n()
  )

stargazer(
  cost_poi_stats,
  type             = "html",
  out              = paste0(out_dir,"cost_poi_summary_stats.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary Statistics: POI Cost",
  covariate.labels = names(cost_poi_stats),
  digits           = 2
)
stargazer(
  cost_poi_stats,
  type             = "latex",
  out              = paste0(out_dir,"cost_poi_summary_stats.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary Statistics: POI Cost",
  covariate.labels = names(cost_poi_stats),
  digits           = 2
)

cost_network_stats <- project_static_dynamic %>%
  summarise(
    Min     = round(min(cost_network,     na.rm=TRUE), 2),
    Q1      = round(unname(quantile(cost_network, .25, na.rm=TRUE)), 2),
    Median  = round(median(cost_network,   na.rm=TRUE), 2),
    Q3      = round(unname(quantile(cost_network, .75, na.rm=TRUE)), 2),
    Max     = round(max(cost_network,      na.rm=TRUE), 2),
    Mean    = round(mean(cost_network,     na.rm=TRUE), 2),
    SD      = round(sd(cost_network,       na.rm=TRUE), 2),
    N       = n()
  )

stargazer(
  cost_network_stats,
  type             = "html",
  out              = paste0(out_dir,"cost_network_summary_stats.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary Statistics: Network Cost",
  covariate.labels = names(cost_network_stats),
  digits           = 2
)
stargazer(
  cost_network_stats,
  type             = "latex",
  out              = paste0(out_dir,"cost_network_summary_stats.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary Statistics: Network Cost",
  covariate.labels = names(cost_network_stats),
  digits           = 2
)


# 1. Compute summary‐stats by Type
# prepare a Phase I “panel” deduped by q_id
phase1 <- project_static_dynamic %>%
  filter(checklist_phase == "Phase I Study", !is.na(Type)) %>%
  distinct(q_id, .keep_all = TRUE)

cost_poi_stats_by_type <- phase1 %>%
  group_by(Type) %>%
  summarise(
    Min     = round(min(cost_poi,     na.rm=TRUE), 2),
    Q1      = round(unname(quantile(cost_poi, .25, na.rm=TRUE)), 2),
    Median  = round(median(cost_poi,   na.rm=TRUE), 2),
    Q3      = round(unname(quantile(cost_poi, .75, na.rm=TRUE)), 2),
    Max     = round(max(cost_poi,      na.rm=TRUE), 2),
    Mean    = round(mean(cost_poi,     na.rm=TRUE), 2),
    SD      = round(sd(cost_poi,       na.rm=TRUE), 2),
    N       = n()
  ) %>%
  ungroup() %>%
  arrange(match(Type, c("Battery", "Solar", "Wind", "Others")))

cost_network_stats_by_type <- phase1 %>%
  group_by(Type) %>%
  summarise(
    Min     = round(min(cost_network,     na.rm=TRUE), 2),
    Q1      = round(unname(quantile(cost_network, .25, na.rm=TRUE)), 2),
    Median  = round(median(cost_network,   na.rm=TRUE), 2),
    Q3      = round(unname(quantile(cost_network, .75, na.rm=TRUE)), 2),
    Max     = round(max(cost_network,      na.rm=TRUE), 2),
    Mean    = round(mean(cost_network,     na.rm=TRUE), 2),
    SD      = round(sd(cost_network,       na.rm=TRUE), 2),
    N       = n()
  ) %>%
  ungroup() %>%
  arrange(match(Type, c("Battery", "Solar", "Wind", "Others")))

# 2. Render each as stargazer tables (HTML + LaTeX)
stargazer(
  cost_poi_stats_by_type,
  type             = "html",
  out              = paste0(out_dir, "cost_poi_summary_stats_by_type.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary Statistics: POI Cost by Type (Phase 1 Costs)",
  covariate.labels = names(cost_poi_stats_by_type),
  digits           = 2
)
stargazer(
  cost_poi_stats_by_type,
  type             = "latex",
  out              = paste0(out_dir, "cost_poi_summary_stats_by_type.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary Statistics: POI Cost by Type (Phase 1 Costs)",
  covariate.labels = names(cost_poi_stats_by_type),
  digits           = 2
)

stargazer(
  cost_network_stats_by_type,
  type             = "html",
  out              = paste0(out_dir, "cost_network_summary_stats_by_type.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary Statistics: Network Cost by Type (Phase 1 Costs)",
  covariate.labels = names(cost_network_stats_by_type),
  digits           = 2
)
stargazer(
  cost_network_stats_by_type,
  type             = "latex",
  out              = paste0(out_dir, "cost_network_summary_stats_by_type.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary Statistics: Network Cost by Type (Phase 1 Costs)",
  covariate.labels = names(cost_network_stats_by_type),
  digits           = 2
)

#----------------------------------------------------------------------------------------------------------------
#                                                 Private State Transitions
#----------------------------------------------------------------------------------------------------------------



#------------------------------------------------- Data Cleaning ---------------------------------------------------------------



#--- 5) ZERO‐COST COUNTS --------------------------------------------------------
make_zero_tbl <- function(d) {
  d %>%
    distinct(q_id) %>%
    mutate(
      zero_poi     = q_id %in% filter(d, cost_poi==0)$q_id,
      zero_network = q_id %in% filter(d, cost_network==0)$q_id
    ) %>%
    summarise(
      zero_poi     = sum(zero_poi),
      zero_network = sum(zero_network),
      total_q      = n()
    )
}

#zc_all  <- make_zero_tbl(project_static_dynamic) %>% mutate(subset="All")
zc_p1   <- make_zero_tbl(filter(project_static_dynamic, checklist_phase=="Phase I Study"))  %>% mutate(subset="Phase I Study")
zc_p2   <- make_zero_tbl(filter(project_static_dynamic, checklist_phase=="Phase II Study")) %>% mutate(subset="Phase II Study")
zero_counts <- bind_rows( zc_p1, zc_p2)

stargazer(
  zero_counts, type="html",
  out              = paste0(out_dir,"zero_cost_counts.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Zero‐Cost Project Counts",
  covariate.labels = c("Zero POI","Zero Network","Total Projects", "Subset"),
  digits           = 2
)
stargazer(
  zero_counts, type="latex",
  out              = paste0(out_dir,"zero_cost_counts.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Zero‐Cost Project Counts",
  covariate.labels = c("Zero POI","Zero Network","Total Projects", "Subset"),
  digits           = 2
)


# Helper to compute zero‐cost by cluster for a given phase name
zero_by_cluster <- function(df, phase_name) {
  df %>%
    filter(checklist_phase == phase_name) %>%
    distinct(cluster, q_id, cost_poi, cost_network) %>%
    group_by(cluster) %>%
    summarise(
      Zero_POI       = sum(cost_poi     == 0, na.rm = TRUE),
      Zero_Network   = sum(cost_network == 0, na.rm = TRUE),
      Total_Projects = n_distinct(q_id)
    ) %>%
    ungroup()
}

# Compute tables
zc_p1_cl <- zero_by_cluster(project_static_dynamic, "Phase I Study")
zc_p2_cl <- zero_by_cluster(project_static_dynamic, "Phase II Study")

# helper to reorder + add total row
reorder_and_totals <- function(df) {
  special <- c("TC", "SGIP-TC")
  
  df2 <- df %>%
    # ensure character
    mutate(cluster = as.character(cluster)) %>%
    # create an ordering key: numeric clusters get their as.numeric(),
    # specials get Inf so they sort last
    mutate(
      cluster_key = if_else(cluster %in% special,
                            Inf,
                            as.numeric(cluster))
    ) %>%
    arrange(cluster_key) %>%
    select(-cluster_key)
  
  # total row
  total_row <- tibble(
    cluster        = "TOTAL",
    Zero_POI       = sum(df2$Zero_POI),
    Zero_Network   = sum(df2$Zero_Network),
    Total_Projects = sum(df2$Total_Projects)
  )
  
  bind_rows(df2, total_row)
}

# apply to both tables
zc_p1_cl <- zc_p1_cl %>% reorder_and_totals()
zc_p2_cl <- zc_p2_cl %>% reorder_and_totals()

# now export exactly as before
stargazer(
  zc_p1_cl,
  type             = "html",
  out              = paste0(out_dir, "zero_cost_by_cluster_phase1.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Zero‐Cost Project Counts by Cluster (Phase I)",
  covariate.labels = c("Cluster", "Zero POI", "Zero Network", "Total Projects"),
  digits           = 0
)
stargazer(
  zc_p1_cl,
  type             = "latex",
  out              = paste0(out_dir, "zero_cost_by_cluster_phase1.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Zero‐Cost Project Counts by Cluster (Phase I)",
  covariate.labels = c("Cluster", "Zero POI", "Zero Network", "Total Projects"),
  digits           = 0
)

stargazer(
  zc_p2_cl,
  type             = "html",
  out              = paste0(out_dir, "zero_cost_by_cluster_phase2.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Zero‐Cost Project Counts by Cluster (Phase II)",
  covariate.labels = c("Cluster", "Zero POI", "Zero Network", "Total Projects"),
  digits           = 0
)
stargazer(
  zc_p2_cl,
  type             = "latex",
  out              = paste0(out_dir, "zero_cost_by_cluster_phase2.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Zero‐Cost Project Counts by Cluster (Phase II)",
  covariate.labels = c("Cluster", "Zero POI", "Zero Network", "Total Projects"),
  digits           = 0
)












#--- 3) PERCENTILES & TOP‐CODING -----------------------------------------------
p99 <- project_static_dynamic %>%
  summarise(
    p99_poi     = quantile(cost_poi,     .99, na.rm=TRUE),
    p99_network = quantile(cost_network, .99, na.rm=TRUE)
  )
print(p99)

cap_poi     <- min(p99$p99_poi,     2000)
cap_network <- min(p99$p99_network, 2000)

project_static_dynamic <- project_static_dynamic %>%
  mutate(
    cost_poi     = pmin(cost_poi,     cap_poi),
    cost_network = pmin(cost_network, cap_network)
  )



#--- 4) area_code LIST ---------------------------------------------------------------
zone_table <- project_static_dynamic %>%
  # drop rows where area_code is missing, if you don’t want an “NA” area_code row
  filter(!is.na(area_code)) %>%  
  group_by(area_code) %>%
  summarise(
    n_obs   = n(),               # total rows in that area_code
    n_qid   = n_distinct(q_id)   # unique q_id’s in that area_code
  ) %>%
  ungroup() %>%
  arrange(area_code)

# export as HTML
stargazer(
  zone_table, 
  type             = "html", 
  out              = paste0(out_dir,"zone_list.html"),
  summary          = FALSE, 
  rownames         = FALSE,
  title            = "area code Listing",
  covariate.labels = c("area code","Obs Count","Project Count"),
  digits           = 2
)

# export as LaTeX
stargazer(
  zone_table, 
  type             = "latex", 
  out              = paste0(out_dir,"zone_list.tex"),
  summary          = FALSE, 
  rownames         = FALSE,
  title            = "area code Listing",
  covariate.labels = c("area code","Obs Count","Project Count"),
  digits           = 2
)


 



## Place holder for now
 

project_static_dynamic <- project_static_dynamic %>%
  mutate(
    area_code = ifelse(is.na(area_code),
                       "others",
                       as.character(area_code))
  )


#---LOG COSTS and Type Dummies -----------------------------------------
project_static_dynamic <- project_static_dynamic %>%
  
  # — first restrict to the three types and re-level so Solar is the reference —
  filter(Type %in% c("Battery", "Solar", "Wind")) %>%
  mutate(
    Type = fct_relevel(Type, "Solar", "Wind", "Battery")
  ) %>% 
  
  # 1) logs of cost_poi & cost_network
  mutate(
    log_cost_poi     = log(cost_poi),
    log_cost_network = log(cost_network)
  ) %>%
  relocate(log_cost_poi,     .after = cost_poi) %>%
  relocate(log_cost_network, .after = cost_network) %>%
  
  mutate(
    log1p_cost_poi     = log1p(cost_poi), # log(cost_poi + 1)
    log1p_cost_network = log1p(cost_network) # log(cost_network + 1)
  ) %>%
  relocate(log1p_cost_poi,     .after = log_cost_poi) %>% 
  relocate(log1p_cost_network, .after = log_cost_network) %>% 

  
  # 2) extract calendar_year from status_date  
  mutate(calendar_year = ymd(status_date) %>% year()) %>%
  relocate(calendar_year, .after = status_date) %>%
  
  # 3) project‐type dummies
  mutate(
    isWind    = if_else(Type == "Wind",    1L, 0L),
    isBattery = if_else(Type == "Battery", 1L, 0L),
  ) %>%
  
  # 4) force area_code to be categorical
  mutate(
    area_code = as.integer(factor(area_code))
  ) %>% 
  mutate(area_code = factor(area_code)) %>% 
 
  # 5) Phase-I “cost_shared” flag
  mutate(
    iscost_shared = case_when(
       is.na(cost_network_shared)                     ~ NA_integer_,           # still missing
      cost_network_shared > 0 & cost_network_shared < 1 ~ 1L,                     # shared
      TRUE                                               ~ 0L,                     # not shared
    )
  ) %>%
  relocate(iscost_shared, .after = cost_network_shared)

 

#------------------------------------------------- queue capacity ---------------------------------------------------------------





#--- 6) QUEUE CAPACITY & SUMMARY STATS -----------------------------------------
#project_static_dynamic <- project_static_dynamic %>%
  
 # mutate(calendar_year = year(ymd(status_date))) %>%
#  group_by(q_id) %>% fill(calendar_year, .direction="down") %>% ungroup() 
  
#zone_year_capacity <- project_static_dynamic %>%
  # instead of filter(action!="exit") use:
 # filter(
  #  checklist_phase != "Interconnection Request",
   # checklist_phase != "GIA",
  #  action != "exit"
  #  )%>%
  #group_by(area_code, calendar_year) %>%
  #summarize(
  #  AMW_at      = sum(MW, na.rm=TRUE),
  #  .groups = "drop"
  #) %>%
  #arrange(area_code, calendar_year) %>%
  #group_by(area_code) %>%
  #mutate(AMW_backlog_at = cumsum(AMW_at)) %>%
  #ungroup()

#project_static_dynamic <- left_join(
 # project_static_dynamic, zone_year_capacity,
#  by = c("area_code","calendar_year")
#)

#--- QUEUE CAPACITY: TWO SEPARATE SUMMARY TABLES --------------------------------

# ensure project_static_dynamic already has AMW_at & AMW_backlog_at

# (a) Contemporaneous summary
AMW_at_stats <- project_static_dynamic %>%
  summarise(
    Min    = round(min(AMW_at,      na.rm=TRUE), 2),
    Q1     = round(unname(quantile(AMW_at, .25, na.rm=TRUE)), 2),
    Median = round(median(AMW_at,   na.rm=TRUE), 2),
    Q3     = round(unname(quantile(AMW_at, .75, na.rm=TRUE)), 2),
    Max    = round(max(AMW_at,      na.rm=TRUE), 2),
    Mean   = round(mean(AMW_at,     na.rm=TRUE), 2),
    SD     = round(sd(AMW_at,       na.rm=TRUE), 2),
    N      = n()
  )

stargazer(
  AMW_at_stats, type="html",
  out              = paste0(out_dir,"AMW_at_stats.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = " AMW Entering Summary (in mW)",
  covariate.labels = names(AMW_at_stats),
  digits           = 2
)
stargazer(
  AMW_at_stats, type="latex",
  out              = paste0(out_dir,"AMW_at_stats.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "AMW Entering Summary (in mW)",
  covariate.labels = names(AMW_at_stats),
  digits           = 2
)

# (b) Cumulative summary
AMW_backlog_at_stats <- project_static_dynamic %>%
  summarise(
    Min    = round(min(AMW_backlog_at,      na.rm=TRUE), 2),
    Q1     = round(unname(quantile(AMW_backlog_at, .25, na.rm=TRUE)), 2),
    Median = round(median(AMW_backlog_at,   na.rm=TRUE), 2),
    Q3     = round(unname(quantile(AMW_backlog_at, .75, na.rm=TRUE)), 2),
    Max    = round(max(AMW_backlog_at,      na.rm=TRUE), 2),
    Mean   = round(mean(AMW_backlog_at,     na.rm=TRUE), 2),
    SD     = round(sd(AMW_backlog_at,       na.rm=TRUE), 2),
    N      = n()
  )

stargazer(
  AMW_backlog_at_stats, type="html",
  out              = paste0(out_dir,"AMW_backlog_at_stats.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "AMW Backlog Summary (in mW)",
  covariate.labels = names(AMW_backlog_at_stats),
  digits           = 2
)
stargazer(
  AMW_backlog_at_stats, type="latex",
  out              = paste0(out_dir,"AMW_backlog_at_stats.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "AMW Backlog Summary (in mW)",
  covariate.labels = names(AMW_backlog_at_stats),
  digits           = 2
)


 

#----------------------------------------------------------------------------------------------------------------
#                                         Initial Cost Regressions
#----------------------------------------------------------------------------------------------------------------

#--- 7) INITIAL COST REGRESSIONS ----------------------------------------------

# 1) Prep the t=1 data


initial_cost_data <- project_static_dynamic %>%
  filter(model_year == 1) %>%
  mutate(
    calendar_year        = as.numeric(calendar_year),
    calendar_year_c      = calendar_year - mean(calendar_year, na.rm = TRUE),
    AMW_at_gW            = AMW_at / 1000,
    AMW_backlog_at_gW    = AMW_backlog_at / 1000,
    AMW_backlog_delta_at_gW = AMW_backlog_delta_at / 1000,
  ) %>%
  drop_na(isWind, isBattery, FCDS, area_code, co_located, voltage_group)

# drop NA/Inf
vars <- c("cost_poi","cost_network","log1p_cost_poi","log1p_cost_network",
          "calendar_year","MW","AMW_at_gW","AMW_backlog_at_gW",
          "isWind","isBattery","FCDS","area_code", "co_located", "voltage_group")

num_vars <- c("cost_poi","cost_network","log1p_cost_poi","log1p_cost_network",
              "calendar_year","MW","AMW_at_gW","AMW_backlog_at_gW")


Initial_cost_data <- initial_cost_data %>%
  drop_na(isWind, isBattery, FCDS, area_code, co_located, voltage_group)


#initial_cost_data <- filter(initial_cost_data, if_all(all_of(vars), ~ !is.na(.) & is.finite(.))) %>% 
# mutate(voltage_level_kV = droplevels(voltage_level_kV))



# grab the voltage levels so we can build labels later
voltage_lvls  <- levels(initial_cost_data$voltage_group)
# we’ll omit the first level as it’s the reference
voltage_labels <- paste0("Voltage ", voltage_lvls[-1])






##------  cost shared interaction  regressions ---
#######
 




# 2) Build formula list in the desired order: 
#    Entering → Entering×Shared → Backlog → Backlog×Shared, both level & log
make_forms <- function(y, ly) {
  base    <- "calendar_year_c + MW + FCDS + factor(co_located) + voltage_group + isWind + isBattery + factor(area_code)"
  entry   <- "AMW_at_gW"
  backlog <- "AMW_backlog_at_gW"
  delta  <- "AMW_backlog_delta_at_gW"
  log_delta <-"log_AMW_backlog_delta_at_gW"
  list(
    lvl_ent          = as.formula(paste(y,  "~", base, "+", entry)),
    lvl_bkl          = as.formula(paste(y,  "~", base, "+", backlog)),
    lvl_del          = as.formula(paste(y,  "~", base, "+", delta)),
    lvl_log_del    = as.formula(paste(y,  "~", base, "+", log_delta)),
    log_ent          = as.formula(paste(ly, "~", base, "+", entry)),
    log_bkl          = as.formula(paste(ly, "~", base, "+", backlog)),
    log_del          = as.formula(paste(ly,  "~", base, "+", delta)),
    log_log_del          = as.formula(paste(ly,  "~", base, "+", log_delta))
  )
}

# 3) Fit the eight models for POI and Network
models_list <- list(
  poi = make_forms("cost_poi",       "log1p_cost_poi")     %>% map(~ lm(.x, data = initial_cost_data)),
  network = make_forms("cost_network","log1p_cost_network") %>% map(~ lm(.x, data = initial_cost_data))
)

# 4) Cluster‐robust SEs (HC1)
get_se   <- function(m) sqrt(diag(vcovHC(m, type = "HC1")))
ses_list <- map(models_list, ~ map(.x, get_se))

# 5) RMSEs
rmse_list <- map(models_list, ~ map_dbl(.x, ~ sqrt(mean(residuals(.x)^2))))

# 6) Common table settings
fe_line <- c("area FE", rep("Yes", 8))
voltage_lvls  <- levels(initial_cost_data$voltage_group)
voltage_labels<- paste0("Voltage ", voltage_lvls[-1])

for(cost in names(models_list)) {
  mods  <- models_list[[cost]]
  ses   <- ses_list[[cost]]
  rmses <- rmse_list[[cost]]
  
  # Dependent var labels (same names)
  if(cost == "poi") {
    dv_lab <- c(
      rep("POI Cost ($/kW)", 4),
      rep("Log POI Cost ($/kW)", 4)
    )
    title    <- "Initial POI Cost Estimates with Time Trend"
    dep_mean <- round(mean(initial_cost_data$cost_poi), 2)
  } else {
    dv_lab   <- c(
      rep("Network Cost ($/kW)", 4),
      rep("Log Network Cost ($/kW)", 4)
    )
    title    <- "Initial Network Cost Estimates with Time Trend"
    dep_mean <- round(mean(initial_cost_data$cost_network), 2)
  }
  
  # Covariate labels
  cov_labels <- c(
    "t",
    "MW (Own Capacity)",
    "FCDS",
    "Co-located with Solar",
    voltage_labels,
    "Wind",
    "Battery",
    "AMW Entering (GW)",
    "AMW Backlog (GW)",
    "Delta AMW Backlog (GW)",
    "Log Delta AMW Backlog (GW)",
    "Intercept"
  )
  
  # Additional lines
  mean_line <- c("Dep. var. mean", rep(dep_mean, 4), rep(NA, 4))
  rmse_line <- c("RMSE", format(round(rmses,2), nsmall = 2))
  
  # 7) HTML output
  stargazer(
    mods, se = ses, type = "html", style = "qje",
    out = paste0(out_dir, "initial_cost_", cost, "_log_level.html"),
    title = title,
    dep.var.labels.include = FALSE,
    column.labels = dv_lab,
    covariate.labels = cov_labels,
    omit = "factor\\(area_code\\)",
    add.lines = list(fe_line, mean_line, rmse_line),
    omit.stat = c("f","ser","rsq"),
    notes = c("Log uses log1p(cost+1)", "t is mean-centered"),
    digits = 2
  )
  
  # 8) LaTeX output
  tex_out <- paste0(out_dir, "initial_cost_", cost, "_log_level.tex")
  stargazer(
    mods, se = ses, type = "latex", style = "qje",
    out = tex_out,
    title = title,
    dep.var.labels.include = FALSE,
    column.labels = dv_lab,
    covariate.labels = cov_labels,
    omit = "factor\\(area_code\\)",
    add.lines = list(fe_line, mean_line, rmse_line),
    omit.stat = c("f","ser"),
    notes = c("Log uses log1p(cost+1)", "t is mean-centered"),
    digits = 2
  )
  # --- 3) Post‐process .tex: escape all $ → \$ ---
  tex_txt <- readLines(tex_out)
  # replace every occurrence of "$/kW" with "\$/kW"
  # in R strings: pattern "\\$/kW" replacement "\\\\$/kW"
  tex_txt <- gsub(
    pattern     = "\\$/kW",
    replacement = "\\\\$/kW",
    x           = tex_txt,
    perl        = TRUE
  )
  writeLines(tex_txt, tex_out)
}




library(dplyr)
library(tidyr)

library(dplyr)
library(purrr)

# Silly Checks
##--------------
Inital_cost_data <- initial_cost_data %>% 
  filter(checklist_phase == "Phase I Study")
all_vars <- c(
  "cost_poi", "log1p_cost_poi",
  "AMW_at_gW", "AMW_backlog_at_gW",
  "calendar_year_c", "MW", "FCDS",
  "co_located", "voltage_group",
  "isWind", "isBattery",
  "area_code"
)

missing_summary <- tibble(
  variable = all_vars
) %>%
  mutate(
    n_na  = map_int(variable, ~ sum(is.na(Inital_cost_data[[.x]]))),
    n_inf = map_int(variable, ~ sum(is.infinite(Inital_cost_data[[.x]])))
  )

# Now you can just print it:
print(missing_summary)


missing_area <- initial_cost_data %>% 
  filter(area_code == 13)  
 

missing_poi_nc_present <- initial_cost_data %>% 
  filter(data_source == "scraped") %>% 
  filter(is.na(cost_poi) & !is.na(cost_network))

missing_nc_poi_present <- initial_cost_data %>% 
  filter(data_source == "scraped") %>% 
  filter(!is.na(cost_poi) & is.na(cost_network))





missing_poi_kw <- initial_cost_data %>% 
  filter(data_source == "kiteworks") %>% 
  filter(is.na(cost_poi) | is.na(cost_network))


missing_poi_all <- initial_cost_data %>% 
  filter(is.na(cost_poi) | is.na(cost_network))


tansitioned <-  initial_cost_data %>% 
  filter(checklist_phase == "TRANSITIONED" | checklist_phase == "Phase II Study" | is.na(checklist_phase)) %>% 
  filter(is.na(cost_poi))


tansitioned <-  initial_cost_data %>% 
  filter(is.na(checklist_phase))


initial_cost_data_p1 <- initial_cost_data %>% 
  filter(checklist_phase == "Phase I Study") %>% 
  filter(data_source == "scraped")


missing_poi_p1 <- initial_cost_data_p1 %>% 
  filter(data_source == "scraped") %>% 
  filter(is.na(cost_poi) | is.na(cost_network))

missing_poi_kw_p1 <- initial_cost_data_p1 %>% 
  filter(data_source == "kiteworks") %>% 
  filter(is.na(cost_poi) | is.na(cost_network))


missing_poi_all_p1 <- initial_cost_data_p1 %>% 
  filter(is.na(cost_poi) | is.na(cost_network))


missing_model_year <- project_static_dynamic %>% 
  filter(is.na(model_year))


library(mice)
library(naniar)
gg_miss_upset(initial_cost_data[all_vars])
md.pattern(initial_cost_data[all_vars])





### Temporary - Diagnostics ################################################################################

 

#--- 1) build exactly seven stepwise formulas -------------------------------
make_diag_forms <- function(y, ly) {
  base     <- "factor(cluster) + MW + FCDS + isWind  + isBattery  + factor(co_located) +  voltage_group"
  contemp  <- paste(base, "+ AMW_at_gW")
  cumul    <- paste(base, "+ AMW_backlog_at_gW")
  delta <- paste(base, "+ AMW_backlog_delta_at_gW")
  log_delta <- paste(base, "+ log_AMW_backlog_delta_at_gW")
  contempZ <- paste(contemp, "+ factor(area_code)")
  cumulZ   <- paste(cumul,   "+ factor(area_code)")
  deltaZ  <- paste(delta,   "+ factor(area_code)")
  log_deltaZ <- paste(log_delta,   "+ factor(area_code)")
  
  list(
    lvl_base    = as.formula(paste0(y,  " ~ ", base)),
    lvl_contemp = as.formula(paste0(y,  " ~ ", contemp)),
    lvl_cumul   = as.formula(paste0(y,  " ~ ", cumul)),
    lvl_delta   =  as.formula(paste0(y, " ~ ", delta)),
    lvl_log_delta = as.formula(paste0(y, " ~ ", log_delta)),
    lvl_cZ      = as.formula(paste0(y,  " ~ ", contempZ)),
    lvl_uZ      = as.formula(paste0(y,  " ~ ", cumulZ)),
    lvl_dZ      = as.formula(paste0(y,  " ~ ", deltaZ)),
    lvl_log_dZ      = as.formula(paste0(y,  " ~ ", log_deltaZ)),
    
    log_base    = as.formula(paste0(ly, " ~ ", base)),
    log_contemp = as.formula(paste0(ly, " ~ ", contemp)),
    log_cumul   = as.formula(paste0(ly, " ~ ", cumul)),
    log_delta   = as.formula(paste0(ly, " ~ ", delta)),
    log_log_delta = as.formula(paste0(ly, " ~ ", log_delta)),
    log_cZ      = as.formula(paste0(ly, " ~ ", contempZ)),
    log_uZ      = as.formula(paste0(ly, " ~ ", cumulZ)),
    log_dZ      = as.formula(paste0(ly, " ~ ", deltaZ)),
    log_log_dZ  = as.formula(paste0(ly,  " ~ ", log_deltaZ))
  )
}

# forms for POI and Network
poi_forms <- make_diag_forms("cost_poi",       "log1p_cost_poi")
net_forms <- make_diag_forms("cost_network",   "log1p_cost_network")

#--- 2) fit the seven models -------------------------------------------------
fit_seven <- function(forms) {
  mods <- map(forms, ~ lm(.x, data=initial_cost_data))
  ses  <- map(mods,  ~ sqrt(diag(vcovHC(.x,"HC1"))))
  rmses <- map_dbl(mods, ~ sqrt(mean(residuals(.x)^2, na.rm = TRUE)))
  list(models = mods, ses = ses, rmses = rmses)
  
}

poi_fit_ic <- fit_seven(poi_forms[c(1:9)])   # level specs
poi_log_ic <- fit_seven(poi_forms[c(10:18)])  # log specs
net_fit_ic <- fit_seven(net_forms[c(1:9)])
net_log_ic <- fit_seven(net_forms[c(10:18)])

# lines to add at bottom
cluster_line <- c("Cluster FE", rep("Yes", 9))
zone_line    <- c("area FE",    rep(c("No","No","No","No","No","Yes","Yes","Yes", "Yes")))
#rmse_line <- c("RMSE", format(round(rmses,2), nsmall = 2))

#--- 3) helper to write one block of 7 models -------------------------------
write_seven <- function(fit, cost, scale) {
  rmses <- fit$rmses 
  rmse_line    <- c("RMSE", format(round(rmses, 2), nsmall = 2))
  
 
  
  html_out <- paste0(
    out_dir,
    "initial_cost_estimate_",
    cost, "_",
    scale, "_diagnostic.html"
  )
  tex_out  <- sub("\\.html$", ".tex", html_out)
  
  
  nmods <- length(fit$models)
  dv_lab <- if (cost=="poi") {
    if (scale=="level") rep("POI Cost ($/kW)", nmods)
    else               rep("Log POI Cost ($/kW)", nmods)
  } else {
    if (scale=="level") rep("Network Cost ($/kW)", nmods)
    else               rep("Log Network Cost ($/kW)", nmods)
  }
  
  
  stargazer(
    fit$models, se=fit$ses,
    type="html", style="qje",
    out = html_out,
    title = paste0(
      "Diagnostic regressions for ",
      toupper(cost),
      " cost (", scale, ")"
    ),
    dep.var.labels.include = FALSE,
    column.labels = dv_lab,
    column.separate = rep(1, nmods),
    covariate.labels=c(
      "MW (Own Capacity)", "FCDS", "Wind",  "Battery", 
      "Co located with a solar plant",  voltage_labels,
      "AMW Entering (GW)",
      "AMW Backlog (GW)",
      "Delta AMW Backlog (GW)",
      "Log Delta AMW Backlog (GW)",
      "Intercept"
    ),
    omit = c("factor\\(cluster\\)", "factor\\(area_code\\)"),
    add.lines = list(cluster_line, zone_line, rmse_line),
    omit.stat = c("f","ser", "rsq"),
    notes = "Log models use log1p(cost)=log(cost+1) to avoid -Inf values.",
    digits = 2
    # no column.labels → yields default (1),(2),…,(7)
  )
  
  stargazer(
    fit$models, se=fit$ses,
    type="latex", style="qje",
    out = tex_out,
    title = paste0(
      "Diagnostic regressions for initial cost estimates of ",
      tolower(cost),
      " cost (", scale, ")"
    ),
    dep.var.labels.include = FALSE,
    dep.var.caption  = "",             # <- ensure the header row is printed
    column.labels    = dv_lab,
    covariate.labels=c(
      "MW (Own Capacity)", "FCDS", "Wind",  "Battery", 
      "Co located with a solar plant",  voltage_labels,
      "AMW Entering (GW)",
      "AMW Backlog (GW)",
      "Delta AMW Backlog (GW)",
      "Log Delta AMW Backlog (GW)",
      "Intercept"
    ),
    omit = c("factor\\(cluster\\)", "factor\\(area_code\\)"),
    add.lines = list(cluster_line, zone_line, rmse_line),
    omit.stat = c("f","ser","rsq"),
    notes = "Log models use log1p(cost)=log(cost+1) to avoid -Inf values.",
    digits = 2
  )
  # --- 3) Post‐process .tex: escape all $ → \$ ---
  tex_txt <- readLines(tex_out)
  # replace every occurrence of "$/kW" with "\$/kW"
  # in R strings: pattern "\\$/kW" replacement "\\\\$/kW"
  tex_txt <- gsub(
    pattern     = "\\$/kW",
    replacement = "\\\\$/kW",
    x           = tex_txt,
    perl        = TRUE
  )
  writeLines(tex_txt, tex_out)
}





#--- 4) write all 4 tables ---------------------------------------------------
write_seven(poi_fit_ic,  "poi",     "level")
write_seven(poi_log_ic,  "poi",     "log")
write_seven(net_fit_ic,  "network", "level")
write_seven(net_log_ic,  "network", "log")

message("All 10-col diagnostic tables written to ", out_dir)




 




# ----------------------------------------------------------------------------------------------------------------------

# Cost Transition regressions

#------------------------------------------------------------------------------------------------------------------------

 
library(lubridate)

cost_transitions_data <- project_static_dynamic %>%
  # ensure Phase I comes before II
  arrange(q_id, model_year) %>%
  
  # 1) For each project, bake in the Phase I shared‐cost flag
  group_by(q_id) %>%
  mutate(
    iscost_shared_ph1 = case_when(
      checklist_phase == "Phase I Study" & is.na(cost_network_shared)                      ~ NA_integer_,  # still missing
      checklist_phase == "Phase I Study" & cost_network_shared > 0 & cost_network_shared < 1 ~ 1L,           # shared
      checklist_phase == "Phase I Study"                                                    ~ 0L,           # not shared
      TRUE                                                                                  ~ NA_integer_
    )
  ) %>%
  # carry that Phase I flag *down* so the Phase II row gets it
  fill(iscost_shared_ph1, .direction = "down") %>%
  ungroup() %>%
  # 1) Keep only Phase I & II, drop forward‐filled
 # filter(
  #  checklist_phase %in% c("Phase I Study", "Phase II Study"),
  #  POI_costs_carried_forward     != 1,
  #  network_costs_carried_forward != 1
  #) %>%
  mutate(
    AMW_at_gW            = AMW_at / 1000,
    AMW_backlog_at_gW    = AMW_backlog_at / 1000,
    AMW_backlog_delta_at_gW = AMW_backlog_delta_at / 1000
  ) %>% 
  
  # 2) Cap cost_network_leverage
  group_by(checklist_phase) %>%
  mutate(
    cap_leverage = min(quantile(cost_network_leverage, 0.99, na.rm = TRUE), 3000),
    cost_network_leverage = pmin(cost_network_leverage, cap_leverage)
    
  ) %>%
  ungroup() %>%
  select(-cap_leverage) %>%
  
  # 3) Ensure time‐ordering
  arrange(q_id, status_date) %>%
  group_by(q_id) %>%
  
  # 4) Compute lags & carry‐forward vars (all in GW where relevant)
  mutate(
    poi_I  = lag(cost_poi),
    net_I = lag(cost_network),
    poi_log_I               = lag(log1p_cost_poi),
    net_log_I               = lag(log1p_cost_network),
    AMW_at_I_gW             = lag(AMW_at_gW),
    AMW_backlog_I_gW        = lag(AMW_backlog_at_gW),
    # keep the current-year backlog delta in GW
    lag_AMW_backlog_delta_at_gW = lag(AMW_backlog_delta_at_gW),
    lag_log_AMW_backlog_delta_at_gW = lag(log_AMW_backlog_delta_at_gW), 
    iscost_shared           = iscost_shared,
    iscost_shared_ph1 = iscost_shared_ph1
  ) %>%
  ungroup() %>%
  
  # 5) Restrict to projects that have both phases
 # group_by(q_id) %>%
  #filter(n_distinct(checklist_phase) == 2) %>%
  #ungroup() %>%
  

  
  # 6) Keep only Phase II rows (where the lags exist)
  filter(
    checklist_phase == "Phase II Study",
    !is.na(poi_I),
    !is.na(net_I),
    !is.na(poi_log_I),
    !is.na(net_log_I),
    !is.na(AMW_at_I_gW),
    !is.na(AMW_backlog_I_gW)
  )  %>% 
  

  relocate(poi_I,             .after = cost_poi) %>% 
  relocate(net_I,             .after = cost_network) %>% 
  relocate(poi_log_I,         .after = log1p_cost_poi) %>%
  relocate(net_log_I,         .after = log1p_cost_network) %>%
  relocate(AMW_at_I_gW,       .after = AMW_at_gW) %>%
  relocate(AMW_backlog_I_gW,  .after = AMW_backlog_at_gW) %>% 
  relocate(iscost_shared_ph1,  .after = iscost_shared)

 
 
# 1) Build all transition formulas (16 per outcome)
make_transition_forms <- function(dep, logdep, lag, loglag) {
  base_0   <- paste(
    "factor(cluster)", "MW", "FCDS",
    "isWind", "isBattery",
    "factor(co_located)", "voltage_group",
    sep = " + "
  )
  base   <- paste(
    "factor(cluster)", "MW", "FCDS",
    "isWind", "isBattery",
    "factor(co_located)", "voltage_group", "iscost_shared_ph1",
    sep = " + "
  )
  entry    <- "AMW_at_I_gW"
  backlog  <- "AMW_backlog_I_gW"
  del   <- "lag_AMW_backlog_delta_at_gW"
  log_del <- "lag_log_AMW_backlog_delta_at_gW"
  entryS   <- paste(entry, "+", entry, ":iscost_shared_ph1")
  backlogS <- paste(backlog, "+", backlog, ":iscost_shared_ph1")
  delS  <-   paste(del, "+", del, ":iscost_shared_ph1")
  log_delS <- paste(log_del, "+", log_del, ":iscost_shared_ph1")
  entryZ   <- paste(entryS, "+ factor(area_code)")
  backlogZ <- paste(backlogS, "+ factor(area_code)")
  delZ <- paste(delS, "+ factor(area_code)")
  log_delZ <- paste(log_delS, "+ factor(area_code)")
  
  list(
    # Level specs
    lvl_lag      = as.formula(paste(dep,     "~", lag)),
    lvl_base0_lag = as.formula(paste(dep,     "~", lag, "+", base_0)),
    lvl_base_lag = as.formula(paste(dep,     "~", lag, "+", base)),
    lvl_ent      = as.formula(paste(dep,     "~", lag, "+", base, "+", entry)),
    lvl_entS     = as.formula(paste(dep,     "~", lag, "+", base, "+", entryS)),
    lvl_entZ     = as.formula(paste(dep,     "~", lag, "+", base, "+", entryZ)),
  
    lvl_bkl      = as.formula(paste(dep,     "~", lag, "+", base, "+", backlog)),
    lvl_bklS     = as.formula(paste(dep,     "~", lag, "+", base, "+", backlogS)),
    lvl_bklZ     = as.formula(paste(dep,     "~", lag, "+", base, "+", backlogZ)),
    
    lvl_del  = as.formula(paste(dep,     "~", lag, "+", base, "+", del)),
    lvl_delS = as.formula(paste(dep,     "~", lag, "+", base, "+", delS)),
    lvl_delZ = as.formula(paste(dep,     "~", lag, "+", base, "+", delZ)),
    
    lvl_ln_del  = as.formula(paste(dep,     "~", lag, "+", base, "+", log_del)),
    lvl_ln_delS = as.formula(paste(dep,     "~", lag, "+", base, "+", log_delS)),
    lvl_ln_delZ = as.formula(paste(dep,     "~", lag, "+", base, "+", log_delZ)),
    
    # Log specs
    log_lag      = as.formula(paste(logdep,  "~", loglag)),
    log_base0_lag = as.formula(paste(logdep,  "~", loglag, "+", base_0)),
    log_base_lag = as.formula(paste(logdep,  "~", loglag, "+", base)),
    
    log_ent      = as.formula(paste(logdep,  "~", loglag, "+", base, "+", entry)),
    log_entS     = as.formula(paste(logdep,  "~", loglag, "+", base, "+", entryS)),
    log_entZ     = as.formula(paste(logdep,  "~", loglag, "+", base, "+", entryZ)),
    
    log_bkl      = as.formula(paste(logdep,  "~", loglag, "+", base, "+", backlog)),
    log_bklS     = as.formula(paste(logdep,  "~", loglag, "+", base, "+", backlogS)),
    log_bklZ     = as.formula(paste(logdep,  "~", loglag, "+", base, "+", backlogZ)),
    
    log_del  = as.formula(paste(logdep,     "~", loglag, "+", base, "+", del)),
    log_delS = as.formula(paste(logdep,     "~", loglag, "+", base, "+", delS)),
    log_delZ = as.formula(paste(logdep,     "~", loglag, "+", base, "+", delZ)),
    
    log_ln_del  = as.formula(paste(logdep,     "~", loglag, "+", base, "+", log_del)),
    log_ln_delS = as.formula(paste(logdep,     "~", loglag, "+", base, "+", log_delS)),
    log_ln_delZ = as.formula(paste(logdep,     "~", loglag, "+", base, "+", log_delZ))
    
  )
}


# 2) Create forms for POI and Network
poi_forms_trans <- make_transition_forms(
  dep    = "cost_poi",
  logdep = "log1p_cost_poi",
  lag    = "poi_I",
  loglag = "poi_log_I"
)
net_forms_trans <- make_transition_forms(
  dep    = "cost_network",
  logdep = "log1p_cost_network",
  lag    = "net_I",
  loglag = "net_log_I"
)

# 3) Specify the desired order of the first 8 specs: lag, base, entering, enteringS, backlog, backlogS, enteringZ, backlogZ
level_order <- c(
  "lvl_lag",       # lag only
  "lvl_base0_lag",
  "lvl_base_lag",  # lag + base
  
  "lvl_ent",       # lag + entering
  "lvl_entS",      # lag + entering×shared
  "lvl_entZ",      # lag + entering×shared + area FE
  "lvl_bkl",       # lag + backlog
  "lvl_bklS",      # lag + backlog×shared
  
  "lvl_bklZ",       # lag + backlog×shared + area FE
  "lvl_del",
  "lvl_delS",
  "lvl_delZ",
  "lvl_ln_del",
  "lvl_ln_delS",
  "lvl_ln_delZ"
)
# And the same for logs
log_order <- sub("^lvl_", "log_", level_order)

# 4) Re-order the form lists
poi_forms_lvl <- poi_forms_trans[level_order]
poi_forms_log <- poi_forms_trans[log_order]
net_forms_lvl <- net_forms_trans[level_order]
net_forms_log <- net_forms_trans[log_order]

# 5) Fit helper
fit_transitions <- function(forms) {
  mods  <- map(forms, ~ lm(.x, data = cost_transitions_data))
  ses   <- map(mods,  ~ sqrt(diag(vcovHC(.x, "HC1"))))
  rmses <- map_dbl(mods, ~ sqrt(mean(residuals(.x)^2, na.rm = TRUE)))
  list(models = mods, ses = ses, rmses = rmses)
}

poi_lvl_trans <- fit_transitions(poi_forms_lvl)
poi_log_trans <- fit_transitions(poi_forms_log)
net_lvl_trans <- fit_transitions(net_forms_lvl)
net_log_trans <- fit_transitions(net_forms_log)

# 6) Common covariate labels, match the 8 specs
cov_labels <- c(
  "Lag Cost ($/kW)",
  "MW (Own Capacity)", "FCDS", "Wind",  "Battery",
  "Co located with a solar plant",  voltage_labels,
  "Shared Cost Phase I",
  "Lag AMW Entering (GW)",
  "Lag AMW Entering × Shared Cost Phase I",
  "Lag AMW Backlog (GW)",
  "Lag AMW Backlog × Shared Cost Phase I",
  "Lag AMW Delta Backlog (GW)",
  "Lag AMW Delta Backlog (GW) x Shared Cost Phase I",
  "Lag Log AMW Delta Backlog (GW)",
  "Lag Log AMW Delta Backlog (GW) x Shared Cost Phase I",
  "Intercept"
)


# 7) Writer for 8‐column tables
write_eight <- function(tr, cost, scale) {
  mods  <- tr$models
  ses   <- tr$ses
  rmses <- tr$rmses
  
  dv_lab <- if (cost == "poi") {
    if (scale=="level") rep("POI Cost ($/kW)", 15) else rep("Log POI Cost ($/kW)", 15)
  } else {
    if (scale=="level") rep("Network Cost ($/kW)", 15) else rep("Log Network Cost ($/kW)", 15)
  }
  
  cluster_line <- c("Cluster FE", rep("Yes", 15))
  zone_line    <- c("area FE", rep(c("No","No","No","No","No","Yes","No", "No", "Yes","No", "No", "Yes", "No", "No", "Yes"), length.out=15))
  rmse_line    <- c("RMSE", format(round(rmses,2), nsmall=2))
  
  out_html <- paste0(out_dir, "transitions_", cost, "_", scale, ".html")
  out_tex  <- sub("\\.html$", ".tex", out_html)
  title    <- paste("Phase I to II", tolower(cost), scale, "transition")
  
  stargazer(
    mods, se=ses, type="html", style="qje",
    out=out_html, title=title,
    dep.var.labels.include=FALSE,
    column.labels=dv_lab,
    covariate.labels=cov_labels,
    omit=c("factor\\(cluster\\)","factor\\(area_code\\)"),
    add.lines=list(cluster_line, zone_line, rmse_line),
    omit.stat=c("f","ser","rsq"),
    digits=2
  )
  stargazer(
    mods, se=ses, type="latex", style="qje",
    out=out_tex, 
    title=title,
    dep.var.labels.include=FALSE,
    column.labels=dv_lab,
    
    covariate.labels=cov_labels,
    omit=c("factor\\(cluster\\)","factor\\(area_code\\)"),
    add.lines=list(cluster_line, zone_line, rmse_line),
    omit.stat=c("f","ser","rsq"),
    digits=2
  )
  
  # --- 3) Post‐process .tex: escape all $ → \$ ---
  tex_txt <- readLines(out_tex )
# replace every occurrence of "$/kW" with "\$/kW"
# in R strings: pattern "\\$/kW" replacement "\\\\$/kW"
tex_txt <- gsub(
  pattern     = "\\$/kW",
  replacement = "\\\\$/kW",
  x           = tex_txt,
  perl        = TRUE
)
  writeLines(tex_txt, out_tex )
}

# 8) Write all four tables
write_eight(poi_lvl_trans, "poi",     "level")
write_eight(poi_log_trans, "poi",     "log")
write_eight(net_lvl_trans, "network", "level")
write_eight(net_log_trans, "network", "log")

message("All four 15‐column transition tables written to ", out_dir)




 
######---Final Regression Table with different cost dependency measures

#--------------
 
 


# 1) Build all transition formulas (16 per outcome)
make_transition_forms <- function(dep, logdep, lag, loglag) {
  base_0   <- paste(
    "factor(cluster)", "MW", "FCDS",
    "isWind", "isBattery",
    "factor(co_located)", "voltage_group",
    sep = " + "
  )
  base   <- paste(
    "factor(cluster)", "MW", "FCDS",
    "isWind", "isBattery",
    "factor(co_located)", "voltage_group", "iscost_shared_ph1",
    sep = " + "
  )
  entry    <- "AMW_at_I_gW"
  backlog  <- "AMW_backlog_I_gW"
  del   <- "lag_AMW_backlog_delta_at_gW"
  log_del <- "lag_log_AMW_backlog_delta_at_gW"
  entryS   <- paste(entry, "+", entry, ":iscost_shared_ph1")
  backlogS <- paste(backlog, "+", backlog, ":iscost_shared_ph1")
  delS  <-   paste(del, "+", del, ":iscost_shared_ph1")
  log_delS <- paste(log_del, "+", log_del, ":iscost_shared_ph1")
  entryZ   <- paste(entryS, "+ factor(area_code)")
  backlogZ <- paste(backlogS, "+ factor(area_code)")
  delZ <- paste(delS, "+ factor(area_code)")
  log_delZ <- paste(log_delS, "+ factor(area_code)")
  
  list(
    # Level specs
    lvl_lag      = as.formula(paste(dep,     "~", lag)),
    lvl_base0_lag = as.formula(paste(dep,     "~", lag, "+", base_0)),
    lvl_base_lag = as.formula(paste(dep,     "~", lag, "+", base)),
    lvl_ent      = as.formula(paste(dep,     "~", lag, "+", base, "+", entry)),
    lvl_entS     = as.formula(paste(dep,     "~", lag, "+", base, "+", entryS)),
    lvl_entZ     = as.formula(paste(dep,     "~", lag, "+", base, "+", entryZ)),
    
    lvl_bkl      = as.formula(paste(dep,     "~", lag, "+", base, "+", backlog)),
    lvl_bklS     = as.formula(paste(dep,     "~", lag, "+", base, "+", backlogS)),
    lvl_bklZ     = as.formula(paste(dep,     "~", lag, "+", base, "+", backlogZ)),
    
    lvl_del  = as.formula(paste(dep,     "~", lag, "+", base, "+", del)),
    lvl_delS = as.formula(paste(dep,     "~", lag, "+", base, "+", delS)),
    lvl_delZ = as.formula(paste(dep,     "~", lag, "+", base, "+", delZ)),
    
    lvl_ln_del  = as.formula(paste(dep,     "~", lag, "+", base, "+", log_del)),
    lvl_ln_delS = as.formula(paste(dep,     "~", lag, "+", base, "+", log_delS)),
    lvl_ln_delZ = as.formula(paste(dep,     "~", lag, "+", base, "+", log_delZ)),
    
    # Log specs
    log_lag      = as.formula(paste(logdep,  "~", loglag)),
    log_base0_lag = as.formula(paste(logdep,  "~", loglag, "+", base_0)),
    log_base_lag = as.formula(paste(logdep,  "~", loglag, "+", base)),
    
    log_ent      = as.formula(paste(logdep,  "~", loglag, "+", base, "+", entry)),
    log_entS     = as.formula(paste(logdep,  "~", loglag, "+", base, "+", entryS)),
    log_entZ     = as.formula(paste(logdep,  "~", loglag, "+", base, "+", entryZ)),
    
    log_bkl      = as.formula(paste(logdep,  "~", loglag, "+", base, "+", backlog)),
    log_bklS     = as.formula(paste(logdep,  "~", loglag, "+", base, "+", backlogS)),
    log_bklZ     = as.formula(paste(logdep,  "~", loglag, "+", base, "+", backlogZ)),
    
    log_del  = as.formula(paste(logdep,     "~", loglag, "+", base, "+", del)),
    log_delS = as.formula(paste(logdep,     "~", loglag, "+", base, "+", delS)),
    log_delZ = as.formula(paste(logdep,     "~", loglag, "+", base, "+", delZ)),
    
    log_ln_del  = as.formula(paste(logdep,     "~", loglag, "+", base, "+", log_del)),
    log_ln_delS = as.formula(paste(logdep,     "~", loglag, "+", base, "+", log_delS)),
    log_ln_delZ = as.formula(paste(logdep,     "~", loglag, "+", base, "+", log_delZ))
    
  )
}


# 2) Create forms for POI and Network
poi_forms_trans_compact <- make_transition_forms(
  dep    = "cost_poi",
  logdep = "log1p_cost_poi",
  lag    = "poi_I",
  loglag = "poi_log_I"
)
net_forms_trans_compact <- make_transition_forms(
  dep    = "cost_network",
  logdep = "log1p_cost_network",
  lag    = "net_I",
  loglag = "net_log_I"
)

# 3) Specify the desired order of the first 8 specs: lag, base, entering, enteringS, backlog, backlogS, enteringZ, backlogZ
level_order <- c(
  "lvl_lag",       # lag only
  "lvl_base0_lag",
  "lvl_base_lag",  # lag + base
  
  "lvl_ent",       # lag + entering
  "lvl_entS",      # lag + entering×shared
  "lvl_entZ",      # lag + entering×shared + area FE
  "lvl_bkl",       # lag + backlog
  "lvl_bklS",      # lag + backlog×shared
  
  "lvl_bklZ",       # lag + backlog×shared + area FE
  "lvl_del",
  "lvl_delS",
  "lvl_delZ",
  "lvl_ln_del",
  "lvl_ln_delS",
  "lvl_ln_delZ"
)
# And the same for logs
log_order <- sub("^lvl_", "log_", level_order)

# 4) Re-order the form lists
poi_forms_lvl <- poi_forms_trans_compact[level_order]
poi_forms_log <- poi_forms_trans_compact[log_order]
net_forms_lvl <- net_forms_trans_compact[level_order]
net_forms_log <- net_forms_trans_compact[log_order]

# 5) Fit helper
fit_transitions <- function(forms) {
  mods  <- map(forms, ~ lm(.x, data = cost_transitions_data))
  ses   <- map(mods,  ~ sqrt(diag(vcovHC(.x, "HC1"))))
  rmses <- map_dbl(mods, ~ sqrt(mean(residuals(.x)^2, na.rm = TRUE)))
  list(models = mods, ses = ses, rmses = rmses)
}

poi_lvl <- fit_transitions(poi_forms_lvl)
poi_log <- fit_transitions(poi_forms_log)
net_lvl <- fit_transitions(net_forms_lvl)
net_log <- fit_transitions(net_forms_log)




# 7) Writer for 8‐column tables
# 5) A new writer: only the four “Z” specifications
# ----------------------------------------------------------------------------
write_eightZ <- function(tr, cost, scale) {
  # Only models whose names start with the right prefix *and* end in "Z"
  prefix <- if (scale=="level") "lvl_" else "log_"
  all_names <- names(tr$models)
  pick <- all_names[
    startsWith(all_names, prefix)  & 
      endsWith(all_names,   "Z")
  ]
  
  # Sanity‐check: this must be length 4
  if (length(pick) != 4) {
    stop("Expected 4 Z‐models, but pick = ", paste(pick, collapse = ", "))
  }
  modsZ  <- tr$models[pick]
  sesZ   <- tr$ses[pick]
  rmsesZ <- tr$rmses[pick]
  n      <- length(modsZ)  # now guaranteed to be 4
  # column labels
  dv_lab <- rep(
    if (cost=="poi") if (scale=="level") "POI Cost ($/kW)" else "Log POI Cost ($/kW)"
    else           if (scale=="level") "Network Cost ($/kW)" else "Log Network Cost ($/kW)",
    n
  )
  
  # extra lines
  cluster_ln <- c("Cluster FE", rep("Yes", n))
  zone_ln    <- c("area FE",    rep("Yes", n))
  rmse_ln    <- c("RMSE",       format(round(rmsesZ, 2), nsmall = 2))
 
  
  out_html <- paste0(out_dir, "complete_transitions_", cost, "_", scale, ".html")
  out_tex  <- sub("\\.html$", ".tex", out_html)
  title    <- paste("Phase I to II", tolower(cost), scale, "transition (Full Model)")
  
  
  row_order <- c(
    "poi_log_I$", 
    "net_log_I$",                                 # the lag in your network log model
    "MW$",                                        # base
    "FCDS$",
    "isWind$",
    "isBattery$",
    "factor\\(co_located\\)1$",                  # note the escaped parens
    "voltage_group\\.L$",
    "voltage_group\\.Q$",
    "iscost_shared_ph1$",
    
    "AMW_at_I_gW$",
    "iscost_shared_ph1:AMW_at_I_gW$",
    
    "AMW_backlog_I_gW$",
    "iscost_shared_ph1:AMW_backlog_I_gW$",
    
    "lag_AMW_backlog_delta_at_gW$",
    "iscost_shared_ph1:lag_AMW_backlog_delta_at_gW$",
    
    "lag_log_AMW_backlog_delta_at_gW$",
    "iscost_shared_ph1:lag_log_AMW_backlog_delta_at_gW$"
  )
  
  # 6) Common covariate labels, match the 8 specs
  cov_labels_Z <- c(
    # base controls
    "Lag Cost ($/kW)",
    "MW (Own Capacity)", "FCDS", "Wind", "Battery",
    "Co-located with a Solar Plant", voltage_labels,
    "Shared Cost Phase I",
    
    # Z-specific terms
    "Lag AMW Entering (GW)", "Lag AMW Entering × Shared Cost Phase I",
    "Lag AMW Backlog (GW)",  "Lag AMW Backlog × Shared Cost Phase I",
    "Lag AMW Delta Backlog (GW)", "Lag AMW Delta Backlog (GW) x Shared Cost Phase I",
    "Lag Log AMW Delta Backlog (GW)", "Lag Log AMW Delta Backlog (GW) x Shared Cost Phase I"
  )
  
  
  stargazer(
    modsZ, se = sesZ, type="html", style="qje",
    out=out_html, title=title,
    dep.var.labels.include=FALSE,
    order = row_order,
    column.labels=dv_lab,
    covariate.labels=cov_labels_Z,
    omit=c("factor\\(cluster\\)","factor\\(area_code\\)"),
    add.lines = list(cluster_ln, zone_ln, rmse_ln),
    omit.stat=c("f","ser","rsq"),
    digits=2
  )
  stargazer(
    modsZ, se = sesZ, type="latex", style="qje",
    out=out_tex, 
    title=title,
    dep.var.labels.include=FALSE,
    order = row_order,
    column.labels=dv_lab,
    covariate.labels=cov_labels_Z,
    omit=c("factor\\(cluster\\)","factor\\(area_code\\)"),
    add.lines = list(cluster_ln, zone_ln, rmse_ln),
    omit.stat=c("f","ser","rsq"),
    digits=2
  )
  
  # --- 3) Post‐process .tex: escape all $ → \$ ---
  tex_txt <- readLines(out_tex )
  # replace every occurrence of "$/kW" with "\$/kW"
  # in R strings: pattern "\\$/kW" replacement "\\\\$/kW"
  tex_txt <- gsub(
    pattern     = "\\$/kW",
    replacement = "\\\\$/kW",
    x           = tex_txt,
    perl        = TRUE
  )
  writeLines(tex_txt, out_tex )
}

# 8) Write all four tables
write_eightZ(poi_lvl, "poi",     "level")
write_eightZ(poi_log, "poi",     "log")
write_eightZ(net_lvl, "network", "level")
write_eightZ(net_log, "network", "log")

message("All four 4-column transition tables written to ", out_dir)

 


#------- Consolidated Tables -------------------------

 

# --- 1) Extract the four lm objects ----------------------------------------

m1 <- poi_log_ic$models[["log_log_dZ"]]        # initial POI, log Δ AMW + cluster & area FE
m2 <- poi_log_trans$models[["log_ln_delZ"]]  # transition POI, lag log Δ AMW + cluster & area FE

m3 <- net_log_ic$models[["log_log_dZ"]]        # initial Network
m4 <- net_log_trans$models[["log_ln_delZ"]]  # transition Network

# --- 2) Compute clustered‐robust SEs (HC1) ----------------------------------

se1 <- sqrt(diag(vcovHC(m1, "HC1")))
se2 <- sqrt(diag(vcovHC(m2, "HC1")))
se3 <- sqrt(diag(vcovHC(m3, "HC1")))
se4 <- sqrt(diag(vcovHC(m4, "HC1")))

models <- list(m1, m2, m3, m4)
rmse_list <- map_dbl(models, ~ sqrt(mean(residuals(.x)^2, na.rm = TRUE)))


 

# --- 3) Define column labels & covariate labels ----------------------------

dv_labels <- c(
  "POI Cost ($/kW)",  # col 1
  "POI Cost ($/kW)",  # col 2
  "Network Cost ($/kW)",  # col 3
  "Network Cost ($/kW)"   # col 4
)
# each label spans exactly one column:
colsep <- rep(1, 4)

# what names does stargazer see for each model?
print(names(coef(m1)))
print(names(coef(m2)))
#print(names(coef(m3)))
#print(names(coef(m4)))


# 2) extract coefs & SEs in that order for each model
#coef_list <- list(
#  coef(m1)[rows_order],
#  coef(m2)[rows_order],
#  coef(m3)[rows_order],
#  coef(m4)[rows_order]
#)
#se_list   <- list(
#  se1[rows_order],
#  se2[rows_order],
#  se3[rows_order],
#  se4[rows_order]
#)


rows_order <- c(
  # your two key rows first:
  "poi_log_I",
  "net_log_I",
  # now all your controls, in the order you like:
  "MW",
  "FCDS",
  "isWind",
  "isBattery",
  "factor\\(co_located\\)1",
  "voltage_group\\.L",
  "voltage_group\\.Q",
  "log_AMW_backlog_delta_at_gW",
  "lag_log_AMW_backlog_delta_at_gW",
  "iscost_shared_ph1",
  "iscost_shared_ph1:lag_log_AMW_backlog_delta_at_gW"
 )

cov_labels <- c(
  "Lag POI Cost ($/kW)",
  "Lag Network Cost ($/kW)",
  "MW (Own Capacity)", "FCDS", "Wind", "Battery",
  "Co-located with a Solar Plant", voltage_labels,
  "Log $\\Delta$ AMW Backlog (GW)",
  "Shared Cost Phase I",
  "Lag Log $\\Delta$ AMW Backlog (GW)",
  "Lag Log $\\Delta$ AMW Backlog (GW) x Shared Phase I cost"
)

n<- 4
cluster_ln <- c("Cluster FE", rep("Yes", n))
zone_ln    <- c("area FE",    rep("Yes", n))
rmse_ln    <- c("RMSE",       format(round(rmse_list, 2), nsmall = 2))

# --- 4) Write one combined LaTeX table -------------------------------------

stargazer(
  m1, m2, m3, m4,
  se              = list(se1, se2, se3, se4),
  type            = "html",
  style           = "qje",
  out             = paste0(out_dir, "consolidated_initial_transition_regressions.html"),
  title           = "Final Initial and Transition (PI to PII) Regressions",
  dep.var.labels.include = FALSE,
  dep.var.caption = "",          # force the header row
  column.labels  = dv_labels,
  column.separate = colsep,
  omit           = c("factor\\(cluster\\)","factor\\(area_code\\)", "Constant"), 
  covariate.labels = cov_labels,
  omit.stat      = c("f","ser","rsq"),
  digits         = 2,
  add.lines = list(cluster_ln, zone_ln, rmse_ln),
 notes          = "All SEs are HC1‐robust. Voltage below 100 kV reference category.Columns 1–2: POI; 3–4: Network."
)

out_tex = paste0(out_dir, "consolidated_initial_transition_regressions.tex")
stargazer(
  m1, m2, m3, m4,
  se              = list(se1, se2, se3, se4),
  type            = "latex",
  style           = "qje",
  out             = paste0(out_dir, "consolidated_initial_transition_regressions.tex"),
  title           = "Final initial cost estimate and cost transition estimate (PI to PII) regressions",
  dep.var.labels.include = FALSE,
  dep.var.caption = "",          # force the header row
  column.labels  = dv_labels,
  column.separate = colsep,
  omit           = c("factor\\(cluster\\)","factor\\(area_code\\)", "Constant"), 
  covariate.labels = cov_labels,
  omit.stat      = c("f","ser","rsq"),
  add.lines = list(cluster_ln, zone_ln, rmse_ln),
  digits         = 2,
  
  notes          = "All SEs are HC1‐robust. Voltage $<$100 kV reference category.Columns 1–2: POI; 3–4: Network."
)

tex_txt <- readLines(out_tex )
# replace every occurrence of "$/kW" with "\$/kW"
# in R strings: pattern "\\$/kW" replacement "\\\\$/kW"
tex_txt <- gsub(
  pattern     = "\\$/kW",
  replacement = "\\\\$/kW",
  x           = tex_txt,
  perl        = TRUE
)
writeLines(tex_txt, out_tex)











cost_transitions_data %>%
  filter(checklist_phase=="Phase II Study") %>%
  summarize(
    co_loc_levels = n_distinct(co_located),
    volt_levels   = n_distinct(voltage_group),
    shared_levels = n_distinct(iscost_shared_ph1, na.rm=TRUE)
  )






# -----------------------------------------------------IV Regressions ------------------------------------


library(dplyr)
library(AER)       # for ivreg()
library(sandwich) # for vcovHC()
library(stargazer)

run_network_iv_multi <- function(
    data           = cost_transitions_data,
    poi_thresh_n   = 80,                  # raw threshold
    out_dir        = paste0(project_root,"output/tables/project_static_dynamic/"),
    controls       = c(
      "MW", "FCDS",
      "isWind", "isBattery",
      "factor(co_located)", "voltage_group",
      "iscost_shared_ph1"
    )
) {
  dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
  
  # natural‐log threshold
  ln_thr <- log(poi_thresh_n)
  
  # 1) build instruments by area/year
  iv_tbl <- data %>%
    # only Phase I rows to get poi_log_I
    filter(checklist_phase=="Phase I Study") %>%
    mutate(
      year = calendar_year,
      # poi_log_I already exists
      flag = if_else(poi_log_I > ln_thr, poi_log_I, NA_real_)
    ) %>%
    group_by(area_code, year) %>%
    summarize(
      iv_mean = mean(flag, na.rm=TRUE),
      iv_p25  = quantile(flag, .25, na.rm=TRUE),
      iv_p50  = quantile(flag, .50, na.rm=TRUE),
      iv_p75  = quantile(flag, .75, na.rm=TRUE),
      .groups="drop"
    ) %>%
    replace_na(list(
      iv_mean=0, iv_p25=0, iv_p50=0, iv_p75=0
    ))
  
  # 2) merge onto the Phase II sample
  df <- data %>%
    filter(checklist_phase=="Phase II Study") %>%
    left_join(iv_tbl, by = c("area_code","calendar_year"="year"))
  
  # 3) run first‑stage & IV regressions
  specs <- c("iv_mean","iv_p25","iv_p50","iv_p75")
  fs_mods <- iv_mods <- list()
  
  ctrl_str <- paste(controls, collapse=" + ")
  for(s in specs) {
    # first stage
    fm1 <- as.formula(
      paste0("lag_log_AMW_backlog_delta_at_gW ~ ", s, " + ", ctrl_str)
    )
    fs_mods[[s]] <- lm(fm1, data=df)
    
    # IV second stage
    fm2 <- as.formula(
      paste0(
        "log1p_cost_network ~ lag_log_AMW_backlog_delta_at_gW + ", ctrl_str,
        " | ", s, " + ", ctrl_str
      )
    )
    iv_mods[[s]] <- ivreg(fm2, data=df)
  }
  
  # 4) extract coefs & SEs (drop intercept)
  rows <- c("lag_log_AMW_backlog_delta_at_gW", controls)
  coef_list <- se_list <- list()
  for(i in seq_along(specs)) {
    s <- specs[i]
    fs <- fs_mods[[s]]; iv <- iv_mods[[s]]
    coef_list[[2*i-1]] <- coef(fs)[rows]
    coef_list[[2*i]]   <- coef(iv)[rows]
    se_list[[2*i-1]]   <- sqrt(diag(vcovHC(fs, "HC1")))[rows]
    se_list[[2*i]]     <- sqrt(diag(vcovHC(iv, "HC1")))[rows]
  }
  
  # 5) column labels & separation
  dv_lab <- unlist(lapply(specs, function(s) c(
    paste0("FS (", s, ")"),
    paste0("IV (", s, ")")
  )))
  colsep <- rep(1, length(dv_lab))
  
  # 6) covariate labels
  cov_labels <- c(
    "Lag Log Δ AMW Backlog",
    "MW (Own Cap.)",
    "FCDS",
    "Wind",
    "Battery",
    "Interconnection Req’d",
    "Volt. (L)",
    "Volt. (Q)",
    "Shared Phase I"
  )
  
  # 7) output files
  out_html <- paste0(out_dir, "network_cost_iv_multi.html")
  out_tex  <- paste0(out_dir, "network_cost_iv_multi.tex")
  
  # HTML
  stargazer(
    fs_mods, iv_mods,
    coef             = coef_list,
    se               = se_list,
    type             = "html",
    style            = "qje",
    out              = out_html,
    title            = "FS & IV (mean, p25, p50, p75 instruments)",
    dep.var.labels.include = FALSE,
    covariate.labels = cov_labels,
    column.labels    = dv_lab,
    column.separate  = colsep,
    omit.stat        = c("f","ser","rsq"),
    digits           = 2,
    notes            = "HC1 SEs; FS=first‑stage, IV=second‑stage"
  )
  
  # LaTeX
  stargazer(
    fs_mods, iv_mods,
    coef             = coef_list,
    se               = se_list,
    type             = "latex",
    style            = "qje",
    out              = out_tex,
    title            = "FS & IV (mean, p25, p50, p75 instruments)",
    dep.var.labels.include = FALSE,
    covariate.labels = cov_labels,
    column.labels    = dv_lab,
    column.separate  = colsep,
    omit.stat        = c("f","ser","rsq"),
    digits           = 2,
    notes            = "HC1 SEs; FS=first‑stage, IV=second‑stage"
  )
  
  # 8) post‑process .tex to escape $/kW
  txt <- readLines(out_tex)
  txt <- gsub("\\$/kW","\\\\$/kW",txt,perl=TRUE)
  writeLines(txt, out_tex)
}

# — run it —
run_network_iv_multi()







## IFS payment exceeding 30% cost calc


#--------------------------------------------------------------------------------------------------------------

 







 

IFS_checks <- project_static_dynamic %>%
  # 1) Keep only Phase I & II + Reassessments
  filter(checklist_phase %in% c("Phase I Study", "Phase II Study", "Reassessment")) %>%
  
  # 2) Time-order within each project
  arrange(q_id, status_date) %>%
  group_by(q_id) %>%
  
  # 3) Compute running minima
  mutate(
    min_poi_so_far     = cummin(cost_poi),
    min_network_so_far = cummin(cost_network)
  ) %>%
  
  # 4) Compute fee_calc
  mutate(
    fee_calc = case_when(
      checklist_phase == "Phase I Study" ~ 0.15 * (cost_poi + cost_network),
      checklist_phase == "Phase II Study" ~ 0.30 * (min_poi_so_far  + min_network_so_far),
      str_detect(checklist_phase, regex("Reassessment", ignore_case = TRUE)) ~
        0.30 * (min_poi_so_far + min_network_so_far),
      TRUE ~ NA_real_
    )
  ) %>%
  
  # 5) Label each phase (use year for reassessments)
  mutate(
    phase_label = case_when(
      checklist_phase == "Phase I Study"  ~ "Phase I Study",
      checklist_phase == "Phase II Study" ~ "Phase II Study",
      checklist_phase == "Reassessment"    ~ paste0("Reassessment ", calendar_year)
    )
  ) %>%
  ungroup() %>%
  
  # 6) Tidy up
  relocate(min_poi_so_far,     .after = cost_poi) %>%
  relocate(min_network_so_far, .after = cost_network) %>%
  relocate(fee_calc,           .after = min_network_so_far)

fee_comparison <- IFS_checks %>%
  group_by(q_id) %>%
  arrange(status_date, .by_group = TRUE) %>%
  mutate(next_fee = lead(fee_calc)) %>%
  ungroup() %>%
  filter(!is.na(cost_poi)) %>%
  group_by(current_phase = phase_label) %>%
  summarise(
    n_projects       = n_distinct(q_id),
    n_higher         = sum(fee_calc > next_fee, na.rm = TRUE),
    prop_higher      = n_higher / n_projects,
    avg_pct_increase = {
      valid <- next_fee != 0
      mean((fee_calc[valid] - next_fee[valid]) / next_fee[valid] * 100,
           na.rm = TRUE)
    }
  ) %>%
  arrange(current_phase)

fee_comparison

fee_comparison <- fee_comparison %>%
  rename(
    "Current Phase"       = current_phase,
    "Count"               = n_projects,
    "Count Higher"        = n_higher,
    "Proportion Higher"   = prop_higher,
    "Percentage Increase (Payment)" = avg_pct_increase
  ) %>%  mutate(
    # Counts are integers already, so no decimals there
    `Proportion Higher`   = round(`Proportion Higher`,   2),
    `Percentage Increase (Payment)` = round(`Percentage Increase (Payment)`, 2)
  )


table_name <- "fee_comparison"                # no extension

# make sure the directory exists
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# file paths
html_path <- paste0(out_dir, "/", table_name, ".html")
tex_path  <- paste0(out_dir, "/", table_name, ".tex")

# export to HTML
stargazer(
  fee_comparison,
  summary = FALSE,           # print the raw tibble, not summary stats
  rownames = FALSE,
  type    = "html",
  out     = html_path,
  title   = "Fee Comparison by Phase",
  digits  = 2
)

# export to LaTeX
stargazer(
  fee_comparison,
  summary = FALSE,
  rownames = FALSE,
  type    = "latex",
  out     = tex_path,
  title   = "Fee Comparison by Phase",
  digits  = 2
)





## Summary stats for network shared and network leverage:
#----------------------------------------------------------------------------------------------------------------------------------------


# --- 1) Network Shared summary by phase --------------------------------------
shared_summary <- project_static_dynamic %>%
  filter(checklist_phase %in% c("Phase I Study","Phase II Study")) %>%
  group_by(Phase = checklist_phase) %>%
  summarise(
    Min     = round(min(cost_network_shared,    na.rm=TRUE), 2),
    Q1      = round(unname(quantile(cost_network_shared, .25, na.rm=TRUE)), 2),
    Mean    = round(mean(cost_network_shared,   na.rm=TRUE), 2),
    Median  = round(median(cost_network_shared, na.rm=TRUE), 2),
    Q3      = round(unname(quantile(cost_network_shared, .75, na.rm=TRUE)), 2),
    Max     = round(max(cost_network_shared,    na.rm=TRUE), 2),
    SD      = round(sd(cost_network_shared,     na.rm=TRUE), 2),
    N       = n(),
    Count   = sum(is.finite(cost_network_shared)),
    .groups         = "drop"
  ) %>%
  arrange(match(Phase, c("Phase I Study","Phase II Study")))

# export HTML
stargazer(
  shared_summary,
  type             = "html",
  out              = paste0(out_dir, "phase_shared_summary.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Own Network Cost Shared (ratio) by Phase",
  covariate.labels = names(shared_summary),
  digits           = 2
)
# export LaTeX
stargazer(
  shared_summary,
  type             = "latex",
  out              = paste0(out_dir, "phase_shared_summary.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Own Network Cost Shared (ratio) by Phase",
  covariate.labels = names(shared_summary),
  digits           = 2
)


# --- 2) Network Leverage summary by phase ------------------------------------
lev_summary <- project_static_dynamic %>%
  filter(checklist_phase %in% c("Phase I Study","Phase II Study")) %>%
  group_by(Phase = checklist_phase) %>%
  summarise(
    Min         = round(min(cost_network_leverage,    na.rm=TRUE), 2),
    Q1          = round(unname(quantile(cost_network_leverage, .25, na.rm=TRUE)), 2),
    Mean        = round(mean(cost_network_leverage,   na.rm=TRUE), 2),
    Median      = round(median(cost_network_leverage, na.rm=TRUE), 2),
    Q3          = round(unname(quantile(cost_network_leverage, .75, na.rm=TRUE)), 2),
    Max         = round(max(cost_network_leverage,    na.rm=TRUE), 2),
    SD          = round(sd(cost_network_leverage,     na.rm=TRUE), 2),
    N         = n(),                                        # all rows
    Count  = sum(is.finite(cost_network_leverage)),      # only finite values
 
    .groups         = "drop"
  ) %>%
  arrange(match(Phase, c("Phase I Study","Phase II Study")))

# export HTML
stargazer(
  lev_summary,
  type             = "html",
  out              = paste0(out_dir, "phase_leverage_summary.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Network Cost Leverage by Phase",
  covariate.labels = names(lev_summary),
  digits           = 2
)
# export LaTeX
stargazer(
  lev_summary,
  type             = "latex",
  out              = paste0(out_dir, "phase_leverage_summary.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Network Cost Leverage by Phase",
  covariate.labels = names(lev_summary),
  digits           = 2
)






# Frequency tables:


library(dplyr)
library(tidyr)
library(stargazer)

# --- 1) Frequency table for cost_network_shared ---
shared_freq <- project_static_dynamic %>%
  filter(checklist_phase %in% c("Phase I Study","Phase II Study")) %>%
  filter(is.finite(cost_network_shared)) %>%
  mutate(
    bin = factor(
      case_when(
        cost_network_shared < 0.2  ~ "0-0.2",
        cost_network_shared < 0.5  ~ "0.2-0.5",
        cost_network_shared < 0.8  ~ "0.5-0.8",
        cost_network_shared <= 1   ~ "0.8-1",
        TRUE                       ~ "1+"
      ),
      levels = c("0-0.2","0.2-0.5","0.5-0.8","0.8-1","1+")
    )
  ) %>%
  count(bin, Phase = checklist_phase) %>%
  pivot_wider(
    names_from  = Phase,
    values_from = n,
    values_fill = 0
  ) %>%
  # **Convert bin to character so stargazer prints the labels**
  mutate(bin = as.character(bin))

stargazer(
  shared_freq,
  type             = "latex",
  out              = paste0(out_dir, "freq_shared_by_phase.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Freq: Own Network Cost Shared (ratio) by Phase",
  covariate.labels = c("Bin","Phase I","Phase II")
)

# --- 2) Frequency table for cost_network_leverage ---
lev_freq <- project_static_dynamic %>%
  filter(checklist_phase %in% c("Phase I Study","Phase II Study")) %>%
  filter(is.finite(cost_network_leverage)) %>%
  mutate(
    bin = factor(
      case_when(
        cost_network_leverage < 1    ~ "<1",
        cost_network_leverage < 2    ~ "1-2",
        cost_network_leverage < 5    ~ "2-5",
        cost_network_leverage < 10   ~ "5-10",
        cost_network_leverage < 15   ~ "10-15",
        TRUE                         ~ "15+"
      ),
      levels = c("<1","1-2","2-5","5-10","10-15","15+")
    )
  ) %>%
  count(bin, Phase = checklist_phase) %>%
  pivot_wider(
    names_from  = Phase,
    values_from = n,
    values_fill = 0
  ) %>%
  # Convert bin to character as well
  mutate(bin = as.character(bin))

stargazer(
  lev_freq,
  type             = "latex",
  out              = paste0(out_dir, "freq_leverage_by_phase.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Freq: Network Cost Leverage by Phase",
  covariate.labels = c("Bin","Phase I","Phase II")
)



library(dplyr)
library(ggplot2)

# 1) Create plot directory
plot_dir <- paste0(project_root, "/output/figures/project_static_dynamic/")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# 2) Take your shared‐frequency table and pivot it long, then compute fractions
shared_long <- shared_freq %>%
  pivot_longer(
    -bin,
    names_to  = "Phase",
    values_to = "count"
  ) %>%
  group_by(Phase) %>%
  mutate(
    fraction = count / sum(count)
  ) %>%
  ungroup()

# 3) Plot fractional shares for Network Shared
p_shared_frac <- ggplot(shared_long, aes(x = bin, y = fraction)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ Phase, ncol = 1) +
  coord_flip() +
  labs(
    title = "Distribition of Own Network Cost Shared (Ratio)",
    x     = "Network Shared Bin",
    y     = "Fraction of Projects"
  ) +
  theme_minimal()

p_shared_frac

ggsave(
  filename = paste0(plot_dir, "network_shared_fraction.png"),
  plot     = p_shared_frac,
  width    = 6, height = 4, dpi = 300
)

# 4) Same for leverage
lev_long <- lev_freq %>%
  pivot_longer(
    -bin,
    names_to  = "Phase",
    values_to = "count"
  ) %>%
  group_by(Phase) %>%
  mutate(
    fraction = count / sum(count)
  ) %>%
  ungroup()

p_lev_frac <- ggplot(lev_long, aes(x = bin, y = fraction)) +
  geom_col(fill = "seagreen") +
  facet_wrap(~ Phase, ncol = 1) +
  coord_flip() +
  labs(
    title = "Distribition of Network Cost Leverage Ratio",
    x     = "Network Leverage Bin",
    y     = "Fraction of Projects"
  ) +
  theme_minimal()

p_lev_frac

ggsave(
  filename = paste0(plot_dir, "network_leverage_fraction.png"),
  plot     = p_lev_frac,
  width    = 6, height = 4, dpi = 300
)


















# --- 1) Prep Phase I panel, one row per project ------------------------------
phase1 <- project_static_dynamic %>%
  filter(checklist_phase == "Phase I Study", !is.na(Type)) %>%
  distinct(q_id, .keep_all = TRUE)

# --- 2) Summary stats for cost_network_shared by Type (Phase I) ---------------
cost_shared_stats_p1 <- phase1 %>%
  group_by(Type) %>%
  summarise(
    Min    = round(min(cost_network_shared,    na.rm = TRUE), 2),
    Q1     = round(unname(quantile(cost_network_shared, .25, na.rm = TRUE)), 2),
    Median = round(median(cost_network_shared, na.rm = TRUE), 2),
    Q3     = round(unname(quantile(cost_network_shared, .75, na.rm = TRUE)), 2),
    Max    = round(max(cost_network_shared,    na.rm = TRUE), 2),
    Mean   = round(mean(cost_network_shared,   na.rm = TRUE), 2),
    SD     = round(sd(cost_network_shared,     na.rm = TRUE), 2),
    N      = n()
  ) %>%
  ungroup() %>%
  arrange(match(Type, c("Battery","Solar","Wind","Others")))

stargazer(
  cost_shared_stats_p1,
  type             = "html",
  out              = paste0(out_dir, "cost_shared_stats_p1.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary: Own Network Cost Shared (Ratio) by Type (Phase I)",
  covariate.labels = names(cost_shared_stats_p1),
  digits           = 2
)
stargazer(
  cost_shared_stats_p1,
  type             = "latex",
  out              = paste0(out_dir, "cost_shared_stats_p1.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary: Own Network Cost Shared (Ratio) by Type (Phase I)",
  covariate.labels = names(cost_shared_stats_p1),
  digits           = 2
)

# --- 3) Summary stats for cost_network_leverage by Type (Phase I) -------------
cost_leverage_stats_p1 <- phase1 %>%
  group_by(Type) %>%
  summarise(
    Min    = round(min(cost_network_leverage,    na.rm = TRUE), 2),
    Q1     = round(unname(quantile(cost_network_leverage, .25, na.rm = TRUE)), 2),
    Median = round(median(cost_network_leverage, na.rm = TRUE), 2),
    Q3     = round(unname(quantile(cost_network_leverage, .75, na.rm = TRUE)), 2),
    Max    = round(max(cost_network_leverage,    na.rm = TRUE), 2),
    Mean   = round(mean(cost_network_leverage,   na.rm = TRUE), 2),
    SD     = round(sd(cost_network_leverage,     na.rm = TRUE), 2),
    N      = n()
  ) %>%
  ungroup() %>%
  arrange(match(Type, c("Battery","Solar","Wind","Others")))

stargazer(
  cost_leverage_stats_p1,
  type             = "html",
  out              = paste0(out_dir, "cost_leverage_stats_p1.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary: Network Leverage by Type (Phase I)",
  covariate.labels = names(cost_leverage_stats_p1),
  digits           = 2
)
stargazer(
  cost_leverage_stats_p1,
  type             = "latex",
  out              = paste0(out_dir, "cost_leverage_stats_p1.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary: Network Leverage by Type (Phase I)",
  covariate.labels = names(cost_leverage_stats_p1),
  digits           = 2
)

# --- 4) Prep Phase II panel, one row per project -----------------------------
phase2 <- project_static_dynamic %>%
  filter(checklist_phase == "Phase II Study", !is.na(Type)) %>%
  distinct(q_id, .keep_all = TRUE)

# --- 5) Summary stats for cost_network_shared by Type (Phase II) --------------
cost_shared_stats_p2 <- phase2 %>%
  group_by(Type) %>%
  summarise(
    Min    = round(min(cost_network_shared,    na.rm = TRUE), 2),
    Q1     = round(unname(quantile(cost_network_shared, .25, na.rm = TRUE)), 2),
    Median = round(median(cost_network_shared, na.rm = TRUE), 2),
    Q3     = round(unname(quantile(cost_network_shared, .75, na.rm = TRUE)), 2),
    Max    = round(max(cost_network_shared,    na.rm = TRUE), 2),
    Mean   = round(mean(cost_network_shared,   na.rm = TRUE), 2),
    SD     = round(sd(cost_network_shared,     na.rm = TRUE), 2),
    N      = n()
  ) %>%
  ungroup() %>%
  arrange(match(Type, c("Battery","Solar","Wind","Others")))

stargazer(
  cost_shared_stats_p2,
  type             = "html",
  out              = paste0(out_dir, "cost_shared_stats_p2.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary:Own Network Cost Shared (Ratio) by Type (Phase II)",
  covariate.labels = names(cost_shared_stats_p2),
  digits           = 2
)
stargazer(
  cost_shared_stats_p2,
  type             = "latex",
  out              = paste0(out_dir, "cost_shared_stats_p2.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary: Own Network Cost Shared (Ratio) by Type (Phase II)",
  covariate.labels = names(cost_shared_stats_p2),
  digits           = 2
)

# --- 6) Summary stats for cost_network_leverage by Type (Phase II) -------------
cost_leverage_stats_p2 <- phase2 %>%
  group_by(Type) %>%
  summarise(
    Min    = round(min(cost_network_leverage,    na.rm = TRUE), 2),
    Q1     = round(unname(quantile(cost_network_leverage, .25, na.rm = TRUE)), 2),
    Median = round(median(cost_network_leverage, na.rm = TRUE), 2),
    Q3     = round(unname(quantile(cost_network_leverage, .75, na.rm = TRUE)), 2),
    Max    = round(max(cost_network_leverage,    na.rm = TRUE), 2),
    Mean   = round(mean(cost_network_leverage,   na.rm = TRUE), 2),
    SD     = round(sd(cost_network_leverage,     na.rm = TRUE), 2),
    N      = n()
  ) %>%
  ungroup() %>%
  arrange(match(Type, c("Battery","Solar","Wind","Others")))

stargazer(
  cost_leverage_stats_p2,
  type             = "html",
  out              = paste0(out_dir, "cost_leverage_stats_p2.html"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary: Network Leverage by Type (Phase II)",
  covariate.labels = names(cost_leverage_stats_p2),
  digits           = 2
)
stargazer(
  cost_leverage_stats_p2,
  type             = "latex",
  out              = paste0(out_dir, "cost_leverage_stats_p2.tex"),
  summary          = FALSE,
  rownames         = FALSE,
  title            = "Summary: Network Leverage by Type (Phase II)",
  covariate.labels = names(cost_leverage_stats_p2),
  digits           = 2
)



#-----------------------------------Graphics-----------------------------------------------------

# assume project_static_dynamic has columns:
#   - checklist_phase  (e.g. "Phase I Study", "Phase II Study")
#   - cost_network_shared   (the continuous ratio)
#   - cost_network_leverage (the continuous ratio)

# 1) prepare your plot folder
plot_dir <- paste0(project_root, "/output/figures/project_static_dynamic/")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# 2) filter to exactly the two phases you care about
df_phases <- project_static_dynamic %>%
  filter(checklist_phase %in% c("Phase I Study", "Phase II Study"))

#------------------------------------------------------------
# 3) Histogram of counts (binwidth = 0.1)
#------------------------------------------------------------
p_shared_counts <- ggplot(df_phases, aes(x = cost_network_shared)) +
  geom_histogram(
    binwidth = 0.1,
    fill     = "steelblue",
    color    = "white",
    boundary = 0     # anchors bins at multiples of 0.1
  ) +
  labs(
    x     = "Cost Network Shared (ratio)",
    y     = "Count"
  ) +
  facet_wrap(~ checklist_phase, ncol = 2) +
  coord_flip() +
  theme_minimal(base_size = 12)

print(p_shared_counts)
ggsave(
  filename = file.path(plot_dir, "network_shared_counts_hist.png"),
  plot     = p_shared_counts,
  width    = 6, height = 4, dpi = 300
)

#------------------------------------------------------------
# 4) Density plot
#------------------------------------------------------------
p_shared_density <- ggplot(df_phases, aes(x = cost_network_shared)) +
  geom_density(
    fill   = "steelblue",
    alpha  = 0.4,
    adjust = 5      # tweak for smoothness
  ) +
  labs(
    x     = "Cost Network Shared",
    y     = "Density"
  ) +
  facet_wrap(~ checklist_phase, ncol = 2) +
  coord_flip() +
  theme_minimal(base_size = 12)

print(p_shared_density)
ggsave(
  filename = file.path(plot_dir, "network_shared_density.png"),
  plot     = p_shared_density,
  width    = 6, height = 4, dpi = 300
)

#------------------------------------------------------------
# 5) Repeat exactly for the leverage variable
#------------------------------------------------------------
p_lev_counts <- ggplot(df_phases, aes(x = cost_network_leverage)) +
  geom_histogram(
    binwidth = 1,
    fill     = "blue",
    color    = "blue",
    boundary = 1
  ) +
  labs(
    x     = "Cost Network Leverage",
    y     = "Count"
  ) +
  facet_wrap(~ checklist_phase, ncol = 2) + xlim(0, 2000) +
  coord_flip() +
  theme_minimal(base_size = 12)

print(p_lev_counts)

ggsave(
  filename = file.path(plot_dir, "network_leverage_counts_hist.png"),
  plot     = p_lev_counts,
  width    = 6, height = 4, dpi = 300
)

p_lev_density <- ggplot(df_phases, aes(x = cost_network_leverage)) +
  geom_density(
    fill   = "blue",
    alpha  = 0.4,
    adjust = 2
  ) +
  labs(
    x     = "Cost Network Leverage",
    y     = "Density"
  ) +
  facet_wrap(~ checklist_phase, ncol = 2) + xlim(0, 2000) +
  coord_flip() +
  theme_minimal(base_size = 12)

print(p_lev_density)
ggsave(
  filename = file.path(plot_dir, "network_leverage_density.png"),
  plot     = p_lev_density,
  width    = 6, height = 4, dpi = 300
)



library(dplyr)
library(ggplot2)

# 1) Filter to the two Phases
df_phases <- project_static_dynamic %>%
  filter(checklist_phase %in% c("Phase I Study", "Phase II Study"))

# 2) Define percentile probs and labels
probs      <- seq(0, 1, by = 0.05)
pct_labels <- seq(0, 100, by = 5)

# 3) Compute percentiles for cost_network_shared
shared_pct <- df_phases %>%
  group_by(checklist_phase) %>%
  summarize(
    percentile = pct_labels,
    value      = quantile(cost_network_shared, probs = probs, na.rm = TRUE),
    .groups    = "drop"
  )

# 4) Plot Network Shared percentiles
p_shared_pct <- ggplot(shared_pct,
                       aes(x = percentile, y = value)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  facet_wrap(~ checklist_phase, ncol = 2) +
  scale_x_continuous(breaks = pct_labels) +
  labs(
    x     = "Percentile (%)",
    y     = "Cost Network Shared (Ratio)"
  ) +
  theme_minimal(base_size = 12)

print(p_shared_pct)
ggsave(
  filename = file.path(plot_dir, "network_shared_percentiles.png"),
  plot     = p_shared_pct,
  width    = 10, height = 6, dpi = 300
)


# 5) Compute percentiles for cost_network_leverage
lev_pct <- df_phases %>%
  group_by(checklist_phase) %>%
  summarize(
    percentile = pct_labels,
    value      = quantile(cost_network_leverage, probs = probs, na.rm = TRUE),
    .groups    = "drop"
  )

# 6) Plot Network Leverage percentiles
p_lev_pct <- ggplot(lev_pct,
                    aes(x = percentile, y = value)) +
  geom_line(size = 1, color = "seagreen") +
  geom_point(size = 2, color = "seagreen") +
  facet_wrap(~ checklist_phase, ncol = 2) +
  scale_x_continuous(breaks = pct_labels) + ylim(0, 5000)
  labs(
    x     = "Percentile (%)",
    y     = "Cost Network Leverage"
  ) +
  theme_minimal(base_size = 12)

print(p_lev_pct)
ggsave(
  filename = file.path(plot_dir, "network_leverage_percentiles.png"),
  plot     = p_lev_pct,
  width    = 10, height = 6, dpi = 300
)



 


##-------------------------------------------------------------- Intercoonnection requests over time-----------------------------



all_projects_summary <- read_csv(paste0(project_root, "/data/working/areas/all_projects_summary_with_zone.csv"))
 

all_projects_summary <- subset(all_projects_summary, 
                               !all_projects_summary$cluster == "[None]")

all_projects_summary$MW <- as.numeric(all_projects_summary$capacity)
all_projects_summary_fuel <- all_projects_summary

all_projects_summary_fuel$fuel <- ifelse(all_projects_summary_fuel$fuel == "Flywheel" |
                                           all_projects_summary_fuel$fuel == "Gravity via Rail" |
                                           all_projects_summary_fuel$fuel == "Geothermal" |
                                           all_projects_summary_fuel$fuel == "Other",
                                         "Other", all_projects_summary_fuel$fuel)

CAISO_queue_capacity <- all_projects_summary_fuel %>%
  group_by(cluster, fuel) %>%
  summarize(capacity = sum(MW))

CAISO_queue_capacity <- subset(CAISO_queue_capacity, 
                               !(CAISO_queue_capacity$cluster == "FT") &
                                 !(CAISO_queue_capacity$cluster == "Interim") &
                                 !(CAISO_queue_capacity$cluster == "SGIP-TC") &
                                 !(CAISO_queue_capacity$cluster == "TC") &
                                 !(CAISO_queue_capacity$cluster == "C15"))

CAISO_queue_capacity <- subset(CAISO_queue_capacity,
                               !(CAISO_queue_capacity$fuel == ""))

bar_CAISO_queue_by_capacity <- ggplot(CAISO_queue_capacity, aes(x = cluster, 
                                                                y = capacity,
                                                                fill = fuel)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Cluster") + ylab("Capacity (MW)") + 
  theme_light() +
  ggtitle("CAISO Generator Capacity and Fuel Type by Cluster")


bar_CAISO_queue_by_capacity

# Update fuel categories to a compact version
all_projects_summary_fuel$fuel_compact <- ifelse(all_projects_summary_fuel$fuel %in% c("Battery", "Solar", "Wind", "Natural Gas"),
                                                 all_projects_summary_fuel$fuel,
                                                 "Other")



# Recalculate capacity using the compact fuel categories
CAISO_queue_capacity_compact <- all_projects_summary_fuel %>%
  group_by(cluster, fuel_compact) %>%
  summarize(capacity = sum(MW))

# Filter out unnecessary clusters as before
CAISO_queue_capacity_compact <- subset(CAISO_queue_capacity_compact, 
                                       !(CAISO_queue_capacity_compact$cluster %in% c("FT", "Interim", "SGIP-TC", "TC", "C15")))

# Ensure no empty fuel categories
CAISO_queue_capacity_compact <- subset(CAISO_queue_capacity_compact,
                                       !(CAISO_queue_capacity_compact$fuel_compact == ""))

# Generate the updated bar chart
bar_CAISO_queue_by_capacity_compact <- ggplot(CAISO_queue_capacity_compact, aes(x = cluster, 
                                                                                y = capacity,
                                                                                fill = fuel_compact)) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Cluster") + ylab("Capacity (MW)") + 
  theme_light() +
  ggtitle("CAISO Generator Capacity and Fuel Type by Cluster")

bar_CAISO_queue_by_capacity_compact


ggsave(filename = paste0(project_root, "/output/figures/bar_CAISO_queue_by_capacity.png"),
       plot = bar_CAISO_queue_by_capacity_compact, width = 8, height = 5, dpi = 300)







# 1) Make sure 'fuel_compact' is a factor with the stacking order you want.
#    Bottom of the stack = first level, top of the stack = last level.
#    Here: "Natural Gas" at the very bottom, then "Other", then "Solar", "Wind", "Battery".
CAISO_queue_capacity_compact$fuel_compact <- factor(
  CAISO_queue_capacity_compact$fuel_compact,
  levels = c( "Battery", "Solar", "Wind", "Natural Gas", "Other")
)

# 2) Create a small helper so that we can label each cluster with its year.
#    Extract the numeric part of "CXX" (e.g. "01" → 1, "14" → 14) and add 2008.
#    (Thus C01 → 2009, …, C14 → 2022.)
cluster_year_mapping <- setNames(
  2008 + as.integer(substr(CAISO_queue_capacity_compact$cluster, 2, 3)),
  CAISO_queue_capacity_compact$cluster
)




# 3) Plot with:
#    - custom fill colors via scale_fill_manual()
#    - no title (we’ll omit ggtitle here)
#    - new y‐axis label
#    - explicit axis lines (axis.line.x / axis.line.y)
#    - no panel border or grid lines
bar_CAISO_queue_by_capacity_compact <- ggplot(
  CAISO_queue_capacity_compact,
  aes(x = cluster, y = capacity, fill = fuel_compact)
) +
  geom_bar(position = "stack", stat = "identity") +
  
  # Two‐line x‐axis labels:
  scale_x_discrete(labels = function(x) {
    paste0(x, "\n", cluster_year_mapping[x])
  }) +
  
  # Custom (muted) fill colors:
  scale_fill_manual(
    values = c(
      "Solar"       = "#CCCC33",  # softer yellow
      "Battery"     = "#339966",  # softer green
      "Wind"        = "blue",
      "Natural Gas" = "black",
      "Other"       = "grey"
    )
  ) +
  
  # Axis labels and legend title:
  xlab("Cluster") +
  ylab("Capacity entering interconnection queue (MW)") +
  labs(fill = "Fuel Type") +
  
  # Base theme plus axis lines only:
  theme_light() +
  theme(
    panel.background = element_blank(),
    panel.grid       = element_blank(),
    panel.border     = element_blank(),
    axis.line.x      = element_line(color = "black"),
    axis.line.y      = element_line(color = "black"),
    
 
      axis.text.x  = element_text(size = 9),   # x-axis tick labels
      axis.text.y  = element_text(size = 9),   # y-axis tick labels
      axis.title.x = element_text(size = 9),   # “Cluster”
      axis.title.y = element_text(size = 9),    # “Capacity…”  
    
    
    
    # Legend below, one row:
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.key.size   = unit(0.9, "lines"),
    legend.spacing.x  = unit(0.5, "lines"),
    legend.title      = element_text(size = 11),
    legend.text       = element_text(size = 10)
  ) +
  guides(
    fill = guide_legend(nrow = 1, byrow = TRUE)
  )

# Display and save (filename unchanged)
print(bar_CAISO_queue_by_capacity_compact)

ggsave(
  filename = file.path(project_root, "output/figures/bar_CAISO_queue_by_capacity.png"),
  plot     = bar_CAISO_queue_by_capacity_compact,
  width    = 8,
  height   = 5,
  dpi      = 300
)



# 1) Load & reshape the Excel “Capacity” sheet
installed_raw <- read_excel(
  path  = file.path(project_root, "/data/working/Electric_Generation_and_Capacity_2024_ada.xlsx"),
  sheet = "Capacity",
  skip      = 1      # drop the “Fuel | Year | …” row
)



installed_long <- installed_raw %>%
  # 1) Rename the first column to Fuel
  rename(Fuel = Year) %>%
  # 2) Pivot all the year‐columns into long form
  pivot_longer(
    cols      = -Fuel,
    names_to  = "Year",
    values_to = "Installed_MW"
  ) %>%
  # 3) Convert to numeric types
  mutate(
    Year         = as.integer(Year),
    Installed_MW = as.numeric(Installed_MW)
  ) %>%
  # 4) Drop the “Total” row
  filter(Fuel != "Total")



 
# filter to just Solar + Wind, then sum
installed_RE <- installed_long %>%
  filter(Fuel %in% c("Solar PV","Wind", "Solar Thermal")) %>%
  group_by(Year) %>%
  summarize(RE_capacity = sum(Installed_MW, na.rm = TRUE), .groups = "drop")

# 2) Build a small lookup for clusters → year, then join
line_data <- CAISO_queue_capacity_compact %>%
  distinct(cluster) %>%
  mutate(Year = 2008 + as.integer(substr(cluster, 2, 3))) %>%
  left_join(installed_RE, by = "Year")

 


bar_with_RE <- ggplot() +
  # your stacked‐bar
  geom_bar(
    data = CAISO_queue_capacity_compact,
    aes(x = cluster, y = capacity, fill = fuel_compact),
    stat = "identity"
  ) +
  
  # overlay the RE line + points, mapped to colour so they enter the legend
  geom_line(
    data = line_data,
    aes(x = cluster, y = RE_capacity, colour = "Installed solar and wind capacity", group = 1),
    size = 1.2
  ) +
  geom_point(
    data = line_data,
    aes(x = cluster, y = RE_capacity, colour = "Installed solar and wind capacity"),
    size = 2
  ) +
  
  # rest of your scales & theme
  scale_x_discrete(labels = function(x) paste0(x, "\n", cluster_year_mapping[x])) +
  
  scale_fill_manual(
    name   = "interconnection request - fuel type",
    values = c(
      Solar         = "#CCCC33",
      Battery       = "#339966",
      Wind          = "blue",
      "Natural Gas" = "black",
      Other         = "grey"
    ),
    guide = guide_legend(order = 1, ncol = 1)          # vertical stack for fills
  ) +
  
  scale_colour_manual(
    name   = "installed capacity",                              # no extra title; shows only the one line item
    values = c("Installed solar and wind capacity" = "firebrick"),
    guide = guide_legend(order = 2, ncol = 1)            # vertical stack for colour
  ) +
  
  xlab("Cluster") +
  ylab("Generation or storage capacity (MW)") +
  
  theme_light() +
  theme(
    panel.background = element_blank(),
    panel.grid       = element_blank(),
    panel.border     = element_blank(),
    axis.line.x      = element_line(color = "black"),
    axis.line.y      = element_line(color = "black"),
    axis.text.x      = element_text(size = 9),
    axis.text.y      = element_text(size = 9),
    axis.title.x     = element_text(size = 9),
    axis.title.y     = element_text(size = 9),
    
    legend.position      = c(0.02, 0.98),      # upper left, using normalized [0–1] coords
    legend.justification = c(0, 1),            # anchor at top‐left corner of legend
    legend.direction     = "vertical",
    legend.background    = element_blank(),     # remove grey box if you like
    legend.key.size      = unit(0.9, "lines"),
    legend.spacing.x     = unit(0.5, "lines")
  )

print(bar_with_RE)



ggsave(
  filename = file.path(project_root, "output/figures/bar_CAISO_queue_by_capacity.png"),
  plot     = bar_with_RE,
  width    = 8,
  height   = 5,
  dpi      = 300
)




##--------------------- Change in Active MW over time-------------------



library(ggplot2)
library(dplyr)

# 1) Prepare the data: compute **mean** ΔAMW backlog by area & year (so negative values won’t be “washed out” by summing)
backlog_delta_plot_data <- project_static_dynamic %>%
  filter(area_code != 13) %>%
  mutate(calendar_year = as.integer(calendar_year)) %>%
  group_by(area_code, calendar_year) %>%
  summarise(
    backlog_delta = sum(AMW_backlog_delta_at, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Build the blue line‐plot with facets
backlog_delta_plot <- ggplot(backlog_delta_plot_data, aes(x = calendar_year, y = backlog_delta)) +
  geom_line(color = "blue", size = 1) +
  
  facet_wrap(~ area_code, nrow = 3, ncol = 4) +
  
  scale_x_continuous(
    breaks = unique(backlog_delta_plot_data$calendar_year),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  labs(
    x = "Calendar Year",
    y = "Active MW in queue",
    #title = "Year-on-Year Change in Queue Backlog by Area"
  ) +
  
  theme_light() +
  theme(
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

# 3) Show the plot
print(backlog_delta_plot)

# 4) Save to file
ggsave(
  filename = file.path(project_root, "output/figures/line_AMW_backlog_delta_by_area.png"),
  plot     = backlog_delta_plot,
  width    = 14,
  height   = 16,
  dpi      = 300
)





 