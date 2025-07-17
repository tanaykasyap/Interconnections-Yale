
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
library(readr)
library(knitr)
library(kableExtra)

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





phase_status <- read.csv(paste0(project_root, "/data/ic_studies/raw/04_intermediate_scraped_data/phase_status/phase_status_updated.csv"))
project_summary <- read.csv(paste0(project_root, "/data/working/all_projects_summary_clean.csv"))
## Importing the Cleaned, Improved Checklist Data
RIMS_checklist_updated <- read_csv(paste0(project_root, 
                                          "/data/working/clean_RIMS_checklist_cluster_updated.csv"))




# -------------------------------
# 2. Create the ph2_present indicator
# -------------------------------
# Extract unique q_ids with "Phase II study" in checklist_phase
phase2_qids <- RIMS_checklist_updated %>%
  filter(checklist_phase == "Phase II Study") %>%
  distinct(q_id) %>%
  pull(q_id)

# Update phase_status by creating ph2_present based on presence in phase2_qids
phase_status <- phase_status %>%
  mutate(ph2_present = if_else(q_id %in% phase2_qids, "yes", "no"))

# Optionally, rearrange columns so that ph2_present appears next to ph_2.
# Assuming phase_status already has a column named ph_2:
phase_status <- phase_status %>%
  relocate(ph2_present, .before = ph_2)

# -------------------------------
# 3. Create the Summary Table (Table 1)
# -------------------------------
# Summarize counts for each variable.
summary_table <- data.frame(
  `No of Phase 2- Checklist` = sum(phase_status$ph2_present == "yes", na.rm = TRUE),
  `No of Phase 2- PDFs present` = sum(phase_status$ph_2 == "yes", na.rm = TRUE)
)

# Present the summary table with kableExtra
summary_table %>%
  kable("html", caption = "Summary of Phase II Presence") %>%
  kable_styling(full_width = FALSE)

# -------------------------------
# 4. Create the q_id List Table (Table 2)
# -------------------------------
# Filter for q_ids where ph2_present is "yes" but ph_2 is "no"
mismatch_qids <- phase_status %>%
  filter(ph2_present == "yes" & ph_2 != "yes") %>%
  select(q_id)

# Present the table with kableExtra
mismatch_qids %>%
  kable("html", caption = "q_ids: Yes in Checklist but No in PDFs") %>%
  kable_styling(full_width = FALSE)


 



