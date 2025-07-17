#----------------------------------------------------------------------------------------------------------------
#-This is the code which merges the static and dynamic datasets to create the master dataset for estimation
#- It is supposed to merge the following data:
# 1. Static Project Characteristics: 
#### a. Data extracted from CAISO - project summary
#### b. External factors - Land Price, Irradiance and WindSpeed data
#### c. Zone definition
#### d. TPD allocation
# 2. Dynamic project-event level data :
#### a. Phase 1, Phase 2 cost data and reassessment (potentially)
#### b. TPD receipt


## Prerequisite- we only use the following directory to build this data:
#~/Dropbox/interconnections_data/data/working/

## ===== PROJECT PATH =====

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
library(sf)
library(tigris)

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

# —————————————————————————————————————————————————————————————————————————-----
#                               Static Data 
# —————————————————————————————————————————————————————————————————————————-----

## ===== IMPORTING DATA =====
scraped_static <- read_csv(paste0(project_root, "/data/ic_studies/clean/project_static_scraped_final_data.csv"))

# Project Characteristics - All project summary
project_summary <- read_csv(paste0(project_root, "/data/working/areas/all_projects_summary_with_zone.csv"))
  
  #read_csv(paste0(project_root, "/data/working/all_projects_summary_clean.csv")) 

# Cleaned TPD Data
tpd_data <- read_csv(paste0(project_root,
                            "/data/tpd/clean/tpd_data.csv"))

## ===== CLEANING RAW DATA =====
projects_summary <- project_summary %>% 
                    # turn every "[None]" in any character column into NA
                    mutate(across(where(is.character), ~ na_if(.x, "[None]"))) %>%
                    select(q_id, project_name, cluster, latitude, longitude, county, 
                           req_deliverability,fuel, capacity, voltage_level_kV,
                           co_located, firm, PTO, zone_id) 

tpd_data_clean <- tpd_data %>%
                  group_by(q_id) %>%
                  filter(alloc_year == max(alloc_year, na.rm = TRUE)) %>%
                  ungroup() %>%
                  select(alloc_year, q_id, affidavit_date, affidavit_status,
                         alloc_group, total_score, TPD_alloc_percent) %>%
                  distinct()

## ===== MERGING DATA FOR STATIC CHARACTERISTICS =====
# 1. Figure out which columns overlap
common_cols  <- intersect(names(projects_summary),
                          names(scraped_static))
scraped_only <- setdiff(names(scraped_static),
                        names(projects_summary))

# 2. Left-join (so you keep every q_id in projects_summary)
merged <- projects_summary %>%
          left_join(scraped_static,
                    by     = "q_id",
                    suffix = c("", ".scraped")) %>%
          left_join(tpd_data_clean,
                    by = "q_id")

# 3. For each overlapping column, overwrite with scraped if it exists
for(col in common_cols) {
  scraped_col <- paste0(col, ".scraped")
  # only do it if the scraped version made it into the join
  if (scraped_col %in% names(merged)) {
    merged[[col]] <- coalesce(merged[[scraped_col]],
                              merged[[col]])
  }
}

# 4. Tidy up: drop all the “.scraped” helper columns
merged <- merged %>%
          select(-ends_with(".scraped"))

# 5. (Optional) Re-position any scraped-only columns at the end
merged <- merged %>%
          relocate(all_of(scraped_only), .after = last_col())

# 6. Inspect
merged %>% count(cluster)           # same as before, but with updates
merged %>% glimpse()               # see new scraped-only fields

# overwrite original:
projects_summary <- merged


#---------------------------------------------------------------------------------------------------------
#-----------------------Cleaning static data----------------------------------------

projects_summary <- projects_summary%>%

    # 1) turn “[None]” into NA
  mutate(cluster = na_if(cluster, "[None]")) %>%
  
  # 2) rewrite the actual cluster values:
  mutate(cluster = case_when(
    str_detect(cluster, "^C\\d+")   ~ as.character(as.integer(str_remove(cluster, "^C"))),
    cluster %in% c("TC","SGIP-TC")   ~ cluster,
    TRUE                            ~ NA_character_
  ))  %>% 

  # 3) new: force voltage_level_kV to a factor
  mutate(
  voltage_level_kV = factor(voltage_level_kV,
                            levels = sort(unique(voltage_level_kV))),
  # and turn co_located "Y"/"N" into 1/0
  co_located       = if_else(co_located == "Y", 1L, 0L)
  )



# If you also want cluster as an ordered factor in that same numeric+special order:
#level_order <- projects_summary %>%
#  filter(!is.na(cluster)) %>%
  # build codes for ordering
#  mutate(code = case_when(
#    str_detect(cluster, "^[0-9]+$") ~ as.integer(cluster),
 #   cluster == "TC"                ~ 32L,
 #   cluster == "SGIP-TC"           ~ 33L
 # )) %>%
 # distinct(cluster, code) %>%
 # arrange(code) %>%
 # pull(cluster)

#projects_summary <- projects_summary %>%
 # mutate(cluster = factor(cluster, levels = level_order, ordered = TRUE))


projects_summary <- projects_summary %>%
  mutate(
    # 1) Map to integer codes
    cluster_code = case_when(
      str_detect(cluster, "^[0-9]+$") ~ as.integer(cluster),
      cluster == "TC"                 ~ 91L,
      cluster == "SGIP-TC"            ~ 92L,
      TRUE                             ~ NA_integer_
    ),
    # 2) Make it a factor, sorted ascending so “1” is the reference
    cluster = factor(
      cluster_code,
      levels = sort(unique(cluster_code)),
      ordered = FALSE
    )
  ) %>%
  select(-cluster_code)






# Adding Binary Variable for TPD Allocation
projects_summary$TPD_received <- ifelse(is.na(projects_summary$TPD_alloc_percent) |
                                       projects_summary$TPD_alloc_percent == 0,
                                       0, 1)

## Relocating TPD_received to After TPD_alloc_percent
projects_summary <- projects_summary %>%
                    relocate(TPD_received, .after = TPD_alloc_percent)

# inspect
projects_summary %>% 
  count(cluster) %>% 
  arrange(cluster)

# verify no "[None]" remains
projects_summary %>% 
  summarise(across(where(is.character), ~ sum(. == "[None]", na.rm=TRUE)))


#----------------------- INITIAL SENSE‑CHECK: how many q_ids duplicate? -----------------------
initial_counts <- projects_summary %>% 
  count(q_id, name = "freq")

cat(">>> BEFORE deduplication:\n")
cat("  total rows of projects_summary:", nrow(projects_summary), "\n")
cat("  distinct q_ids            :", n_distinct(projects_summary$q_id), "\n")
cat("  q_ids occurring once      :", sum(initial_counts$freq == 1), "\n")
cat("  q_ids with duplicates     :", sum(initial_counts$freq > 1), "\n\n")


#-----------------------Duplicates in q_id------------------------------------------------
 
library(purrr)

event_checklist <- read_csv(paste0(project_root, "/data/working/clean_RIMS_checklist_cluster_core.csv"))

# static columns to compare (everything except q_id)
static_cols <- projects_summary %>% 
  select(-q_id, -cluster) %>% 
  names()

# 1. restrict to valid-cluster rows
ps_valid <- projects_summary  #%>% filter(!is.na(cluster))

# 2. find q_ids with >1 row
dup_groups <- ps_valid %>% 
  group_by(q_id) %>% 
  filter(n()>1) %>% 
  ungroup()

# build one summary table of all q_id with >1 row
dup_summary <- ps_valid %>%
  group_by(q_id) %>%
  filter(n() > 1) %>%
  # for each static column, count how many distinct values
  summarise(across(all_of(static_cols), n_distinct), .groups = "drop") %>%
  # classify and record which fields vary
  mutate(
    type = if_all(all_of(static_cols), ~ . == 1) %>% if_else("true duplicate", "partial duplicate"),
    varying_fields = pmap_chr(across(all_of(static_cols)), ~ {
      vals <- c(...)
      cols <- static_cols[which(vals > 1)]
      paste(cols, collapse = ", ")
    })
  )
 
# split into two tibbles
true_dups_tbl    <- dup_summary %>% filter(type == "true duplicate")    %>% select(q_id)
partial_dups_tbl <- dup_summary %>% filter(type == "partial duplicate") %>% select(q_id, varying_fields)
 

# assume:
#  • projects_summary       — your original data.frame
#  • static_cols            — character vector of the static columns to compare
#  • event_checklist        — for Rule 1 lookup
#  • true_dups_tbl, partial_dups_tbl  — computed on original

# extract q_id vectors
true_qids    <- true_dups_tbl$q_id
partial_qids <- partial_dups_tbl$q_id

# 1. collapse true duplicates
collapsed_true <- projects_summary %>%
  filter(q_id %in% true_qids) %>%
  group_by(q_id) %>%
  slice(1) %>%
  ungroup()

# 2. keep all other rows that were not in true_dups or partial_dups
non_dup <- projects_summary %>%
  filter(!q_id %in% c(true_qids, partial_qids))

# 3. isolate the partial‑dup rows
partial_rows <- projects_summary %>%
  filter(q_id %in% partial_qids)

# 4. build 1–1 checklist lookup
checklist_one <- event_checklist %>%
  group_by(q_id) %>%
  summarise(project_name_chk = first(project_name), .groups="drop")

# 5. apply Rules 1–3 (no final slice)
candidates <- partial_rows %>%
  left_join(checklist_one, by="q_id") %>%
  group_by(q_id) %>%
  group_modify(~{
    df <- .x
    
    
    # Rule 1
    if (any(df$project_name == df$project_name_chk, na.rm=TRUE))
      df <- filter(df, project_name == project_name_chk)
    # Rule 2
    if (nrow(df)>1 && any(df$req_deliverability=="Full Capacity", na.rm=TRUE))
      df <- filter(df, req_deliverability=="Full Capacity")
    # Rule 2.5: if still multiple, prefer your “valid” deliverability values
    valid_deliv <- c("Energy Only","Partial Capacity")
    if (nrow(df)>1 && any(df$req_deliverability %in% valid_deliv, na.rm=TRUE)) {
      df <- filter(df, req_deliverability %in% valid_deliv)
    }
    # Rule 3: then prefer non‑missing cluster
    if (nrow(df)>1 && any(!is.na(df$cluster))) {
      df <- filter(df, !is.na(cluster))
    }
    
    # Rule 4
    if (nrow(df)>1)
      df <- filter(df, capacity == max(capacity, na.rm=TRUE))
    
    # Final fallback: if still multiple, take first (as we only have 2 dupliactes left all this)
    if (nrow(df)>1) {
      df <- slice(df, 1)
    }
    
    df
  }) %>%
  ungroup() %>%
  select(-project_name_chk)

# 5) collapse any “new true‑dups” that are now identical on static_cols
collapsed_new_true <- candidates %>%
  group_by(q_id, across(all_of(static_cols))) %>%
  slice(1) %>%
  ungroup()

# 6) whatever remains ambiguous after that is your final still_ambiguous
still_ambiguous <- collapsed_new_true %>%
  group_by(q_id) %>%
  filter(n()>1) %>%
  ungroup()

# 7) build partial_dups_tbl2 from still_ambiguous
partial_dups_tbl2 <- still_ambiguous %>%
  group_by(q_id) %>%
  summarise(across(all_of(static_cols), ~ n_distinct(.x, na.rm=TRUE)), .groups="drop") %>%
  mutate(
    varying_fields = pmap_chr(across(all_of(static_cols)), ~ {
      vals <- c(...); cols <- static_cols[which(vals>1)]; paste(cols, collapse=", ")
    })
  ) %>%
  select(q_id, varying_fields)


# 8) your cleaned summary (if you need it)
projects_summary <- bind_rows(
  non_dup,
  collapsed_true,
  collapsed_new_true %>% filter(!q_id %in% still_ambiguous$q_id)
)

# — report —
cat("Original true‑dups collapsed:", length(true_qids), "\n")
cat("Original partial‑dups processed:", length(partial_qids), "\n")
cat("New identical duplicates collapsed:", 
    length(setdiff(candidates$q_id, still_ambiguous$q_id)), "\n")
cat("Remaining partial duplicates:", nrow(partial_dups_tbl2), "\n\n")

print(partial_dups_tbl2)


 
#----------------------- FINAL SENSE‑CHECK: after deduplication -----------------------
# projects_summary_clean is your output
final_counts <- projects_summary %>% 
  count(q_id, name = "freq")

cat(">>> AFTER deduplication:\n")
cat("  total rows of projects_summary_clean:", nrow(projects_summary), "\n")
cat("  distinct q_ids                   :", n_distinct(projects_summary$q_id), "\n")
cat("  q_ids occurring once             :", sum(final_counts$freq == 1), "\n")
cat("  q_ids still duplicated (should be 0):", sum(final_counts$freq > 1), "\n")





# ---------------------------------------------------------------------------------------------------------
# Cleaning q_id
#----------------------------------------------------------------------------------------------------------
# 1) flag which q_id are “alphanumeric” (i.e. not purely digits)
projects_summary <- projects_summary %>%
  mutate(
    is_alnum = !is.na(q_id) & !str_detect(q_id, "^\\d+$")
  )

# how many alphanumeric?
projects_summary %>% 
  summarise(n_alnum = sum(is_alnum), 
            pct_alnum = mean(is_alnum) * 100)

# 2) compute the “form” by replacing digits → “#” and letters → “A”
projects_summary <- projects_summary %>%
  mutate(
    form = q_id %>%
      str_replace_all("\\d", "#") %>%    # every digit → #
      str_replace_all("[A-Za-z]", "A")   # every letter → A
  )

# 3) frequency of each form (only for the alphanumeric ones)
form_freq <- projects_summary %>%
  filter(is_alnum) %>%
  count(form, name = "n_qid") %>%
  arrange(desc(n_qid))

# 4) for each form, count how many have cluster vs. NA cluster
form_cluster_summary <- projects_summary %>%
  filter(is_alnum) %>%
  mutate(cluster_flag = if_else(is.na(cluster), "cluster_NA", "cluster_valid")) %>%
  count(form, cluster_flag) %>%
  pivot_wider(names_from = cluster_flag, values_from = n, values_fill = 0) %>%
  mutate(total = cluster_valid + cluster_NA) %>%
  arrange(desc(total))

# 5) display
form_freq
form_cluster_summary


# 1) convert every NA in each data‐frame to ""  
form_freq_clean <- form_freq %>% 
  mutate(across(everything(), ~ replace_na(as.character(.x), "")))

form_cluster_clean <- form_cluster_summary %>% 
  mutate(across(everything(), ~ replace_na(as.character(.x), "")))

 



# now simulate “extract numeric part”:
dup_test <- projects_summary %>%
  mutate(
    numeric_only = str_extract(q_id, "\\d+")    # pull out first run of digits
  )

# how many q_ids in this filtered set?
total_q   <- nrow(dup_test)

# how many unique original q_ids?
unique_q  <- n_distinct(dup_test$q_id)

# how many unique numeric_only values?
unique_num <- n_distinct(dup_test$numeric_only)

# how many collisions: i.e. numeric_only groups of size ≥ 2?
# find collisions
collisions <- dup_test %>%
  group_by(numeric_only) %>%
  filter(n() > 1) %>%
  summarize(
    count      = n(),
    originals  = paste(unique(q_id), collapse = ", ")
  ) %>%
  ungroup() %>%
  # add duplicates = count − 1
  mutate(duplicates = count - 1)

# summary of how many duplicate rows in total
total_duplicates <- sum(collisions$duplicates)



n_collisions <- nrow(collisions)

# summary:
dup_summary2<-tibble(
  total_q_ids               = total_q,
  unique_original_q_ids     = unique_q,
  unique_numeric_only       = unique_num,
  num_numeric_duplicates    = n_collisions,
  pct_qids_in_collision     = n_collisions / total_q * 100
) %>% print()

# and if you want to inspect which numeric values collide:
#collisions %>% arrange(desc(count)) %>% print(n = Inf)




#--------------------------------------------------- Transform q_id----------------------------------
# ─── 1. duplicate check BEFORE transform (only valid-cluster rows) ────────────────
dup_before <- projects_summary %>% 
  filter(!is.na(cluster) & duplicated(q_id)) %>% 
  pull(q_id)
cat("Duplicated q_id (valid-cluster) before transform:", length(dup_before), "\n")
if(length(dup_before)>0) print(unique(dup_before))







# ─── 2. create project_id only for valid-cluster rows ─────────────────────────────────
projects_summary <- projects_summary %>%
  rowwise() %>%
  mutate(
    project_id = if (is.na(cluster)) {
      NA_character_
    } else {
      this_q       <- as.character(q_id)
      num_part     <- str_extract(this_q, "^\\d+")
      letters_part <- str_extract(this_q, "[A-Za-z]+$")
      
      # 4‑digit numeric part
      num4 <- str_pad(num_part, width = 4, side = "left", pad = "0")
      
      # letter code → integer
      letter_code <- if (is.na(letters_part)) {
        0L
      } else {
        codes <- str_split(letters_part, "")[[1]] %>%
          map_int(~ match(toupper(.x), LETTERS))
        reduce(codes, ~ .x * 26L + .y, .init = 0L)
      }
      
      # pad letter_code to exactly 3 digits
      letter3 <- str_pad(as.character(letter_code), width = 3, side = "left", pad = "0")
      
      # combine into exactly 7 characters
      paste0(num4, letter3)
    }
  ) %>%
  ungroup() %>%
  relocate(project_id, .after = q_id)

# Comments:
#  - only rows with !is.na(cluster) get transformed
#  - numeric part → 4-digit zero-pad; letters → positions; then zero-pad to 7


# ─── 3. duplicate check AFTER transform (only among non-NA project_id) ────────────────
dup_after <- projects_summary %>% 
  filter(!is.na(project_id) & duplicated(project_id)) %>% 
  select(q_id,project_id)
cat("Duplicated project_id after transform:", length(dup_after), "\n")
if(length(dup_after)>0) print(unique(dup_after))


# ─── 4. view sample ───────────────────────────────────────────────────────────────
projects_summary %>% 
  filter(!is.na(cluster)) %>% 
  select(q_id, cluster, project_id) %>% 
  slice(1:10) %>% 
  print(n = 10)


 



missing_zone <- projects_summary %>% 
  filter(is.na(zone_id))

missing_zone_true<- missing_zone %>% 
  filter(!is.na(latitude) & !is.na(longitude))


output <- paste0(project_root, "/data/working/areas/missing_zones.csv")
readr::write_csv(missing_zone_true, output)



#-----------------------------------------------------------------------------------------------------------
# Extenral factors
# Placeholder for now as we do not have the dataset
# external_factors <-read_csv(paste0(project_root, "/data/working/external_factors.csv"))
#-----------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------
# Zone definition
#-----------------------------------------------------------------------------------------------------------
# Placeholder for now as we do not have the dataset- Thus using PTO as a zone definition, should be replaced later
# zone_data <-read_csv(paste0(project_root, "/data/working/zone_data.csv"))
#zone_data<-read_csv(paste0(project_root, "/data/working/all_projects_summary_clean.csv"))
##Placeholder

# Zone data is now included in the projects summary

projects_summary<-projects_summary %>% 
  select(-is_alnum, -form)


#---------------------------------------------------------------------------------------------------------
##- Create area_code and area_name from zone_id
 
  
  
projects_summary <- projects_summary %>%
  mutate(
    # 1) Replace underscores with a single space, then collapse any extra spaces
    area_name = zone_id %>%
      str_replace_all("_", " ") %>%
      str_squish(),
    
    # 2) Turn that clean name into a numeric code (alphabetical order)
    area_code = as.integer(
      factor(
        area_name,
        levels = sort(unique(area_name)),
        ordered = TRUE
      )
    )
  )

projects_summary <- projects_summary  %>%
  mutate(
    area_code = ifelse(is.na(area_code),
                       99,
                       area_code)
  )


# Build a lookup table for reference:
zone_lookup <- projects_summary %>%
  distinct(zone_id, area_code, area_name) %>%
  arrange(area_code)

# View your mapping:
zone_lookup
  
 











 

# ── 1. FREQUENCY ON RAW projects_summary ────────────────────────────────────────
# ── helper: count missing vs present using nrow() not n() ───────────────────────
count_missing <- function(df, col){
  vec   <- df[[col]]
  total <- nrow(df)
  is_miss <- is.na(vec) | (is.character(vec) & vec == "")
  n_missing   <- sum(is_miss)
  n_present   <- total - n_missing
  tibble(
    column      = col,
    n_missing   = n_missing,
    n_present   = n_present,
    pct_missing = n_missing  / total * 100,
    pct_present = n_present  / total * 100
  )
}

# ── frequency on raw projects_summary ───────────────────────────────────────────
raw_one_per_project <- projects_summary %>% 
  distinct(project_id, .keep_all = TRUE)

freq_raw <- raw_one_per_project %>% 
  # for every column except the ID
  select(-project_id) %>% 
  names() %>% 
  map_dfr(~ count_missing(raw_one_per_project, .x))





# --------------------------- Merge static data ----------------------------------------------------
# place holder, when avaiable we merge the above 3 datasets to create project_characteristics_static data.
# for now we use the projects_summary 


#static_tables <- list(
#  projects   = projects_summary,
  # external = external_factors,
  # zone       = zone_data
#)

# For each table, show total rows and how many distinct q_id
 #imap(static_tables, ~ {
 # tibble(
 #   table       = .y,
  #  rows        = nrow(.x),
  #  distinct_id = n_distinct(.x$project_id)
 # )
#}) %>% bind_rows()
 
 
 # 2. One‐big‐join: reduce with left_join
 #raw_join <- reduce(
  # static_tables,
 #  left_join,
  # by = "project_id"
 #)
 
 
 # 3. Collapse back to one row per q_id
 #    Here we simply keep the first occurrence of each q_id;
 #    you could also summarize or pick e.g. the max, a concatenation, etc.
 #project_characteristics_static <- raw_join %>% 
  # select(-q_id.y) %>% 
  # rename(q_id = q_id.x) %>% 
  # group_by(project_id) %>% 
 #  slice(1) %>% 
 #  ungroup()
 # 4. Check
 #nrow(raw_join)                        # maybe > n_distinct(q_id)
 #n_distinct(raw_join$project_id)             # how many unique q_id
 #nrow(project_characteristics_static)  # should == n_distinct(raw_join$q_id) 



 

 project_characteristics_static <-  projects_summary %>%
   
 
   
 
   # 1) Rename
   rename(MW = capacity) %>% 
   
   # 2) build new vars
   mutate(
     # indicator: 1 if full-capacity requested
     FCDS = if_else(req_deliverability == "Full Capacity", 1L, 0L),
     
     # categorical type variable
     Type = case_when(
       fuel    == "Solar"   ~ "Solar",
       fuel    == "Wind"    ~ "Wind",
       fuel    == "Battery" ~ "Battery",
       TRUE                 ~ "Others"
     ),
     
     # (optional) turn into a factor with a meaningful order
     Type = factor(Type, levels = c("Solar","Wind","Battery","Others"))
   ) %>%
   
   # 3) drop the old columns
   select(-req_deliverability, -fuel)
 
 
project_characteristics_static <- project_characteristics_static %>% 
  arrange(cluster)


project_characteristics_static <- project_characteristics_static %>%
  mutate(
    # this will replace exactly “NA000” with an actual NA of the same type
    project_id = na_if(project_id, "NA000")
  )

output_path <- paste0(project_root, "/data/input/project_static.csv")
readr::write_csv(project_characteristics_static, output_path)
message("Wrote project_characteristics_static dataset to ", output_path)


# ── 3. FREQUENCY ON STATIC DATASET ─────────────────────────────────────────────
# ── frequency on static dataset ────────────────────────────────────────────────
static_one_per_project <- project_characteristics_static

freq_static <- static_one_per_project %>% 
  select(-project_id) %>% 
  names() %>% 
  map_dfr(~ count_missing(static_one_per_project, .x))

# ── inspect or write out ───────────────────────────────────────────────────────
freq_raw
freq_static


# ——————————————————————————————————————————————————————————————————————————————
#                               Dynamic Data 
# ——————————————————————————————————————————————————————————————————————————————


# Here we only call a single dataset - Dynamic Panel data/ Dynamic project-event level data


dynamic_panel <- read_excel(paste0(project_root, "/data/input/dynamic_table.xlsx"))

dynamic_panel <- dynamic_panel %>% 
  rowwise() %>%
  mutate(
    project_id =  {
      this_q       <- as.character(q_id)
      num_part     <- str_extract(this_q, "^\\d+")
      letters_part <- str_extract(this_q, "[A-Za-z]+$")
      
      # 4‑digit numeric part
      num4 <- str_pad(num_part, width = 4, side = "left", pad = "0")
      
      # letter code → integer
      letter_code <- if (is.na(letters_part)) {
        0L
      } else {
        codes <- str_split(letters_part, "")[[1]] %>%
          map_int(~ match(toupper(.x), LETTERS))
        reduce(codes, ~ .x * 26L + .y, .init = 0L)
      }
      
      # pad letter_code to exactly 3 digits
      letter3 <- str_pad(as.character(letter_code), width = 3, side = "left", pad = "0")
      
      # combine into exactly 7 characters
      paste0(num4, letter3)
    }
  ) %>%
  ungroup() %>%
  relocate(project_id, .after = q_id)

output_path <- paste0(project_root, "/data/input/project_dynamic.csv")
readr::write_csv(dynamic_panel, output_path)
message("Wrote dynamic_panel dataset to ", output_path)

# ——————————————————————————————————————————————————————————————————————————————
#                               Master Data 
# ——————————————————————————————————————————————————————————————————————————————

# Merge the project_characteristics_static and dynamic_panel on two keys: q_id and calender year


project_static_dynamic <- dynamic_panel %>%
  # join the static characteristics onto every dynamic record
  left_join(project_characteristics_static, by = "project_id") %>% 
  rename(q_id = q_id.x) %>% 
  select(- q_id.y)

# sanity check:
#   number of rows in master_data should equal nrow(dynamic_panel)
stopifnot(nrow(project_static_dynamic) == nrow(dynamic_panel))



# A) Count how many would be fixed
fix_counts <- project_static_dynamic %>%
  filter(checklist_phase == "Phase I Study") %>%
  summarise(
    poi_fixed    = sum(is.na(cost_poi)     & !is.na(cost_network)),
    network_fixed = sum(is.na(cost_network) & !is.na(cost_poi))
  )

print(fix_counts)
#> # A tibble: 1 × 2
#>   poi_fixed network_fixed
#>       <int>         <int>
#> 1        20            15  # example output

# B) Apply the fixes in-place
project_static_dynamic <- project_static_dynamic %>%
  mutate(
    # for Phase I only, fill missing POI where network exists
    cost_poi = if_else(
      checklist_phase == "Phase I Study" &
        is.na(cost_poi) &
        !is.na(cost_network),
      0,
      cost_poi
    ),
    # likewise, fill missing network where POI exists
    cost_network = if_else(
      checklist_phase == "Phase I Study" &
        is.na(cost_network) &
        !is.na(cost_poi),
      0,
      cost_network
    )
  )

# C) Confirm that we made exactly those changes
fix_counts_after <- project_static_dynamic %>%
  filter(checklist_phase == "Phase I Study") %>%
  summarise(
    poi_fixed_after    = sum(is.na(cost_poi)  & !is.na(cost_network)),
    network_fixed_after = sum(is.na(cost_network) & !is.na(cost_poi))
  )

print(fix_counts_after)




# Area- MW 


project_static_dynamic <- project_static_dynamic %>%
  
  mutate(calendar_year = year(ymd(status_date))) %>%
  group_by(q_id) %>% fill(calendar_year, .direction="down") %>% ungroup() 

zone_year_capacity <- project_static_dynamic %>%
  # instead of filter(action!="exit") use:
  filter(
    checklist_phase != "Interconnection Request",
    checklist_phase != "GIA",
    action != "exit"
  )%>%
  group_by(area_code, calendar_year) %>%
  summarize(
    queue_capacity_current      = sum(MW, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  arrange(area_code, calendar_year) %>%
  group_by(area_code) %>%
  mutate(queue_capacity_cumulative = cumsum(queue_capacity_current)) %>%
  ungroup()

project_static_dynamic <- left_join(
  project_static_dynamic, zone_year_capacity,
  by = c("area_code","calendar_year")
)

library(dplyr)
library(lubridate)

# 1) Compute calendar_year and fill it down within each project
#project_static_dynamic <- project_static_dynamic %>%
 # mutate(
  #  calendar_year = year(ymd(status_date))
  #) %>%
  #group_by(q_id) %>%
  #fill(calendar_year, .direction = "down") %>%
  #ungroup()

# 2) Build area‐year queue capacities and compute delta in one table
#zone_year_capacity <- project_static_dynamic %>%
 # filter(
  #  checklist_phase != "Interconnection Request",
   # checklist_phase != "GIA",
    #action != "exit"
  #) %>%
  #group_by(area_code, calendar_year) %>%
  #summarize(
   # queue_capacity_current    = sum(MW, na.rm = TRUE),
  #  .groups = "drop"
  #) %>%
  #arrange(area_code, calendar_year) %>%
  #group_by(area_code) %>%
  #mutate(
   # queue_capacity_cumulative = cumsum(queue_capacity_current),
  #  delta_queue_capacity      = queue_capacity_cumulative - lag(queue_capacity_cumulative),
    # for the first year in each zone, set delta to zero
   # delta_queue_capacity      = replace_na(delta_queue_capacity, 0)
  #) %>%
  #ungroup()

# 3) Join back into the main table in one go
#project_static_dynamic <- project_static_dynamic %>%
 # left_join(
  #  zone_year_capacity,
   # by = c("area_code", "calendar_year")
  #)

library(dplyr)
library(lubridate)

# 1) Ensure we have calendar_year
project_static_dynamic <- project_static_dynamic %>%
  mutate(calendar_year = year(ymd(status_date))) %>%
  group_by(q_id) %>%
  fill(calendar_year, .direction = "down") %>%
  ungroup()

# 2) Build zone–year summary with all four metrics
zone_year_capacity <- project_static_dynamic %>%
  # Compute AMW_at = sum of MW for *new* entrants (Phase I Study) in that year
  filter(checklist_phase == "Phase I Study") %>%
  group_by(area_code, calendar_year) %>%
  summarise(
    AMW_at = sum(MW, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Join on AMW_active_at (previously queue_capacity_current)
  left_join(
    project_static_dynamic %>%
      filter(
        checklist_phase != "Interconnection Request",
        checklist_phase != "GIA",
        action != "exit"
      ) %>%
      group_by(area_code, calendar_year) %>%
      summarise(
        AMW_active_at = sum(MW, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("area_code", "calendar_year")
  ) %>%
  # Fill any years with no entrants or no active projects with 0
  mutate(
    AMW_at       = replace_na(AMW_at, 0),
    AMW_active_at = replace_na(AMW_active_at, 0)
  ) %>%
  arrange(area_code, calendar_year) %>%
  group_by(area_code) %>%
  mutate(
    # AMW_backlog_at = entrants this year + *sum* of active in the prior year
    AMW_backlog_at = AMW_at + lag((AMW_active_at), default = 0),
    # Year‐on‐year change in backlog
    AMW_backlog_delta_at = AMW_backlog_at - lag(AMW_backlog_at, default = 0),
    log_AMW_backlog_at = log1p(AMW_backlog_at),
    log_AMW_backlog_at_gW = log1p(AMW_backlog_at/1000),
    log_AMW_backlog_delta_at = log_AMW_backlog_at - lag(log_AMW_backlog_at,
          default = first(log_AMW_backlog_at)) , # the default to log_AMW_backlog_at makes the first value of the log delta zero.
    log_AMW_backlog_delta_at_gW = log_AMW_backlog_at_gW - lag(log_AMW_backlog_at_gW,
                                                     default = first(log_AMW_backlog_at_gW)) 
  ) %>%
  ungroup()

# 3) Merge back if you like
project_static_dynamic <- project_static_dynamic %>%
  left_join(
    zone_year_capacity,
    by = c("area_code", "calendar_year")
  )


output_path <- paste0(project_root, "/data/input/area_mw_data.csv")
readr::write_csv(zone_year_capacity, output_path)

output_path <- paste0(project_root, "/data/input/project_static_dynamic.csv")
readr::write_csv(project_static_dynamic, output_path)
message("Wrote master dataset to ", output_path)

 
# 1) Grouping values
phases <- project_static_dynamic %>% 
  pull(checklist_phase) %>% 
  unique()

years  <- c(0, 1, 2)

# 2) Metrics helper
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
      sum( is.na(df$data_source)),
      sum(df$data_source == "scraped",   na.rm = TRUE),
      sum(df$data_source == "kiteworks", na.rm = TRUE),
      sum(!is.na(df$cost_poi)),
      sum( is.na(df$cost_poi)),
      sum(near(df$cost_poi,  0), na.rm = TRUE),
      sum(is.infinite(df$cost_poi), na.rm = TRUE),
      sum(!is.na(df$cost_network)),
      sum( is.na(df$cost_network)),
      sum(near(df$cost_network,  0), na.rm = TRUE),
      sum(is.infinite(df$cost_network), na.rm = TRUE)
    )
  )
}

# 3) Full‐dataset stats
all_stats <- compute_metrics(project_static_dynamic) %>%
  rename(Dataset = Value)

# 4) Per‐phase stats
phase_stats <- map_dfc(phases, function(ph) {
  compute_metrics(filter(project_static_dynamic, checklist_phase == ph)) %>%
    pull(Value)
})
colnames(phase_stats) <- phases

# 5) Per‐year stats (0,1,2)
year_stats <- map_dfc(years, function(y) {
  compute_metrics(filter(project_static_dynamic, model_year == y)) %>%
    pull(Value)
})
colnames(year_stats) <- paste0("Year ", years)

# 6) Combine into one wide table
summary_table <- bind_cols(
  Metric   = all_stats$Metric,
  Dataset  = all_stats$Dataset,
  phase_stats,
  year_stats
)

# 7) Display with kable()
kable(
  summary_table,
  col.names = c("Metric", "project_static_dynamic", phases, paste0("Year ", years)),
  digits   = 0,
  align    = c("l", rep("r", length(phases) + length(years) + 1))
)

# 8) Export via stargazer (rename Dataset col)
summary_for_stargazer <- summary_table %>%
  rename(project_static_dynamic = Dataset)

stargazer(
  summary_for_stargazer,
  summary   = FALSE,
  rownames  = FALSE,
  style     = "qje",
  type      = "latex",
  out       = paste0(out_dir, "project_static_dynamic_summary.tex")
)
stargazer(
  summary_for_stargazer,
  summary   = FALSE,
  rownames  = FALSE,
  style     = "qje",
  type      = "html",
  out       = paste0(out_dir, "project_static_dynamic_summary.html")
)








