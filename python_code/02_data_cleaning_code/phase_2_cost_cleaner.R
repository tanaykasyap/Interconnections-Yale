
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



scraped_itemized_data<- read.csv(paste0(project_root, "data/ic_studies/raw/04_intermediate_scraped_data/phase_2_cost_data/all_clusters/costs_phase_2_all_clusters_itemized.csv"))
scraped_total_data<- read.csv(paste0(project_root, "data/ic_studies/raw/04_intermediate_scraped_data/phase_2_cost_data/all_clusters/costs_phase_2_all_clusters_total.csv"))
phase_status <- read.csv(paste0(project_root, "data/ic_studies/raw/04_intermediate_scraped_data/phase_status/phase_status_updated.csv"))
project_summary <- read.csv(paste0(project_root, "/data/working/all_projects_summary_clean.csv"))


################################################################################################################
###################################### Data Cleaning #########################################################




scraped_itemized_data<- scraped_itemized_data %>% select(-item)
scraped_total_data<- scraped_total_data %>% select(-item)




######### Fill in missing firm level characteristics from project summary ########################


######### Filling in any missing cluster numbers########################

# Create a cleaned phase_status data frame that preserves non-numeric cluster values.
# Also include the ph_1 column for later counting.
project_summary_clusters <- project_summary %>%
  mutate(q_id = as.character(q_id),
         # Extract numeric part; if there are no digits (e.g., "TC" or "SGIP-TC"), this yields NA.
         cluster_numeric = as.integer(gsub("\\D", "", cluster)),
         # If numeric extraction is NA, use the original cluster_number; otherwise, use the numeric value as a string.
         cluster_clean = ifelse(is.na(cluster_numeric), cluster, as.character(cluster_numeric))) %>%
  # Keep only the columns we need (include Style and ph_1 for grouping and counting)
  select(q_id, cluster_clean)


# ---- Convert q_id to character in data sets ----
scraped_itemized_data <- scraped_itemized_data %>% mutate(q_id = as.character(q_id))
scraped_total_data <- scraped_total_data %>% mutate(q_id = as.character(q_id))


scraped_itemized_data <- scraped_itemized_data %>%
  mutate(q_id = sub("\\.0$", "", q_id))

phase_status <- phase_status %>%
  mutate(q_id = sub("\\.0$", "", as.character(q_id)))

# ---- Left join the cluster number from project summary into  itemized data and total based on q_id ----
scraped_itemized_data <- scraped_itemized_data %>%
  left_join(project_summary_clusters %>% 
              select(q_id, cluster_clean) %>% 
              distinct() %>% 
              rename(cluster_phase = cluster_clean),
            by = "q_id") %>%
  # If data already has a cluster, keep it; otherwise, use cluster_phase from phase_status_cleaned.
  mutate(cluster = ifelse(is.na(cluster), cluster_phase, cluster),
         # Convert the final cluster to character so that non-numeric values are preserved.
         cluster = as.character(cluster)) %>%
  select(-cluster_phase)

scraped_total_data <- scraped_total_data %>%
  left_join(project_summary_clusters %>% 
              select(q_id, cluster_clean) %>% 
              distinct() %>% 
              rename(cluster_phase = cluster_clean),
            by = "q_id") %>%
  mutate(cluster = ifelse(is.na(cluster), cluster_phase, cluster),
         cluster = as.character(cluster)) %>%
  select(-cluster_phase)



# ---- Order the cluster values: TC, SGIP-TC, then numeric in ascending order ----
special_clusters <- c("TC", "SGIP-TC")
# Determine the other clusters that are numeric (they are stored as character)
other_clusters <- setdiff(unique(scraped_itemized_data$cluster), special_clusters)
# Convert remaining values to numeric, sort them, then convert back to character
other_levels <- as.character(sort(as.numeric(other_clusters)))
 
other_levels <- trimws(other_levels)
other_levels <- sort(unique(other_levels))
new_levels <- c(special_clusters, other_levels)

other_levels
new_levels


scraped_itemized_data <- scraped_itemized_data %>%
  mutate(cluster = factor(cluster, levels = new_levels))
scraped_total_data <- scraped_total_data %>%
  mutate(cluster = factor(cluster, levels = new_levels))





#------------------- type_of_upgrade--------------------------------------------------------
unique_types <- scraped_itemized_data %>% 
  distinct(type_of_upgrade) %>% 
  arrange(type_of_upgrade) %>% 
  pull(type_of_upgrade)

print(unique_types)

scraped_itemized_data <- scraped_itemized_data %>%
  mutate(type_of_upgrade = factor(type_of_upgrade, levels = unique_types))

scraped_total_data <- scraped_total_data %>%
  mutate(type_of_upgrade = factor(type_of_upgrade, levels = unique_types))






#------------ Updating, latitude, longitude, capacity, req_deliverability--------------------

# Create a cleaned version of project_summary with the relevant columns.
project_summary_cleaned <- project_summary %>%
  mutate(q_id = as.character(q_id),
         capacity = as.numeric(capacity)) %>%
  select(q_id, req_deliverability, latitude, longitude, capacity)

# Helper function to update a column only if the original is NA or an empty string.
update_if_empty <- function(original, new) {
  ifelse(is.na(original) | original == "", new, original)
}

# ---- Update scraped_itemized_data ----
scraped_itemized_data <- scraped_itemized_data %>%
  mutate(q_id = as.character(q_id)) %>%
  left_join(project_summary_cleaned, by = "q_id", suffix = c("", "_ps"),
            relationship = "many-to-many") %>%
  mutate(
    # For req_deliverability: if the scraped value is "Full" or "Partial", keep it;
    # otherwise, update it from the project summary.
    req_deliverability = ifelse(req_deliverability %in% c("Full", "Partial"),
                                req_deliverability,
                                req_deliverability_ps),
    # Rename the values as required using case_when.
    req_deliverability = case_when(
      req_deliverability == "Full" ~ "Full Capacity",
      req_deliverability == "Partial" ~ "Partial Capacity",
      TRUE ~ req_deliverability
    ),
    # Directly use latitude and longitude from project_summary
    latitude = latitude_ps,
    longitude = longitude_ps,
    req_deliverability = factor(req_deliverability),
    # For capacity, convert the scraped value to numeric. If that yields NA and
    # a non-NA project summary value exists, use the project summary value.
    capacity = ifelse(is.na(as.numeric(capacity)) & !is.na(capacity_ps),
                      capacity_ps, as.numeric(capacity))
  ) %>%
  select(-req_deliverability_ps, -latitude_ps, -longitude_ps, -capacity_ps)

# ---- Update scraped_total_data ----
scraped_total_data <- scraped_total_data %>%
  mutate(q_id = as.character(q_id)) %>%
  left_join(project_summary_cleaned, by = "q_id", suffix = c("", "_ps"),
            relationship = "many-to-many") %>%
  mutate(
    req_deliverability = ifelse(req_deliverability %in% c("Full", "Partial"),
                                req_deliverability,
                                req_deliverability_ps),
    req_deliverability = case_when(
      req_deliverability == "Full" ~ "Full Capacity",
      req_deliverability == "Partial" ~ "Partial Capacity",
      TRUE ~ req_deliverability
    ),
    # Directly use latitude and longitude from project_summary
    latitude = latitude_ps,
    longitude = longitude_ps,
    req_deliverability = factor(req_deliverability),
    capacity = ifelse(is.na(as.numeric(capacity)) & !is.na(capacity_ps),
                      capacity_ps, as.numeric(capacity))
    
  ) %>%
  select(-req_deliverability_ps, -latitude_ps, -longitude_ps, -capacity_ps)

# If any table in the original pdf had None under either upgrade or description or like --, then we code the costs(just estimated and escalated) to be zero rather than as NA. As NA is supposed to represent if the table or column did not exist in the original table in the pdf.
scraped_itemized_data <- scraped_itemized_data %>%
  mutate(
    description = ifelse(description == "--", "None", description),
    estimated_time_to_construct = ifelse(estimated_time_to_construct == "--", "0", estimated_time_to_construct)
  )



scraped_itemized_data <- scraped_itemized_data %>%
  mutate(
    estimated_cost = ifelse(upgrade == "None" & is.na(estimated_cost), 0, estimated_cost),
    escalated_cost = ifelse(upgrade == "None" & is.na(escalated_cost), 0, escalated_cost),
    cost_allocation_factor = ifelse(upgrade == "None" & is.na(cost_allocation_factor), 0, cost_allocation_factor)
  )

# Replace NA with 0 if description is "None"
scraped_itemized_data<- scraped_itemized_data %>%
  mutate(
    estimated_cost = ifelse(description == "None" & is.na(estimated_cost), 0, estimated_cost),
    escalated_cost = ifelse(description == "None" & is.na(escalated_cost), 0, escalated_cost),
    cost_allocation_factor = ifelse(description == "None" & is.na(cost_allocation_factor), 0, cost_allocation_factor)
  )




# Replace NA values in cost_allocation_factor column with 0 (we do not want to do this)
#scraped_itemized_data <- scraped_itemized_data %>%
#  mutate(cost_allocation_factor = ifelse(is.na(cost_allocation_factor), 0, cost_allocation_factor))
# Replace cost_allocation_factor where the current value is 0 or NA and the respective total costs are positive
scraped_itemized_data <- scraped_itemized_data %>%
  mutate(
    cost_allocation_factor = ifelse(
      cost_allocation_factor == 0 & !is.na(total_escalated_cost) & total_escalated_cost > 0,
      (escalated_cost / total_escalated_cost) * 100,
      cost_allocation_factor
    ),
    cost_allocation_factor = ifelse(
      cost_allocation_factor == 0 & !is.na(total_estimated_cost) & total_estimated_cost > 0,
      (estimated_cost / total_estimated_cost) * 100,
      cost_allocation_factor
    ),
    cost_allocation_factor = ifelse(
      is.na(cost_allocation_factor) & !is.na(total_escalated_cost) & total_escalated_cost > 0,
      (escalated_cost / total_escalated_cost) * 100,
      cost_allocation_factor
    ),
    cost_allocation_factor = ifelse(
      is.na(cost_allocation_factor) & !is.na(total_estimated_cost) & total_estimated_cost > 0,
      (estimated_cost / total_estimated_cost) * 100,
      cost_allocation_factor
    )
    
    
  )



scraped_itemized_data <- scraped_itemized_data %>%
  mutate(
    cost_allocation_factor = pmin(cost_allocation_factor, 100)
  )


#----------------------------------updating the max time to construct ------------------------------

# Helper function to extract the maximum value from a string that may represent a range (e.g., "6-20")
extract_max <- function(x) {
  if (is.na(x)) return(NA_real_)
  parts <- unlist(strsplit(as.character(x), "-"))
  nums <- as.numeric(parts)
  max(nums, na.rm = TRUE)
}

# ----------------------------------
# Step 1: Update the itemized data
# ----------------------------------

max_times <- scraped_itemized_data %>%
  # Create a new column converting estimated_time_to_construct to a numeric max value.
  mutate(estimated_time_numeric = sapply(estimated_time_to_construct, extract_max)) %>%
  group_by(q_id, type_of_upgrade) %>%
  summarise(computed_max = max(estimated_time_numeric, na.rm = TRUE),
            .groups = "drop") %>%
  # Replace -Inf with NA if no non-missing value exists.
  mutate(computed_max = ifelse(is.infinite(computed_max), NA, computed_max))

scraped_itemized_data <- scraped_itemized_data %>%
  left_join(max_times, by = c("q_id", "type_of_upgrade")) %>%
  mutate(
    max_time_to_construct = ifelse(
      is.na(max_time_to_construct) | max_time_to_construct == "",
      computed_max,
      max_time_to_construct
    )
  ) %>%
  select(-computed_max)

# ----------------------------------
# Step 2: Update the total data
# ----------------------------------

scraped_total_data <- scraped_total_data %>%
  left_join(max_times, by = c("q_id", "type_of_upgrade")) %>%
  mutate(
    max_time_to_construct = ifelse(
      is.na(max_time_to_construct) | max_time_to_construct == "",
      computed_max,
      max_time_to_construct
    )
  ) %>%
  select(-computed_max)

scraped_itemized_data <- scraped_itemized_data %>%
  mutate(total_estimated_cost = if_else(total_estimated_cost < estimated_cost, estimated_cost, total_estimated_cost))





# ----------------------- Create Codebooks -----------------------

# Helper function to generate a codebook with variable labels
# ----------------------- Create Codebooks -----------------------

# Helper function to generate a codebook with variable labels and summary statistics for numeric variables
# ----------------------- Create Codebooks -----------------------

# Helper function to generate a codebook with variable labels and a merged Levels/ Summary column
make_codebook <- function(df, max_levels = 10) {
  total_obs <- nrow(df)
  tibble(
    Variable = names(df),
    Label = map_chr(df, ~ {
      lbl <- attr(.x, "label")
      if (is.null(lbl)) "" else lbl
    }),
    Class = map_chr(df, ~ paste(class(.x), collapse = ", ")),
    N_Total = total_obs,
    N_Missing = map_int(df, ~ sum(is.na(.x))),
    N_Unique = map_int(df, ~ n_distinct(.x)),
    `Levels/ Summary` = map_chr(df, ~ {
      if (is.numeric(.x)) {
        # For numeric variables, provide summary statistics.
        min_val <- min(.x, na.rm = TRUE)
        med_val <- median(.x, na.rm = TRUE)
        mean_val <- mean(.x, na.rm = TRUE)
        max_val <- max(.x, na.rm = TRUE)
        sprintf("Min: %g; Median: %g; Mean: %g; Max: %g", min_val, med_val, mean_val, max_val)
      } else if (is.factor(.x)) {
        # For factor variables, report levels.
        paste(levels(.x), collapse = ", ")
      } else {
        ""
      }
    })
  )
}





# Define a named vector of common variable labels
common_labels <- c(
  q_id = "Unique project identifier in the Queue",
  original = "Indicates if the IC phase 2 study had an addendum study",
  cluster = "Cluster number",
  req_deliverability = "Requested Deliverability Status",
  latitude = "Latitude of the project location",
  longitude = "Longitude of the project location",
  capacity = "Project capacity (MW)",
  point_of_interconnection = "Interconnection point",
  type_of_upgrade = "Type of upgrade - IF, Network (RNU, LDNU), OPNU, CANU",
  upgrade = "Exact assigned upgrade (unique identifier)",
  description = "Further information about assigned upgrade",
  cost_allocation_factor = "% of total cost across all projects for an upgrade, allocated to you",
  estimated_cost = "Assigned cost burden ($1000s) for an upgrade",
  total_estimated_cost = "Total cost burden ($1000s) across all projects who share the upgrade",
  escalated_cost = "Inflation adjusted estimated_cost at the time of expected completion of the upgrade",
  total_escalated_cost = "Inflation adjusted total estimated cost at the time of expected completion of the upgrade",
  cost_total_inf_flag = "Binary flag: 1 if Assigned costs were positive but cost allocation factor is zero",
  cost_shared_flag = " Flag for if you share your network cost with another project i.e cost allocation in (0,100)",
  cost_network_own = " Own project total allocated network cost (estimated cost)",
  cost_network_total = "Total cost across all projects for an upgrade belonging to network type upgrade",
  cost_network_own_shared = "Own network cost where you share some upgrade level with another project",
  cost_network_shared = "Measure of Dependency, proprtion of your own network cost where you share some upgrade with another project",
  cost_network_leverage= "Second measure of dependency, total $/kW spent by other projects on completing upgrades shared with you",
  roi = "Interest rate used to deflate costs"
)

# Helper function to apply labels based on the mapping above.
apply_labels <- function(df, labels) {
  for (var in names(labels)) {
    if (var %in% names(df)) {
      attr(df[[var]], "label") <- labels[[var]]
    }
  }
  return(df)
}


# Now apply these labels to each dataset.
scraped_itemized_data <- apply_labels(scraped_itemized_data, common_labels)
scraped_total_data <- apply_labels(scraped_total_data, common_labels)


#aggregate_data_wide <- apply_labels(aggregate_data_wide, common_labels)




codebook_itemized <- make_codebook(scraped_itemized_data)
codebook_total <- make_codebook(scraped_total_data)




#-------------------------- Save CSV files--------------------------------------


# Save scraped_itemized_data as CSV using paste0 to create the file path
write.csv(scraped_itemized_data,
          file = paste0(project_root, "data/ic_studies/clean/costs_phase_2_project_upgrade_data.csv"),
          row.names = FALSE)

# Save scraped_total_data as CSV
write.csv(scraped_total_data,
          file = paste0(project_root, "data/ic_studies/clean/costs_phase_2_project_type_data.csv"),
          row.names = FALSE)

write.csv(codebook_itemized,
          file = paste0(project_root, "data/ic_studies/clean/costs_phase_2_codebook_project_upgrade_data.csv"),
          row.names = FALSE)

write.csv(codebook_total,
          file = paste0(project_root, "data/ic_studies/clean/costs_phase_2_codebook_project_type_data.csv"),
          row.names = FALSE)


#-------------------------- Creating Aggregate Data------------------------------------------------------
#-------------------------------------------------------------------------------------------

#----------------------------------Deinflating costs-----------------------------------------------


# Make sure max_time_to_construct is numeric (if not already)
scraped_itemized_data <- scraped_itemized_data %>%
  mutate(max_time_to_construct = as.numeric(max_time_to_construct)) %>% 
  mutate(cost_allocation_factor = if_else(cost_allocation_factor > 100, 100, cost_allocation_factor)) 

# Step 1: Calculate estimated years and ROI directly on scraped_itemized_data
scraped_itemized_data <- scraped_itemized_data %>%
  mutate(
    estimated_years = max_time_to_construct / 12,
    roi = ifelse(estimated_cost > 0 & estimated_years > 0,
                 (escalated_cost / estimated_cost)^(1 / estimated_years) - 1,
                 NA)
  )

# Step 2: Calculate deinflated (descalated) cost using the computed ROI
scraped_itemized_data <- scraped_itemized_data %>%
  mutate(
    descalated_cost = ifelse(
      !is.na(roi),
      escalated_cost / ((1 + roi)^(max_time_to_construct / 12)),
      NA
    )
  ) %>%
  relocate(roi, descalated_cost, .after = estimated_cost)

# Step 3: Plot Scatter Plot of Estimated Cost vs. Descalated Cost
desc_both <- ggplot(scraped_itemized_data, aes(x = estimated_cost, y = descalated_cost)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Scatter Plot of Estimated Costs vs. Descalated Costs ",
       x = "Estimated Cost",
       y = "Descalated Cost") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

# Print the scatter plot for visualization
desc_both

options(scipen = 999)

# Step 4: Identify Outliers
outliers <- scraped_itemized_data %>%
  filter(abs(estimated_cost - descalated_cost) >= 9000)

# Print the outliers to see the details
print(outliers)

# Add explanation for the analysis
print("Calculated the max estimated time to construct for each type of upgrade. Used this time variable to calculate the rate of interest for total cost data (not itemized), then applied the calculated ROI to deinflate costs in the itemized data.")



# Deinflating all costs:
#Rule: If a q_id has both est and esc costs, deinflate as above,
# If only esc costs, as roi dist is not a point mass, use the following allocation rule:
#1. Within cluster,type of upgrade, match the q_id with the same time to construct, use the corresponding roi
#2. If no such match exists, allow time matching across type of upgrades within the cluster
#3. If no such match exists as well, take an avg of the roi, within cluster, type of upgrade.

scraped_itemized_data <- scraped_itemized_data %>%
  mutate(max_time_to_construct = as.numeric(max_time_to_construct))

scraped_itemized_data <- scraped_itemized_data %>%
  mutate(
    roi = ifelse(
      estimated_cost > 0 & max_time_to_construct > 0,
      (escalated_cost / estimated_cost)^(1 / (max_time_to_construct / 12)) - 1,
      NA
    )
  )

# Step 2: Fill ROI for q_ids with only escalated costs
scraped_itemized_data <- scraped_itemized_data %>%
  group_by(cluster) %>%
  mutate(
    roi = ifelse(
      is.na(roi) & escalated_cost > 0 & estimated_cost == 0,
      
      # Prefer ROI with the same construction time and same type of upgrade
      ifelse(
        any(!is.na(roi) & max_time_to_construct == max_time_to_construct & type_of_upgrade == type_of_upgrade),
        roi[!is.na(roi) & max_time_to_construct == max_time_to_construct & type_of_upgrade == type_of_upgrade][1],
        
        # If not found, find ROI with the same construction time, any type of upgrade
        ifelse(
          any(!is.na(roi) & max_time_to_construct == max_time_to_construct),
          roi[!is.na(roi) & max_time_to_construct == max_time_to_construct][1],
          
          # If neither available, use average ROI for the type of upgrade within the cluster
          mean(roi[!is.na(roi) & type_of_upgrade == type_of_upgrade], na.rm = TRUE)
        )
      ),
      
      roi
    )
  ) %>%
  ungroup()

# Step 3: Calculate descalated costs again based on the filled ROI values
scraped_itemized_data <- scraped_itemized_data %>%
  mutate(
    descalated_cost = ifelse(
      !is.na(roi) & escalated_cost > 0,
      escalated_cost / ((1 + roi)^(max_time_to_construct / 12)),
      NA
    )
  )

scraped_itemized_data <- scraped_itemized_data %>%
  mutate(
    total_descalated_cost = ifelse(
      !is.na(roi) & total_escalated_cost > 0,
      total_escalated_cost / ((1 + roi)^(max_time_to_construct / 12)),
      NA
    )
  )



#same for the total dataset:


# Step 4: Plot Scatter Plot of Estimated Cost vs. Descalated Cost
desc_esconly <- ggplot(scraped_itemized_data, aes(x = estimated_cost, y = descalated_cost)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Scatter Plot of Estimated Costs vs. Descalated Costs",
       x = "Estimated Cost",
       y = "Descalated Cost") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma)

# Print the scatter plot for visualization
desc_esconly

options(scipen = 999)

# Step 5: Identify Outliers
outliers <- scraped_itemized_data %>%
  filter(abs(estimated_cost - descalated_cost) >= 9000)

# Print the outliers to see the details
print(outliers)


# Merge the deinflated and estimated costs


scraped_itemized_data <- scraped_itemized_data %>%
  mutate(
    estimated_cost = ifelse(
      is.na(estimated_cost) | estimated_cost == 0,
      descalated_cost,
      estimated_cost
    ),
    
    total_estimated_cost = ifelse(
      is.na(total_estimated_cost) | total_estimated_cost == 0,
      total_descalated_cost,
      total_estimated_cost
      
    ))

# Step 2: Drop specified columns
scraped_itemized_data <- scraped_itemized_data %>%
  select(-c(escalated_cost, descalated_cost,total_escalated_cost, total_descalated_cost))

# Step 3: Replace any NA in the newly constructed estimated_cost column with zero
scraped_itemized_data <- scraped_itemized_data %>%
  mutate(estimated_cost = replace_na(estimated_cost, 0),
         total_estimated_cost = replace_na(total_estimated_cost,0))








scraped_itemized_data <- scraped_itemized_data %>%
  select(
    -estimated_years,
    -estimated_time_to_construct,
    
  )

attributes(scraped_itemized_data$type_of_upgrade)










#------------------------------------- Filling in missing or 0 total costs across all projects----------------



## We shall have two measures of cost dependency, cost_type_total is the total cost across all projects for that particular upgrade type 
# Step 1: Compute cost values and update total_estimated_cost if it is NA or zero
scraped_itemized_data <- scraped_itemized_data %>%
  mutate(cost_factor = cost_allocation_factor / 100) %>%
  group_by(q_id, type_of_upgrade, upgrade) %>%
  mutate(
    # Compute the total cost; this may produce Inf if cost_factor is 0.
    computed_cost_total = estimated_cost / cost_factor,
    # Create a flag: 1 if computed_cost_total is infinite, 0 otherwise.
    cost_total_inf_flag = if_else(is.infinite(computed_cost_total), 1, 0)) %>% 
  # Recode cost_allocation_factor and update related columns if flag is 1
  mutate(
    cost_allocation_factor = if_else(cost_total_inf_flag == 1, 100, cost_allocation_factor),
    cost_factor = cost_allocation_factor / 100,  # update cost_factor based on the new allocation factor
    computed_cost_total = estimated_cost / cost_factor,
    cost_own = computed_cost_total * cost_factor
  ) %>%
  ungroup() %>%
  mutate(
    # Update total_estimated_cost: if it is NA or 0, use the computed_cost_total; otherwise, keep it.
    total_estimated_cost = if_else(is.na(total_estimated_cost) | total_estimated_cost == 0,
                                   computed_cost_total,
                                   total_estimated_cost)
  ) 


scraped_itemized_data <- scraped_itemized_data %>%
  # 1) flag “shared” network costs at the q_id/type_of_upgrade/upgrade level
  group_by(q_id, type_of_upgrade, upgrade) %>%
  mutate(
    cost_shared_flag = as.integer(
      type_of_upgrade != "PTO_IF" &  type_of_upgrade != "ADNU" &
        cost_allocation_factor  > 0     &
        cost_allocation_factor  < 100
    )
  ) %>%
  ungroup() %>% 
  # 2) for each q_id,  
  # Now, instead of summing across the q_id, just copy estimated_cost
  mutate(
    cost_network_own_shared = if_else(
      cost_shared_flag == 1,
      estimated_cost,   # for shared rows, grab the row's own estimated_cost
      0                 # or NA_real_ if you’d rather have NA for non-shared
    )
  ) %>% 
  ungroup() %>% 
  select(-cost_allocation_factor)





#----------------------------------- Compressing the type of upgrade into PTO_IF and Network------------------

#  Recode type_of_upgrade into "PTO_IF" and "Network" (for LDNU and RNU) and mark others as "drop"

# 1) First, build your two-way cost totals (but leave out cost_network_own_shared)
aggregated_costs <- scraped_itemized_data %>% 
  mutate(
    new_type_of_upgrade = case_when(
      type_of_upgrade == "PTO_IF"          ~ "PTO_IF",
      type_of_upgrade %in% c("LDNU","RNU") ~ "Network",
      TRUE                                 ~ "drop"
    )
  ) %>%
  filter(new_type_of_upgrade != "drop") %>%
  group_by(q_id, new_type_of_upgrade) %>%
  summarise(
    estimated_cost       = sum(estimated_cost,       na.rm = TRUE),
    total_estimated_cost = sum(total_estimated_cost, na.rm = TRUE),
    cost_network_own_shared = sum(cost_network_own_shared, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Next, pull out your one-row-per-q_id network-shared total
shared_by_qid <- scraped_itemized_data %>%
  group_by(q_id) %>%
  summarise(
    cost_shared_flag = first(
      cost_shared_flag[cost_shared_flag != 0],
      default = 0
    ),
    .groups = "drop"
  )

# 3) Now join them together: every q_id will have both PTO_IF and Network rows,
#    and each row will carry the same cost_network_own_shared underneath.
aggregated_temp <- aggregated_costs %>%
  left_join(shared_by_qid, by = "q_id") %>%
  mutate(
    # keep the shared‐cost only on the Network rows; hide it on PTO_IF
    cost_shared_flag = if_else(
      new_type_of_upgrade == "Network",
      # for Network: use the joined value, or 0 if it was missing
      coalesce(cost_shared_flag, 0),
      # for PTO_IF: always NA
      NA_real_
    )
  )
# Step 4: Prepare the base data (one row per q_id)
base_data <- scraped_itemized_data %>%
  select(q_id, original, cluster, req_deliverability, latitude, longitude, capacity, point_of_interconnection,cost_total_inf_flag) %>%
  distinct(q_id, .keep_all = TRUE)

# Step 5: Merge the aggregated cost data with the base data and add Phase variable
aggregate_data <- aggregated_temp %>%
  left_join(base_data, by = "q_id") %>%
  # Rename new_type_of_upgrade to type_of_upgrade for the final dataset
  rename(type_of_upgrade = new_type_of_upgrade) %>%
  # Add a new variable 'Phase' hard coded as "Phase II" and convert it to a factor
  mutate(Phase = factor(2, levels = c(2))) %>%
  # Reorder columns to insert Phase after point_of_interconnection
  select(
    q_id,
    original,
    cluster,
    req_deliverability,
    latitude,
    longitude,
    capacity,
    point_of_interconnection,
    Phase,
    type_of_upgrade,
    estimated_cost,
    total_estimated_cost,
    cost_total_inf_flag,
    cost_shared_flag,
    cost_network_own_shared
  )

# View the final aggregated data
head(aggregate_data)



# Convert the aggregated dataset into a wide format.
aggregate_data_wide <- aggregate_data %>%
  pivot_wider(
    id_cols     =        c(
      q_id, original, cluster,
      req_deliverability, latitude, longitude,
      capacity, point_of_interconnection,
      Phase, cost_total_inf_flag,
    ),
    names_from  = type_of_upgrade,
    values_from = c(estimated_cost, total_estimated_cost, cost_network_own_shared,  cost_shared_flag),
    names_glue  = "{.value}_{type_of_upgrade}",
    values_fn   = list(
      estimated_cost        = sum,
      total_estimated_cost  = sum,
      cost_network_own_shared = ~ .x[1]
    )
  ) %>%
  rename(
    cost_poi_own        = estimated_cost_PTO_IF,
    cost_network_own    = estimated_cost_Network,
    cost_poi_total      = total_estimated_cost_PTO_IF,
    cost_network_total  = total_estimated_cost_Network,
    cost_network_own_shared = cost_network_own_shared_Network  # or PTO_IF – they’re the same single value
  ) %>%
  mutate(
    cost_network_shared = cost_network_own_shared/ cost_network_own, # this is a measure of how dependent are you on other projects
    cost_network_leverage = case_when(
      cost_shared_flag_Network == 1 ~ (cost_network_total - cost_network_own) / capacity,
      TRUE                  ~ 0
    ) # this is a measure of like how much the upgrade completion depends on 
    # your project finishing
  ) %>% 
  select(- cost_network_own_shared_PTO_IF, cost_shared_flag_Network, cost_shared_flag_PTO_IF )


# View the resulting wide dataset
head(aggregate_data_wide)

attr(aggregate_data_wide$cost_network_own, "label") <- "Assigned Network Cost ($1000s) for the project"
attr(aggregate_data_wide$cost_poi_own, "label")  <- "Assigned IF Cost ($1000s) for the project"
attr(aggregate_data_wide$cost_network_total, "label") <- "Total Network Cost ($1000s) across all projects with which we have a shared Network upgrade component"
attr(aggregate_data_wide$cost_poi_total, "label")  <- "Total IF Cost ($1000s) across all projects with which we have a shared IF upgrade component"
attr(aggregate_data_wide$Phase, "label")  <- "Phase of the IC study"

aggregate_data_wide <- apply_labels(aggregate_data_wide, common_labels)

codebook_final <- make_codebook(aggregate_data_wide) 


write.csv(aggregate_data_wide,
          file = paste0(project_root, "data/ic_studies/clean/costs_phase_2_final_data.csv"),
          row.names = FALSE)

write.csv(codebook_final,
          file = paste0(project_root, "data/ic_studies/clean/costs_phase_2_codebook_final.csv"),
          row.names = FALSE)



