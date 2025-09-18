# --------------------------------------------------------------
# Project Constants File
# --------------------------------------------------------------
# Purpose:
# - Define variables specific to this project only.
# - These should not impact other projects.
# --------------------------------------------------------------

# ------------------------------------------------------------------------------
# Project-Specific Variables
# ------------------------------------------------------------------------------
project_name <- "project_placeholder_to_replace"
standardised_schema <- "stage" # NOTE: it is best practice to use a dated version of stage
primary_dbname <- "cdm_dpp_haematologicalmalignancies"
primary_diagnosis_icd10_regex <- "^C900"
exploratory_diagnosis_icd10_regex <- "^C902"
medication_name_list <- c("aspirin")
minimum_study_entry_date <- as.Date("2015-01-01")

# ------------------------------------------------------------------------------
# Data Storage
# ------------------------------------------------------------------------------
project_root <- path("/mnt/users_shared/Users/RWE_Projects/", project_name) # global
lead_role <- "analyst_1"
qc_role <- "analyst_2"
analysts <- c(lead_role, qc_role)

cdm_data_storage_name <- here::here("data", "raw", paste0(project_name, "_cdm_data.rds"))
raw_data_storage_name <- here::here("data", "raw", paste0(project_name, "_raw_data.rds"))
study_population_data_storage_name <- here::here("data", "raw", paste0(project_name, "_study_population.rds"))

analysis_data_storage_name <- here::here("data", "processed", paste0(project_name, "_analysis_data.rds"))
analysis_cohorts_storage_name <- here::here("data", "processed", paste0(project_name, "_analysis_cohorts.rds"))

message("Project-specific constants loaded successfully.")
