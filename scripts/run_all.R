# ------------------------------------------------------------------------------
# Full Analysis Pipeline
# ------------------------------------------------------------------------------
# Purpose:
#   - Run the entire project analysis pipeline:
#       1. Initialise the project environment.
#       2. Load project-specific data (either from stored files or via data collection).
#       3. Process and clean data.
#       4. Analyse data and generate project outputs.
#
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# STEP 0: Initialise the Project Environment
# ------------------------------------------------------------------------------
# Clean environment
rm(list = ls())

# This step sets up global variables and configurations for the project.
source(here::here("scripts", "00_initialise", "init.R"))

# ------------------------------------------------------------------------------
# STEP 1: Load Project-Specific Data
# ------------------------------------------------------------------------------
# If the primary project data (saved as an RDS file) exists, load it; otherwise, run the
# data collection scripts to create it. To force data collection (i.e., update the data),
# manually set run_data_collection to TRUE.
run_data_collection <- !all(file.exists(c(cdm_data_storage_name, raw_data_storage_name, study_population_data_storage_name)))

if (run_data_collection) {
  message("Running data collection for project ", project_name, "...")
  source(here::here("scripts", "01_data_collection", "run_data_collection.R"))
  message("Project data freshly collected for project ", project_name, ".")
} else {
  message("Loading saved collected data for project ", project_name, "...")
  cdm_data <- readRDS(cdm_data_storage_name)
  raw_data <- readRDS(raw_data_storage_name)
  study_population <- readRDS(study_population_data_storage_name)
  message("Project data loaded from stored files for project ", project_name, ".")
}

# ------------------------------------------------------------------------------
# STEP 2: Process and Clean Data
# ------------------------------------------------------------------------------
# This step runs the data cleaning and processing scripts to generate the
# final analysis dataset.
run_data_cleaning <- !all(file.exists(c(analysis_data_storage_name)))

if (run_data_cleaning) {
  # If the analysis data file does not exist, run the data cleaning scripts.
  # This will create the final analysis dataset.
  message("Running data cleaning for project ", project_name, "...")
  source(here::here("scripts", "02_data_cleaning", "run_data_cleaning.R"))
  message("Data cleaning complete for project ", project_name, ".")
} else {
  # If the analysis data file exists, load it.
  message("Loading analysis data for project ", project_name, "...")
  analysis_data <- readRDS(analysis_data_storage_name)
  message("Analysis data loaded for project ", project_name, ".")
}

# ------------------------------------------------------------------------------
# STEP 3: Generate Study Cohort(s) from Cleaned Data
# ------------------------------------------------------------------------------
run_cohort_generation <- !all(file.exists(c(analysis_cohorts_storage_name)))

if (run_cohort_generation) {
  # If the analysis cohorts file does not exist, run the cohort generation scripts.
  # This will create the final analysis cohorts.
  message("Running cohort generation for project ", project_name, "...")
  source(here::here("scripts", "03_cohort_generation", "run_cohort_generation.R"))
  message("Cohort generation complete for project ", project_name, ".")
} else {
  # If the analysis cohorts file exists, load it.
  message("Loading analysis cohorts for project ", project_name, "...")
  analysis_cohorts <- readRDS(analysis_cohorts_storage_name)
  message("Analysis cohorts loaded for project ", project_name, ".")
}

# ------------------------------------------------------------------------------
# STEP 4: Analyse Data and Create Project Outputs
# ------------------------------------------------------------------------------
# This step runs the data analysis scripts to generate project outputs.
source(here::here("scripts", "04_data_analysis", "run_data_analysis.R"))
message("Data analysis complete for project ", project_name, ".")
