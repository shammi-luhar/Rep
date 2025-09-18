# ------------------------------------------------------------------------------
# Create Analysis Dataset: Clean and Derive Data
# ------------------------------------------------------------------------------
# Purpose:
#   This script cleans individual tables contained in the 'cdm_data' and 'raw_data'
#   objects using dedicated cleaning functions. It then calls derivation functions
#   to generate additional tables. The final cleaned tables are stored in an object
#   called 'analysis_data', with an element 'derived_data' holding the derived tables.
#   Cleaned tables may involve derivation of new columns, filtering, and other
#   transformations within a single CDM table.
#   Derived tables take a new shape and may be created from multiple cleaned tables.
#
# Prerequisites:
#   - The objects 'cdm_data' and 'raw_data' must be loaded in the environment.
#   - Cleaning and derivation functions must be available. These are sourced below.
#
# ------------------------------------------------------------------------------

# Load the cleaning and derivation functions
source(here::here("scripts", "02_data_cleaning", "data_cleaning_fcns.R"))
source(here::here("scripts", "02_data_cleaning", "data_derivation_fcns.R"))

# ------------------------------------------------------------------------------
# Clean Data and Assemble into analysis_data Object
# ------------------------------------------------------------------------------
analysis_data <- list()

# Clean each CDM table in turn
analysis_data$cdm_data <- list(
  clinical_observations = clean_clinical_observations(cdm_data$clinical_observations),
  diagnoses = clean_diagnoses(cdm_data$diagnoses),
  medications = clean_medications(cdm_data$medications)
)

# Clean each raw table in turn
analysis_data$raw_data <- list(
  pathology_reports = clean_pathology_reports(raw_data$pathology_reports)
)

# ------------------------------------------------------------------------------
# Derive Additional Tables
# ------------------------------------------------------------------------------
# Derive new tables from the cleaned CDM and raw data. For instance, calculate
# BMI from weight and height data.
analysis_data$derived_data <- list(
  bmi_observations = derive_bmi(analysis_data$cdm_data$clinical_observations)
)

# --------------------------------------------------------------
# Save derived/cleaned Data for Future Use
# --------------------------------------------------------------
message("Saving cleaned/derived analysis data...")
saveRDS(analysis_data, analysis_data_storage_name)
message("Analysis data successfully saved to ", analysis_data_storage_name)

# ------------------------------------------------------------------------------
# Final Message
# ------------------------------------------------------------------------------
message("Finalised analysis_data and derived_data have been created. No further cleaning is required before analysis.")
