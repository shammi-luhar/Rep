# --------------------------------------------------------------
# Data Collection Script for Project
# --------------------------------------------------------------
# Purpose:
# - Connect to the database
# - Define diagnosis study population using ICD-10 codes
# - Load standardized clinical data for the defined cohort
# - Save the collected data for reproducibility and further analysis
# --------------------------------------------------------------

# Load required functions
source(here::here("scripts", "01_data_collection", "cdm_data_collection_fcns.R"))
source(here::here("scripts", "01_data_collection", "raw_data_collection_fcns.R"))

# --------------------------------------------------------------
# Database Connection Setup
# --------------------------------------------------------------
message("Establishing database connection...")
connection <- create_db_connection(dbname = primary_dbname)

# Verify that the connection is valid
if (!DBI::dbIsValid(connection)) {
  stop("The database connection is invalid after creation.")
}

# --------------------------------------------------------------
# Define Diagnosis Study Population using ICD-10 Codes
# --------------------------------------------------------------
message("Defining study population using ICD-10 codes...")
study_population <- build_diagnosis_cohort(
  conn = connection,
  icd10_pattern = primary_diagnosis_icd10_regex,
  schema = standardised_schema,
  min_date = minimum_study_entry_date, # Delete this row if you dont need to filter by date
  collect = FALSE
)

# --------------------------------------------------------------
# Load CDM Clinical Data
# --------------------------------------------------------------
message("Loading standardized clinical data...")

# Un-comment this line to see al available tables in schema
# get_tables_in_schema(con = connection, schema = standardised_schema)

# Define how CDM tables should be filtered (or not) before being collected
cdm_table_info <- tibble::tribble(
  ~schema, ~table, ~filter_expression,
  standardised_schema, "medications", build_grepl_expression(
    regex_list = medication_name_list,
    column = "medication_name"
  ),
  standardised_schema, "clinical_observations", "(observation_result == 'Yes') & (observation_name == 'GCS Assessed')",
  standardised_schema, "diagnoses", "" # Return the complete table after joining with cohort.
)

# Ensure that cdm_table_info is defined and not empty
if (!exists("cdm_table_info") || nrow(cdm_table_info) == 0) {
  message("The variable 'cdm_table_info' is not defined or is empty.")
}

cdm_data <- load_multiple_filtered_cdm_tables(
  conn = connection,
  table_info = cdm_table_info,
  cohort = study_population,
  collect = TRUE
)

# --------------------------------------------------------------
# Load Raw Clinical Data
# --------------------------------------------------------------
message("Loading raw clinical data...")

# Define which raw tables should be collected and how the patient_id column should be built
raw_table_info <- tibble::tribble(
  ~schema,                           ~table,                    ~prefix,
  raw_schemas$raw_ouh_schema,        "pathology_reports",       "ouh_"
)

# Ensure that raw_table_info is defined and not empty
if (!exists("raw_table_info") || nrow(raw_table_info) == 0) {
  message("The variable 'raw_table_info' is not defined or is empty.")
}

raw_data <- load_multiple_raw_tables(
  connection,
  raw_table_info,
  cohort = study_population,
  collect = TRUE
)

# --------------------------------------------------------------
# Collect Study Population as R Object
# --------------------------------------------------------------
message("Converting study population to an R object...")
study_population <- study_population %>% collect()

# --------------------------------------------------------------
# Save Collected Data for Future Use
# --------------------------------------------------------------
message("Saving collected data...")
saveRDS(cdm_data, cdm_data_storage_name)
saveRDS(raw_data, raw_data_storage_name)
saveRDS(study_population, study_population_data_storage_name)
message("Collected data successfully saved to ", here::here("data"))

# --------------------------------------------------------------
# Close Database Connection
# --------------------------------------------------------------
message("Closing database connection...")
end_db_connection(connection)

message("Data collection script completed successfully.")
