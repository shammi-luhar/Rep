# ------------------------------------------------------------------------------
# Creation of Analysis Cohort(s)
# ------------------------------------------------------------------------------
# Purpose:
#   This script cleans ...
#
# Prerequisites:
#   - The 'analysis_data' object must be loaded in the environment.
#   - Cohort generation functions must be available. These are sourced below.
#
# ------------------------------------------------------------------------------

# Load the cohort generation functions
source(here::here("scripts", "03_cohort_generation", "cohort_generation_fcns.R"))

# ------------------------------------------------------------------------------
# Generate One or More Analysis Cohort
# ------------------------------------------------------------------------------
analysis_cohorts <- list(
  # Generate a primary study cohort
  primary_cohort = generate_primary_cohort(analysis_data),
  # Generate a further exploratory cohort
  exploratory_cohort = generate_exploratory_cohort(analysis_data)
)

# If the analysis cohort is the same as the study population, this should be
# still set as the analysis cohort here. e.g,
# analysis_cohorts <- list(
#   # Generate a primary study cohort
#   primary_cohort = study_population,
# )

# --------------------------------------------------------------
# Save Cohort Details for Future Use
# --------------------------------------------------------------
message("Saving generated cohort details...")
saveRDS(analysis_cohorts, analysis_cohorts_storage_name)
message("Analysis cohorts successfully saved to ", analysis_cohorts_storage_name)

# ------------------------------------------------------------------------------
# Final Message
# ------------------------------------------------------------------------------
message("Analysis cohorts have been generated.")
