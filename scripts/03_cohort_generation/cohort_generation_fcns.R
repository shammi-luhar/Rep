# ------------------------------------------------------------------------------
# Cohort Generation Functions
# ------------------------------------------------------------------------------
# Purpose:
#   This file defines functions to generate one or more cohorts from the
#   analysis_data object. Each function takes this object as input and returns
#   a table of patient_ids at the minimum. These patient_id values can be used
#   to restrict analyses to a particular cohort by sub-setting the analysis_data
#   object when performing analyses
# ------------------------------------------------------------------------------

################################################################################
# NOTE: These functions are purely examples to show how this file should be
#       structured and how functions should be named. These functions should
#       not be used in a live project and should be replaced by the user.
################################################################################

#' Build Primary Analysis Cohort
#'
#' Returns a tibble containing patient_id values that fulfill the eligibility
#' criteria of the primary cohort. Eligibility consists of the patient having
#' the study primary diagnosis code C900 at any time.
#'
#' @param analysis_data A list containing cleaned data from the CDM and raw sources.
#' @return A data frame with a patient_id column.
generate_primary_cohort <- function(analysis_data) {
  analysis_cohort <- analysis_data$cdm_data$diagnoses %>%
    filter(grepl(primary_diagnosis_icd10_regex, diagnosis_code, ignore.case = TRUE)) %>%
    distinct(patient_id)

  return(analysis_cohort)
}

#' Build Exploratory Analysis Cohort
#'
#' Returns a tibble containing patient_id values that fulfill the eligibility
#' criteria of the exploratory cohort. Eligibility consists of the presence of
#' a C902 ICD-10 diagnosis code at any time in the 5 years after the minimum
#' study entry date.
#'
#' @param analysis_data A list containing cleaned data from the CDM and raw sources.
#' @return A data frame with a patient_id column and the date of first eligibility.
generate_exploratory_cohort <- function(analysis_data) {
  analysis_cohort <- analysis_data$cdm_data$diagnoses %>%
    filter(
      grepl(exploratory_diagnosis_icd10_regex, diagnosis_code, ignore.case = TRUE),
      diagnosis_date < minimum_study_entry_date + 5 * 365
    ) %>%
    group_by(patient_id) %>%
    summarise(cohort_entry_date = min(diagnosis_date))

  return(analysis_cohort)
}
