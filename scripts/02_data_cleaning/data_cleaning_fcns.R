# ------------------------------------------------------------------------------
# Data Cleaning Functions
# ------------------------------------------------------------------------------
# Purpose:
#   This file defines functions to clean individual tables from the CDM and raw
#   datasets. Each function takes a single table as input and returns a cleaned
#   version of that table.
#
# ------------------------------------------------------------------------------

################################################################################
# NOTE: These functions are purely examples to show how this file should be
#       structured and how functions should be named. These functions should
#       not be used in a live project and should be replaced by the user.
################################################################################

#' Clean Diagnoses Table
#'
#' Removes rows with missing diagnosis codes and converts the diagnosis date
#' column to Date format.
#'
#' @param diagnoses A data frame containing diagnoses data from the CDM.
#' @return A data frame with cleaned diagnoses data.
clean_diagnoses <- function(diagnoses) {
  cleaned <- diagnoses %>%
    filter(!is.na(diagnosis_code)) %>%
    mutate(diagnosis_date = as.Date(diagnosis_date))
  return(cleaned)
}

#' Clean Clinical Observations Table
#'
#' Filters out records with missing patient_id codes.
#'
#' @param procedures A data frame containing clinical observations data from the CDM.
#' @return A data frame with cleaned clinical observations data.
clean_clinical_observations <- function(clinical_observations) {
  cleaned <- clinical_observations %>%
    filter(!is.na(patient_id))
  return(cleaned)
}

#' Clean Medications Table
#'
#' Filters out records with missing patient_id codes
#'
#' @param medications A data frame containing CDM medications.
#' @return A data frame with cleaned medications results.
clean_medications <- function(medications) {
  cleaned <- medications %>%
    filter(!is.na(patient_id))
  return(cleaned)
}

#' Clean Pathology Reports Table
#'
#' Filters out records with missing snomed_t codes
#'
#' @param medications A data frame containing raw pathology reports.
#' @return A data frame with cleaned pathology reports.
clean_pathology_reports <- function(pathology_reports) {
  cleaned <- pathology_reports %>%
    filter(!is.na(snomed_t))
  return(cleaned)
}
