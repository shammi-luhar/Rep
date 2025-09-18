# ------------------------------------------------------------------------------
# Data Derivation Functions
# ------------------------------------------------------------------------------
# Purpose:
#   This file defines functions that derive new variables or tables from the cleaned
#   data. Examples include calculating BMI from weight and height data.
#
# ------------------------------------------------------------------------------

#' Calculate BMI from Clinical Observations with Height in cm
#'
#' This function calculates the Body Mass Index (BMI) for patients based on weight and height
#' observations recorded in a clinical observations table. The input table must have the columns:
#'   - `patient_id`: Unique patient identifier.
#'   - `observation_date`: Date of the observation (will be converted to Date).
#'   - `observation_name`: Name of the observation; expected values include "weight" and "height" (case insensitive).
#'   - `observation_result`: Numeric value for the observation.
#'   - `observation_unit`: Unit of measurement. For weight, it should be in kilograms. For height, the unit
#'     is assumed to be centimeters.
#'
#' The function calculates BMI using weight and height observations recorded on the same day. If no height
#' is recorded on the day a weight measure is taken, the most recent previous height is used. If no height is
#' available, that weight observation is omitted from the BMI calculation.
#'
#' BMI is calculated as:
#'
#' BMI = weight (kg)/height^2 (m^2)
#'
#'
#' @param clinical_observations A tibble (or data frame) containing the clinical observations.
#'
#' @return A tibble with the following columns:
#'   - `patient_id`: Unique patient identifier.
#'   - `observation_date`: The date corresponding to the weight observation used for BMI calculation.
#'   - `weight`: Weight in kilograms.
#'   - `height`: Height in meters (converted from centimeters).
#'   - `bmi`: The calculated Body Mass Index.
#'
#' @examples
#' \dontrun{
#' # Assuming 'clinical_observations' is a tibble with the required columns:
#' bmi_results <- calculate_bmi(clinical_observations)
#' head(bmi_results)
#' }
derive_bmi <- function(clinical_observations) {
  # Check that required columns exist
  required_cols <- c("patient_id", "observation_date", "observation_name", "observation_result", "observation_unit")
  missing_cols <- setdiff(required_cols, names(clinical_observations))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in clinical_observations: ", paste(missing_cols, collapse = ", "))
  }

  # Convert input to tibble and ensure observation_date is a Date
  obs <- as_tibble(clinical_observations) %>%
    mutate(observation_date = as.Date(observation_date))

  # Extract weight observations (assumed to be in kg)
  weight_obs <- obs %>%
    filter(tolower(observation_name) == "weight") %>%
    mutate(
      weight_date = observation_date,
      weight = as.numeric(observation_result)
    ) %>%
    select(patient_id, weight_date, weight)

  # Extract height observations and convert from cm to meters (assumes height is always recorded in cm)
  height_obs <- obs %>%
    filter(tolower(observation_name) == "height") %>%
    mutate(
      height_date = observation_date,
      height = as.numeric(observation_result) / 100
    ) %>%
    select(patient_id, height_date, height)

  # For each weight observation, find the most recent height observation on or before the weight_date.
  # If no height is available, that weight observation is dropped.
  bmi_data <- weight_obs %>%
    left_join(height_obs, by = "patient_id") %>%
    filter(height_date <= weight_date) %>%
    group_by(patient_id, weight_date, weight) %>%
    filter(height_date == max(height_date)) %>%
    ungroup() %>%
    mutate(bmi = weight / (height^2)) %>%
    select(patient_id, observation_date = weight_date, weight, height, bmi)

  return(bmi_data)
}
