# --------------------------------------------------------------
# Initialization Script for Project Workflow
# --------------------------------------------------------------
# Purpose:
# - Load necessary libraries
# - Define project directory
# - Load credentials and constants
# - Ensure proper working directory setup
# --------------------------------------------------------------

# Load necessary packages
suppressMessages({
  library(tidyverse) # Core R packages for data manipulation
  library(dbplyr) # Database integration for dplyr
  library(here)
  library(RPostgres)
  library(rlang)
  library(fs)
  library(glue)
  # ADD FURTHER PACKAGES HERE:
})

# --------------------------------------------------------------
# Security Warning: Handle IPD Carefully
# --------------------------------------------------------------
message("
####################################################
BEWARE OF SAVING IPD OUTSIDE OF THE COMPUTE INSTANCE
IPD INCLUDES ANY PATIENT IDENTIFIABLE INFORMATION:
    - PATIENT IDS
    - COUNT VALUES <5
    - NOTE: REGRESSION MODELS INCLUDE IPD AS WELL
####################################################
")

# --------------------------------------------------------------
# Load Helper Functions & Configurations
# --------------------------------------------------------------
source(here("scripts", "00_initialise", "helper_functions.R")) # Load project-specific functions

# Load sensitive credentials securely
source("~/credentials.R")

# Load project-specific constants and operational settings
source(here("data", "input", "operational_constants.R"))
source(here("data", "input", "project_constants.R"))

# Initialise global data storage for QC purposes
init_project(
  proj_root = project_root,
  analysts = analysts
)

# Project-specific variables

# --------------------------------------------------------------
# Load Any Further Input Files
# --------------------------------------------------------------

# EXAMPLE:
# mapping_file_path <- here("data", "input", "mapping_file.csv")
# if (file.exists(mapping_file_path)) {
#   mapping1 <- read_csv(mapping_file_path)
# } else {
#   warning("Mapping file not found: ", mapping_file_path)
# }

message("Initialisation script completed successfully.")
