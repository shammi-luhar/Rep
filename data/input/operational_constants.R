# --------------------------------------------------------------
# RWE-Wide Constants File
# --------------------------------------------------------------
# Purpose:
# - Define variables that are common across multiple projects.
# - These should rarely change unless required across all projects.
# --------------------------------------------------------------

# Global variables
rwe_project_folder <- "/mnt/users_shared/Users/RWE_Projects/"
MONTH <<- 30.4375
ANALYSIS_DATE <<- Sys.Date()
YEAR_IN_DAYS <<- 365.25

raw_schemas <- list(
  raw_ouh_schema = "raw_automated_ouh",
  raw_hhft_schema = "raw_automated_hhft",
  raw_cuh_schema = "raw_automated_cambridge",
  raw_chelwest_schema = "raw_automated_chelwest",
  raw_mkuh_schema = "raw_automated_mkuh"
)

# Arcturis chart colours
arcturis_colors <- list(
  flare     = "#FF4613",
  matter    = "#FFFFFF",
  lunar     = "#B1B1B1",
  expanse   = "#131E29",
  sol       = "#FFE13C",
  rubine    = "#FF005F",
  ice       = "#80D5FF",
  verdigris = "#00FFAA"
)
