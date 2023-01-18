# Load packages required to define the pipeline:
library(here)
library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("adehabitatLT", "lubridate", "tidyverse"),
  format = "rds"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Targets pipeline
list(
  # Read raw data
  tar_target(laal_path,
             here("data", "USGS", "LAAL_eObs", "LAAL_eObs_clean.csv")),
  tar_target(laal_tracks_raw, read_laal(laal_path))
)
