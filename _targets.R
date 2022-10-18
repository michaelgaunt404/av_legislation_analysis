# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(here)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble", "here", "tidyverse", "gauntlet", "lubridate", "purrr"
               ,"readxl", "rvest"
               ,"tidymodels", "tidytext", "textclean", "textstem", "textrecipes"
  ), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# tar_make_future() configuration (okay to leave alone):
future::plan(future.callr::callr)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
list(
  tar_target(av_leg_summary,
             here("data", "manual_extract_av_leg_summary.xlsx"), format = "file")
  ,tar_target(data_av_leg_sum, proces_av_leg_sum(av_leg_summary))
  ,tar_target(data_av_leg_full, scrape_av_bill_legislation())
  ,tar_target(data_av_leg_sum_ngrams, process_summary_text(data = data_av_leg_sum))
)






