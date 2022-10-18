#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script loads libraries for targets development.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: works like a shiny global file
#-------- just used to load libraries when you need them
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

# library(targets)
# library(here)
# library(readxl)
#
# library(tidyverse)
# library(gauntlet)
# library(lubridate)

# library(sf)
# library(tigris)
# library(leaflet)
# library(leafem)
# library(mapview)
#
# library(crosstalk)
# library(plotly)
#
# library(DBI)
# library(bigrquery)
#
# library(htmltools)

pkgs = c("tibble", "here", "tidyverse", "gauntlet", "lubridate", "purrr"
         ,"readxl", "rvest"
         ,"tidymodels", "tidytext", "textclean", "textstem", "textrecipes"
)

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(pkgs)


#script end=====================================================================


