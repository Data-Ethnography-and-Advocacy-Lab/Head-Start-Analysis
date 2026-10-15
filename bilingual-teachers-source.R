library(tidycensus)
library(purrr)
library(dplyr)
library(ggplot2)
library(scales)

variables <- load_variables(2024, "acs5")

years <- list(2024, 2020, 2016, 2012)

over_18_other_lang_ma <- map_dfr(
  years,
  ~ get_acs(
    geography = "us", 
    variables = "B16001_002", 
    year = .x, 
    survey = "acs5"),
  .id = "year")