library(tidycensus)
library(purrr)
library(dplyr)
library(ggplot2)
library(scales)

variables <- load_variables(2024, "acs5")

years <- list(2024, 2020, 2016, 2012)

english_only_pop <- map_dfr(
  years,
  ~ get_acs(
    geography = "us", 
    variables = "B16001_002", 
    year = .x, 
    survey = "acs5"),
  .id = "year")

total_pop <- map_dfr(
  years,
  ~ get_acs(
    geography = "us", 
    variables = "B16001_001", 
    year = .x, 
    survey = "acs5"),
  .id = "year")

english_only_pop_ma <- map_dfr(
  years,
  ~ get_acs(
    geography = "state", 
    state = "MA",
    variables = "B16001_002", 
    year = .x, 
    survey = "acs5"),
  .id = "year")

total_pop_ma <- map_dfr(
  years,
  ~ get_acs(
    geography = "state", 
    state = "MA",
    variables = "B16001_001", 
    year = .x, 
    survey = "acs5"),
  .id = "year")