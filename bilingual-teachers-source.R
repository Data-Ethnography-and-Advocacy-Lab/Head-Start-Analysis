library(tidycensus)
library(purrr)
library(dplyr)

variables <- load_variables(2024, "acs5")

years <- list(2024, 2020, 2016, 2012)

#bilingual population
english_only_pop_ma <- map_dfr(
  years,
  ~ get_acs(
    geography = "state", 
    variables = "B16001_002", 
    year = .x, 
    survey = "acs5",
    state = "MA"),
  .id = "year")

total_pop_ma <- map_dfr(
  years,
  ~ get_acs(
    geography = "state", 
    variables = "B16001_001", 
    year = .x, 
    survey = "acs5",
    state = "MA"),
  .id = "year")

ma_2024_total <- total_pop_ma |>
  filter(year == 1)

ma_2024_english_only <- english_only_pop_ma |>
  filter(year ==1)

pums_vars_2024 <- pums_variables |>
  filter(year == 2024, survey == "acs1")

pums_vars_2024 |>
  distinct(var_code, var_label, data_type, level)

ma_pums_recoded <- get_pums(
  variables = c("OCCP", "LANX"),
  state = "MA",
  survey = "acs1",
  year = 2024,
  recode = TRUE
)

acs1_years <- c(2021, 2022, 2023, 2024, 2025)

#ma_pums_temporal <- map_dfr(
#  acs1_years,
#  ~ get_pums(
 #   variables = c("OCCP", "LANX"),
#    state = "MA",
 #   survey = "acs1",
 #   year = .x,
  #  recode = TRUE
 # )
#)