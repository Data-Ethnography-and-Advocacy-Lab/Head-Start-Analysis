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

acs1_variables <- load_variables(2024, "acs1")

acs1_years <- c(2021, 2022, 2023, 2024)

total_pop_by_state <- map_dfr(
  acs1_years,
  ~ get_acs(
    geography = "state", 
    variables = "B01003_001", 
    year = .x, 
    survey = "acs1"),
  .id = "year")

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

pums_recoded_2024 <- get_pums(
  variables = c("OCCP", "LANX"),
  state = "all",
  survey = "acs1",
  year = 2024,
  recode = TRUE
)

pums_recoded_2023 <- get_pums(
  variables = c("OCCP", "LANX"),
  state = "all",
  survey = "acs1",
  year = 2023,
  recode = TRUE
)

pums_recoded_2022 <- get_pums(
  variables = c("OCCP", "LANX"),
  state = "all",
  survey = "acs1",
  year = 2022,
  recode = TRUE
)

pums_recoded_2021 <- get_pums(
  variables = c("OCCP", "LANX"),
  state = "all",
  survey = "acs1",
  year = 2021,
  recode = TRUE
)

total_pop_ma_acs1 <- 
  map_dfr(
    acs1_years,
    ~ get_acs(
      geography = "state", 
      variables = "B01003_001", 
      year = .x, 
      survey = "acs1",
      state = "MA"),
    .id = "year")
total_pop_by_year <-
  map_dfr(
    acs1_years,
    ~ get_acs(
      geography = "us", 
      variables = "B01003_001", 
      year = .x, 
      survey = "acs1"),
    .id = "year")