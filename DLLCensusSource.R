library(patchwork)
library(tidycensus)
library(purrr)
library(dplyr)
library(ggplot2)
library(scales)
library(readxl)

source("pir_source.R")

variables <- load_variables(2024, "acs1")
acs1_years <- list(2024, 2023, 2022, 2021)

OS <- .Platform$OS.type

if (OS == "unix"){
  temp_path <- "data/" # MAC file path
} else if (OS == "windows"){
  temp_path <- "..\\Head-Start-Analysis\\data\\" # windows file path
} else {
  print("ERROR: OS could not be identified")
}

child_pop <- read_excel(paste0(temp_path, "child_pop_state_age.xlsx"), .name_repair = make.names) |>
  mutate(TimeFrame = as.numeric(TimeFrame),
         Data = as.numeric(Data),
         Location = toupper(Location)) |>
  filter(LocationType == "State" & 
           Single.Age %in% c("<1", "1", "2", "3", "4", "5") &
           TimeFrame %in% c(2021:2025)) 
#_______________________________________

## Adding data from ACS B16007
eo5_17 <- map_dfr(
  acs1_years,
  ~ get_acs(
    geography = "us", 
    variables = "B16007_003", 
    year = .x, 
    survey = "acs1"),
  .id = "year")

total5_17 <- map_dfr(
  acs1_years,
  ~ get_acs(
    geography = "us", 
    variables = "B16007_002", 
    year = .x, 
    survey = "acs1"),
  .id = "year")

non_e_517 <- eo5_17 |>
  left_join(
    total5_17|>
      select(year, GEOID, NAME, totalpop = estimate),
    by = c("year", "GEOID", "NAME"))

non_e_517 <- non_e_517 |>
  rename(eng_only = estimate)

non_e_517 <- non_e_517 |>
  mutate(non_e_pop = totalpop-eng_only)

non_e_517 <- non_e_517 |>
  mutate(percent_non_e = non_e_pop / totalpop)

non_e_517$year <- as.integer(non_e_517$year)
non_e_517[1, "year"] <- as.integer(2024)
non_e_517[2, "year"] <- as.integer(2023)
non_e_517[3, "year"] <- as.integer(2022)
non_e_517[4, "year"] <- as.integer(2021)

## Adding child population data

child_pop_2024 <- child_pop[child_pop$TimeFrame == 2024,]
agechart <- aggregate(Data ~ Location, data = child_pop_2024, FUN = sum, na.rm = TRUE) |>
  arrange(desc(Data))

agechart <- agechart |>
  mutate(proxy_24 = Data * 0.2204553)
agechart <- agechart |>
  mutate(proxy_23 = Data * 0.2187957)
agechart <- agechart |>
  mutate(proxy_22 = Data * 0.2144183)
agechart <- agechart |>
  mutate(proxy_21 = Data * 0.2132899)
#_______________________________________

## Adding ACS B16007 and child population data for MA
eo5_17_ma <- map_dfr(
  acs1_years,
  ~ get_acs(
    geography = "state", 
    state = "MA", 
    variables = "B16007_003", 
    year = .x, 
    survey = "acs1"),
  .id = "year")

total5_17_ma <- map_dfr(
  acs1_years,
  ~ get_acs(
    geography = "state", 
    state = "MA",
    variables = "B16007_002", 
    year = .x, 
    survey = "acs1"),
  .id = "year")

non_e_517_ma <- eo5_17_ma |>
  left_join(
    total5_17_ma|>
      select(year, GEOID, NAME, totalpop = estimate),
    by = c("year", "GEOID", "NAME"))

non_e_517_ma <- non_e_517_ma |>
  rename(eng_only = estimate)

non_e_517_ma <- non_e_517_ma |>
  mutate(non_e_pop = totalpop-eng_only)

non_e_517_ma <- non_e_517_ma |>
  mutate(percent_non_e = non_e_pop / totalpop)

non_e_517_ma$year <- as.integer(non_e_517_ma$year)
non_e_517_ma[1, "year"] <- as.integer(2024)
non_e_517_ma[2, "year"] <- as.integer(2023)
non_e_517_ma[3, "year"] <- as.integer(2022)
non_e_517_ma[4, "year"] <- as.integer(2021)

ma_child_pop <- child_pop_2024 |>
  filter(Location == "MASSACHUSETTS")

ma_agechart <- aggregate(Data ~ Single.Age, data = ma_child_pop, FUN = sum, na.rm = TRUE)

ma_agechart <- ma_agechart |>
  mutate(proxy_24 = Data * 0.2538558)
ma_agechart <- ma_agechart |>
  mutate(proxy_23 = Data * 0.2526584)
ma_agechart <- ma_agechart |>
  mutate(proxy_22 = Data * 0.2418832)
ma_agechart <- ma_agechart |>
  mutate(proxy_21 = Data * 0.2404779)
#_______________________________________

## Headline Statistics Code
statewide_dll <- sum(ma_agechart$proxy_24)
statewide_children <- sum(ma_child_pop$Data)
statewide_percent <- statewide_dll/statewide_children

headstart_dll_years <- pir_a |>
  group_by(pir_year) |>
  summarize(total_child_enroll = sum(A.10.g, na.rm = TRUE), 
            total_dll_enroll = sum(A.27, na.rm = TRUE),
            percent_dll = total_dll_enroll / total_child_enroll * 100) |>
  arrange(desc(percent_dll))

headstart_dll_states <- pir_a |>
  group_by(State, pir_year) |>
  filter(pir_year == 2024) |>
  summarize(total_child_enroll = sum(A.10.g, na.rm = TRUE), 
            total_dll_enroll = sum(A.27, na.rm = TRUE),
            percent_dll = total_dll_enroll / total_child_enroll * 100) |>
  arrange(desc(percent_dll))

headstart_dll_ma <- pir_a |>
  filter(State == "MA") |>
  group_by(pir_year) |>
  summarize(total_child_enroll = sum(A.10.g, na.rm = TRUE), 
            total_dll_enroll = sum(A.27, na.rm = TRUE),
            percent_dll = total_dll_enroll / total_child_enroll * 100) |>
  arrange(desc(percent_dll))
#_______________________________________

child_pop_21_24 <- child_pop[child_pop$TimeFrame %in% 2021:2024,]
ma_child_pop_21_24 <- child_pop_21_24 |>
  filter(Location == "MASSACHUSETTS")
ma_agechart_2124 <- aggregate(Data ~ Single.Age + TimeFrame, data = ma_child_pop_21_24, FUN = sum, na.rm = TRUE)

ma_agechart_2124 <- ma_agechart_2124 |>
  mutate(proxy_24 = Data * 0.2538558)
ma_agechart_2124 <- ma_agechart_2124 |>
  mutate(proxy_23 = Data * 0.2526584)
ma_agechart_2124 <- ma_agechart_2124 |>
  mutate(proxy_22 = Data * 0.2418832)
ma_agechart_2124 <- ma_agechart_2124 |>
  mutate(proxy_21 = Data * 0.2404779)

ma_agechart_2124_groups <- aggregate(
  cbind(Data, proxy_24) ~ TimeFrame,
  data = ma_agechart_2124,
  FUN = sum,
  na.rm = TRUE)

ma_dllenroll <- left_join(ma_agechart_2124_groups, headstart_dll_ma, 
                           by = c("TimeFrame" = "pir_year"))|>
  mutate(dll_prop = total_dll_enroll/proxy_24)
#_______________________________________

agechart_2124 <- aggregate(Data ~ Single.Age + TimeFrame, data = child_pop_21_24, 
                              FUN = sum, na.rm = TRUE)

agechart_2124 <- agechart_2124 |>
  mutate(proxy_24 = Data * 0.2204553)
agechart_2124 <- agechart_2124 |>
  mutate(proxy_23 = Data * 0.2187957)
agechart_2124 <- agechart_2124 |>
  mutate(proxy_22 = Data * 0.2144183)
agechart_2124 <- agechart_2124 |>
  mutate(proxy_21 = Data * 0.2132899)

agechart_2124_groups <- aggregate(
  cbind(Data, proxy_24) ~ TimeFrame,
  data = agechart_2124,
  FUN = sum,
  na.rm = TRUE)

usa_dllenroll <- left_join(agechart_2124_groups, headstart_dll_years, 
                           by = c("TimeFrame" = "pir_year"))|>
  mutate(dll_prop = total_dll_enroll/proxy_24)
