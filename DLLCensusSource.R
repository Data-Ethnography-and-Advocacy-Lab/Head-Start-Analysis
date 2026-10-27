library(patchwork)
library(tidycensus)
library(purrr)
library(dplyr)
library(ggplot2)
library(scales)
library(readxl)

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


## Calculating US Bilingual Population
bilingual_pop <- english_only_pop |>
  left_join(
    total_pop|>
      select(year, GEOID, NAME, totalpop = estimate),
    by = c("year", "GEOID", "NAME"))

bilingual_pop <- bilingual_pop |>
  mutate(bilingualpop = totalpop-estimate)

bilingual_pop <- bilingual_pop |>
  mutate(percent_bilingual = bilingualpop / totalpop * 100)

bilingual_pop$year <- as.character(bilingual_pop$year)
bilingual_pop[1, "year"] <- as.character("2020-2024")
bilingual_pop[2, "year"] <- as.character("2016-2020")
bilingual_pop[3, "year"] <- as.character("2012-2016")
bilingual_pop[4, "year"] <- as.character("2008-2012")


## Calculating MA Bilingual Population
bilingual_pop_ma <- english_only_pop_ma |>
  left_join(
    total_pop_ma|>
      select(year, GEOID, NAME, totalpop = estimate),
    by = c("year", "GEOID", "NAME"))

bilingual_pop_ma <- bilingual_pop_ma |>
  mutate(bilingualpop = totalpop-estimate)

bilingual_pop_ma <- bilingual_pop_ma |>
  mutate(percent_bilingual = bilingualpop / totalpop * 100)

bilingual_pop_ma$year <- as.character(bilingual_pop_ma$year)
bilingual_pop_ma[1, "year"] <- as.character("2020-2024")
bilingual_pop_ma[2, "year"] <- as.character("2016-2020")
bilingual_pop_ma[3, "year"] <- as.character("2012-2016")
bilingual_pop_ma[4, "year"] <- as.character("2008-2012")

## Adding data from ACS B16007
eo5_17 <- map_dfr(
  years,
  ~ get_acs(
    geography = "us", 
    variables = "B16007_003", 
    year = .x, 
    survey = "acs5"),
  .id = "year")

total5_17 <- map_dfr(
  years,
  ~ get_acs(
    geography = "us", 
    variables = "B16007_002", 
    year = .x, 
    survey = "acs5"),
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

non_e_517$year <- as.character(non_e_517$year)
non_e_517[1, "year"] <- as.character("2020-2024")
non_e_517[2, "year"] <- as.character("2016-2020")
non_e_517[3, "year"] <- as.character("2012-2016")
non_e_517[4, "year"] <- as.character("2008-2012")

## Adding child pop

child_pop_2025 <- child_pop[child_pop$TimeFrame == 2025,]
agechart <- aggregate(Data ~ Single.Age, data = child_pop_2025, FUN = sum, na.rm = TRUE)

agechart <- agechart |>
  mutate(proxy_percent24 = Data * 0.2173485)
agechart <- agechart |>
  mutate(proxy_percent20 = Data * 0.2198973)
agechart <- agechart |>
  mutate(proxy_percent16 = Data * 0.2203787)
agechart <- agechart |>
  mutate(proxy_percent12 = Data * 0.2161038)

#doing the same thing for MA

eo5_17_ma <- map_dfr(
  years,
  ~ get_acs(
    geography = "state", 
    state = "MA", 
    variables = "B16007_003", 
    year = .x, 
    survey = "acs5"),
  .id = "year")

total5_17_ma <- map_dfr(
  years,
  ~ get_acs(
    geography = "state", 
    state = "MA",
    variables = "B16007_002", 
    year = .x, 
    survey = "acs5"),
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

non_e_517_ma$year <- as.character(non_e_517_ma$year)
non_e_517_ma[1, "year"] <- as.character("2020-2024")
non_e_517_ma[2, "year"] <- as.character("2016-2020")
non_e_517_ma[3, "year"] <- as.character("2012-2016")
non_e_517_ma[4, "year"] <- as.character("2008-2012")

ma_child_pop <- child_pop_2025 |>
  filter(Location == "MASSACHUSETTS")

ma_agechart <- aggregate(Data ~ Single.Age, data = ma_child_pop, FUN = sum, na.rm = TRUE)

ma_agechart <- ma_agechart |>
  mutate(proxy_percent24 = Data * 0.2436643)
ma_agechart <- ma_agechart |>
  mutate(proxy_percent20 = Data * 0.2334502)
ma_agechart <- ma_agechart |>
  mutate(proxy_percent16 = Data * 0.2203640)
ma_agechart <- ma_agechart |>
  mutate(proxy_percent12 = Data * 0.2100803)


#headline stats code
source("pir_source.R")

statewide_dll <- sum(ma_agechart$proxy_percent24)
statewide_children <- sum(non_e_517_ma[1,7])
statewide_percent <- statewide_dll/statewide_children

headstart_dll_states <- pir_a |>
  group_by(pir_year) |>
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

child_pop_21_24 <- child_pop[child_pop$TimeFrame %in% 2021:2024,]
ma_child_pop_21_24 <- child_pop_21_24 |>
  filter(Location == "MASSACHUSETTS")
ma_agechart_2124 <- aggregate(Data ~ Single.Age + TimeFrame, data = ma_child_pop_21_24, FUN = sum, na.rm = TRUE)

ma_agechart_2124 <- ma_agechart_2124 |>
  mutate(proxy_24 = Data * 0.2436643)
ma_agechart_2124 <- ma_agechart_2124 |>
  mutate(proxy_20 = Data * 0.2334502)
ma_agechart_2124 <- ma_agechart_2124 |>
  mutate(proxy_16 = Data * 0.2203640)
ma_agechart_2124 <- ma_agechart_2124 |>
  mutate(proxy_12 = Data * 0.2100803)

ma_agechart_2124_groups <- aggregate(
  cbind(Data, proxy_24) ~ TimeFrame,
  data = ma_agechart_2124,
  FUN = sum,
  na.rm = TRUE)

ma_dllenroll <- left_join(ma_agechart_2124_groups, headstart_dll_ma, 
                           by = c("TimeFrame" = "pir_year"))|>
  mutate(dll_prop = total_dll_enroll/proxy_24)

agechart_2124 <- aggregate(Data ~ Single.Age + TimeFrame, data = child_pop_21_24, 
                              FUN = sum, na.rm = TRUE)

agechart_2124 <- agechart_2124 |>
  mutate(proxy_24 = Data * 0.2173485)
agechart_2124 <- agechart_2124 |>
  mutate(proxy_20 = Data * 0.2198973)
agechart_2124 <- agechart_2124 |>
  mutate(proxy_16 = Data * 0.2203787)
agechart_2124 <- agechart_2124 |>
  mutate(proxy_12 = Data * 0.2161038)

agechart_2124_groups <- aggregate(
  cbind(Data, proxy_24) ~ TimeFrame,
  data = agechart_2124,
  FUN = sum,
  na.rm = TRUE)

usa_dllenroll <- left_join(agechart_2124_groups, headstart_dll_states, 
                           by = c("TimeFrame" = "pir_year"))|>
  mutate(dll_prop = total_dll_enroll/proxy_24)
