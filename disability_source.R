library(tidyverse)
library(readxl)

OS <- .Platform$OS.type

if (OS == "unix"){
  temp_path <- "data/" # MAC file path
} else if (OS == "windows"){
  temp_path <- "..\\Head-Start-Analysis\\data\\" # windows file path
} else {
  print("ERROR: OS could not be identified")
}

idea_files <- tribble(
  ~YEAR_END, ~partc, ~partb,
  2021, "2021-cchildcountandsettings-1.xlsx", "2021-bchildcountandedenvironment-2.xlsx",
  2022, "2122-cchildcountandsettings-1.xlsx", "2122-bchildcountandedenvironment-2.xlsx",
  2023, "2223-cchildcountandsettings-1.xlsx", "2223-bchildcountandedenvironment-2.xlsx",
  2024, "2324-cchildcountandsettings-1.xlsx", "2324-bchildcountandedenvironment-2.xlsx",
  2025, "2425-cchildcountandsettings-1.xlsx", "2425-bchildcountandedenvironment-2.xlsx")

num_dash <- function(x) suppressWarnings(as.numeric(na_if(as.character(x), "-")))

read_partc <- function(year_end, file) {
  read_excel(paste0(temp_path, file), skip = 8, .name_repair = make.names) |>
    transmute(STATENAME = State,
              YEAR_END  = year_end,
              IDEA_PARTC_0_2 = num_dash(`Number.served.birth.through.2.years`))
}

read_partb <- function(year_end, file) {
  raw <- read_excel(paste0(temp_path, file), skip = 8, .name_repair = make.names)
  disab_col <- names(raw)[str_detect(names(raw), "^All.disabilities")]
  raw |>
    transmute(STATENAME = State,
              YEAR_END  = year_end,
              IDEA_PARTB_3_5 = num_dash(.data[[disab_col]]))
}

idea_partc <- map2_df(idea_files$YEAR_END, idea_files$partc, read_partc)
idea_partb <- map2_df(idea_files$YEAR_END, idea_files$partb, read_partb)

idea_disability <- full_join(idea_partc, idea_partb,
                             by = c("STATENAME", "YEAR_END")) |>
  mutate(STATENAME = toupper(STATENAME),
         IDEA_DISABLED_0_5 = IDEA_PARTC_0_2 + IDEA_PARTB_3_5) |>
  filter(!is.na(STATENAME))

# Statewide child population under 5
child_pop <- read_excel(paste0(temp_path, "Child population by single age.xlsx"),
                        .name_repair = make.names) |>
  mutate(TimeFrame = as.numeric(TimeFrame),
         Data = as.numeric(Data),
         Location = toupper(Location)) |>
  filter(LocationType == "State" &
           Single.Age %in% c("<1", "1", "2", "3", "4", "5") &
           TimeFrame %in% c(2021:2025))

pop_under_5 <- child_pop |>
  group_by(Location, TimeFrame) |>
  summarize(POPULATION_UNDER_5 = sum(Data), .groups = "drop")

# Head Start data
hs_disability_by_state <-
  combined_pir |>
  group_by(pir_year, State) |>
  summarize(TOTAL_HS_ENROLL  = sum(A.10.g, na.rm = TRUE),
            HS_IEP           = sum(C.24, na.rm = TRUE),
            HS_IFSP          = sum(C.25, na.rm = TRUE),
            HS_DISABLED      = HS_IEP + HS_IFSP,
            HS_PROGRAMS      = n(),
            PERCENT_HS_DISABLED = HS_DISABLED / TOTAL_HS_ENROLL * 100,
            .groups = "drop")

# Create shared key (combined_pir uses 2-letter ST, IDEA uses full name
st_xwalk <- tibble(ST = state.abb, STATENAME = toupper(state.name)) |>
  add_row(ST = "DC", STATENAME = "DISTRICT OF COLUMBIA") |>
  add_row(ST = "PR", STATENAME = "PUERTO RICO")

# Merged frame for analysis
disability_under_5 <-
  idea_disability |>
  left_join(st_xwalk, by = "STATENAME") |>
  left_join(pop_under_5,
            by = c("STATENAME" = "Location", "YEAR_END" = "TimeFrame")) |>
  left_join(hs_disability_by_state,
            by = c("ST" = "State", "YEAR_END" = "pir_year")) |>
  mutate(STATEWIDE_PROP_DISABLED = IDEA_DISABLED_0_5 / POPULATION_UNDER_5 * 100,
         HS_SHARE_OF_STATE       = HS_DISABLED / IDEA_DISABLED_0_5 * 100)

# Child Population Statistics: https://datacenter.aecf.org/data/tables/100-child-population-by-single-age?loc=1&loct=1#detailed/1/any/false/1096/42,43,44,45/418
# Ages 3-5 IDEA Statistics: https://data.ed.gov/dataset/idea-section-618-data-products-static-tables-part-b-count-environ-table2
# Ages 0-2 IDEA Statistics: https://data.ed.gov/dataset/idea-section-618-data-products-static-tables-part-c-child-count-and-settings-table-1/resources?resource=f28a5f96-d3b5-4fb7-af87-413e9be64e47