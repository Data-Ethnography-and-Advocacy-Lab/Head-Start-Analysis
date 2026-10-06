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

ed_homeless <- read_csv(paste0(temp_path, "ed_express_homeless_21_24.csv"), name_repair = make.names)

child_pop <- read_excel(paste0(temp_path, "child_pop_state_age.xlsx"), .name_repair = make.names) |>
  mutate(TimeFrame = as.numeric(TimeFrame),
         Data = as.numeric(Data),
         Location = toupper(Location)) |>
  filter(LocationType == "State" & 
           Single.Age %in% c("<1", "1", "2", "3", "4", "5") &
           TimeFrame %in% c(2021:2025)) 

hs_homeless_by_state <-
  combined_pir |>
  group_by(pir_year, State) |>
  summarize(TOTAL_HS_ENROLL = sum(A.10.g, na.rm = TRUE), 
            TOTAL_HS_HOMELESS_ENROLL = sum(C.48, na.rm = TRUE),
            HS_HOMELESS_NA = sum(is.na(C.48)),
            HS_PROGRAMS = n(),
            PERCENT_HS_ENROLL_HOMELESS = TOTAL_HS_HOMELESS_ENROLL / TOTAL_HS_ENROLL * 100)

ed_homeless_grade_1 <- ed_homeless |>
  select(1:2, 8:9, 15) |>
  filter(Age.Grade == "Grade 1")

ed_homeless_young <- ed_homeless |>
  mutate(Age.Grade = tolower(Age.Grade)) |>
  unite(col = "Indicator", Age.Grade, Data.Group, sep = " - ") |>
  filter(Indicator %in% c("age 3 through 5 (not kindergarten) - 655",
                          "age birth through 5 (not kindergarten) - 818")) |>
  select(1:2, 7:9) |>
  mutate(YEAR_END = str_extract(School.Year, "(?<=-).*"),
         YEAR_END = as.numeric(YEAR_END))

ed_homeless_char <- ed_homeless |>
  select(1:2, 8:9, 14) |>
  filter(Characteristics %in% c("Doubled-up", "Hotels/motels", "Shelters transitional housing", "Unsheltered")) |>
  mutate(Value = as.numeric(Value)) |>
  group_by(School.Year, State) |>
  mutate(TOTAL_HOMELESS_ST_YR = sum(Value)) |>
  ungroup() |>
  mutate(PERCENT_CHAR_ST_YR = Value/TOTAL_HOMELESS_ST_YR) |>
  mutate(YEAR_END = str_extract(School.Year, "(?<=-).*"),
         YEAR_END = as.numeric(YEAR_END))


ed_totals_22 <- read_csv(paste0(temp_path, "ccd_sea_052_2122_l_1a_071722.csv"))
ed_totals_23 <- read_csv(paste0(temp_path, "ccd_sea_052_2223_l_1a_083023.csv"))
ed_totals_24 <- read_csv(paste0(temp_path, "ccd_sea_052_2324_l_1a_073124.csv"))
ed_totals_25 <- read_csv(paste0(temp_path, "ccd_sea_052_2425_l_1a_073025.csv"))
ed_totals_grade_1 <- rbind(ed_totals_22, 
                           ed_totals_23, 
                           ed_totals_24, 
                           ed_totals_25) |>
  filter(GRADE == "Grade 1" & TOTAL_INDICATOR == "Subtotal 4 - By Grade") |>
  left_join(ed_homeless_grade_1, by = c("SCHOOL_YEAR" = "School.Year", "STATENAME" = "State" )) |>
  mutate(Value = as.numeric(Value),
         PERCENT_HOMELESS_GRADE_1 = Value/STUDENT_COUNT) |>
  mutate(YEAR_END = str_extract(SCHOOL_YEAR, "(?<=-).*"),
         YEAR_END = as.numeric(YEAR_END)) |>
  left_join(child_pop, by = c("YEAR_END" = "TimeFrame", "STATENAME" = "Location")) |>
  rename(TOTAL_HOMELESS_GRADE_1 = Value,
         POPULATION_BY_AGE = Data) |>
  mutate(TOTAL_HOMELESS_BY_AGE = round(PERCENT_HOMELESS_GRADE_1 * POPULATION_BY_AGE, 0)) |>
  select(-RACE_ETHNICITY, -SEX, -c(TOTAL_INDICATOR:Data.Description), -Age.Grade, -LocationType, -DataFormat)

ed_totals_under_5 <- ed_totals_grade_1 |>
  group_by(STATENAME, YEAR_END, ST) |>
  summarize(
    POPULATION_UNDER_5 = sum(POPULATION_BY_AGE),
    HOMELESS_UNDER_5_GRADE_1 = sum(TOTAL_HOMELESS_BY_AGE)) |>
  left_join(ed_homeless_young, by = c("STATENAME" = "State", "YEAR_END" = "YEAR_END")) |>
  mutate(PIVOT_COL_NAME = case_when(Indicator == "age 3 through 5 (not kindergarten) - 655" ~ "HOMELESS_3_5_655", 
                                    Indicator == "age birth through 5 (not kindergarten) - 818" ~ "HOMELESS_0_5_818")) |>
  select(-Indicator, -Data.Description) |>
  pivot_wider(names_from = PIVOT_COL_NAME,
              values_from = Value) |>
  left_join(hs_homeless_by_state, by = c("ST" = "State", "YEAR_END" = "pir_year"))

ed_totals_under_5_char <- ed_totals_under_5 |>
  left_join(ed_homeless_char |> select(2, 5, 7, 8), by = c("STATENAME" = "State", "YEAR_END" = "YEAR_END")) |>
  mutate(TOTAL_CHAR_UNDER_5 = HOMELESS_UNDER_5_GRADE_1 * PERCENT_CHAR_ST_YR)

rm(ed_totals_22, ed_totals_23, ed_totals_24, ed_totals_25, child_pop)

#Child Population Statistics: https://datacenter.aecf.org/data/tables/100-child-population-by-single-age?loc=1&loct=1#detailed/1/any/false/1096/42,43,44,45/418

#Query for Ed Homeless data: https://eddataexpress.ed.gov/download/data-builder/data-download-tool?f%5B0%5D=level%3AState%20Education%20Agency&f%5B1%5D=school_year%3A2021-2022&f%5B2%5D=school_year%3A2022-2023&f%5B3%5D=school_year%3A2023-2024&f%5B4%5D=state_name%3AALABAMA&f%5B5%5D=state_name%3AALASKA&f%5B6%5D=state_name%3AARIZONA&f%5B7%5D=state_name%3AARKANSAS&f%5B8%5D=state_name%3ABUREAU%20OF%20INDIAN%20EDUCATION&f%5B9%5D=state_name%3ACALIFORNIA&f%5B10%5D=state_name%3ACOLORADO&f%5B11%5D=state_name%3ACONNECTICUT&f%5B12%5D=state_name%3ADELAWARE&f%5B13%5D=state_name%3ADISTRICT%20OF%20COLUMBIA&f%5B14%5D=state_name%3AFLORIDA&f%5B15%5D=state_name%3AGEORGIA&f%5B16%5D=state_name%3AHAWAII&f%5B17%5D=state_name%3AIDAHO&f%5B18%5D=state_name%3AILLINOIS&f%5B19%5D=state_name%3AINDIANA&f%5B20%5D=state_name%3AIOWA&f%5B21%5D=state_name%3AKANSAS&f%5B22%5D=state_name%3AKENTUCKY&f%5B23%5D=state_name%3ALOUISIANA&f%5B24%5D=state_name%3AMAINE&f%5B25%5D=state_name%3AMARYLAND&f%5B26%5D=state_name%3AMASSACHUSETTS&f%5B27%5D=state_name%3AMICHIGAN&f%5B28%5D=state_name%3AMINNESOTA&f%5B29%5D=state_name%3AMISSISSIPPI&f%5B30%5D=state_name%3AMISSOURI&f%5B31%5D=state_name%3AMONTANA&f%5B32%5D=state_name%3ANEBRASKA&f%5B33%5D=state_name%3ANEVADA&f%5B34%5D=state_name%3ANEW%20HAMPSHIRE&f%5B35%5D=state_name%3ANEW%20JERSEY&f%5B36%5D=state_name%3ANEW%20MEXICO&f%5B37%5D=state_name%3ANEW%20YORK&f%5B38%5D=state_name%3ANORTH%20CAROLINA&f%5B39%5D=state_name%3ANORTH%20DAKOTA&f%5B40%5D=state_name%3AOHIO&f%5B41%5D=state_name%3AOKLAHOMA&f%5B42%5D=state_name%3AOREGON&f%5B43%5D=state_name%3APENNSYLVANIA&f%5B44%5D=state_name%3APUERTO%20RICO&f%5B45%5D=state_name%3ARHODE%20ISLAND&f%5B46%5D=state_name%3ASOUTH%20CAROLINA&f%5B47%5D=state_name%3ASOUTH%20DAKOTA&f%5B48%5D=state_name%3ATENNESSEE&f%5B49%5D=state_name%3ATEXAS&f%5B50%5D=state_name%3AUTAH&f%5B51%5D=state_name%3AVERMONT&f%5B52%5D=state_name%3AVIRGINIA&f%5B53%5D=state_name%3AWASHINGTON&f%5B54%5D=state_name%3AWEST%20VIRGINIA&f%5B55%5D=state_name%3AWISCONSIN&f%5B56%5D=state_name%3AWYOMING