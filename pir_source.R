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

pir_import_a <- function(year) {
  
  filename <- paste0(temp_path, "pir_export_", year, ".xlsx")
  var
  pir_data <- read_excel(filename, sheet = 1, skip = 1)
  pir_data <- pir_data |>
    mutate(pir_year = year) |>
    select(pir_year, Region:City, A.10.g, A.13.d, A.27) |>
    filter(Region != "Totals")
}

pir_import_b <- function(year) {
  
  filename <- paste0(temp_path, "pir_export_", year, ".xlsx")
  var
  pir_data <- read_excel(filename, sheet = 2, skip = 1)
  pir_data <- pir_data |>
    mutate(pir_year = year) |>
    select(pir_year, Region:City, B.14) |>
    filter(Region != "Totals")
}

pir_import_c <- function(year) {
  filename <- paste0(temp_path, "pir_export_", year, ".xlsx")
  pir_data <- read_excel(filename, sheet = 3, skip = 1)
  pir_data <- pir_data |>
    mutate(pir_year = year) |>
    select(pir_year, Region:City, C.47, C.48, C.22, C.22.a, C.22.a.1, C.22.b,
           C.23.a, C.23.b, C.23.c, C.23.d,
           C.24, C.24.a.2, C.24.b,
           C.25, C.25.b) |>
    filter(Region != "Totals")
}

year <- c(2021:2025)


pir_a <- map_df(year, pir_import_a)
pir_b <- map_df(year, pir_import_b)
pir_c <- map_df(2023:2025, pir_import_c)

pir_list <- list(pir_a, pir_b, pir_c)
combined_pir <- pir_list |>
  reduce(full_join, by = colnames(pir_a)[1:9])