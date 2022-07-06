## code to prepare `mcv_different_sources` dataset goes here

library(readr)
mcv_different_sources <- read_csv("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/mcv_different_sources.csv")


usethis::use_data(mcv_different_sources, overwrite = TRUE)
