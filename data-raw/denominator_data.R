## code to prepare `denominator_data` dataset goes here


denominator_data <- readxl::read_xlsx("/Volumes/Robinson/Afenet-projects/afenet-nigeria/Dhis2WithSormas/data-raw/Denominator-2018-2022-LGA.xlsx")%>%
  pivot_longer(cols = `2018`:`2022`, names_to = "Year", values_to = "Denominator")

usethis::use_data(denominator_data, overwrite = TRUE)
