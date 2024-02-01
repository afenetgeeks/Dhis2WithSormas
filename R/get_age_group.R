
#' get_sormas_cases_by_age_group
#' @import dplyr stringr tidyr readr
#' @export

get_age_group <- function(sormas_cleaned){

  sormas_cases_by_age_group <-   distinct(sormas_cleaned)  |>
    rename_with(str_to_title) |>
    rename("LGA" = Lga ) |>
    select(-c(State_id, Lga_id, `Case Classification`)) |>
    pivot_wider(names_from = `Vaccination Status`,
                values_from = `Vaccination Status`) %>%
    mutate(across(c(Vaccinated, Unvaccinated, Unknown), ~ if_else(is.na(.x), 0, 1))) %>%
    mutate(Age_numeric = case_when(!str_detect(Age, "[[:alpha:]]")~  as.numeric(Age),
                                   str_detect(Age,  "Months") ~  as.numeric(str_extract(Age, pattern = "[[:digit:]]"))/12,
                                   str_detect(Age,  "Days") ~  as.numeric(str_extract(Age, pattern = "[[:digit:]]"))/365,
                                   TRUE~ 0)) %>%
    mutate(`Age group` = case_when( Age_numeric < 0.75 ~ "< 9",
                                    Age_numeric <= 4.916667 ~ "9 - 59",
                                    Age_numeric <= 15 ~ "60 - 180",
                                    TRUE ~ "> 180") ) %>%
    select(-Age_numeric )

  sormas_cases_by_age_group_f <- sormas_cases_by_age_group %>%
    group_by(`Age group`, Year, Months) %>%
    summarise(across(c(Vaccinated, Unvaccinated, Unknown), ~ sum(.x, na.rm = TRUE)),.groups = "drop") %>%
    mutate(State = "Federal Government",
           LGA = "State level data")


  sormas_cases_by_age_group_states <- sormas_cases_by_age_group %>%
    group_by(`Age group`, Year, Months, State) %>%
    summarise(across(c(Vaccinated, Unvaccinated, Unknown), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(LGA = "State level data")


  sormas_cases_by_age_group_lga <- sormas_cases_by_age_group %>%
    group_by(`Age group`, Year, Months, State, LGA) %>%
    summarise(across(c(Vaccinated, Unvaccinated, Unknown), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

  sormas_cases_by_age_group <- bind_rows(sormas_cases_by_age_group_f, sormas_cases_by_age_group_states, sormas_cases_by_age_group_lga )
  sormas_cases_by_age_group
}


