
#' get_sormas_cases_by_age_group
#' @import dplyr stringr tidyr readr
#' @export

get_age_group <- function(sormas_cleaned){

  sormas_cases_by_age_group <-   distinct(sormas_cleaned)  %>%
    select(-c(State_id, LGA_id)) |>
    mutate(state = case_when(state == "Fct" ~ "Federal Capital Territory", TRUE ~as.character(state)),
           months = stringr::str_to_sentence(stringr::str_trunc(months,3, ellipsis = ""))) |>
    pivot_wider(names_from = vaccination_status, values_from = confirmed_cases, values_fn = sum) |>
    setNames(c("State", "LGA", "Age group", "Months", "Year",  "Uncategorised","Unvaccinated", "Vaccinated","Unknown"))


  sormas_cases_by_age_group_f <- sormas_cases_by_age_group %>%
    group_by(`Age group`, Year, Months) %>%
    summarise(across(c(Uncategorised, Unvaccinated, Vaccinated, Unknown), ~ sum(.x, na.rm = TRUE)),.groups = "drop") %>%
    mutate(State = "Federal Government",
           LGA = "State level data")


  sormas_cases_by_age_group_states <- sormas_cases_by_age_group %>%
    group_by(`Age group`, Year, Months, State) %>%
    summarise(across(c(Uncategorised, Unvaccinated, Vaccinated, Unknown), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(LGA = "State level data")


  sormas_cases_by_age_group_lga <- sormas_cases_by_age_group %>%
    group_by(`Age group`, Year, Months, State, LGA) %>%
    summarise(across(c(Uncategorised, Unvaccinated, Vaccinated, Unknown), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

  sormas_cases_by_age_group <- bind_rows(sormas_cases_by_age_group_f, sormas_cases_by_age_group_states, sormas_cases_by_age_group_lga )
  sormas_cases_by_age_group

}


