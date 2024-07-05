#' add measles
#' @import dplyr stringr
#' @export
#'
#'
#'

get_mcv_different_sources <- function(mcv1_annualized_all_time, mcv2_annualized_all_time){

  mcv_annualized_f <-  bind_rows( mcv1_annualized_all_time, mcv2_annualized_all_time)%>%
    filter(State  == "Federal Government") %>%
    select(-c( Months, LGA)) %>%
    mutate(Indicator = as.character(case_when(Indicator == "hvYqrmR6MPq" ~ "Dhis2 (MCV1)",
                                              Indicator == "Fbj3AkYjCie" ~ "Dhis2 (MCV2)"))) %>%
    pivot_wider(names_from = Indicator, values_from = Values,id_cols = Year)


  # ONLY Federal data
  # mcv_different_sources.csv - extracted from data used during GEEKS triangualtion training

  Dhis2WithSormas::mcv_different_sources %>%
    dplyr::full_join(mcv_annualized_f, by=c("Year" = "Year")) %>%
    arrange(Year) %>%
    mutate(`Dhis2 (MCV1)` = case_when(Year == "2013" ~ 15.2,
                                      Year == "2014" ~ 60.6,
                                      Year == "2015" ~ 61.7,
                                      Year == "2016" ~ 74.9,
                                      Year == "2017" ~ 82.6,
                                      Year == "2018" ~ 73.8,
                                      Year == "2019" ~ 75.7,
                                      Year == "2020" ~ 71.7,
                                      Year == "2021" ~ 67.1,
                                      Year == "2022" ~ 73.6,
                                      Year == "2023" ~ 75.3,
                                      TRUE~ `Dhis2 (MCV1)`),
           `Dhis2 (MCV2)` = case_when(
             Year == "2019" ~ 9.2,
             Year == "2020" ~ 18.6,
             Year == "2021" ~ 27,
             Year == "2022" ~ 34.8,
             Year == "2023" ~ 40.9,

             TRUE~ `Dhis2 (MCV2)`))

}


#' add Penta
#' @import dplyr stringr
#' @export
#'
#'
#'

get_penta_different_sources <- function(penta1_coverage_annualized_all_time, penta3_coverage_annualized_all_time){

  penta_annualized_f <- bind_rows( penta1_coverage_annualized_all_time, penta3_coverage_annualized_all_time)%>%
  filter(State  == "Federal Government") %>%
  select(-c( Months, LGA)) %>%
  pivot_wider(names_from = Indicator, values_from = Values,id_cols = Year) |>
  rename("DHIS2 (Penta1)" = `Penta 1 Coverage`,
         "DHIS2 (Penta3)" = `Penta 3 Coverage (Monthly)`)

  Dhis2WithSormas::penta_different_sources %>%
    dplyr::full_join(penta_annualized_f, by=c("Year" = "Year")) %>%
    arrange(Year) %>%
    mutate(`DHIS2 (Penta1)` = case_when(
                                      Year == "2015" ~ NA,
                                      Year == "2016" ~ 82.8,
                                      Year == "2017" ~ 91.2,
                                      Year == "2018" ~ 86.9,
                                      Year == "2019" ~ 88.0,
                                      Year == "2020" ~ 81.7,
                                      Year == "2021" ~ 77.0,
                                      Year == "2022" ~ 82.3,
                                      Year == "2023" ~ 89.0,
                                      TRUE~ `DHIS2 (Penta1)`),
           `DHIS2 (Penta3)` = case_when(
             Year == "2015" ~ 65.7,
             Year == "2016" ~ 75.4,
             Year == "2017" ~ 84.4,
             Year == "2018" ~ 79.9,
             Year == "2019" ~ 82.0,
             Year == "2020" ~ 76.9,
             Year == "2021" ~ 72.7,
             Year == "2022" ~ 77.1,
             Year == "2023" ~ 84.0,
             TRUE~ `DHIS2 (Penta3)`))

}



