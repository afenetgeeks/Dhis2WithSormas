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
                                      TRUE~ `Dhis2 (MCV1)`),
           `Dhis2 (MCV2)` = case_when(
             Year == "2019" ~ 9.2,
             Year == "2020" ~ 18.6,
             Year == "2021" ~ 27,
             TRUE~ `Dhis2 (MCV2)`))




}






