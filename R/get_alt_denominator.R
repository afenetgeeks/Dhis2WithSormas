
#' Calculate Alternative denominator
#' @import stringr
#' @param string a dataframe from sormas
#' @inheritParams stringr::str_split
#'
#' @return a dataframe.
#'
#' @examples
#' cleaned_2_dose_data <- "sormas_cleaner(file = "path")"
#'
calc_alt_denominator_monthly <- function(ind_value, denominator){

  alt_den_val = (ind_value/(denominator/12))*100

  alt_den_val
}

#' Get alternative denominator for 2 dose antigen e.g Measles, Penta
#' @import stringr
#' @param string a dataframe from sormas
#' @param first_dose the earlier dose coverage e.g Measles 1 coverage
#' @param later_dose the later dose coverage e.g Measles 2 coverage
#' @param first_dose_given the earlier dose given e.g Measles 1 given
#' @param later_dose_given the later dose given e.g Measles 2 given
#' @param denominator_data alternative denominator data
#' @inheritParams stringr::str_split
#'
#' @return a dataframe.
#' @export
#'
#' @examples
#' cleaned_2_dose_data <- "sormas_cleaner(file = "path")"

get_2_dose_alt_denominator <- function(sormas_cleaned,first_dose, later_dose,first_dose_given, later_dose_given, denominator_data){

  # cases_cleaned_federal <- sormas_cleaned%>%
  #   group_by(Months,Year) %>%
  #   summarise(`Cases (CaseBased)` = n())

  cases_cleaned_federal <- sormas_cleaned|>
    group_by(Months,Year) %>%
    summarise(`Cases (CaseBased)` = n(), .groups = "drop")


  cases_cleaned_states_state_level <-  sormas_cleaned  %>%
    group_by(Year, Months, State) %>%
    summarise(`Cases (CaseBased)` = n(), .groups = "drop") |>
    mutate(LGA = "State level data")

  cases_cleaned_lga <- sormas_cleaned%>%
    group_by(Year, Months, State, LGA) %>%
    summarise(`Cases (CaseBased)` = n(),.groups = "drop")


  cases_cleaned_states <- bind_rows(cases_cleaned_states_state_level,cases_cleaned_lga)


  first_dose_coverage_monthly_df <- first_dose %>%
    rename("first dose coverage" = Values) %>%
    select(!c( Indicator))


  second_dose_coverage_monthly_df <- later_dose %>%
    rename("later dose coverage" = Values) %>%
    select(!c( Indicator))

  coverage_monthly_df <-   first_dose_coverage_monthly_df %>%
    left_join(second_dose_coverage_monthly_df,
              by =c("Months" = "Months", "Year" = "Year", "State" = "State", "LGA" = "LGA"))

  ####################

  coverage_monthly_f <- coverage_monthly_df %>%
    filter(State == "Federal Government")


  coverage_monthly_s <- coverage_monthly_df  %>%
    filter(State != "Federal Government")

  #######
  combined_by_federal <- coverage_monthly_f %>%
    left_join(cases_cleaned_federal , by=c("Months" = "Months", "Year" = "Year"))

  ##################################################################################################################

  # States

  combined_by_states <- coverage_monthly_s %>%
    left_join(cases_cleaned_states ,
              by=c("Months" = "Months", "Year" = "Year", "State" = "State", "LGA" = "LGA"))

  combined_dhis2_sormas <- bind_rows(combined_by_federal, combined_by_states)


  ####

 first_dose_given_edited <- first_dose_given %>%
    rename("first dose given"= Values) %>%
    select(-Indicator)


 first_dose_given_f <-first_dose_given_edited %>%
    filter(State == "Federal Government")

 first_dose_given_s <- first_dose_given_edited %>%
    filter(State != "Federal Government")

  ####

  later_dose_given_edited <- later_dose_given %>%
    rename("later dose given" = Values) %>%
    select(-Indicator)


  later_dose_given_f <- later_dose_given_edited %>%
    filter(State == "Federal Government")


  later_dose_given_s <-  later_dose_given_edited %>%
    filter(State != "Federal Government")

  ##

  state_alt_den <- denominator_data %>%
    group_by(State, Year) %>%
    summarise(Denominator = sum(Denominator),LGA = "State level data")


  alt_den_long_s <- bind_rows(state_alt_den, denominator_data) %>%
    mutate(State = str_replace(State, "Fct, Abuja", "Federal Capital Territory"),
           State = str_replace(State, "Akwa Ibom", "Akwa-Ibom"),
           Year = as.numeric(Year))

  combined_first_dose_given_altden_states <- first_dose_given_s %>%
    left_join(later_dose_given_s, by = c("State" = "State",
                                       "Year" = "Year",
                                       "LGA" = "LGA",
                                       "Months" = "Months")) %>%
    left_join(alt_den_long_s, by = c("State" = "State",
                                     "Year" = "Year",
                                     "LGA" = "LGA")) %>%

    mutate("first dose coverage alt denominator" = round(calc_alt_denominator_monthly(`first dose given`, Denominator))) %>%
    mutate("later dose coverage alt denominator" = round(calc_alt_denominator_monthly(`later dose given`, Denominator))) %>%
    select(!c(Denominator))




  # sum up all values for states' alt denominator for each month per year
  alt_den_long_f <- alt_den_long_s %>%
    group_by(Year) %>%
    summarise(Denominator = sum(Denominator))

  combined_first_dose_given_f_altden_federal <- first_dose_given_f %>%
    left_join(later_dose_given_f, by = c("State" = "State",
                                       "Year" = "Year",
                                       "LGA" = "LGA",
                                       "Months" = "Months") ) %>%
    left_join(alt_den_long_f, by = c("Year" = "Year")) %>%
    mutate("first dose coverage alt denominator" = round(calc_alt_denominator_monthly(`first dose given`, Denominator))) %>%
    mutate("later dose coverage alt denominator" = round(calc_alt_denominator_monthly(`later dose given`, Denominator))) %>%
    select(!c(Denominator))

  alt_den <- bind_rows(combined_first_dose_given_f_altden_federal, combined_first_dose_given_altden_states)


   alt_den %>%
    left_join(combined_dhis2_sormas,
              by = c("Year" = "Year",
                     "Months" = "Months",
                     "State" = "State",
                     "LGA" = "LGA"))

}






##############

###### Other diseases.

#' Get alternative denominator for a disease of your choice
#' @import stringr
#' @param string a dataframe from sormas
#' @inheritParams stringr::str_split
#'
#' @return a dataframe.
#' @export
#'
#' @examples
#' cleaned_2_dose_data <- "sormas_cleaner(file = "path")"

get_alt_denominator <- function(coverage, sormas_cleaned,  doses_given, denominator_data){

  coverage_monthly_edited <- coverage  %>%
    rename("Disease Coverage" = Values) %>%
    select(-Indicator)

  coverage_monthly_f <- coverage_monthly_edited %>%
    filter(State == "Federal Government")

  coverage_monthly_s <- coverage_monthly_edited   %>%
    filter(State != "Federal Government")

  ### cases
  cases_sormas_federal <- sormas_cleaned %>%
    group_by(Year, Months) %>%
    summarise("Disease Cases" = n(), State = "Federal Government", LGA = "State level data")


  cases_sormas_state <- sormas_cleaned %>%
    group_by(State, Year, Months) %>%
    summarise("Disease Cases" = n(), LGA = "State level data")


  cases_sormas_LGA <- sormas_cleaned %>%
    group_by(State, LGA, Year, Months) %>%
    summarise("Disease Cases" = n())

  combined_by_federal <- coverage_monthly_f %>%
    left_join(cases_sormas_federal  , by=c("Months" = "Months", "Year" = "Year", "LGA" = "LGA", "State" = "State"))

##################################################################################################################
  # States
  combined_by_states <- coverage_monthly_s %>%
    left_join( bind_rows(cases_sormas_state, cases_sormas_LGA ),
               by=c("Months" = "Months", "Year" = "Year", "State" = "State", "LGA" = "LGA"))

  combined_dhis2_sormas <- bind_rows(combined_by_federal, combined_by_states)

  ##############################################
  doses_given_edited <- doses_given %>%
    rename("Vaccine doses given" = Values) %>%
    select(-Indicator)

  doses_given_f <- doses_given_edited %>%
    filter(State == "Federal Government")

  doses_given_s <-  doses_given_edited %>%
    filter(State != "Federal Government")

  ################################## alt denominator

  state_alt_den <- denominator_data %>%
    group_by(State, Year) %>%
    summarise(Denominator = sum(Denominator),LGA = "State level data")

  #############################

  alt_den_long_s <- bind_rows(state_alt_den, denominator_data) %>%
    mutate(State = str_replace(State, "Fct, Abuja", "Federal Capital Territory"),
           State = str_replace(State, "Akwa Ibom", "Akwa-Ibom"),
           Year = as.numeric(Year))

  combined_given_altden_states <- doses_given_s %>%
    left_join(alt_den_long_s, by = c("State" = "State",
                                     "Year" = "Year",
                                     "LGA" = "LGA")) %>%
    mutate("Disease Alt Denominator" = round(calc_alt_denominator_monthly(`Vaccine doses given`, Denominator))) %>%
    select(!c(Denominator))

  # sum up all values for states' alt denominator for each month per year
  alt_den_long_f <- alt_den_long_s %>%
    group_by(Year) %>%
    summarise(Denominator = sum(Denominator))

  combined_given_altden_federal <- doses_given_f %>%
    left_join(alt_den_long_f, by = c("Year" = "Year")) %>%
    mutate("Disease Alt Denominator" = round(calc_alt_denominator_monthly(`Vaccine doses given`, Denominator))) %>%

    select(!c(Denominator))

  ###########################################

  alt_den <- bind_rows(combined_given_altden_federal, combined_given_altden_states)

    alt_den %>%
    left_join(combined_dhis2_sormas,
              by = c("Year" = "Year",
                     "Months" = "Months",
                     "State" = "State",
                     "LGA" = "LGA"))
}
