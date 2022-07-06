
#' Calculate Alternative denominator
#' @import stringr
#' @param string a dataframe from sormas
#' @inheritParams stringr::str_split
#'
#' @return a dataframe.
#'
#' @examples
#' cleaned_measles_data <- "sormas_cleaner(file = "path")"
#'
calc_alt_denominator_monthly <- function(ind_value, denominator){

  alt_den_val = (ind_value/(denominator/12))*100

  alt_den_val
}

#' Get alternative denominator for measles only
#' @import stringr
#' @param string a dataframe from sormas
#' @inheritParams stringr::str_split
#'
#' @return a dataframe.
#' @export
#'
#' @examples
#' cleaned_measles_data <- "sormas_cleaner(file = "path")"

get_measles_alt_denominator <- function(sormas_cleaned,mcv1, mcv2, measles1_given, measles2_given, denominator_data){

  cases_cleaned_federal <- sormas_cleaned%>%
    group_by(Months,Year) %>%
    summarise(`Measles Cases (CaseBased)` = n())


  cases_cleaned_states_state_level <- sormas_cleaned%>%
    group_by(Year, Months, State) %>%
    summarise(Cases = n()) %>%
    rename(`Measles Cases (CaseBased)` = Cases) %>%
    mutate(LGA = "State level data")

  cases_cleaned_lga <- sormas_cleaned%>%
    group_by(Year, Months, State, LGA) %>%
    summarise(Cases = n()) %>%
    rename(`Measles Cases (CaseBased)` = Cases)


  cases_cleaned_states <- bind_rows(cases_cleaned_states_state_level,cases_cleaned_lga)


  measles1_coverage_monthly_df <- mcv1 %>%
    rename("MCV 1" = Values) %>%
    select(!c( Indicator))


  measles2_coverage_monthly_df <- mcv2 %>%
    rename("MCV 2" = Values) %>%
    select(!c( Indicator))

  mcv_monthly_df <-   measles1_coverage_monthly_df %>%
    left_join(measles2_coverage_monthly_df,
              by =c("Months" = "Months", "Year" = "Year", "State" = "State", "LGA" = "LGA"))

  ####################

  mcv_monthly_f <- mcv_monthly_df %>%
    filter(State == "Federal Government")


  mcv_monthly_s <- mcv_monthly_df  %>%
    filter(State != "Federal Government")

  #######
  combined_by_federal <- mcv_monthly_f %>%
    left_join(cases_cleaned_federal , by=c("Months" = "Months", "Year" = "Year"))

  ##################################################################################################################

  # States

  combined_by_states <- mcv_monthly_s %>%
    left_join(cases_cleaned_states ,
              by=c("Months" = "Months", "Year" = "Year", "State" = "State", "LGA" = "LGA"))

  combined_dhis2_sormas <- bind_rows(combined_by_federal, combined_by_states)


  ####

  measles1_given_edited <- measles1_given %>%
    rename("Measles 1 given" = Values) %>%
    select(-Indicator)


  measles1_given_f <- measles1_given_edited %>%
    filter(State == "Federal Government")

  measles1_given_s <-  measles1_given_edited %>%
    filter(State != "Federal Government")

  ####

  measles2_given_edited <- measles2_given %>%
    rename("Measles 2 given" = Values) %>%
    select(-Indicator)


  measles2_given_f <- measles2_given_edited %>%
    filter(State == "Federal Government")


  measles2_given_s <-  measles2_given_edited %>%
    filter(State != "Federal Government")

  ##

  state_alt_den <- denominator_data %>%
    group_by(State, Year) %>%
    summarise(Denominator = sum(Denominator),LGA = "State Level data")


  alt_den_long_s <- bind_rows(state_alt_den, denominator_data) %>%
    mutate(State = str_replace(State, "Fct, Abuja", "Federal Capital Territory"),
           State = str_replace(State, "Akwa Ibom", "Akwa-Ibom"),
           Year = as.numeric(Year))

  combined_measles1_given_altden_states <- measles1_given_s %>%
    left_join(measles2_given_s, by = c("State" = "State",
                                       "Year" = "Year",
                                       "LGA" = "LGA",
                                       "Months" = "Months")) %>%
    left_join(alt_den_long_s, by = c("State" = "State",
                                     "Year" = "Year",
                                     "LGA" = "LGA")) %>%
    mutate("MCV 1 Alt Denominator" = round(calc_alt_denominator_monthly(`Measles 1 given`, Denominator))) %>%
    mutate("MCV 2 Alt Denominator" = round(calc_alt_denominator_monthly(`Measles 2 given`, Denominator))) %>%
    select(!c(Denominator))




  # sum up all values for states' alt denominator for each month per year
  alt_den_long_f <- alt_den_long_s %>%
    group_by(Year) %>%
    summarise(Denominator = sum(Denominator))

  combined_measles1_given_altden_federal <- measles1_given_f %>%
    left_join(measles2_given_f, by = c("State" = "State",
                                       "Year" = "Year",
                                       "LGA" = "LGA",
                                       "Months" = "Months") ) %>%
    left_join(alt_den_long_f, by = c("Year" = "Year")) %>%
    mutate("MCV 1 Alt Denominator" = round(calc_alt_denominator_monthly(`Measles 1 given`, Denominator))) %>%
    mutate("MCV 2 Alt Denominator" = round(calc_alt_denominator_monthly(`Measles 2 given`, Denominator))) %>%
    select(!c(Denominator))

  alt_den <- bind_rows(combined_measles1_given_altden_federal, combined_measles1_given_altden_states)


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
#' cleaned_measles_data <- "sormas_cleaner(file = "path")"

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
