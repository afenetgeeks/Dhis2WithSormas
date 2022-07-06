#' add measles
#' @import dplyr stringr
#' @export
#'
#'
#'

get_discrepancy <- function(main_vaccine_given, other_vaccine_given){
  main_vaccine_given <- main_vaccine_given%>%
    rename("main_vaccine_given" = Values) %>%
    select(-Indicator)

  other_vaccine_given <-other_vaccine_given%>%
    rename("other_vaccine_given" = Values)%>%
    select(-Indicator)

  main_vaccine_given%>%
    left_join(other_vaccine_given, by = c("Year" = "Year", "State" = "State", "Months" = "Months", "LGA" = "LGA") ) %>%
    group_by(Year, State, Months, LGA, `main_vaccine_given`, other_vaccine_given) %>%
    summarise("discrepancy" = round(((sum(`main_vaccine_given`,na.rm = T) - sum(other_vaccine_given,na.rm = T))/other_vaccine_given)*100), .groups = "drop")
}




