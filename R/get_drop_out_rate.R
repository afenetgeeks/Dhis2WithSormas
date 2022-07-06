#' add measles
#' @import dplyr stringr
#' @export
#'

get_drop_out_rate <- function(first_dose, second_dose){

 first_dose <- first_dose %>%
    rename("first_dose" = Values) %>%
    select(- Indicator )

  second_dose <-  second_dose %>%
    rename("second_dose" = Values) %>%
    select(- Indicator )

 first_dose %>%
    left_join(second_dose,
              by=c("Months" = "Months", "Year" = "Year", "State" = "State", "LGA" = "LGA")) %>%
    group_by(Months, Year, State, LGA, first_dose, second_dose) %>%
    summarise("dropout_rate" =
                round(((sum(first_dose, na.rm = T) - sum(second_dose, na.rm = T))/ first_dose)*100), .groups = "drop")
}



