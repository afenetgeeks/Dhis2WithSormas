#' add measles
#' @import dplyr stringr
#' @export
#'

get_drop_out_rate <- function(first_dose, later_dose){

 first_dose <- first_dose %>%
    rename("first_dose" = Values) %>%
    select(- Indicator )

  later_dose <-  later_dose %>%
    rename("later_dose" = Values) %>%
    select(- Indicator )

 first_dose %>%
    left_join(later_dose,
              by=c("Months" = "Months", "Year" = "Year", "State" = "State", "LGA" = "LGA")) %>%
    group_by(Months, Year, State, LGA, first_dose, later_dose) %>%
    summarise("dropout_rate" =
                round(((sum(first_dose, na.rm = T) - sum(later_dose, na.rm = T))/ first_dose)*100), .groups = "drop")
}



