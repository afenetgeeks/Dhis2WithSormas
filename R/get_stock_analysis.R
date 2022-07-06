#' add measles
#' @import dplyr stringr
#' @export
#'
#'

add_measles_1_2_given <-  function(measles1_given, measles2_given){

  ########
  measles1_given_df <- measles1_given %>%
    rename("1 given" = Values) %>%
    select(- Indicator)

  measles2_given_df <- measles2_given %>%
    rename("2 given" = Values) %>%
    select(- Indicator)

  measles1_given_df %>%
    left_join(measles2_given_df,
            by = c("Months" = "Months", "Year" = "Year", "State" = "State", "LGA" = "LGA")) %>%
    group_by(Months, Year,State,LGA) %>%
    summarise(`Values` = sum(`1 given` , `2 given`, na.rm = T), Indicator = "Measles 1 & 2 given", .groups = "drop")

}


#' getting other diseases with one dose
#' @export
#' add measles

get_stock_analysis <- function(received,opening_balance,opened_used, doses_given){

  OB_plus_Received_df <- received %>%
    left_join( opening_balance ,
               by = c("Months" = "Months",
                      "Year" = "Year",
                      "LGA" = "LGA",
                      "State" = "State")) %>%
    select(-c("Indicator.x", "Indicator.y")) %>%
    rename("Received" = Values.x ,
           "Opening Balance" = Values.y) %>%
    mutate("Doses Available (Opening Balance+Received)" = Received+`Opening Balance`) %>%
    group_by(Months, Year,State, LGA) %>%
    summarise(`Doses Available (Opening Balance+Received)` = sum(`Doses Available (Opening Balance+Received)`, na.rm = TRUE), .groups = "drop") %>%
    ungroup()
  #####

  opened_used_df <- opened_used %>%
    rename("Vaccine - Doses Opened (used)" = Values) %>%
    select(- Indicator)

  ###

  doses_given_df <- doses_given %>%
    rename("doses_given" = Values) %>%
    select(- Indicator)


  #############################################################################################

  combined <- OB_plus_Received_df %>%
    left_join(opened_used_df,
              by = c("Months" = "Months", "Year" = "Year", "State" = "State", "LGA" = "LGA")) %>%
    left_join(doses_given_df,
              by = c("Months" = "Months", "Year" = "Year", "State" = "State", "LGA" = "LGA"))

  combined %>% group_by(Months, Year,State,LGA) %>%
    summarise(`doses_given`,

              `Doses Wastage Rate` = ((`Vaccine - Doses Opened (used)` - `doses_given`)/
                                        `Vaccine - Doses Opened (used)`)*100,

              `Doses Available (Opening Balance+Received)`, `Vaccine - Doses Opened (used)`,.groups = "drop" )

}









