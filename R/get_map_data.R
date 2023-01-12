#' Connecting
#'
#' @description connecting to Dhis2

#'
#' @noRd
#' @import datimutils
#' @export

###

get_coverage_map <- function(annual_data, monthly_data){

  annual_data <- annual_data  %>%
    mutate(Months = "Ann")

  bind_rows(annual_data, monthly_data) %>%
  filter(State != "Federal Government") %>%
    rename("Coverage" = Values) %>%
    mutate("Coverage %" = case_when(
      Coverage < 50 ~ "0 - 49%",
      Coverage >= 50 & Coverage < 80 ~ "50 - 79%",
      Coverage >= 80 & Coverage <= 100 ~ "85 - 100%",
      Coverage > 100 ~ "> 100%",
      TRUE ~ "Others"
    )) %>%
    mutate(`Coverage %` = factor(`Coverage %`, levels = c("0 - 49%", "50 - 79%", "85 - 100%", "> 100%"))) %>%
    select(-c( Indicator))
}

### Sormas








