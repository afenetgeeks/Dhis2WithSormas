
#' Get the period type year or year_month
#'
#' @param tag months vector
#'
#' @return a tag
#' @noRd
#'
get_period_type <- function(months){

  if(length(months)==0){

    period_type <- "year"

  } else{

    period_type <- "year_month"
  }
  period_type
}

#' Get the period type year or year_month
#'
#' @param tag months vector
#'
#' @return a tag
#' @noRd
#' @export

get_periods_str <- function(year, months){

  all_periods_vector <- c()

  period_type <- get_period_type(months)

  if (period_type == "year_month") {
    for (y in 1:length(year)) {
      year_month_str <- paste(paste0(year[y],months,collapse=";"),sep=";")
      all_periods_vector <- append(all_periods_vector,year_month_str)
    }
  } else {
    for (y in 1:length(year)) {
      year_str <- paste(paste0(year[y],collapse=";"),sep=";")
      all_periods_vector <- append(all_periods_vector,year_str)
    }
  }
  period_string <- paste0(all_periods_vector[1:length(all_periods_vector)], collapse = ";")

  period_string
}





get_ou_str <- function(ou_ids){


  if(length(ou_ids) == 1){
    ou_string = ou_ids
  } else{
    ou_string <- paste0(ou_ids, collapse = ";")

  }
  ou_string
}

###




#' Get the period type year or year_month
#'
#' @param tag months vector
#'
#' @return a tag
#' @noRd
#' @importFrom tibble tibble
#' @import stringr XML httr dhis2r
#'


getAnalytics_cleaned <- function(indicator_id,
                                 months = c("01","02","03","04","05","06","07","08", "09","10","11","12"),
                                 year,
                                 organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"),
                                 username= dw$Dhis2_username,
                                 password= dw$Dhis2_password
                                 ){

  period <-  get_periods_str(year = year, months = months)

  p_type <- get_period_type(months = months)

  # ou_string <- get_ou_str(ou_ids = organisation_uint)

  dhis2_connection <- dhis2r::Dhis2r$new(base_url = "https://dhis2nigeria.org.ng/dhis/",
                                      username = username,
                                      password = password,
                                      api_version_position = "after")

  dhis2_data <- dhis2_connection$get_analytics(analytic = indicator_id,
                                 org_unit = organisation_uint,
                                 period = period) |>
    set_names(c("Data", "Organisation unit", "Period", "Value"))

#########################

  dhis2_data_ou <- dhis2_data %>%
   # mutate(Data = indicator_name) %>%
    left_join(organisation_uints_cleaned, by = c("Organisation unit" = "id")) %>%
    select(-`Organisation unit`)


  dhis2_data_state_level <- dhis2_data_ou %>%
    filter(level %in% c(1, 2) ) %>%
    rename("State" =name) %>%
    mutate(LGA = "State level data") %>%
    select(-c(`parent.id`, level))

  dhis2_data_lga_level <- dhis2_data_ou %>%
    filter(level == 3) %>%
    left_join(states %>% select(id, name),
              by = c("parent.id" = "id")) %>%
    rename(LGA = `name.x`,
           State = `name.y`) %>%
    select(-c(`parent.id`, level))

  dhis2_ou_cleaned <- bind_rows(dhis2_data_state_level, dhis2_data_lga_level)


  if(p_type == "year"){

    df1 <- dhis2_ou_cleaned %>%
      set_names(c("Indicator", "Year","Values","State","LGA") )%>%
      mutate(Year = as.numeric(Year),
             Months = NA,
             Values =  as.numeric(Values))

  }else{

    df1 <- dhis2_ou_cleaned  %>%
      set_names(c("Indicator", "Month_Year","Values","State","LGA") )%>%
      mutate(Months = lubridate::parse_date_time(Month_Year, "%Y%m") %>%
                          lubridate::month(label = T, abbr = T),
             Year = lubridate::parse_date_time(Month_Year, "%Y%m") %>%
               lubridate::year(),
             Values = as.numeric(Values)) %>%
      select(-Month_Year)

  }

  return(df1)

}
