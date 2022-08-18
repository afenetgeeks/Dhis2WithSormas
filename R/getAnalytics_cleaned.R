
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

#' Get the period type year or year_month
#'
#' @param tag months vector
#'
#' @return a tag
#' @noRd
#' @importFrom datimutils getAnalytics
#' @importFrom tibble tibble
#' @import stringr XML httr
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


  # indicator_name <-  datimutils::getAnalytics(dx = indicator_id,
  #                                          ou = "LEVEL-1",
  #                                          pe = period,
  #                                          return_names = T,
  #                                          retry = 3) %>% pull(Data) %>% unique()


  # dhis2_data <-  datimutils::getAnalytics(dx = indicator_id,
  #                                          ou = organisation_uint,
  #                                          pe = period,
  #                                          return_names = F,
  #                                         retry = 6) %>%tibble()

  ##################################
  urlAnalytics <- "https://dhis2nigeria.org.ng/dhis/api/29/analytics.xml?"
  urlIndicatorID <- paste0("dimension=dx:", indicator_id)
  urlPeriod <- paste0("dimension=pe:", period)
  urlOU <- paste0("dimension=ou:", organisation_uint)
  urlE <- "displayProperty=NAME&hierarchyMeta=true&outputIdScheme=UID"


  # URLencode
  url <- URLencode(paste(paste0(urlAnalytics, urlPeriod),
                         urlOU, urlIndicatorID,urlE,
                         sep="&"))

  xml_response <- httr::GET(url, httr::authenticate(dw$Dhis2_username, dw$Dhis2_password))

  resp_content <- httr::content(xml_response)


  parsed_content <- XML::xmlParse(resp_content)

  # get the root
  parsed_xml_root <- XML::xmlRoot(parsed_content)

  # convert xml data to dataframe
  dhis2_data <- XML::xmlToDataFrame(nodes = XML::xmlChildren(XML::xmlRoot(parsed_content)[["rows"]])) %>%
    set_names(c("Data", "Period", "Organisation unit", "Value"))%>% tibble()


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
               lubridate::year()) %>%
      select(-Month_Year)

  }

  return(df1)

}
