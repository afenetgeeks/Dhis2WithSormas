## code to prepare `organisation_uints_cleaned` dataset goes here


## code to prepare `states` dataset goes here

library(dplyr)
library(stringr)


dw <- config::get("development_stream2")

dhis2_connection <- dhis2r::Dhis2r$new(base_url = "https://dhis2nigeria.org.ng/dhis/",
                                       username= dw$Dhis2_username,
                                       password= dw$Dhis2_password,
                                       api_version_position = "after")

########################################################


states <- dhis2_connection$get_metadata(endpoint = "organisationUnits", fields = c("id","name", "level" ,"parent")) |>

states <- getMetadata(organisationUnits,
                      "level:eq:2",
                      fields = ":all"
) %>%tibble() %>%
  select(id, name, level ,parent.id) %>%
  mutate(name = name %>%
           str_remove(pattern = "^[:alpha:]{2}[:blank:]") %>%
           str_remove(pattern = "^[:blank:][:alpha:]{2}[:blank:]") %>%
           str_remove("[:blank:]State") %>%
           str_remove("[:blank:]state")) %>% tibble()

usethis::use_data(states, overwrite = TRUE)
