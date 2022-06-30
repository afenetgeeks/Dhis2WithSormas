## code to prepare `organisation_uints_cleaned` dataset goes here


## code to prepare `states` dataset goes here

library(dplyr)
library(stringr)
library(datimutils)


dw <- config::get("development_stream2")

loginToDATIM(
  base_url = "https://dhis2nigeria.org.ng/dhis/",
  username = dw$dhis2_username,
  password = dw$dhis2_password
)


########################################################

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
