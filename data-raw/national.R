## code to prepare `national` dataset goes here

library(datimutils)
library(tidyverse)
library(dplyr)
library(httptest)

national <- getMetadata(organisationUnits,
                    "level:eq:1",
                    fields = ":all"
)%>% tibble() %>%
  select(id, name, level)%>%
  mutate(name = name %>%
           str_remove(pattern = "^[:alpha:]{2}[:blank:]") %>%
           str_remove(pattern = "^[:blank:][:alpha:]{2}[:blank:]"))

usethis::use_data(national, overwrite = TRUE)
