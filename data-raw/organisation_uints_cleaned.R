## code to prepare `organisation_uints_cleaned` dataset goes here


library(dplyr)

#########################################################


national_ou <- national %>% mutate(`parent.id` = "s5DPBsdoE8b")

#########################################################

states

###########################################################

lgas_ou <- lgas_coordinates %>% select(LGA_id,LGA,State_id) %>%
  set_names("id", "name", "parent.id") %>%
  mutate(level = 3)

###############################

organisation_uints_cleaned <- bind_rows(national_ou, states, lgas_ou)

usethis::use_data(organisation_uints_cleaned, overwrite = TRUE)
