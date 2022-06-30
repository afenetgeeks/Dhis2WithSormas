## code to prepare `lgas` dataset goes here

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

lgas <- getMetadata(organisationUnits,
                    "level:eq:3",
                    fields = ":all")%>% tibble()%>%
  select(id, name, level ,parent.id)%>%
  filter(!str_detect(name, "Cold Store")) %>%
  mutate(name = name %>%
           str_remove(pattern = "^[:alpha:]{2}[:blank:]") %>%
           str_remove(pattern = "^[:blank:][:alpha:]{2}[:blank:]") %>%
           str_remove("[:blank:]Local Government Area"),
         name = as.character(case_when(name == "Abuja Municipal Area Council" ~ "Abuja Municipal",
                                       TRUE ~ name )))


#####################

lgas_parented  <- lgas %>%
  left_join(states %>%
              select(c("id", "name" )),
            by= c("parent.id" = "id")) %>%
  select(-level) %>%
  purrr::set_names("LGA_id", "LGA", "State_id", "State")


########################################


lga_latlon <- readr::read_csv("data-raw/lga_latlon_geocodes.csv")%>%
  mutate(State = State%>%
           str_remove(pattern = "^[:alpha:]{2}[:blank:]") %>%
           str_remove(pattern = "^[:blank:][:alpha:]{2}[:blank:]") %>%
           str_remove("[:blank:]State") %>%
           str_remove("[:blank:]state")) %>%
  mutate(State = case_when(State == "Akwa Ibom" ~ "Akwa-Ibom",
                           TRUE ~ State))


lga_latlon %>%
  anti_join(lgas_parented , by = c("LGA" = "LGA"))


lgas_coordinates <- lga_latlon %>%
  mutate(modified_LGA = as.character(case_when(
    LGA == "Ngor Okpala" ~ "Ngor-Okpala",
    LGA == "Akoko South-East" ~ "Akoko South East",
    LGA == "Arewa Dandi"~"Arewa",
    LGA == "Atakunmosa East"~"Atakumosa East",
    LGA == "Atakunmosa West"~"Atakumosa West",
    LGA == "Aiyedire"~"Ayedire",
    LGA == "Birnin Magaji/Kiyaw"~"Birnin Magaji",
    LGA == "Bursari"~"Busari",
    LGA == "Emuoha"~"Emohua",
    LGA == "Umu Nneochi" ~ "Umunneochi",
    LGA == "Gbonyin" ~ "Aiyekire (Gbonyin)",
    LGA == "Igbo Eze North" ~ "Igbo-Eze North",
    LGA == "Igbo Eze South" ~ "Igbo-Eze South",
    LGA == "Akoko North-East" ~ "Akoko North East",
    LGA == "Oshodi-Isolo" ~ "Oshodi/Isolo",
    LGA == "Ido Osi" ~ "Ido-Osi",
    LGA == "Akoko North-West" ~ "Akoko North West",
    LGA == "Akoko South-West" ~ "Akoko South West",
    LGA == "Ile Oluji/Okeigbo" ~ "Ile-Oluji/Okeigbo",
    LGA == "Ajeromi-Ifelodun" ~ "Ajeromi/Ifelodun",
    LGA == "Ezinihitte" ~ "Ezinihitte-Mbaise",
    LGA == "Ekiti South-West" ~ "Ekiti South West",
    LGA == "Ifako-Ijaiye" ~ "Ifako/Ijaye",
    LGA == "Nsit-Ubium" ~ "Nsit Ubium",
    LGA == "Ahiazu Mbaise" ~ "Ahiazu-Mbaise",
    LGA == "Karim Lamido" ~ "Karim-Lamido",
    LGA == "Isiala Ngwa North" ~ "Isiala-Ngwa North",
    LGA == "Garun Mallam" ~ "Garum Mallam",
    LGA == "Osisioma" ~ "Osisioma Ngwa",
    LGA == "Nsit-Ibom" ~ "Nsit Ibom",
    LGA == "Aboh Mbaise" ~ "Aboh-Mbaise",
    LGA == "Isiala Ngwa South" ~ "Isiala-Ngwa South",
    LGA == "Ibadan South-East" ~ "Ibadan South East",
    LGA == "Igbo Etiti" ~ "Igbo-Etiti",
    LGA == "Chafe" ~ "Tsafe",
    LGA == "Ado-Odo/Ota" ~ "Ado Odo/Ota",
    LGA == "Ibadan North-West" ~ "Ibadan North West",
    LGA == "Abuja Municipal Area Council" ~ "Abuja Municipal",
    LGA == "Ibiono-Ibom" ~ "Ibiono Ibom",
    LGA == "Amuwo-Odofin" ~ "Amuwo Odofin",
    LGA == "Igalamela Odolu" ~ "Igalamela-Odolu",
    LGA == "Dange Shuni" ~ "Dange-Shuni",
    LGA == "Dutsin Ma" ~ "Dutsin-Ma",
    LGA == "Oke Ero" ~ "Oke-Ero",
    LGA == "Wasagu/Danko" ~ "Danko/Wasagu",
    LGA == "Nsit-Atai" ~ "Nsit Atai",
    LGA == "Ola Oluwa" ~ "Ola-Oluwa",
    LGA == "Yakuur" ~ "Yakurr",
    LGA == "Unuimo" ~ "Onuimo",
    LGA == "Udung-Uko" ~ "Udung Uko",
    LGA == "Ibadan North-East" ~ "Ibadan North East",
    LGA == "Dambatta" ~ "Danbatta",
    LGA == "Eti Osa" ~ "Eti-Osa",
    LGA == "Isi Uzo" ~ "Isi-Uzo",
    LGA == "Aiyedaade" ~ "Ayedaade",
    LGA == "Ibeju-Lekki" ~ "Ibeju/Lekki",
    LGA == "Mayo Belwa" ~ "Mayo-Belwa",
    LGA == "Mkpat-Enin" ~ "Mkpat Enin",
    LGA == "Nasarawa Egon" ~ "Nasarawa Eggon",
    LGA == "Gayuk" ~ "Guyuk",
    LGA == "Fufure" ~ "Fufore",
    LGA == "Sule Tankarkar" ~ "Sule-Tankarkar",
    str_detect(LGA,"(Edda)") ~ "Afikpo South",
    LGA == "Akuku-Toru" ~ "Akuku Toru",
    LGA == "Ehime Mbano" ~ "Ehime-Mbano",
    LGA == "Ardo Kola" ~ "Ardo-Kola",
    LGA == "Kumi" ~ "Kurmi",
    LGA == "Port Harcourt" ~ "Port-Harcourt",
    LGA == "Damban" ~ "Dambam",
    LGA == "Tafawa Balewa" ~ "Tafawa-Balewa",
    LGA == "Moya" ~ "Muya",
    LGA == "Omuma" ~ "Omumma",
    LGA == "Oturkpo" ~ "Otukpo",
    LGA == "Mopa Muro" ~ "Mopa-Muro",
    LGA == "Ibadan South-West" ~ "Ibadan South West",
    LGA == "Ikpoba Okha" ~ "Ikpoba-Okha",
    LGA == "Biriniwa" ~ "Birniwa",
    LGA == "Kiri Kasama" ~ "Kiri Kasamma",
    LGA == "Obi Ngwa" ~ "Obi Nwga",
    LGA == "Shagamu" ~ "Sagamu",
    LGA == "Shagamu" ~ "Sagamu",
    TRUE~LGA))) %>%
  left_join(lgas_parented,
            by = c("modified_LGA" = "LGA",
                   "State"  = "State")) %>%
  select(-c(LGA,  Country )) %>%
  rename(LGA = modified_LGA) %>% tibble()

usethis::use_data(lgas_coordinates, overwrite = TRUE)
