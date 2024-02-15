
#' Clean sormas data
#' @import stringr readr
#' @param string a dataframe from sormas
#' @inheritParams stringr::str_split
#'
#' @return a dataframe.
#'
#' @examples
#' cleaned_measles_data <- "sormas_cleaner(file = "path")"
#' @export


sormas_cleaner  <- function(file, skip = 2, disease = c("diphtheria", "measles", "yellow fever", "meningitis (csm)")) {

  disease <- match.arg(disease)
   sormas_lgas <- readr::read_csv(file = file,skip = skip) |>
     mutate(across(c(`Disease name`, `Disease`), str_to_lower))

   # sormas_lgas <- readr::read_csv(file = "/Volumes/Robinson/Afenet-projects/afenet-nigeria/Dhis2WithSormas/data-raw/sormas_meningitis_cases_Dec_2023.csv",
   #                             skip = 2) |>
   #   mutate(across(c(`Disease name`, `Disease`), str_to_lower))

   if(disease == "diphtheria"){

     sormas_lgas  <- sormas_lgas |>
       filter(str_detect(`Disease name`, pattern = "^dep|^dip")) |>
       mutate(`Disease name`= "diphtheria") |>
       filter(`Disease name` == disease) |>
       select(-`Disease`) |>
       rename(`Disease` = `Disease name`)

   }else {
     sormas_lgas  <- sormas_lgas |>
       select(-`Disease name`)
   }

   sormas_lgas_cleaned <- sormas_lgas |>
    mutate(`Responsible state` = str_replace(`Responsible state`, pattern = "Fct" ,replacement = "Federal Capital Territory")) %>%
    mutate(LGA = as.character(case_when(
      `Responsible LGA`  == "Ibeju Lekki" ~ "Ibeju/Lekki",
      `Responsible LGA`  == "Orire" ~ "Ori Ire",
      `Responsible LGA`  == "Ilesha West" ~ "Ilesa West",
      `Responsible LGA`  == "Ilesha East" ~ "Ilesa East",
      `Responsible LGA`  == "Ijebu ode" ~ "Ijebu Ode",
      `Responsible LGA`  == "Mbatoli" ~ "Mbaitoli",
      `Responsible LGA`  == "Shagamu" ~ "Sagamu",
      `Responsible LGA`  == "Atakunmosa East" ~ "Atakumosa East",
      `Responsible LGA`  == "Atakunmosa West" ~ "Atakumosa West",
      `Responsible LGA`  == "Aiyedade" ~ "Ayedaade",
      `Responsible LGA`  == "Odo-Otin" ~ "Odo Otin",
      `Responsible LGA`  == "Bursari" ~ "Busari",
      `Responsible LGA`  == "Birnin Magaji/Kiyaw" ~ "Birnin Magaji",
      `Responsible LGA`  == "Arewa Dandi" ~ "Arewa",
      `Responsible LGA`  == "Ile-Oluji-Okeigbo" ~ "Ile-Oluji/Okeigbo",
      `Responsible LGA`  == "Qua an Pan" ~ "Qua'an Pan",
      `Responsible LGA`  == "Nasarawa Egon" ~ "Nasarawa Eggon",
      `Responsible LGA`  == "Mai Adua" ~ "Mai'Adua",
      `Responsible LGA`  == "Jema a" ~ "Jema'a",
      `Responsible LGA`  == "Aiyedire" ~ "Ayedire",
      `Responsible LGA`  == "Jama are" ~ "Jama'are",
      `Responsible LGA`  == "Obafemi-Owode" ~ "Obafemi Owode",
      `Responsible LGA`  == "Munya" ~ "Muya",
      `Responsible LGA`  == "Emuoha" ~ "Emohua",
      `Responsible LGA`  == "Obi Ngwa" ~ "Obi Nwga",
      `Responsible LGA`  == "Umu-Nneochi" ~ "Umunneochi",
      `Responsible LGA`  == "Egbado South" ~ "Yewa South",
      `Responsible LGA`  == "Nassarawa" ~ "Nasarawa",
      TRUE~`Responsible LGA`))) %>%
    left_join(lgas_coordinates,
              by = c("LGA" = "LGA", `Responsible state`  = "State")) %>%
    select(-c(`Responsible LGA`)) %>%
    rename(State = `Responsible state`) %>%
    filter(!is.na(LGA))

  sormas_lgas_cleaned_locations <- sormas_lgas_cleaned %>%
    separate(`GPS coordinates of the address`, c("sormas_lat", "sormas_long"), sep = ", ", remove = T) %>%
    distinct() %>%
    mutate(Long =  as.numeric(str_extract(sormas_long, "^[[:digit:]][[:punct:]][[:digit:]]{2,}")),
           Lat = as.numeric(sormas_lat)) %>%
    mutate(Lat = case_when(
      is.na(Lat) ~ lat,
      TRUE~Lat)) %>%
    mutate(Long = case_when(
      is.na(Long)~lon,
      TRUE~Long
    )) %>%
    select(-c(lat, lon, sormas_lat, sormas_long)) %>%
    mutate(`Vaccination status` = `Vaccination status` %>% replace_na("Unknown"))


  sormas_cleaned  <- sormas_lgas_cleaned_locations %>%
    mutate("Months" = `Date of report (dd/MM/yyyy)` %>%
             lubridate::parse_date_time("%d%m%y") %>%
             lubridate::month(label = TRUE,abbr = TRUE)) %>%
    mutate("Year" = `Date of report (dd/MM/yyyy)` %>%
             lubridate::parse_date_time("%d%m%y") %>%
             lubridate::year()) %>%
    select(-`Date of report (dd/MM/yyyy)`)

  return(sormas_cleaned)
}


