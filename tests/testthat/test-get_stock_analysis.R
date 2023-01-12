test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



received <- read_csv("/Volumes/Robinson/Afenet-projects/afenet-nigeria/Dhis2WithSormas/data-raw/measles_Received.csv")

opening_balance <- read_csv("/Volumes/Robinson/Afenet-projects/afenet-nigeria/Dhis2WithSormas/data-raw/measles_OB.csv")

opened_used <- read_csv("/Volumes/Robinson/Afenet-projects/afenet-nigeria/Dhis2WithSormas/data-raw/measles_opened_used.csv")

measles1_given <- read_csv("/Volumes/Robinson/Afenet-projects/afenet-nigeria/Dhis2WithSormas/data-raw/measles1_given.csv")

measles2_given <- read_csv("/Volumes/Robinson/Afenet-projects/afenet-nigeria/Dhis2WithSormas/data-raw/measles2_given.csv")

measles_years <-  c(2017, 2018, 2019, 2020, 2021, 2022)

years_vector <-  measles_years

doses_given <- add_measles_1_2_given(measles1_given = measles1_given, measles2_given = measles2_given)







##############
library(readr)
received <- read_csv("/Volumes/Robinson/Afenet-projects/afenet-nigeria/Dhis2WithSormas/data-raw/yf_Received.csv")

opening_balance <- read_csv("/Volumes/Robinson/Afenet-projects/afenet-nigeria/Dhis2WithSormas/data-raw/yf_OB.csv")

opened_used <- read_csv("/Volumes/Robinson/Afenet-projects/afenet-nigeria/Dhis2WithSormas/data-raw/yf_Doses_Opened.csv")

doses_given<- read_csv("/Volumes/Robinson/Afenet-projects/afenet-nigeria/Dhis2WithSormas/data-raw/yf_Vaccine_Given.csv")



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

            `Doses Available (Opening Balance+Received)`, `Vaccine - Doses Opened (used)` )


get_stock_analysis(received =  received,
                   opening_balance = opening_balance,
                   opened_used = opened_used,
                   doses_given = doses_given,
                   years_vector = years_vector )




