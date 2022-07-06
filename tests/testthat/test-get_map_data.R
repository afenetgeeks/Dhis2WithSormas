test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



measles_sormas_cleaned%>% select(State, LGA) %>%
  distinct() %>%
  anti_join(measles1_coverage_mixed %>% select(State, LGA) %>%
              distinct()) %>% arrange() %>% View()


measles1_coverage_annually <- read_csv("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/measles1_coverage_annualized.csv")

measles1_coverage_monthly <- read_csv("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/measles1_coverage_monthly.csv")


get_coverage_map(annual_data = measles1_coverage_annually,
                 monthly_data = measles1_coverage_monthly
)



measles_sormas_cleaned <- sormas_cleaner("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/sormas_measles.csv")%>%
  select("##Case ID","Disease", "Long", "Lat", "Months", "Year", "State", "LGA")

