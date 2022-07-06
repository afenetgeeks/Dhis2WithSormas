test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})




measles_sormas_cleaned <- sormas_cleaner("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/sormas_measles.csv")

measles_years <-  c(2017, 2018, 2019, 2020, 2021, 2022)

get_age_group(sormas_cleaned = measles_sormas_cleaned)
