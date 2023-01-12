test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


measles1_given <- read_csv("/Volumes/Robinson/Afenet-projects/afenet-nigeria/Dhis2WithSormas/data-raw/measles1_given.csv")

yf_vaccine_given <- read_csv("/Volumes/Robinson/Afenet-projects/afenet-nigeria/Dhis2WithSormas/data-raw/yf_Vaccine_Given.csv")


get_discrepancy(main_vaccine_given = measles1_given, other_vaccine_given =yf_vaccine_given )
