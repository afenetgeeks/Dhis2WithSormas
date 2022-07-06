test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



measles1_coverage_monthly <- read_csv("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/measles1_coverage_monthly.csv")

measles2_coverage_monthly <- read_csv("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/measles2_coverage_monthly.csv")

get_drop_out_rate(first_dose  = measles1_coverage_monthly,
                  second_dose =  measles2_coverage_monthly)
