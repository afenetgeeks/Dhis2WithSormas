test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


measles1_coverage_annualized_all_time <- read_csv("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/measles1_coverage_annualized_all_time.csv")

measles2_coverage_annualized_all_time <- read_csv("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/measles2_coverage_annualized_all_time.csv")


# Data aggregated by Year, State

get_mcv_different_sources(mcv1_annualized_all_time = measles1_coverage_annualized_all_time,
                 mcv2_annualized_all_time = measles2_coverage_annualized_all_time
)
