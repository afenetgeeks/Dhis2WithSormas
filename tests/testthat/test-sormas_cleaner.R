test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


sormas_cleaner("data-raw/sormas_yellow_fever.csv")

sormas %>%
  select(`Responsible LGA`)%>%
  distinct() %>%
  anti_join(lgas_coordinates %>% select(LGA),
            by = c(`Responsible LGA` = "LGA"))

