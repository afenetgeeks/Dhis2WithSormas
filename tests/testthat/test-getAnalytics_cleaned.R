test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})





data_cleaned <- getAnalytics_cleaned(indicator_id ="tb5NGUMB9dQ",
                                     months = c("01","02","03","04","05","06","07","08", "09","10","11","12"),
                                     year = c("2021", "2022")  )

