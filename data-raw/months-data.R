## code to prepare `months-data` dataset goes here

library(tibble)

months_data <- tibble(name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
       num = c("01", "02",  "03", "04",   "05"  , "06",  "07",   "08",   "09",  "10",  "11",  "12"))


usethis::use_data(months_data, overwrite = TRUE)
