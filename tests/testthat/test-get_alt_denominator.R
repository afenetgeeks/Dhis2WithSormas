test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


# from sormas

measles1_years <-  c( 2018, 2019, 2020, 2021, 2022)
measles2_years <-  c(2020, 2021, 2022)

years_vector <- measles1_years

months_vector <-  c("01","02","03","04","05","06","07","08", "09","10","11","12")


sormas_cleaned <- sormas_cleaner("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/sormas_measles.csv")

measles1_coverage_monthly <- read_csv("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/measlecoverage_monthly.csv")

measles2_coverage_monthly <- read_csv("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/measles2_coverage_monthly.csv")

measles1_given <- read_csv("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/measles1_given.csv")

measles2_given <-read_csv("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/measles2_given.csv")


###################


get_measles_alt_denominator(sormas_cleaned = sormas_cleaned,
                            mcv1 =measlecoverage_monthly,
                            mcv2 = measles2_coverage_monthly,
                            measles1_given = measles1_given,
                            measles2_given = measles2_given,
                            lga_alt_den = lga_alt_den)



#########


# first yellow-fever plot

yf_coverage_monthly  <- read_csv("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/yf_coverage_monthly.csv")

sormas_yellow_fever_cleaned <- sormas_cleaner("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/sormas_yellow_fever.csv")

yf_given<- read_csv("/Volumes/Robinson/Afenet-projects/Dhis2WithSormas/data-raw/yf_Vaccine_Given.csv")

get_alt_denominator(coverage = yf_coverage_monthly,
                    sormas_cleaned = sormas_yellow_fever_cleaned ,
                    doses_given = yf_given, lga_alt_den = lga_alt_den)



