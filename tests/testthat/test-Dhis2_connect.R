test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


dw <- config::get("development_stream2")


loginToDATIM(
  base_url = dw$Dhis2_base_url,
  username = dw$Dhis2_username,
  password = dw$Dhis2_password
)


connection <- DBI::dbConnect(RMariaDB::MariaDB(),
               username = dw$DB_user,
               password = dw$DB_password,
               host     = dw$DB_host,
               port     = dw$DB_port,
               dbname   = dw$DB_name)
