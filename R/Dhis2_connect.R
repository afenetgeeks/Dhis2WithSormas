#' Connecting
#'
#' @description connecting to Dhis2

#'
#' @noRd
#' @import datimutils

dw <- config::get("development_stream2")

loginToDATIM(
  base_url = "https://dhis2nigeria.org.ng/dhis/",
  username = dw$dhis2_username,
  password = dw$dhis2_password
)

