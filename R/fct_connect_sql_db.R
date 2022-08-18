#' connect_sql_db
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom RMariaDB MariaDB
#' @importFrom pool dbPool


dw <- config::get(file = "./inst/app/www/config.yml", "development_stream2")

# connection <- pool::dbPool(
#   drv = RMariaDB::MariaDB(),
#   RMariaDB::MariaDB(),
#   username = dw$DB_user,
#   password = dw$DB_password,
#   host     = dw$DB_host,
#   port     = dw$DB_port,
#   dbname   = dw$DB_name,
#   idleTimeout = 3600000  # 1 hour)
# )
#
#
#
# shiny::onStop(function() {
#   pool::poolClose(connection)
# })


