#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny datimutils
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(

      h1("Dhis2WithSormas"),
      shinyFeedback::useShinyFeedback(),

      tabsetPanel(type = "tabs",

                  # 1.
                  tabPanel(
                    h4("Measles"),



                    # 2.
                    div(class = "row-page",


                        mod_measles_ui("measles_1"),




                        )),

                  # 1.
                  tabPanel(
                    h4("Yellow Fever"),

                    # 2.
                    div(class = "row-page")),

                  # 1.
                  tabPanel(
                    h4("Meningitis"),

                    # 2.
                    div(class = "row-page"))


                  )



    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Dhis2WithSormas"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
