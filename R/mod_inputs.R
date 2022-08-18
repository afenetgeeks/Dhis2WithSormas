#' inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_inputs_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(column(width = 12,

                    tags$span(   selectInput(inputId = ns("year"),label = "Year",
                                             choices = 2022:lubridate::year(Sys.time()),
                                             selected = lubridate::year(Sys.time())),

                                 selectInput(inputId = ns("month"),label = "Month",
                                             choices = c("01","02","03","04","05","06","07","08", "09","10","11","12"),
                                             selected = current_month(lubridate::month(Sys.time())))

                    )))


  )
}

#' inputs Server Functions
#'
#' @noRd
mod_inputs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dropdown_inputs <- reactiveValues()

    observe({
      dropdown_inputs$year_input <-   input$year

      dropdown_inputs$month_input <-  input$month

    })

    return(dropdown_inputs)

  })
}

## To be copied in the UI
# mod_inputs_ui("inputs_1")

## To be copied in the server
# mod_inputs_server("inputs_1")
