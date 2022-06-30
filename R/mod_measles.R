#' measles UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
mod_measles_ui <- function(id){
  ns <- NS(id)
  tagList(

    fileInput(ns("upload"),  NULL, accept = c(".csv")),

    tableOutput(ns("head"))

  )
}

#' measles Server Functions
#' @importFrom tidyr separate replace_na
#' @import stringr
#' @importFrom purrr set_names
#' @noRd
mod_measles_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    data <- reactive({

      req(input$upload,cancelOutput = T)

      disease <- sormas_cleaner(input$upload$datapath)%>%
        pull(Disease) %>% unique()


      shinyFeedback::feedbackSuccess(inputId = "upload", ( disease == "Measles"), "Yes,this is Measles data")

      df <- switch(disease,

                   Measles = sormas_cleaner(input$upload$datapath),

                   shinyFeedback::feedbackDanger(inputId = "upload", ( disease != "Measles"), "Please,this is Not Measles data"))


    })

    output$head <- renderTable({
      data()
    })






















  })
}

## To be copied in the UI
# mod_measles_ui("measles_1")

## To be copied in the server
# mod_measles_server("measles_1")
