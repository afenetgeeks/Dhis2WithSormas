#' yellow_fever UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_yellow_fever_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' yellow_fever Server Functions
#'
#' @noRd 
mod_yellow_fever_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_yellow_fever_ui("yellow_fever_1")
    
## To be copied in the server
# mod_yellow_fever_server("yellow_fever_1")
