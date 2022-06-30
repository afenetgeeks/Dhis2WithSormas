#' meningitis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_meningitis_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' meningitis Server Functions
#'
#' @noRd 
mod_meningitis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_meningitis_ui("meningitis_1")
    
## To be copied in the server
# mod_meningitis_server("meningitis_1")
