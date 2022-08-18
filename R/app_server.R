#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  dropdown_inputs <- mod_inputs_server("inputs_1")

  mod_measles_server("measles_1",
                     year_input = reactive({dropdown_inputs$year_input}),
                     month_input =  reactive({dropdown_inputs$month_input})
                     )

  mod_yellow_fever_server("yellow_fever_1",
                          year_input = reactive({dropdown_inputs$year_input}),
                          month_input =  reactive({dropdown_inputs$month_input})
                          )

  mod_meningitis_server("meningitis_1",
                    year_input = reactive({dropdown_inputs$year_input}),
                    month_input =  reactive({dropdown_inputs$month_input})
                    )

}
