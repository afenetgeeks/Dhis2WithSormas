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

    div(class = "row",

        div(class = "output-div",
            h6("Upload the Yellow Fever sormas data  Below"),
            fileInput(ns("upload"),  NULL, accept = c(".csv")),

            h6("Click this button to pull data from Dhis2, I Will take some time depending on how stable dhis2 is."),
            actionButton(inputId =  ns("dhis2"), label = "Get Dhis2 data")
        ),

        div(class = "output-div",
            reactableOutput(ns("sormas_table"), height = "190px" )
        )

    ),


    #Chart 1: Confirmed Yellow Fever cases, Coverage, Alt denominator
    div(class = "row",

        div(class = "output-div",
            h6("Chart 1: Confirmed Yellow Fever cases, Coverage, Alt denominator"),

            actionButton(inputId =  ns("alt_update"), label = "Update Alt data")
        ),

        div(class = "output-div",
            h1(textOutput(ns("alt_status") ))
        )

    ),

    ## Chart 2: Age group of confirmed Yellow Fever cases by vaccination status

    div(class = "row",


        div(class = "output-div",

            h6("Chart 2: Age group of confirmed Yellow Fever cases by vaccination status"),

            actionButton(inputId =  ns("age_update"), label = "Update age data")
        ),

        div(class = "output-div",
            h1(textOutput(ns("age_status") ))
        )

    ),

    #### Chart 3: Yellow Fever vaccine stock analysis & vaccine given

    div(class = "row",

        div(class = "output-div",

            h6("Chart 3: Yellow Fever vaccine stock analysis & vaccine given"),


            actionButton(inputId =  ns("stock_update"), label = "Update stock data")
        ),

        div(class = "output-div",
            h1(textOutput(ns("stock_status") ))
        )

    ),
#######

    div(class = "row",

        div(class = "output-div",
            h6("Chart 5:Yellow Fever coverage"),
            actionButton(inputId =  ns("map_yf_update"), label = "Update map coverage data")
        ),

        div(class = "output-div",
            h1(textOutput(ns("map_yf_status") ))
        )

    ),

    div(class = "row",

        div(class = "output-div",
            h6("Chart 5: Confirmed Yellow Fever cases"),
            actionButton(inputId =  ns("map_cases_update"), label = "Update map cases data")
        ),

        div(class = "output-div",
            h1(textOutput(ns("map_cases_status") ))
        )

    )

  )
}

#' yellow_fever Server Functions
#'
#' @noRd
mod_yellow_fever_server <- function(id ,year_input , month_input){



  # stopifnot(is.reactive(year_input))
  # stopifnot(is.reactive(month_input))
  #

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    waitress <- Waitress$new(theme = "overlay-percent")


    sormas_data <- reactive({

      req(input$upload,cancelOutput = T)

      id <- showNotification("Reading data...", duration = NULL, closeButton = FALSE, type ="message" )
      on.exit(removeNotification(id), add = TRUE)

      disease <- sormas_cleaner(input$upload$datapath)%>%
        pull(Disease) %>% unique()


      shinyFeedback::feedbackSuccess(inputId = "upload", ( disease == "Yellow Fever"), "Yes,this is Yellow Fever data")

      df <- switch(disease,
                   "Yellow Fever" = sormas_cleaner(input$upload$datapath),
                   shinyFeedback::feedbackDanger(inputId = "upload", ( disease != "Yellow Fever"), "Please,this is Not Yellow Fever data"))

      df
    })


    output$sormas_table <- renderReactable({

      reactable(
        head(sormas_data(), 3))

    })

    output$dhis2_table <- renderReactable({

      reactable(  head(sormas_data(), 3))

    })



    dhis2_datasets <-  reactiveValues()
    progress_list <-  reactiveValues()



    observeEvent(input$dhis2,{

      ##########################

      waitress$inc(10) # increase by 10%

      Sys.sleep(1)

      dhis2_datasets$yf_coverage_monthly <- getAnalytics_cleaned(indicator_id = "RtXOV3xQvWT",
                                                                 months = month_input(),
                                                                 year = year_input(),
                                                                 organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3")
      )
      ##################

      waitress$inc(10) # increase by 10%

      Sys.sleep(1)


      dhis2_datasets$yf_given  <- getAnalytics_cleaned(indicator_id = "KpKx2MosfIV",
                                                       months = month_input(),
                                                       year = year_input(),
                                                       organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))

      ##################


      waitress$inc(10) # increase by 10%

      Sys.sleep(1)



      dhis2_datasets$yf_received  <- getAnalytics_cleaned(indicator_id = "GkFMGCF5p4n",
                                                          months = month_input(),
                                                          year = year_input(),
                                                          organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3")
      )

      ##################
      waitress$inc(10) # increase by 10%

      Sys.sleep(1)


      dhis2_datasets$yf_opening_balance  <-  getAnalytics_cleaned(indicator_id = "iKUmaZuN5l0",
                                                                  months = month_input(),
                                                                  year = year_input(),
                                                               organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))
      ##################
       waitress$inc(10) # increase by 10%

      Sys.sleep(1)


      dhis2_datasets$yf_opened_used  <-getAnalytics_cleaned(indicator_id = "OiNgd1uT2Mp",
                                                            months = month_input(),
                                                            year = year_input(),
                                                            organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3")
      )

      ##################

      waitress$inc(10) # increase by 10%

      Sys.sleep(1)

      dhis2_datasets$yf_coverage_annually <- getAnalytics_cleaned(indicator_id = "RtXOV3xQvWT",
                                                                  months = c(),
                                                                  year = year_input(),
                                                                  organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))

    })




    ####################Chart 1: Confirmed cases, coverage,Alt denominator"

    observeEvent(input$alt_update, {

      id <- showNotification("Updating Remote Database....", duration = NULL, closeButton = FALSE, type ="message" )
      on.exit(removeNotification(id), add = TRUE)

      Sys.sleep(2)

      # yf_alt_denominator <- get_alt_denominator(coverage = dhis2_datasets$yf_coverage_monthly,
      #                                           sormas_cleaned = sormas_yellow_fever_cleaned ,
      #                                           doses_given = dhis2_datasets$yf_given,
      #                                           denominator_data = denominator_data)
      #
      # dbAppendTable(connection , name = "yf_alt_denominator", value = yf_alt_denominator )

      progress_list$alt_status <- "Updated ✅"


    })


    output$alt_status <- renderText({

      progress_list$alt_status


    })


    #################### Chart 2: Age Group of Confirmed Yellow Fever Cases by Vaccination Status

    observeEvent(input$age_update, {

      # yf_age_group <- get_age_group(sormas_cleaned = sormas_data() )
      #
      # dbAppendTable(connection, name = "yf_age_group", value = yf_age_group )

      progress_list$age_status <-  "Updated ✅"

    })


    output$age_status<- renderText({

      progress_list$age_status

    })


    ####################Chart 3:YF Stock Analysis & YF vaccine given

    observeEvent(input$stock_update, {


      # yf_stock_analysis <- get_stock_analysis(doses_given = dhis2_datasets$yf_given,
      #                                         received = dhis2_datasets$yf_received,
      #                                         opening_balance = dhis2_datasets$yf_opening_balance,
      #                                         opened_used = dhis2_datasets$yf_opened_used)
      #
      # dbAppendTable(connection, name = "yf_stock_analysis", value = yf_stock_analysis )


      progress_list$stock_status <-  "Updated ✅"


    })


    output$stock_status <- renderText({
      progress_list$stock_status

    })


    ####################Chart 4: Co-administered Antigen Discrepancy: MCV 1 & Yellow Fever given by State
#
#     observeEvent(input$discrepancy_update, {
#
#       measles_yf_discrepancy <- get_discrepancy(main_vaccine_given = dhis2_datasets$yf_given,
#                                                 other_vaccine_given = dhis2_datasets$yf_vaccine_given)
#
#       dbAppendTable(connection, name = "measles_yf_discrepancy", value = measles_yf_discrepancy )
#
#       progress_list$discrepancy_status <- "Updated ✅"
#
#
#     })
#
#
#     output$discrepancy_status <- renderText({
#       progress_list$discrepancy_status
#
#     })




    ####################Chart 5: Yellow Fever coverage

    observeEvent(input$map_yf_update, {

      # yf_coverage_map <- get_coverage_map(annual_data = dhis2_datasets$yf_coverage_annually,
      #                                     monthly_data = dhis2_datasets$yf_coverage_monthly)
      #
      #
      # dbAppendTable(connection, name = "yf_coverage_map", value = yf_coverage_map )


      progress_list$map_yf_status <- "Updated ✅"


    })


    output$map_yf_status <- renderText({
      progress_list$map_yf_status

      })


    ####################Chart 5: Yellow Fever cases

    observeEvent(input$map_cases_update, {

      # yf_cases_map <- sormas_data() %>%
      #   select("##Case ID","Disease", "Long", "Lat", "Months", "Year", "State", "LGA")
      #
      # dbAppendTable(connection, name = "yf_cases_map", value = yf_cases_map )

      progress_list$map_cases_status <-  "Updated ✅"


    })


    output$map_cases_status <- renderText({
      progress_list$map_cases_status

    })


  })

}

## To be copied in the UI
# mod_yellow_fever_ui("yellow_fever_1")

## To be copied in the server
# mod_yellow_fever_server("yellow_fever_1")
