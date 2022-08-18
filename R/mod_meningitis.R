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

    div(class = "row",

        div(class = "output-div",
            h6("Upload the Meningitis sormas data  Below"),
            fileInput(ns("upload"),  NULL, accept = c(".csv")),

            h6("Click this button to pull data from Dhis2, I Will take some time depending on how stable dhis2 is."),
            actionButton(inputId =  ns("dhis2"), label = "Get Dhis2 data")
        ),

        div(class = "output-div",
            reactableOutput(ns("sormas_table"), height = "190px" )
        )
    ),


    #Chart 1: Confirmed Meningitis cases, Coverage, Alt denominator
    div(class = "row",

        div(class = "output-div",
            h6("Chart 1: Confirmed Meningitis cases, Coverage, Alt denominator"),

            actionButton(inputId =  ns("alt_update"), label = "Update Alt data")
        ),

        div(class = "output-div",
            h1(textOutput(ns("alt_status") ))
        )

    ),

    ## Chart 2: Age group of confirmed Meningitis cases by vaccination status

    div(class = "row",


        div(class = "output-div",

            h6("Chart 2: Age group of confirmed Meningitis cases by vaccination status"),

            actionButton(inputId =  ns("age_update"), label = "Update age data")
        ),

        div(class = "output-div",
            h1(textOutput(ns("age_status") ))
        )

    ),

    #### Chart 3: Meningitis vaccine stock analysis & vaccine given

    div(class = "row",

        div(class = "output-div",

            h6("Chart 3: Meningitis vaccine stock analysis & vaccine given"),


            actionButton(inputId =  ns("stock_update"), label = "Update stock data")
        ),

        div(class = "output-div",
            h1(textOutput(ns("stock_status") ))
        )

    ),
    #######

    div(class = "row",

        div(class = "output-div",
            h6("Chart 5:Meningitis coverage"),
            actionButton(inputId =  ns("map_men_A_update"), label = "Update map coverage data")
        ),

        div(class = "output-div",
            h1(textOutput(ns("map_men_A_status") ))
        )

    ),

    div(class = "row",

        div(class = "output-div",
            h6("Chart 5: Confirmed Meningitis cases"),
            actionButton(inputId =  ns("map_cases_update"), label = "Update map cases data")
        ),

        div(class = "output-div",
            h1(textOutput(ns("map_cases_status") ))
        )

    )

  )
}

#' meningitis Server Functions
#'
#' @noRd
mod_meningitis_server <- function(id ,year_input , month_input){

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


      shinyFeedback::feedbackSuccess(inputId = "upload", ( disease == "Meningitis (CSM)"), "Yes,this is Meningitis (CSM) data")

      df <- switch(disease,
                   "Meningitis (CSM)" = sormas_cleaner(input$upload$datapath),
                   shinyFeedback::feedbackDanger(inputId = "upload", ( disease != "Meningitis (CSM)"), "Please,this is Not Meningitis (CSM) data"))

      df
    })


    output$sormas_table <- renderReactable({

      reactable(
        head(sormas_data(), 3))

    })


    dhis2_datasets <-  reactiveValues()
    progress_list <-  reactiveValues()



    observeEvent(input$dhis2,{

      ##########################

      waitress$inc(10) # increase by 10%

      Sys.sleep(1)

      dhis2_datasets$men_A_coverage_monthly <-getAnalytics_cleaned(indicator_id = "SruoPzzt3ES",
                                                                   months = month_input(),
                                                                   year = year_input(),
                                                                   organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))
      ##################

      waitress$inc(10) # increase by 10%

      Sys.sleep(1)


      dhis2_datasets$men_A_given <-  getAnalytics_cleaned(indicator_id = "iOUKrRANJwi",
                                                          months = month_input(),
                                                          year = year_input(),
                                                          organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))
      ##################


      waitress$inc(10) # increase by 10%

      Sys.sleep(1)



      dhis2_datasets$men_A_received  <- getAnalytics_cleaned(indicator_id = "yH7RI3urgYi",
                                                             months = month_input(),
                                                             year = year_input(),
                                                             organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3")
      )

      ##################
      waitress$inc(10) # increase by 10%

      Sys.sleep(1)


      dhis2_datasets$men_A_opening_balance  <-  getAnalytics_cleaned(indicator_id = "BWBsbKHaCrz",
                                                                     months = month_input(),
                                                                     year = year_input(),
                                                                     organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))
      ##################
      waitress$inc(10) # increase by 10%

      Sys.sleep(1)


      dhis2_datasets$men_A_opened_used  <- getAnalytics_cleaned(indicator_id = "snrl5tiWGLE",
                                                                months = month_input(),
                                                                year = year_input(),
                                                                organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3")
      )




      ###########
      waitress$inc(10) # increase by 10%

      Sys.sleep(1)

      dhis2_datasets$measles1_given <- getAnalytics_cleaned(indicator_id = "j6mCLL1X4yG",
                                             months = month_input(),
                                             year = year_input(),
                                             organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))

      ##################

      waitress$inc(10) # increase by 10%

      Sys.sleep(1)

      dhis2_datasets$men_A_coverage_annually <- getAnalytics_cleaned(indicator_id = "SruoPzzt3ES",
                                                                     months = c(),
                                                                     year = year_input(),
                                                                     organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))
    })




    ####################Chart 1: Confirmed cases, coverage,Alt denominator"

    observeEvent(input$alt_update, {

      id <- showNotification("Updating Remote Database....", duration = NULL, closeButton = FALSE, type ="message" )
      on.exit(removeNotification(id), add = TRUE)

      Sys.sleep(2)

      # men_A_alt_denominator <- get_alt_denominator(coverage = dhis2_datasets$men_A_coverage_monthly,
      #                                              sormas_cleaned = sormas_meningitis_cleaned ,
      #                                              doses_given = dhis2_datasets$men_A_given,
      #                                              denominator_data = denominator_data)
      #
      # dbAppendTable(connection , name = "men_A_alt_denominator", value = men_A_alt_denominator )
      progress_list$alt_status <- "Updated ✅"


    })


    output$alt_status <- renderText({

      progress_list$alt_status


    })


    #################### Chart 2: Age Group of Confirmed Meningitis Cases by Vaccination Status

    observeEvent(input$age_update, {

      # meningitis_age_group <- get_age_group(sormas_cleaned = sormas_data())
      #
      # dbAppendTable(connection, name = "meningitis_age_group", value = meningitis_age_group )

      progress_list$age_status <-  "Updated ✅"

    })


    output$age_status<- renderText({

      progress_list$age_status

    })


    ####################Chart 3:men_A Stock Analysis & men_A vaccine given

    observeEvent(input$stock_update, {


      # men_A_stock_analysis <- get_stock_analysis(doses_given = dhis2_datasets$men_A_given,
      #                                            received = dhis2_datasets$men_A_received,
      #                                            opening_balance = dhis2_datasets$men_A_opening_balance,
      #                                            opened_used = dhis2_datasets$men_A_opened_used)
      #
      # dbAppendTable(connection, name = "men_A_stock_analysis", value = men_A_stock_analysis )

      progress_list$stock_status <-  "Updated ✅"


    })


    output$stock_status <- renderText({
      progress_list$stock_status

    })


    ###################Chart 4: Co-administered Antigen Discrepancy: MCV 1 & Meningitis given by State

        observeEvent(input$discrepancy_update, {


          # measles_men_A_discrepancy <- get_discrepancy(main_vaccine_given = dhis2_datasets$measles1_given,
          #                                              other_vaccine_given = dhis2_datasets$men_A_given)
          #
          #
          # dbAppendTable(connection, name = "measles_men_A_discrepancy", value = measles_men_A_discrepancy )

          progress_list$discrepancy_status <- "Updated ✅"


        })


        output$discrepancy_status <- renderText({
          progress_list$discrepancy_status

        })




    ####################Chart 5: Meningitis coverage

    observeEvent(input$map_men_A_update, {

      # men_A_coverage_map <- get_coverage_map(annual_data = dhis2_datasets$men_A_coverage_annually,
      #                                        monthly_data = dhis2_datasets$men_A_coverage_monthly)
      #
      #
      # dbAppendTable(connection, name = "men_A_coverage_map", value = men_A_coverage_map )

      progress_list$map_men_A_status <- "Updated ✅"


    })


    output$map_men_A_status <- renderText({
      progress_list$map_men_A_status

    })


    ####################Chart 5: Meningitis cases

    observeEvent(input$map_cases_update, {

      # meningitis_cases_map <- sormas_data() %>%
      #   select("##Case ID","Disease", "Long", "Lat", "Months", "Year", "State", "LGA")
      #
      #
      # dbAppendTable(connection, name = "meningitis_cases_map",value = meningitis_cases_map )
      progress_list$map_cases_status <-  "Updated ✅"


    })


    output$map_cases_status <- renderText({
      progress_list$map_cases_status

    })


  })

}

## To be copied in the UI
# mod_meningitis_ui("meningitis_1")

## To be copied in the server
# mod_meningitis_server("meningitis_1")
