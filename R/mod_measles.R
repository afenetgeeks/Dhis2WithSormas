#' measles UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr shinyWidgets reactable
mod_measles_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(class = "row",

        div(class = "output-div",
            h6("Upload the Measles sormas data  Below"),
            fileInput(ns("upload"),  NULL, accept = c(".csv")),

            h6("Click this button to pull data from Dhis2, I Will take some time depending on how stable dhis2 is."),
            actionButton(inputId =  ns("dhis2"), label = "Get Dhis2 data")
            ),

        div(class = "output-div",
            reactableOutput(ns("sormas_table"), height = "190px" )
            )
    ),


    #Chart 1: Confirmed meningitis cases, Coverage,and Alt denominator
    div(class = "row",

        div(class = "output-div",
            h6("Chart 1: Confirmed measles cases, MCV 1 overage,Alt denominator"),

            actionButton(inputId =  ns("alt_update"), label = "Update Alt data")
        ),

        div(class = "output-div",
            h1(textOutput(ns("alt_status") ))
        )

    ),

    ## Chart 2: Age Group of Confirmed Measles Cases by Vaccination Status

    div(class = "row",


        div(class = "output-div",

            h6("Chart 2: Age Group of Confirmed Measles Cases by Vaccination Status"),

            actionButton(inputId =  ns("age_update"), label = "Update age data")
        ),


        div(class = "output-div",
            h1(textOutput(ns("age_status") ))
        )

    ),

    #### Chart 3: MCV Stock Analysis & MCV (1 & 2) given

    div(class = "row",

        div(class = "output-div",

            h6("Chart 3: MCV Stock Analysis & MCV (1 & 2) given"),


            actionButton(inputId =  ns("stock_update"), label = "Update Stock data")
        ),


        div(class = "output-div",
            h1(textOutput(ns("stock_status") ))
        )

    ),

    ###  Chart 4: Co-administered Antigen Discrepancy: MCV 1 & Yellow Fever given
    div(class = "row",

        div(class = "output-div",

            h6("Chart 4: Co-administered Antigen Discrepancy: MCV 1 & Yellow Fever given"),
            actionButton(inputId =  ns("discrepancy_update"), label = "Update Discrepancy data")
        ),


        div(class = "output-div",
            h1(textOutput(ns("discrepancy_status") ))
        )

    ),

    ###  Chart 5: MCV 1, MCV 2 Coverage & Drop Out Rate
    div(class = "row",

        div(class = "output-div",
            h6("Chart 5: MCV 1, MCV 2 Coverage & Drop Out Rate"),

            actionButton(inputId =  ns("dropout_update"), label = "Update drop out data")
        ),

        div(class = "output-div",
            h1(textOutput(ns("dropout_status") ))
        )

    ),

    ## Chart 6: National MCV Coverage (%) by different sources, Nigeria (National)

    div(class = "row",

        div(class = "output-div",
            h6("Chart 6: National MCV Coverage (%) by different sources, Nigeria (National)"),
            actionButton(inputId =  ns("national_mcv_update"), label = "Update National MCV data")
        ),

        div(class = "output-div",
            h1(textOutput(ns("national_mcv_status") ))
        )

    ),


    div(class = "row",

        div(class = "output-div",
            h6("Chart 7: MCV 1 coverage"),
            actionButton(inputId =  ns("map_mcv_update"), label = "Update MCV 1 data")
        ),


        div(class = "output-div",
            h1(textOutput(ns("map_mcv_status") ))
        )

    ),

    div(class = "row",

        div(class = "output-div",
            h6("Chart 7: Confirmed Measles cases"),
            actionButton(inputId =  ns("map_cases_update"), label = "Update Measles cases data")
        ),


        div(class = "output-div",
            h1(textOutput(ns("map_cases_status") ))
        )

    )

  )
}

#' measles Server Functions
#' @importFrom tidyr separate replace_na
#' @import stringr waiter
#' @importFrom purrr set_names
#' @noRd
mod_measles_server <- function(id ,year_input , month_input ){

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


      shinyFeedback::feedbackSuccess(inputId = "upload", ( disease == "Measles"), "Yes,this is Measles data")

      df <- switch(disease,
                   Measles = sormas_cleaner(input$upload$datapath),
                   shinyFeedback::feedbackDanger(inputId = "upload", ( disease != "Measles"), "Please,this is Not Measles data"))

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

        # dhis2_datasets$measles1_coverage_monthly <- getAnalytics_cleaned(indicator_id = "tb5NGUMB9dQ",
        #                                                   months = month_input(),
        #                                                   year = year_input(),
        #                                                   organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3") )

        ##################

        waitress$inc(10) # increase by 10%

        Sys.sleep(1)

      # dhis2_datasets$measles2_coverage_monthly <- getAnalytics_cleaned(indicator_id = "qagHWllYle8",
      #                                                   months = month_input(),
      #                                                   year = year_input(),
      #                                                   organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))
      #

        waitress$inc(10) # increase by 10%

        Sys.sleep(1)


      # dhis2_datasets$measles1_given <- getAnalytics_cleaned(indicator_id = "j6mCLL1X4yG",
      #                                        months = month_input(),
      #                                        year = year_input(),
      #                                        organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))
      #

        waitress$inc(10) # increase by 10%

        Sys.sleep(1)


      # dhis2_datasets$measles2_given <- getAnalytics_cleaned(indicator_id = "oB7SLTRHciH",
      #                                        months = month_input(),
      #                                        year = year_input(),
      #                                        organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))

        waitress$inc(10) # increase by 10%

        Sys.sleep(1)


      #
      # dhis2_datasets$measles_received <-  getAnalytics_cleaned(indicator_id = "XklyJxzoyxR",
      #                                           months = month_input(),
      #                                           year = year_input(),
      #                                           organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))

        waitress$inc(10) # increase by 10%

        Sys.sleep(1)


      # dhis2_datasets$measles_opening_balance <- getAnalytics_cleaned(indicator_id = "vtHJVTAguch",
      #                                                 months = month_input(),
      #                                                 year = year_input(),
      #                                                 organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))

        waitress$inc(10) # increase by 10%

        Sys.sleep(1)




      # dhis2_datasets$measles_opened_used<- getAnalytics_cleaned(indicator_id = "hg1D2tXg3aB",
      #                                            months = month_input(),
      #                                            year = year_input(),
      #                                            organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))


        waitress$inc(10) # increase by 10%

        Sys.sleep(1)

      #
      # dhis2_datasets$yf_vaccine_given <- getAnalytics_cleaned(indicator_id = "KpKx2MosfIV",
      #                                          months = month_input(),
      #                                          year = year_input(),
      #                                          organisation_uint = c("LEVEL-1", "LEVEL-2","LEVEL-3"))

        waitress$inc(10) # increase by 10%

        Sys.sleep(1)

      # dhis2_datasets$measles1_coverage_annualized_all_time <- getAnalytics_cleaned(indicator_id = "hvYqrmR6MPq",
      #                                                                              months = c(),
      #                                                                              year = 2021:year_input(),
      #                                                                              organisation_uint = "LEVEL-1")

        waitress$inc(10) # increase by 10%

        Sys.sleep(1)

      # dhis2_datasets$measles2_coverage_annualized_all_time <- getAnalytics_cleaned(indicator_id = "Fbj3AkYjCie",
      #                                                                              months = c(),
      #                                                                              year = 2021:year_input(),
      #                                                                              organisation_uint = "LEVEL-1")

      waitress$close()
      })




    ####################Chart 1: Confirmed measles cases, MCV 1 overage,Alt denominator"

    observeEvent(input$alt_update, {



      id <- showNotification("Updating Remote Database....", duration = NULL, closeButton = FALSE, type ="message" )
      on.exit(removeNotification(id), add = TRUE)

      Sys.sleep(2)

    # measles_alt_denominator <- get_measles_alt_denominator(sormas_cleaned = dhis2_datasets$measles_sormas_cleaned,
    #                                                        mcv1 = dhis2_datasets$measles1_coverage_monthly,
    #                                                        mcv2 = dhis2_datasets$measles2_coverage_monthly,
    #                                                        measles1_given = dhis2_datasets$measles1_given,
    #                                                        measles2_given = dhis2_datasets$measles2_given,
    #                                                        denominator_data = dhis2_datasets$denominator_data)

   # dbAppendTable(connection , name = "measles_alt_denominator", value = measles_alt_denominator )


    progress_list$alt_status <- "Updated ✅"


    })


    output$alt_status <- renderText({

       progress_list$alt_status


    })


    #################### Chart 2: Age Group of Confirmed Measles Cases by Vaccination Status

    observeEvent(input$age_update, {

      measles_age_group <- get_age_group(sormas_cleaned = dhis2_datasets$measles_sormas_cleaned)

      dbAppendTable(connection, name = "measles_age_group", value = measles_age_group )

      progress_list$age_status <-  "Updated ✅"


    })


    output$age_status<- renderText({

      progress_list$age_status

    })


    ####################Chart 3: MCV Stock Analysis & MCV (1 & 2) given

    observeEvent(input$stock_update, {


      measles_1_2_given <- add_measles_1_2_given(measles1_given = dhis2_datasets$measles1_given,
                                                 measles2_given = dhis2_datasets$measles2_given)


      measles_stock_analysis <- get_stock_analysis(doses_given = measles_1_2_given,
                                                   received = dhis2_datasets$measles_received,
                                                   opening_balance = dhis2_datasets$measles_opening_balance,
                                                   opened_used = dhis2_datasets$measles_opened_used)


      dbAppendTable(connection, name = "measles_stock_analysis",
                    value = measles_stock_analysis )


      progress_list$stock_status <-  "Updated ✅"


    })


    output$stock_status <- renderText({
      progress_list$stock_status

    })


    ####################Chart 4: Co-administered Antigen Discrepancy: MCV 1 & Yellow Fever given by State

    observeEvent(input$discrepancy_update, {

      measles_yf_discrepancy <- get_discrepancy(main_vaccine_given = dhis2_datasets$measles1_given,
                                                other_vaccine_given = dhis2_datasets$yf_vaccine_given)

      dbAppendTable(connection, name = "measles_yf_discrepancy", value = measles_yf_discrepancy )

      progress_list$discrepancy_status <- "Updated ✅"


    })


    output$discrepancy_status <- renderText({
      progress_list$discrepancy_status

    })


    #################### Chart 5: MCV 1, MCV 2 Coverage & Drop Out Rate

    observeEvent(input$dropout_update, {


      mcv1_mcv2_dropout_rate <- get_drop_out_rate(first_dose  = dhis2_datasets$measles1_coverage_monthly,
                                                  second_dose =  dhis2_datasets$measles2_coverage_monthly)

      dbAppendTable(connection, name = "mcv1_mcv2_dropout_rate", value = mcv1_mcv2_dropout_rate )


      progress_list$dropout_status <-  "Updated ✅"


    })


    output$dropout_status  <- renderText({
      progress_list$dropout_status

    })


    ####################Chart 6: National MCV Coverage (%) by different sources, Nigeria (National)

    observeEvent(input$national_mcv_update, {

      req(dhis2_datasets$measles1_coverage_annualized_all_time, dhis2_datasets$measles2_coverage_annualized_all_time)


      id <- showNotification("Updating the Database...", duration = NULL, closeButton = FALSE, type ="message" )
      on.exit(removeNotification(id), add = TRUE)


      mcv_different_sources <- get_mcv_different_sources(mcv1_annualized_all_time = dhis2_datasets$measles1_coverage_annualized_all_time,
                                                         mcv2_annualized_all_time = dhis2_datasets$measles2_coverage_annualized_all_time)

      dbAppendTable(connection, name = "mcv_different_sources", value = mcv_different_sources)

      progress_list$national_mcv_status <-  "Updated ✅"


    })


    output$national_mcv_status <- renderText({
      progress_list$national_mcv_status

    })

    ####################Chart 7: MCV 1 coverage

    observeEvent(input$map_mcv_update, {


      measles_coverage_map <- get_coverage_map(annual_data = dhis2_datasets$measles1_coverage_annually,
                                               monthly_data = dhis2_datasets$measles1_coverage_monthly)


      dbAppendTable(connection, name = "measles_coverage_map",
                    value = measles_coverage_map )


      progress_list$map_mcv_status <- "Updated ✅"


    })


    output$map_mcv_status <- renderText({
      progress_list$map_mcv_status

    })



    ####################Chart 7: Confirmed Measles cases

    observeEvent(input$map_cases_update, {


      measles_cases_map <- sormas_data()%>%
        select("##Case ID","Disease", "Long", "Lat", "Months", "Year", "State", "LGA")

      dbAppendTable(connection, name = "measles_cases_map", value = measles_cases_map )
      progress_list$map_cases_status <-  "Updated ✅"


    })


    output$map_cases_status <- renderText({
      progress_list$map_cases_status

    })


  })
}

## To be copied in the UI
# mod_measles_ui("measles_1")

## To be copied in the server
# mod_measles_server("measles_1")
