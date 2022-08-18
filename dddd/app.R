library(shiny)
library(waiter)

ui <- fluidPage(
  useWaiter(),
  useHostess(),
  waiterShowOnLoad(
    color = "#f7fff7",
    hostess_loader(
      "loader",
      preset = "circle",
      text_color = "black",
      class = "label-center",
      center_page = TRUE
    )
  )
)

server <- function(input, output){
  hostess <- Hostess$new("loader", infinite = TRUE)

  hostess$start()

  # ... computation here ... #
  Sys.sleep(5) # simulating a 5 seconds computation

  hostess$close()
  waiter_hide()
}

shinyApp(ui, server)
