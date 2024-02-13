#https://mastering-shiny.org/scaling-modules.html#scaling-modules

#------------------------  Data entry type buttons  ----------------------------

dataChoicesInput <- function(id, label = "Please choose data entry method",
                            choices = c("Manual entry", "File upload", "Example data"),
                            selected = "Manual entry", inline = TRUE
                            ) {
  ns <- NS(id)
  radioButtons(ns("radio_group"),
    label = label, choices = choices, selected = selected, inline = inline
  )
}

dataChoicesServer <- function(id) {
  moduleServer(id,
    function(input, output, session) {
      reactive({
        input$radio_group
      })
    }
  )
}


# # For testing ------------------------------------------------------------------
# library(shiny)
# dataChoicesApp <- function() {
#   ui <- fluidPage(
#     includeCSS("www/style.css"),
#     dataChoicesInput("choose_data_entry"),
#     textOutput("my_choices")
#   )
#   server <- function(input, output, session) {
#     my_choices <- dataChoicesServer("choose_data_entry")
#     output$my_choices <- renderText(my_choices())
#   }
#   shinyApp(ui, server)
# }
# dataChoicesApp()
