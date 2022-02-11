
#https://mastering-shiny.org/scaling-modules.html#scaling-modules


#------------------------  Data entry type buttons  ----------------------------

dataChoicesInput <- function(id, label = "Please choose data entry method",
                            choices = c("Manual entry", "File upload", "Example data"),
                            selected = "Manual entry",
                            status = "data-choices-button",
                            size = "lg", individual = TRUE,
                            check_icon = list(yes = tags$i(class = "fa fa-check"))
                            ) {
  ns <- NS(id)
  my_button <- shinyWidgets::radioGroupButtons(
    ns("radio_group"),
    label = label, choices = choices, selected = selected, status = status,
    size = size, individual = individual, checkIcon = check_icon
  )
  my_button$children[[1]] <- tagAppendAttributes(
    my_button$children[[1]], class = "data-choices-label"
  )
  my_button
}

dataChoicesServer <- function(id) {
  moduleServer(id,
    function(input, output, session) {
      reactive({
        shinyjs::enable("calculate-calculate", asis = TRUE)
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
