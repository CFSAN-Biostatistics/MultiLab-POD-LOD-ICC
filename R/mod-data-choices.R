
#https://mastering-shiny.org/scaling-modules.html#scaling-modules


#------------------------  Data entry type buttons  ----------------------------

dataChoicesInput <- function(id, label = "Please choose data entry method",
                            choices = c("Manual entry", "File upload", "Example data"),
                            selected = "Manual entry",
                            size = "lg", individual = TRUE,
                            check_icon = list(yes = tags$i(class = "fa fa-check"))
                            ) {
  #Allows adding CSS to button text (class 'choose-data-entry-text')
  ns <- NS(id)
  my_button <- shinyWidgets::radioGroupButtons(
    ns("radio_group"),
    label = label, choices = choices, selected = selected, size = size,
    individual = individual, checkIcon = check_icon
  )
  my_button$children[[1]] <- tagAppendAttributes(
    my_button$children[[1]], class = "data-choices-label"
  )
  my_button$children[[3]]$children[[1]]$children[[1]][[1]] <- tagAppendAttributes(
    my_button$children[[3]]$children[[1]]$children[[1]][[1]],
    class = "data-choices-button"
  )
  my_button$children[[3]]$children[[1]]$children[[1]][[2]] <- tagAppendAttributes(
    my_button$children[[3]]$children[[1]]$children[[1]][[2]],
    class = "data-choices-button"
  )
  my_button$children[[3]]$children[[1]]$children[[1]][[3]] <- tagAppendAttributes(
    my_button$children[[3]]$children[[1]]$children[[1]][[3]],
    class = "data-choices-button"
  )
  # Add <span> elements to existing text & set class
  choice_base <- my_button$children[[3]]$children[[1]]$children[[1]]
  choice1 <- choice_base[[1]]$children[[1]]$children[[4]]
  choice2 <- choice_base[[2]]$children[[1]]$children[[4]]
  choice3 <- choice_base[[3]]$children[[1]]$children[[4]]
  choice1 <- HTML(paste0("<span class='choose-data-entry-text'>", choice1, "</span>"))
  choice2 <- HTML(paste0("<span class='choose-data-entry-text'>", choice2, "</span>"))
  choice3 <- HTML(paste0("<span class='choose-data-entry-text'>", choice3, "</span>"))
  my_button$children[[3]]$children[[1]]$children[[1]][[1]]$children[[1]]$children[[4]] <- choice1
  my_button$children[[3]]$children[[1]]$children[[1]][[2]]$children[[1]]$children[[4]] <- choice2
  my_button$children[[3]]$children[[1]]$children[[1]][[3]]$children[[1]]$children[[4]] <- choice3
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
