
#-----------------------------  Helper functions  ------------------------------

selectInput_sidebar <- function(my_id, label, choices, selected = NULL) {
  my_dropdown <- selectInput(my_id, label = label,
    choices = choices, selected = selected,
    selectize = FALSE
  )
  tagAppendAttributes(my_dropdown, class = "sidebar-select")
}

#selectInput_sidebar("my_id", label = "My Label", choices = 1:3, selected = 2)


#------------------------  Sidebar select dropdowns  ---------------------------

sidebarSelectInput <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput_sidebar(ns("lod_choice"), label = "Choose LOD",
      choices = c("LOD50" = "50"), selected = "LOD50"
    ),
    selectInput_sidebar(ns("conf_level"), label = "Confidence level",
      choices = c("90%", "95%"), selected = "95%"
    )
  )
}

#sidebarSelectInput("my_id")


sidebarSelectServer <- function(id) {
  moduleServer(id,
    function(input, output, session) {
      list(
        lod_choice      = reactive(input$lod_choice),
        lod_perc        = reactive(as.numeric(input$lod_choice)),
        lod_prob        = reactive(as.numeric(input$lod_choice) / 100),
        conf_level      = reactive(input$conf_level),
        conf_level_prob = reactive(as.numeric(gsub("%", "", input$conf_level)) / 100)
      )
    }
  )
}

# # For testing ------------------------------------------------------------------
# library(shiny)
# sidebarSelectApp <- function() {
#   ui <- fluidPage(
#     includeCSS("www/style.css"),
#     shinyjs::useShinyjs(),
#     sidebarSelectInput("my_id"),
#     verbatimTextOutput("my_choices")
#   )
#   server <- function(input, output, session) {
#     my_values <- sidebarSelectServer("my_id")
#     output$my_choices <- renderPrint(
#       list(
#         lod_choice      = my_values$lod_choice(),
#         lod_perc        = my_values$lod_perc(),
#         lod_prob        = my_values$lod_prob(),
#         conf_level      = my_values$conf_level(),
#         conf_level_prob = my_values$conf_level_prob()
#       )
#     )
#   }
#   shinyApp(ui, server)
# }
# sidebarSelectApp()
