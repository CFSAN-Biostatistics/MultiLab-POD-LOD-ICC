#---------------  Choose LOD Unit & Calculate buttons  -------------------------

runAnalysisInput <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("lod_unit"),
      label = "Please choose LOD unit",
      choices = c("CFU/test portion", "CFU/g", "CFU/mL"),
      selected = "CFU/test portion", inline = TRUE
    ),
    actionButton(ns("calculate"),
      label = "Calculate", icon = icon2("calculator"),
      class = "calculate-button"
    )
  )
}

runAnalysisServer <- function(id, sidebar_select, choose_data_entry) {
  stopifnot(is.reactive(sidebar_select$lod_choice))
  stopifnot(is.reactive(choose_data_entry))
  moduleServer(id,
    function(input, output, session) {
      eventReactive(input$calculate, {
        list(
          date_time = strftime(Sys.time(), format = "", tz = "", usetz = TRUE),
          choose_data_entry = choose_data_entry(),
          lod_unit   = input$lod_unit,
          lod_choice = sidebar_select$lod_choice(),
          lod_perc   = sidebar_select$lod_perc(),
          lod_prob   = sidebar_select$lod_prob(),
          conf_level = sidebar_select$conf_level(),
          conf_level_prob = sidebar_select$conf_level_prob()
        )
      })
    }
  )
}


# # For testing ------------------------------------------------------------------
# library(shiny)
# source("R/mod-sidebar-select.R")
# source("R/mod-data-choices.R")
# source("R/helpers-element-modifications.R")
# runAnalysisApp <- function() {
#   ui <- fluidPage(
#     includeCSS("www/style.css"),
#     shinyjs::useShinyjs(),
#     fluidRow(dataChoicesInput("choose_data_entry")),
#     fluidRow(sidebarSelectInput("my_id1")),
#     verbatimTextOutput("my_df1"),
#     fluidRow(
#       column(width = 4, align = "center",
#         runAnalysisInput("my_id2")
#       )
#     ),
#     verbatimTextOutput("my_df2")
#   )
#   server <- function(input, output, session) {
#     choose_data_entry <- dataChoicesServer("choose_data_entry")
#     my_sidebar_select <- sidebarSelectServer("my_id1")
#     output$my_df1 <- renderPrint({
#       list(
#         lod_choice      = my_sidebar_select$lod_choice(),
#         lod_perc        = my_sidebar_select$lod_perc(),
#         lod_prob        = my_sidebar_select$lod_prob(),
#         conf_level      = my_sidebar_select$conf_level(),
#         conf_level_prob = my_sidebar_select$conf_level_prob()
#       )
#     })
#     my_calc <- runAnalysisServer("my_id2", my_sidebar_select, choose_data_entry)
#     output$my_df2 <- renderPrint({
#       req(my_calc())
#       my_calc()
#     })
#   }
#   shinyApp(ui, server)
# }
# runAnalysisApp()
