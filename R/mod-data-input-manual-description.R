#---------------------  Experiment Description module  -------------------------

experimentDescriptionInput <- function(id,
    title = "Experiment Description",
    default_labs, min_labs, max_labs, default_levels, min_levels, max_levels,
    default_size, min_size) {

  ns <- NS(id)
  tagList(
    tags$h2(title, class = "description-title", align = "center"),
    wellPanel(class = "experiment-description",
      fluidRow(
        column(width = 6, align = "center", class = "experiment-description-inner",
          wellPanel(class = "inside-panel",
            textInput(ns("exp_name"), label = "Experiment name"),
            textInput(ns("exp_date"), label = "Experiment date"),
            textInput(ns("matrix"),   label = "Food matrix"),
            textInput(ns("microorganism"), label = "Microorganism")
          )
        ),
        column(width = 6, align = "center", class = "experiment-description-inner",
          wellPanel(class = "inside-panel",
            numericInput(
              ns("num_labs"), label = "How many labs?",
              value = default_labs, min = min_labs, max = max_labs
            ),
            numericInput(
              ns("num_levels"), label = "How many inoculum levels?",
              value = default_levels, min = min_levels, max = max_levels
            ),
            numericInput(
              ns("sample_size"), label = "Test portion size (g or mL)",
              value = default_size, min = min_size
            )
          )
        )
      )
    )
  )
}


experimentDescriptionServer <- function(id, min_labs, max_labs, min_levels, max_levels, min_size) {
  moduleServer(id,
    function(input, output, session) {
      iv <- InputValidator$new()  #'shinyvalidate' package
      iv$add_rule("num_labs",    sv_required())
      iv$add_rule("num_levels",  sv_required())
      iv$add_rule("sample_size", sv_required())
      iv$add_rule("num_labs",    sv_integer())
      iv$add_rule("num_levels",  sv_integer())
      iv$add_rule("sample_size", sv_numeric())
      iv$add_rule("num_labs",    sv_gte(min_labs))
      iv$add_rule("num_levels",  sv_gte(min_levels))
      iv$add_rule("sample_size", sv_gt(min_size))
      iv$add_rule("num_labs",    sv_lte(max_labs))
      iv$add_rule("num_levels",  sv_lte(max_levels))
      list(
        iv            = iv,
        exp_name      = reactive(input$exp_name),
        exp_date      = reactive(input$exp_date),
        matrix        = reactive(input$matrix),
        microorganism = reactive(input$microorganism),
        num_labs      = reactive(input$num_labs),
        num_levels    = reactive(input$num_levels),
        sample_size   = reactive(input$sample_size)
      )
    }
  )
}


# # For testing ------------------------------------------------------------------
# library(shiny)
# library(shinyvalidate)
# source("R/helpers-validate-inputs.R")
# my_default_labs <- 2
# my_min_labs     <- 2
# my_max_labs     <- 4
# my_default_levels <- 3
# my_min_levels     <- 2
# my_max_levels     <- 4
# my_default_size <- 25
# my_min_size     <- 0
#
# experimentDescriptionApp <- function() {
#   ui <- fluidPage(
#     includeCSS("www/style.css"),
#     shinyjs::useShinyjs(),
#     experimentDescriptionInput("my_id", title = "Experiment Description",
#       my_default_labs, my_min_labs, my_max_labs,
#       my_default_levels, my_min_levels, my_max_levels,
#       my_default_size, my_min_size
#     ),
#     verbatimTextOutput("my_input")
#   )
#   server <- function(input, output, session) {
#     my_data <- experimentDescriptionServer("my_id",
#       my_min_labs, my_max_labs, my_min_levels, my_max_levels, my_min_size
#     )
#     my_data$iv$enable()
#     output$my_input <- renderPrint(
#       list(
#         iv            = my_data$iv,
#         exp_name      = my_data$exp_name(),
#         exp_date      = my_data$exp_date(),
#         matrix        = my_data$matrix(),
#         microorganism = my_data$microorganism(),
#         num_labs      = my_data$num_labs(),
#         num_levels    = my_data$num_levels(),
#         sample_size   = my_data$sample_size()
#       )
#     )
#   }
#   shinyApp(ui, server)
# }
# experimentDescriptionApp()
