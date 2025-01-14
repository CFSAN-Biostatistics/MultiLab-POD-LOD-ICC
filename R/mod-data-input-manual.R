#----------------------------  Helper functions  -------------------------------

labName <- function(id, lab_number) {
  my_input <- shiny::textInput(id,
    label = paste0("Laboratory ", lab_number, " Name:"),
    value = paste0("Lab ", lab_number)
  )
  shiny::tagAppendAttributes(my_input, class = "lab-name")
}

# library(shiny)
# labName(NS("lab1", "lab_name"), 1)

labNumeric <- function(id, data_type, aria_label) {
  if (data_type == "inoculum") {
    my_step <- 0.01
  } else if (data_type == "ntest") {
    my_step <- 1
  } else if (data_type == "npos") {
    my_step <- 1
  } else {
    stop("Problem with labNumeric() helper function.")
  }
  my_input <- numericInput(
    id, label = "", value = 0, min = 0, step = my_step
  )
  my_input <- shiny::tagAppendAttributes(my_input,
    class = "lab-well-inner-data"
  )
  my_input$children[[2]] <- shiny::tagAppendAttributes(
    my_input$children[[2]], `aria-label` = aria_label
  )
  my_input
}

# labNumeric(NS("lab1", paste0("level", 1)), "inoculum", "my aria label")
# labNumeric(NS("lab1", paste0("ntest", 1)), "ntest", "my aria label")
# labNumeric(NS("lab1", paste0("npos", 1)), "npos", "my aria label")


addFillButtons <- function(column) {
  # Will not have same namespace as the module
  if (column == "inoculum") {
    fill_button <- actionButton("fill_inoc_level",
      label = "Fill other labs", class = "fill-labs",
      `aria-label` = "Fill inoculum level for other labs"
    )
    clear_button <- actionButton("clear_inoc_level",
      label = "Clear other labs", class = "fill-labs",
      `aria-label` = "Clear inoculum level for other labs"
    )
  } else if (column == "tested") {
    fill_button <- actionButton("fill_ntubes",
      label = "Fill other labs", class = "fill-labs",
      `aria-label` = "Fill number of inoculated tubes for other labs"
    )
    clear_button <- actionButton("clear_ntubes",
      label = "Clear other labs", class = "fill-labs",
      `aria-label` = "Clear number of inoculated tubes for other labs"
    )
  } else if (column == "positive") {
    fill_button <- NULL
    clear_button <- actionButton("clear_npos",
      label = "Clear other labs", class = "fill-labs",
      `aria-label` = "Clear number of positive tubes for other labs"
    )
  } else {
    stop("Problem with addFillButtons() helper function.")
  }
  fluidRow(style = "padding-bottom: 20px;",
    fluidRow(fill_button), fluidRow(clear_button)
  )
}

# addFillButtons(column = "inoculum")
# addFillButtons(column = "tested")
# addFillButtons(column = "positive")


#-----------------------  Lab-Level Data module  -------------------------------

labLevelInput <- function(id, max_levels, default_levels) {
  #id: "lab1", "lab2", etc.
  ns <- NS(id)
  lab_number <- gsub("[^0-9]", "", id)
  stopifnot(!is.na(lab_number))
  level_indices <- seq_len(max_levels)
  wellPanel(id = ns("panel"), class = "lab-well",
    labName(ns("lab_name"), lab_number),
    fluidRow(
      column(width = 4, align = "center", class = "lab-well-inner",
        tags$div(style = "padding: 10px;",
          tags$span("Inoculation level", class = "lab-well-column-names"),
          tags$span("(per test portion)", class = "lab-well-column-names")
        ),
        #https://shiny.rstudio.com/gallery/creating-a-ui-from-a-loop.html
        lapply(level_indices, function(i) {
          aria_label <- paste("inoculum level", i, "for lab", lab_number)
          my_input <- labNumeric(ns(paste0("level", i)), "inoculum", aria_label)
          if (i > default_levels) {
            shinyjs::hidden(my_input)
          } else {
            my_input
          }
        }),
        if (lab_number == 1) addFillButtons("inoculum")
      ),
      column(width = 4, align = "center", class = "lab-well-inner",
        tags$div(style = "padding: 10px;",
          tags$span("How many", class = "lab-well-column-names"),
          tags$span("inoculated tubes?", class = "lab-well-column-names")
        ),
        lapply(level_indices, function(i) {
          aria_label <- paste0("number of tubes tested for lab ", lab_number, ", level ", i)
          my_input <- labNumeric(ns(paste0("ntest", i)), "ntest", aria_label)
          if (i > default_levels) {
            shinyjs::hidden(my_input)
          } else {
            my_input
          }
        }),
        if (lab_number == 1) addFillButtons("tested")
      ),
      column(width = 4, align = "center", class = "lab-well-inner",
        tags$div(style = "padding: 10px;",
          tags$span("How many", class = "lab-well-column-names"),
          tags$span("positive tubes?", class = "lab-well-column-names")
        ),
        lapply(level_indices, function(i) {
          aria_label <- paste0("number of positive tubes for lab ", lab_number, ", level ", i)
          my_input <- labNumeric(ns(paste0("npos", i)), "npos", aria_label)
          if (i > default_levels) {
            shinyjs::hidden(my_input)
          } else {
            my_input
          }
        }),
        if (lab_number == 1) addFillButtons("positive")
      )
    )
  )
}

#labLevelInput("lab1", max_levels = 3, default_levels = 2)



labLevelServer <- function(id, chosen_levels) {
  # Returns a reactive data frame.
  lab_number <- as.integer(sub("lab", "", id, fixed = TRUE))
  moduleServer(id,
    function(input, output, session) {
      reactive({
        level_indices <- seq_len(chosen_levels)
        # inoculum level, number of inoculated tubes, number of positive tubes
        inoculum <- vapply(level_indices, FUN.VALUE = 0, function(i) {
          element_id <- paste0("level", i)
          input[[element_id]]  #input[["lab1-level1"]], etc.
        })
        ntest <- vapply(level_indices, FUN.VALUE = 0, function(i) {
          element_id <- paste0("ntest", i)
          input[[element_id]]  #input$lab1_ntest1, etc.
        })
        npos <- vapply(level_indices, FUN.VALUE = 0, function(i) {
          element_id <- paste0("npos", i)
          input[[element_id]]  #input$lab1_npos1, etc.
        })
        data.frame(
          lab_id   = lab_number,
          lab_name = input$lab_name,
          inoculum = inoculum,
          ntest    = ntest,
          npos     = npos
        )
      })
    }
  )
}

# # Unit testing -----------------------------------------------------------------
# library(shiny)
# library(shinyvalidate)
# my_chosen_levels <- 3
# my_max_levels    <- 4
# labLevelApp <- function() {
#   ui <- fluidPage(
#     includeCSS("www/style.css"),
#     shinyjs::useShinyjs(),
#     labLevelInput("lab1", my_max_levels, my_chosen_levels),
#     verbatimTextOutput("my_df")
#   )
#   server <- function(input, output, session) {
#     my_lab_data  <- labLevelServer("lab1", my_chosen_levels)
#     output$my_df <- renderPrint(my_lab_data())
#   }
#   shinyApp(ui, server)
# }
# labLevelApp()



#------------------------  All-labs Data module  -------------------------------

labsInput <- function(id, max_labs, default_labs, max_levels, default_levels) {
  ns <- NS(id)
  tagList(
    labLevelInput(ns("lab1"), max_levels, default_levels),
    lapply(2:max_labs, function(i) {
      lab_box <- labLevelInput(ns(paste0("lab", i)),  #"[id]-lab2", "[id]-lab3", etc.
        max_levels, default_levels
      )
      if (i > default_labs) {
        shinyjs::hidden(lab_box)
      } else {
        lab_box
      }
    })
  )
}

#labsInput("lab_data", max_labs = 2, default_labs = 2, max_levels = 2, default_levels = 2)


labsServer <- function(id, description, max_labs, max_levels, all_panel_names,
    fill_inoc_levels, clear_inoc_levels, fill_ntubes, clear_ntubes,
    fill_npos, clear_npos, calc_button) {

  stopifnot(is.reactive(description$exp_name))
  indices_all_labs   <- seq_len(max_labs)
  indices_other_labs <- indices_all_labs[-1]
  indices_all_levels <- seq_len(max_levels)

  moduleServer(id,
    function(input, output, session) {
      observeEvent(c(description$num_labs(), description$num_levels()), {
        req(description$num_labs() > 0)
        req(description$num_levels() > 0)
        indices_num_labs   <- seq_len(description$num_labs())
        indices_num_levels <- seq_len(description$num_levels())
        panel_names        <- all_panel_names[indices_num_labs]
        for (i in indices_all_labs) {
          if (all_panel_names[i] %in% panel_names) {
            shinyjs::show(all_panel_names[i])
            lab_id <- paste0("lab", i)
            my_levels  <- paste0(lab_id, "-level", indices_num_levels)
            all_levels <- paste0(lab_id, "-level", indices_all_levels)
            all_ntest  <- paste0(lab_id, "-ntest", indices_all_levels)
            all_npos   <- paste0(lab_id, "-npos",  indices_all_levels)
            for (j in indices_all_levels) {
              if (all_levels[j] %in% my_levels) {
                shinyjs::show(all_levels[j])
                shinyjs::show(all_ntest[j])
                shinyjs::show(all_npos[j])
              } else {
                shinyjs::hide(all_levels[j])
                shinyjs::hide(all_ntest[j])
                shinyjs::hide(all_npos[j])
              }
            }
          } else {
            shinyjs::hide(all_panel_names[i])
          }
        }
      })
      observeEvent(fill_inoc_levels(), {
        lab1_inoc_levels <-vapply(indices_all_levels, function(i) {
          input[[paste0("lab1-level", i)]]
        }, FUN.VALUE = 0)
        for (i in indices_other_labs) {
          lab_id <- paste0("lab", i)
          for (j in indices_all_levels) {
            input_name <- paste0(lab_id, "-level", j)
            updateNumericInput(session = session,
              inputId = input_name, value = lab1_inoc_levels[j]
            )
          }
        }
      })
      observeEvent(clear_inoc_levels(), {
        for (i in indices_other_labs) {
          lab_id <- paste0("lab", i)
          for (j in indices_all_levels) {
            input_name <- paste0(lab_id, "-level", j)
            updateNumericInput(session = session,
              inputId = input_name, value = 0
            )
          }
        }
      })
      observeEvent(fill_ntubes(), {
        lab1_inoc_levels <-vapply(indices_all_levels, function(i) {
          input[[paste0("lab1-ntest", i)]]
        }, FUN.VALUE = 0)
        for (i in indices_other_labs) {
          lab_id <- paste0("lab", i)
          for (j in indices_all_levels) {
            input_name <- paste0(lab_id, "-ntest", j)
            updateNumericInput(session = session,
              inputId = input_name, value = lab1_inoc_levels[j]
            )
          }
        }
      })
      observeEvent(clear_ntubes(), {
        for (i in indices_other_labs) {
          lab_id <- paste0("lab", i)
          for (j in indices_all_levels) {
            input_name <- paste0(lab_id, "-ntest", j)
            updateNumericInput(session = session,
              inputId = input_name, value = 0
            )
          }
        }
      })
      observeEvent(clear_npos(), {
        for (i in indices_other_labs) {
          lab_id <- paste0("lab", i)
          for (j in indices_all_levels) {
            input_name <- paste0(lab_id, "-npos", j)
            updateNumericInput(session = session,
              inputId = input_name, value = 0
            )
          }
        }
      })
      labs_data <- reactive({
        calc_button()
        req(description$iv$is_valid())  #don't run analysis if description has problems
        input$fill_inoc_level
        input$clear_inoc_level
        input$fill_ntubes
        input$clear_ntubes
        input$clear_npos
        num_labs         <- description$num_labs()
        indices_num_labs <- seq_len(num_labs)
        num_levels <- description$num_levels()
        sample_size <- description$sample_size()
        sample_size <- as.numeric(sample_size)
        labs_data_list <- lapply(indices_num_labs, function(i) {
          labLevelServer(paste0("lab", i), num_levels)()
        })
        combined_dfs <- dplyr::bind_rows(labs_data_list)
        combined_dfs$inoculum_per_unit <- combined_dfs$inoculum / sample_size
        combined_dfs$sample_size <- sample_size
        combined_dfs
      })
      reactive({
        list(
          data_model     = labs_data,
          exp_name       = description$exp_name,
          exp_date       = description$exp_date,
          matrix         = description$matrix,
          microorganism  = description$microorganism,
          sample_size    = description$sample_size,
          fname_uploaded = reactive(NULL),
          uploaded_data_for_preview = reactive(NULL)
        )
      })
    }
  )
}


# # Testing ----------------------------------------------------------------------
# library(shiny)
# library(shinyvalidate)
# source("R/mod-data-input-manual-description.R")
# my_max_labs       <- 4
# my_min_labs       <- 2
# my_default_labs   <- 2
# my_max_levels     <- 4
# my_min_levels     <- 2
# my_default_levels <- 2
# my_min_size       <- 0
# my_default_size   <- 25
# my_panel_names <- paste0("lab", 1:my_max_labs, "-panel")
#
# labsLevelApp <- function() {
#   ui <- fluidPage(
#     includeCSS("www/style.css"),
#     shinyjs::useShinyjs(),
#     actionButton("click_me", label = "Click me"),
#     experimentDescriptionInput("my_description", title = "Experiment Description",
#       my_default_labs, my_min_labs, my_max_labs,
#       my_default_levels, my_min_levels, my_max_levels,
#       my_default_size, my_min_size
#     ),
#     verbatimTextOutput("my_list"),
#     labsInput("lab_data", my_max_labs, my_default_labs, my_max_levels, my_default_levels)
#   )
#   server <- function(input, output, session) {
#     my_description <- experimentDescriptionServer("my_description",
#       my_min_labs, my_max_labs, my_min_levels, my_max_levels, my_min_size
#     )
#     my_description$iv$enable()
#     my_data <- labsServer("lab_data", my_description,
#       my_max_labs, my_max_levels, my_panel_names,
#       reactive(input$fill_inoc_level), reactive(input$clear_inoc_level),
#       reactive(input$fill_ntubes), reactive(input$clear_ntubes),
#       reactive(input$fill_npos), reactive(input$clear_npos),
#       reactive(input$click_me)
#     )
#     output$my_list <- renderPrint({
#       dat <- my_data()
#       list(
#         data_model     = dat$data_model(),
#         exp_name       = dat$exp_name(),
#         exp_date       = dat$exp_date(),
#         matrix         = dat$matrix(),
#         microorganism  = dat$microorganism(),
#         sample_size    = dat$sample_size(),
#         fname_uploaded = dat$fname_uploaded(),
#         uploaded_data_for_preview = dat$uploaded_data_for_preview()
#       )
#     })
#   }
#   shinyApp(ui, server)
# }
# labsLevelApp()
