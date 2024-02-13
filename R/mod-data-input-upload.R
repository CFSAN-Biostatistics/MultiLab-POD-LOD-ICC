#https://mastering-shiny.org/scaling-modules.html#scaling-modules

#--------------------------  Data upload button  -------------------------------

uploadDataUI <- function(id, label = "Upload Excel file (.xlsx)",
                         multiple = FALSE, accept = ".xlsx", width = "65%",
                         button_label = span("Browse files", class = "upload-browse"),
                         placeholder = "No file selected") {
  # https://stackoverflow.com/questions/62220495/r-shiny-restrict-fileinput-to-filename-pattern-and-not-just-file-type
  # https://stackoverflow.com/questions/190852/how-can-i-get-file-extensions-with-javascript
  ns <- NS(id)
  onchange <- "enforceFileExtension(this)"
  my_input <- shiny::fileInput(
    inputId = ns("upload_data"), label = label,
    multiple = multiple, accept = accept,
    width = width, buttonLabel = button_label, placeholder = placeholder
  )
  my_input$children[[2]]$children[[1]]$children[[1]]$children[[2]]$attribs$onchange <- onchange
  my_input$children[[2]]$children[[2]]$attribs$`aria-label` <- "Uploaded file"
  tagList(
    div(class = "download-template",
      "Please",
      a("download the Excel (.xlsx) template",
        href = "MultiLab-POD-LOD-ICC-template.xlsx",
        download = NA, class = "template-link"
      ),
      " to your hard drive and add your data.",
      p("Then save the file and upload for processing.")
    ),
    my_input,
    tags$h2("Data Preview"),
    shinydashboard::box(width = 12,
      htmlOutput(ns("uploaded_test_portion"),
        style = "font-size: 22px; margin-bottom: 5px;"
      ),
      tableOutput(ns("uploaded_data_preview"))
    )
  )
}

#https://shiny.rstudio.com/articles/modules.html
uploadDataServer <- function(id, calc_button) {
  moduleServer(id,
    function(input, output, session) {

      observeEvent(calc_button(), {
        if (calc_button()$choose_data_entry == "File upload") {
          validateUploadExistence(!is.null(input$upload_data), session)
        }
      })

      uploaded_file <- reactive({
        req(input$upload_data)
        input$upload_data
      })

      my_list <- eventReactive(uploaded_file(), {
        # A list of reactives to return
        file_name <- uploaded_file()$name
        validateExtension(file_name, session)  #helpers-validate-inputs.R
        workbook <- uploaded_file()$datapath
        validateUploadSheetNames(workbook, session)
        description <- openxlsx::read.xlsx(workbook, sheet = "description",
          sep.names = "_", rows = 1:2, cols = 1:5
        )
        validateUploadTestPortionSize(description, session = session)
        inoc_levels <- openxlsx::read.xlsx(workbook, sheet = "inoculation-levels",
          sep.names = "_", rows = 1:2, cols = 2:13
        )
        inoc_levels <- inoc_levels[, !is.na(inoc_levels)]
        counts <- openxlsx::read.xlsx(workbook, sheet = "counts",
          sep.names = "_", rows = 1:31, cols = 1:26
        )
        counts <- counts[, colSums(!is.na(counts)) > 0]  #drop columns with only NA
        counts <- counts[rowSums(!is.na(counts[, 3:ncol(counts)])) > 0, ]  #drop rows with only NA
        validateUploadMinimumLabs(counts, session)
        validateUploadDimensions(counts, inoc_levels, session)
        validateUploadCounts(counts, session)
        counts_n <- dplyr::select(counts,
          Lab_Number, Lab_Name, dplyr::starts_with("n", ignore.case = FALSE)
        )
        counts_n <- tidyr::pivot_longer(counts_n,
          cols = dplyr::starts_with("n", ignore.case = FALSE),
          names_to = "var_n", values_to = "n"
        )
        counts_n <- dplyr::rename(counts_n, d = var_n)
        counts_n$d <- sub("n", replacement = "d", x = counts_n$d, ignore.case = FALSE)
        counts_y <- dplyr::select(counts,
          Lab_Number, Lab_Name, dplyr::starts_with("y", ignore.case = FALSE)
        )
        counts_y <- tidyr::pivot_longer(counts_y,
          cols = dplyr::starts_with("y", ignore.case = FALSE),
          names_to = "var_y", values_to = "y"
        )
        counts_y <- dplyr::rename(counts_y, d = var_y)
        counts_y$d <- sub("y", replacement = "d", x = counts_y$d, ignore.case = FALSE)
        counts2 <- dplyr::left_join(counts_n, counts_y, by = c("Lab_Number", "Lab_Name", "d"))
        dils <- data.frame(
          d = colnames(inoc_levels),
          inoc_level = as.numeric(inoc_levels)
        )
        validateUploadDilutions(dils, session = session)
        counts2 <- dplyr::left_join(counts2, dils, by = "d")
        counts2 <- dplyr::select(counts2, Lab_Number:d, inoc_level, n, y)
        counts2$sample_size <- description$`Test_portion_size_(g_or_mL)`
        counts2 <- dplyr::select(counts2, -d)
        counts2 <- dplyr::rename(counts2,
          lab_id = Lab_Number, lab_name = Lab_Name, inoculum = inoc_level,
          ntest  = n, npos = y
        )
        counts2 <- as.data.frame(counts2)
        sample_size <- as.numeric(counts2$sample_size)
        inoculum    <- as.numeric(counts2$inoculum)
        counts2$lab_id      <- as.integer(counts2$lab_id)
        counts2$inoculum    <- inoculum
        counts2$inoculum_per_unit <- inoculum / sample_size
        counts2$ntest       <- as.integer(counts2$ntest)
        counts2$npos        <- as.integer(counts2$npos)
        counts2$sample_size <- sample_size
        counts2 <- na.omit(counts2)
        counts2 <- dplyr::select(counts2,
          lab_id, lab_name, inoculum, inoculum_per_unit, ntest, npos, sample_size
        )
        validateData(counts2, session = session)
        uploaded_data_for_preview <- counts2
        uploaded_data_for_preview$sample_size <- NULL
        colnames(uploaded_data_for_preview) <-
          c(
            "Lab ID", "Lab Name", "Level Per Portion", "Level Per Unit",
            "Inoculated Tubes", "Positive Tubes"
          )
        list(
          iv             = NULL,
          data_model     = reactive(counts2),
          exp_name       = reactive(description$Experiment_name),
          exp_date       = reactive(description$Experiment_date),
          matrix         = reactive(description$Food_matrix),
          microorganism  = reactive(description$Microorganism),
          sample_size    = reactive(description$`Test_portion_size_(g_or_mL)`),
          fname_uploaded = reactive(file_name),
          uploaded_data_for_preview = reactive(uploaded_data_for_preview)
        )
      })

      observeEvent(my_list(), {
        output$uploaded_test_portion <- renderUI({
          test_portion_size <- my_list()$sample_size()
          paste("Test portion size:", test_portion_size, "g or mL")
        })
        output$uploaded_data_preview <- renderTable({
          my_list()$uploaded_data_for_preview()
          }, striped = TRUE, bordered = TRUE, align = 'c', caption = "Uploaded data preview"
        )
      })

      my_list

    }
  )
}

# # For testing ------------------------------------------------------------------
# library(shiny)
# source("R/helpers-validate-inputs.R")
# uploadDataApp <- function() {
#   ui <- fluidPage(
#     includeCSS("www/style.css"),
#     shinyjs::useShinyjs(),
#     fluidRow(align = "center", uploadDataUI("my_id")),
#     verbatimTextOutput("my_list")
#   )
#   server <- function(input, output, session) {
#     my_data <- uploadDataServer("my_id")
#     output$my_list <- renderPrint({
#       my_data <- my_data()  #note this was a reactive list of reactive objects
#       list(
#         iv             = my_data$iv,
#         data_model     = my_data$data_model(),
#         exp_name       = my_data$exp_name(),
#         exp_date       = my_data$exp_date(),
#         matrix         = my_data$matrix(),
#         microorganism  = my_data$microorganism(),
#         sample_size    = my_data$sample_size(),
#         fname_uploaded = my_data$fname_uploaded(),
#         uploaded_data_for_preview = my_data$uploaded_data_for_preview()
#       )
#     })
#   }
#   shinyApp(ui, server)
# }
# uploadDataApp()
