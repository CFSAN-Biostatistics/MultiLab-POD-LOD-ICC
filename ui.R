
################################  Sidebar  #####################################

my_sidebar <- sidebarMenu(id = "my_tabs",
  menuItem("Calculator", tabName = "calculator",
    icon = icon("calculator", lib = "font-awesome")
  ),
  menuItem("Results", tabName = "results",
    icon = icon("chart-line", lib = "font-awesome")
  ),
  menuItem("Analysis Details", tabName = "analysis_details",
    icon = icon("info", lib = "font-awesome")
  ),
  menuItem("References", tabName = "references",
    icon = icon("info", lib = "font-awesome")
  ),
  hr(),
  fluidRow(
    column(width = 12, align = "center",
      sidebarSelectInput("sidebar_select")
    )
  ),
  hr(),
  h4(em("Authors: "), "S. Wang, J. Ihrie", align = "center"),
  h4(em("App version: "), glob_app_version, align = "center"),
  hr(),
  div(id = "mail_to",
    "Please",
    a("email John Ihrie",
      href = "mailto:John.Ihrie@fda.hhs.gov", class = "email-link"
    ),
    "with suggestions or bug reports."
  )
)

##############################  Calculator  ####################################

my_calculator <- tabItem(tabName = "calculator",
  fluidRow(
    column(width = 8,
      fluidRow(id = "data_choices_div", align = "center",
        dataChoicesInput("choose_data_entry")
      ),
      #tableOutput("data_model"),  #for testing only
      #verbatimTextOutput("data_object"),  #for testing only
      #verbatimTextOutput("model_results"),  #for testing only
      #htmlOutput("warning_messages_results_UI"),  #for testing only (should be in Results)
      fluidRow(id = "manual_data_panel",
        experimentDescriptionInput("experiment_description",
          title = "Experiment Description",
          glob_default_labs, glob_min_labs, glob_max_labs,
          glob_default_levels, glob_min_levels, glob_max_levels,
          glob_default_size, glob_min_size
        ),
        #tableOutput("manual_test_table"),  #for testing only
        fluidRow(align = "center",
          span("Lab-Level Data", class = "description-title")
        ),
        labsInput("lab_data",
          glob_max_labs, glob_default_labs, glob_max_levels, glob_default_levels
        )
      ),
      shinyjs::hidden(
        fluidRow(id = "upload_data_panel", align = "center",
          uploadDataUI("upload_file")
        )
      ),
      shinyjs::hidden(
        fluidRow(id = "example_data_panel", align = "center",
          shinydashboard::box(width = 12,
            span(
              "Test portion size:", glob_sample_size_example, glob_sample_unit_example,
              style = "font-size: 20px;"
            ),
            tableOutput("example_data")
          )
        )
      )
    ),
    column(width = 4, align = "left", style = "min-width: 350px;",
      fluidRow(
        column(width = 10, align = "center", offset = 1,
          runAnalysisInput("calculate")
        )
      ),
      notes,
      data_entry_instructions,
      analysis_instructions
    )
  )
)

#################################  Results  ####################################

my_results <- tabItem(tabName = "results",
  fluidRow(
    column(width = 6, h2(strong("Results"))),
    column(width = 3,
      shinyjs::disabled(
        downloadResultsUI("spreadsheet")
      )
    )
  ),
  tabsetPanel(id = "mod",
    type = "tabs",
    tabPanel(
      title = HTML("<strong>Model Parameters & LOD</strong>"),
      value = "model_params", icon = icon("table", lib = "font-awesome"),
      column(width = 12,
        fluidRow(
          shinydashboard::box(
            title = div("Model Parameter Estimates", class = "results-title"),
            width = 12,
            fluidRow(
              column(width = 12, align = "center",
                valueBoxOutput("log_mean_effect", width = 3),
                valueBoxOutput("se_log_mean_effect", width = 3),
                valueBoxOutput("sigma", width = 3),
                valueBoxOutput("ICC", width = 3)
              )
            )
          )
        ),
        fluidRow(
          shinydashboard::box(
            title = div("Level of Detection", class = "results-title"),
            width = 9,
            fluidRow(
              column(width = 12, align = "center",
                valueBoxOutput("LOD", width = 4),
                valueBoxOutput("LOD_LCL", width = 4),
                valueBoxOutput("LOD_UCL", width = 4)
              )
            )
          )
        ),
        fluidRow(htmlOutput("warning_messages_results_UI"))
      )
    ),
    tabPanel(
      title = HTML("<strong>POD Curves</strong>"), value = "pod_curves",
      icon = icon("chart-line", lib = "font-awesome"),
      plotOutput("POD_plots", width = "75%", height = "650px")
    )
  )
)

#########################  Dashboard layout  ###################################

ui <- dashboardPage(
  dashboardHeader(title = glob_app_title, titleWidth = 275),
  dashboardSidebar(my_sidebar, width = 275),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    shinyWidgets::useSweetAlert(),
    tags$html(lang = "en-US"),
    #https://stackoverflow.com/questions/45706670/shiny-dashboadpage-lock-dashboardheader-on-top
    tags$script(HTML("$('body').addClass('fixed');")),
    tags$head(
      #https://stackoverflow.com/questions/51294246/how-can-i-include-meta-tags-in-my-r-shiny-app
      tags$meta(charset = "utf-8"),
      tags$meta(name = "description",
        content = paste(
          "The MultiLab POD/LOD/ICC R Shiny app is for interlaboratory",
          "microbiological method validation studies. This app implements the",
          "random intercept complementary log-log model suggested by Jarvis",
          "et al. (2019) to estimate probability of detection (POD) and level",
          "of detection (LOD) from a multi-laboratory validation study for a",
          "qualitative (binary) microbiological assay. This app also",
          "calculates the intra-laboratory correlation coefficient (ICC) to",
          "estimate the proportion of total variance attributable to",
          "between-laboratory variance."
        )
      ),
      tags$script(withMathJax()),
      #includeCSS("www/style.css"),  #for code development only!!!
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),  #for production
      #includeScript("www/jscript.js", defer = "defer")  #for code development only!!!
      tags$script(src = "jscript.js", defer = "defer")  #for production
    ),
    tabItems(
      my_calculator,
      my_results,
      my_analysis_details,
      my_references
    )
  ),
  title = glob_app_title
)
################################################################################
