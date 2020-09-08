
################################  Sidebar  #####################################

my_sidebar <- sidebarMenu(
  id = "my_tabs",
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert(),
  menuItem("Calculator", tabName = "calculator",
    icon = icon("calculator", lib = "font-awesome")
  ),
  menuItem("Results", tabName = "results",
    icon = icon("line-chart", lib = "font-awesome")
  ),
  menuItem("Analysis Details", tabName = "analysis_details",
           icon = icon("info", lib = "font-awesome")
  ),
  menuItem("References", tabName = "references",
    icon = icon("info", lib = "font-awesome")
  ),
  hr(),
  fluidRow(
    column(width = 2),
    column(width = 8, align = "center",
      selectInput("lod_choice", label = "Choose LOD",
        width = "100%",
        choices = c("LOD25", "LOD50", "LOD75"),
        selected = "LOD50"
      )
    )
  ),
  fluidRow(
    column(width = 2),
    column(width = 8, align = "center",
      selectInput("conf_level", label = "Confidence level",
        width = "100%",
        choices = c("80%", "90%", "95%", "99%"),
        selected = "95%"
      )
    )
  ),
  hr(),
  # # -----------------------------  for testing only  ---------------------------
  # fluidRow(
  #   column(width = 1),
  #   column(width = 10, align = "left",
  #     selectInput("choose_model", label = "For app development only...",
  #       width = "100%",
  #       choices = c("prefer random intercept", "fixed effects", "random intercept"),
  #       selected = "prefer random intercept"
  #     )
  #   )
  # ),
  # hr(),
  # # ----------------------------------------------------------------------------
  h4(em("Authors: "), "S. Wang, J. Ihrie", align = "center"),
  h4(em("App version: "), glob_app_version, align = "center"),
  hr(),
  em(
    h5("Please send suggestions or bug reports to", align = "center"),
    h5(
      a("John.Ihrie@fda.hhs.gov",
        href = paste0(
          "mailto:John.Ihrie@fda.hhs.gov",
          "?subject=", glob_app_title, " bug/suggestion"
        )
      ), align = "center"
    )
  )

)


##############################  Calculator  ####################################

my_calculator <- tabItem(
  tabName = "calculator",

  fluidRow(

    # Experiment Description
    column(width = 8, align = "center",

      wellPanel(
        style = "color:white; background:midnightblue; border-color:black; border-width:3px",
        h3(strong("Experiment Description")),
        br(),
        tags$style(
          #placeholder fonts
          "#exp_name::placeholder {color:dimgrey; font-size:16px; text-align:center}",
          "#microorganism::placeholder {color:dimgrey; font-size:16px; text-align:center}",
          "#sample_size::placeholder {color:dimgrey; font-size:16px; text-align:center}",
          "#matrix::placeholder {color:dimgrey; font-size:16px; text-align:center}",
          "#exp_date::placeholder {color:dimgrey; font-size:16px; text-align:center}",
          #typed text font & background
          "#exp_name {background-color:white; color:black; font-size:16px; text-align:center}",
          "#microorganism {background-color:white; color:black; font-size:16px; text-align:center}",
          "#sample_size {background-color:white; color:black; font-size:16px; text-align:center}",
          "#matrix {background-color:white; color:black; font-size:16px; text-align:center}",
          "#num_labs {background-color:white; color:black; font-size:16px; text-align:center}",
          "#exp_date {background-color:white; color:black; font-size:16px; text-align:center}",
          "#num_levels {background-color:white; color:black; font-size:16px; text-align:center}"
        ),
        fluidRow(

          column(width = 7, align = "center",
            wellPanel(
              style = "color:white; background:steelblue",
              textInput("exp_name",
                label = div(style = "font-size:18px", "Experiment name"),
                width = "100%", placeholder = "< experiment name >"
              ),
              textInput("exp_date",
                label = div(style = "font-size:18px", "Experiment date"),
                width = "100%", placeholder = "< experiment date >"
              ),
              textInput("matrix",
                label = div(style = "font-size:18px", "Food Matrix"),
                width = "100%", placeholder = "< food matrix >"
              ),
              textInput("microorganism",
                label = div(style = "font-size:18px", "Microorganism"),
                width = "100%", placeholder = "< microorganism >"
              )
            )
          ),
          column(width = 5, align = "center",
            wellPanel(
              style = "color:white; background:steelblue;",
              numericInput("num_labs",
                label = div(
                  style = "font-size:18px",
                  "How many labs?"
                ),
                width = "100%", value = glob_default_labs,
                min = glob_min_labs, max = glob_max_labs
              ),
              numericInput("num_levels",
                label = div(
                  style = "font-size:18px",
                  "How many inoculum levels?"
                ),
                width = "100%", value = glob_default_levels,
                min = glob_min_levels, max = glob_max_levels
              ),
              numericInput("sample_size",
                label = div(
                  style = "font-size:18px",
                  HTML("Sample size")
                ),
                width = "100%", value = glob_default_size,
                min = glob_min_size
              ),
              shinyWidgets::radioGroupButtons(
                inputId = "sample_unit",
                label = div(
                  style = "font-size:18px",
                  "Sample unit"
                ),
                choices = c("g", "mL"),
                size = "lg",
                individual = TRUE,
                checkIcon = list(
                  yes = tags$i(
                    class = "fa fa-circle",
                    style = "color: steelblue"
                  ),
                  no = tags$i(
                    class = "fa fa-circle-o",
                    style = "color: steelblue")
                )
              )
            )
          )
        )
      ),

      shinyWidgets::switchInput(
        inputId = "use_example",
        label = "Use example data?",
        value = FALSE,
        onLabel = "Yes",
        offLabel = "No",
        onStatus = "success",
        offStatus = "danger",
        size = "large",
        labelWidth = "150px",
        handleWidth = "50px",
        disabled = FALSE,
        inline = TRUE,
        width = "auto"
      ),


      br(),
      fluidRow(
        h3(strong("Lab-Level Data"))
      ),

      # wellSingleLabUI(id = "1", lab_id = 1, num_levels = 30,  #for testing
      #   num_levels_default = 3)

      # initialize data input boxes for each lab

      #first lab
      #"lab1_inoc_level1", "lab1_ntest1", "lab1_npos1", etc.
      wellSingleLabUI(
        id = "well_lab_1",
        lab_id = 1, num_levels = glob_max_levels,
        num_levels_default = glob_default_levels,
        fluidRow(
          column(width = 4,
            actionButton("fill_inoc_level_d",
              label = "Fill other labs",
              width = "60%",
              style = glob_style_fill_labs
            )
          ),
          column(width = 4,
            actionButton("fill_ntubes_n",
              label = "Fill other labs",
              width = "60%",
              style = glob_style_fill_labs
            )
          )
        ),
        fluidRow(
          column(width = 4,
            actionButton("clear_inoc_level_d",
              label = "Clear other labs",
              width = "60%",
              style = glob_style_fill_labs
            )
          ),
          column(width = 4,
            actionButton("clear_ntubes_n",
              label = "Clear other labs",
              width = "60%",
              style = glob_style_fill_labs
            )
          ),
          column(width = 4,
            actionButton("clear_npos_y",
              label = "Clear other labs",
              width = "60%",
              style = glob_style_fill_labs
            )
          )
        )
      ),

      #labs 2 thru 30
      #https://shiny.rstudio.com/gallery/creating-a-ui-from-a-loop.html
      lapply(2:glob_max_labs, function(i) {
        lab_box <- wellSingleLabUI(
          id = as.character("well_lab_", i),
          lab_id = i, num_levels = glob_max_levels,
          num_levels_default = glob_default_levels
        )
        if (i > glob_default_labs) {
          shinyjs::hidden(lab_box)
        } else {
          lab_box
        }
      }),

      conditionalPanel("input.use_example",
        shinydashboard::box(
          width = 12,
            h4(
              paste(
                "Sample size:", glob_sample_size_example,
                glob_sample_unit_example
              )
            ),
            tableOutput("example_data")
        )
      )
    ),  #end of data entry


    ############################  Second column  ###############################

    column(width = 4, align = "left",
      fluidRow(
        column(width = 3),
        column(width = 6, align = "center",
          br(),
          #https://dreamrs.github.io/shinyWidgets/reference/progressSweetAlert.html
          shinyWidgets::useSweetAlert(), # needed for 'progressSweetAlert()'
          shinyWidgets::actionBttn("calculate", label = "Calculate",
            icon = icon("calculator"),
            style = "gradient",
            color = "success",
            size = "lg",
            block = TRUE,
            no_outline = TRUE
          )
        )
      ),
      br(),
      shinydashboard::box(
        width = 12, status = "info",
        title = div(
          strong("Notes"),
          style = "font-size: 22px; line-height: 1.1"
        ),
        solidheader = FALSE, background = "blue",
        collapsible = TRUE, collapsed = FALSE,
        tags$ul(
          tags$li(
            p(
              "Please don't use the back arrow in your browser. Use the",
              "refresh button instead."
            )
          ),
          tags$li(
            p(
              "See", strong(em('Analysis Details')),
              "in the sidebar for information about the models and R",
              "packages used."
            )
          ),
          tags$li(
            p(
              "You can click", em("Use example data?"), "to bypass the data",
              "entry sections."
            )
          ),
          tags$li(
            p(
              "The example data are obtained from Jarvis et al. (2019) and the",
              "Excel tool developed by Jarvis et al. Please see",
              strong(em("References")), "in the sidebar for the full citation."
            )
          ),
          style = "font-size: 16px; line-height: 1.1"
        )
      ),

      shinydashboard::box(
        title = div(
          strong("Instructions"),
          style = "font-size: 22px; line-height: 1.1"
        ),
        width = 12, status = "info",
        solidheader = FALSE, background = NULL,
        collapsible = TRUE, collapsed = FALSE,
        tags$ol(
          tags$li(
            p(
              "Please enter the experiment name, etc. in the",
              strong("Experiment Description"), "box.",
            )
          ),
          tags$li(
            p(
              "In the first box under", strong("Lab-Level Data"), ":"
            )
          ),
          tags$ol(type = "a",
            tags$li(
              p(
                "Change the lab name if desired."
              )
            ),
            tags$li(
              p(
                "In the first column, enter the inoculation levels (densities).",
                "If all the labs used the same levels, click",
                em("Fill other labs"), "underneath the column to populate the",
                "remaining labs."
              )
            ),
            tags$li(
              p(
                "In the second column, enter the number of tubes inoculated at",
                "each level. Again, click", em("Fill other labs"),
                "if applicable."
              )
            ),
            tags$li(
              p(
                "In the third column, enter the number of positive tubes at",
                "each level."
              )
            )
          )
        ),
        tags$ol(start = "3",
          tags$li(
            p(
              "Fill in the missing information for the remaining boxes (labs)."
            )
          ),
          tags$li(
            p(
              "In the sidebar, choose the desired LOD (25, 50, or 75) and the",
              "level for the confidence limits."
            )
          ),
          tags$li(
            p(
              HTML(
                "Click &nbsp;",
                "<span style = 'color: white; background-color: green'>",
                  "&nbsp; Calculate &nbsp;",
                "</span>",
                "&nbsp; in the upper right corner."
              )
            )
          ),
          tags$li(
            p(
              "After the calculations are complete, you will be automatically",
              "re-directed to the", strong(em("Results")), "page, which",
              "contains two tabs:"
            )
          ),
          tags$ol(type = "a",
            tags$li(
              p(
                "The first tab gives estimates of lab effects, ICC, and LOD.",
                "A button to download the results as an Excel file is included."
              )
            ),
            tags$li(
              p(
                "The second tab shows a plot of POD vs. inoculation level.",
              )
            )
          )
        ),

        style = "font-size: 16px; line-height: 1.1"
      )

    )
  )
)


#################################  Results  ####################################

my_results <- tabItem(
  tabName = "results",

  h2(strong("Results")),
  br(),
  tabsetPanel(
    id = "mod",
    type = "tabs",

    tabPanel(
      title = HTML(
        "<strong style = 'font-size:20px'>",
          "Model Parameters & LOD",
        "</strong>"
      ),
      value = "model_params",
      icon = icon("table", lib = "font-awesome"),
      htmlOutput("warning_message1"),
      htmlOutput("warning_message2"),

      br(),
      column(
        width = 9,
        box(
          title = tags$p("Model Parameter Estimates", style = "font-size: 125%;"),
          width = 12,
          fluidRow(
            column(width = 12, align = "center",
              valueBoxOutput("log_mean_effect", width = 4),
              valueBoxOutput("se_log_mean_effect", width = 4),
              valueBoxOutput("ICC", width = 4)
            )
          )
        ),
        box(
          title = tags$p("Level of Detection", style = "font-size: 125%;"),
          width = 12,
          fluidRow(
            column(width = 12, align = "center",
              valueBoxOutput("LOD", width = 4),
              valueBoxOutput("LOD_LCL", width = 4),
              valueBoxOutput("LOD_UCL", width = 4)
            )
          )
        )
      ),
      column(width = 3,
        shinyjs::disabled(
          shinyWidgets::downloadBttn("download_results", label = "Download Results",
            style = "gradient",
            color = "primary",
            size = "lg",
            block = FALSE,
            no_outline = FALSE
          )
        )
      )
    ),

    tabPanel(
      title = HTML(
        "<strong style = 'font-size:20px'>",
          "POD Curves",
        "</strong>"
      ),
      value = "pod_curves",
      icon = icon("line-chart", lib = "font-awesome"),
      plotOutput("POD_plots",
        width = "75%", height = "650px"
      )
    )
  )
)




###########################  Analysis Details  #################################

my_analysis_details <- tabItem(tabName = "analysis_details",

  h2(strong("Analysis Details")),
  br(),

  em(h3("Background")),
  div(
    p(
      "This app implements the random intercept complementary log-log model",
      "suggested by Jarvis et al. (2019) to estimate probability of",
      "detection (POD) and level of detection (LOD) from a multi-laboratory",
      "validation study for a qualitative (binary) microbiological assay."
    ),
    p(
      "The model is fit using R statistical software, which does not have",
      "the constraints seen in spreadsheet applications such as the",
      "tool described in Jarvis et al. (2019)."
    ),
    style = "font-size: 18px; line-height: 1.25"
  ),

  br(),
  em(h3("POD and LOD")),
  div(
    p(
      "The", em("lme4"), "R package (Bates et al., 2015) is used to fit the",
      "random intercept model, which is a Generalized Linear Mixed [Effects]",
      "Model (GLMM). If the algorithms used to fit the model fail to converge,",
      "the standard", em("stats"), "R package is used to fit a Generalized",
      "Linear [Fixed Effects] Model (GLM) instead."
    ),
    p(
      "The model is then used to estimate POD and LOD for the assay. The",
      "confidence limits for LOD are obtained using bootstrapping for the",
      "random intercept model or the method described in Jarvis et al. (2019)",
      "for the fixed effects model."
    ),
    style = "font-size: 18px; line-height: 1.25"
  ),

  br(),
  em(h3("ICC")),
  div(
    p(
      "This app also calculates the intraclass correlation coefficient (ICC)",
      "to estimate the proportion of total variance attributable to",
      "between-laboratory variance. A small ICC value indicates that an assay",
      "has little dependence on laboratory choice and is therefore more robust."
    ),
    p(
       "The ICC is calculated using the", em("performance"),
       "R package (Lüdecke et al., 2020) for the random intercept model or the",
       em("aod"), "package (Lesnoff et al., 2012) for the fixed effects model
       (using the Donner approach)."
    ),
    style = "font-size: 18px; line-height: 1.25"
  )
)


#############################  References  #####################################

my_references <- tabItem(tabName = "references",

  h2(strong("References")),
  br(),
  tags$ol(
    tags$li(
      p(
        'Bates D, Maechler M, Bolker B, Walker S (2015).',
        strong('Fitting linear mixed-effects models using lme4.'),
        em('Journal of Statistical Software,'), '67(1), 1-48.'
      )
    ),
    tags$li(
      p(
        'Jarvis B, Wilrich C, Wilrich P-T (2019).',
        strong(
          'Estimation of the POD function and the LOD of a binary',
          'microbiological measurement method from an interlaboratory experiment.',
        ),
        em('Journal of AOAC International,'), '102(5), 1617-1623.'
      )
    ),
    tags$li(
      p(
        'Lesnoff M, Lancelot R (2012).',
        strong('aod: Analysis of overdispersed data.'),
        'R package, URL https://cran.r-project.org/package=aod'
      )
    ),
    tags$li(
      p(
        'Lüdecke D, Makowski D, Waggoner P, Patil I (2020).',
        strong('performance: Assessment of regression models performance.'),
        'R package, URL https://CRAN.R-project.org/package=performance'
      )
    ),

  style = "font-size: 18px; line-height: 1.2"
  )
)



#########################  Dashboard layout  ###################################

ui <- dashboardPage(
  dashboardHeader(
    title = glob_app_title,
    titleWidth = 275
  ),
  dashboardSidebar(
    my_sidebar, width = 275
  ),
  dashboardBody(
    #https://stackoverflow.com/questions/45706670/shiny-dashboadpage-lock-dashboardheader-on-top
    tags$script(HTML("$('body').addClass('fixed');")),
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
