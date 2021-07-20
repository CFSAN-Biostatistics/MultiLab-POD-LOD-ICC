
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
        choices = c("LOD50"),
        selected = "LOD50"
      )
    )
  ),
  fluidRow(
    column(width = 2),
    column(width = 8, align = "center",
      selectInput("conf_level", label = "Confidence level",
        width = "100%",
        choices = c("90%", "95%"),
        selected = "95%"
      )
    )
  ),
  hr(),
  h4(em("Authors: "), "S. Wang, J. Ihrie", align = "center"),
  h4(em("App version: "), glob_app_version, align = "center"),
  hr(),
  div(
    id = "mail_to",
    "Please",
    a("email John Ihrie",
      href = "mailto:John.Ihrie@fda.hhs.gov", class = "email-link"
    ),
    "with suggestions or bug reports."
  )
)


##############################  Calculator  ####################################

my_calculator <- tabItem(
  tabName = "calculator",

  fluidRow(

    # Experiment Description
    column(width = 8, align = "center",

      fluidRow(
        radioGroupButtons2(
          inputId = "choose_data_entry",
          label = "Please choose data entry method",
          choices = c("Manual entry", "File upload", "Example data"),
          selected = "Manual entry",
          size = "lg",
          individual = TRUE,
          checkIcon = list(
            yes = tags$i(class = "fa fa-check")
          )
        )
      ),

      # wellSingleLabUI(id = "1", lab_id = 1, num_levels = 30,  #for testing only
      #   num_levels_default = 3)

      conditionalPanel(
        condition = 'input.choose_data_entry == "Manual entry"',

        wellPanel(
          id = "experiment_description",
          h3(strong("Experiment Description")),
          br(),
          fluidRow(
            column(width = 6, align = "center",
              wellPanel(
                id = "exp_description_left",
                textInput("exp_name",
                  label = "Experiment name",
                  width = "100%",
                  placeholder = "< experiment name >"
                ),
                textInput("exp_date",
                  label = "Experiment date",
                  width = "100%",
                  placeholder = "< experiment date >"
                ),
                textInput("matrix",
                  label = "Food matrix",
                  width = "100%",
                  placeholder = "< food matrix >"
                ),
                textInput("microorganism",
                  label = "Microorganism",
                  width = "100%",
                  placeholder = "< microorganism >"
                )
              )
            ),
            column(width = 6, align = "center",
              wellPanel(
                id = "exp_description_right",
                numericInput("num_labs",
                  label = "How many labs?",
                  width = "100%", value = glob_default_labs,
                  min = glob_min_labs, max = glob_max_labs
                ),
                numericInput("num_levels",
                  label = "How many inoculum levels?",
                  width = "100%", value = glob_default_levels,
                  min = glob_min_levels, max = glob_max_levels
                ),
                numericInput("sample_size",
                  label = "Test portion size (g or mL)",
                  width = "100%", value = glob_default_size,
                  min = glob_min_size
                )
              )
            )
          )
        ),

        fluidRow(
          h3(strong("Lab-Level Data"))
        ),
        # Initialize data input boxes for each lab
        #"lab1_inoc_level1", "lab1_ntest1", "lab1_npos1", etc.
        #first lab
        wellSingleLabUI(
          id = "well_lab_1",
          lab_id = 1, num_levels = glob_max_levels,
          num_levels_default = glob_default_levels,
          fluidRow(
            column(width = 4,
              actionButton("fill_inoc_level_d",
                label = "Fill other labs",
                width = "60%",
                class = "fill-labs"
              )
            ),
            column(width = 4,
              actionButton("fill_ntubes_n",
                label = "Fill other labs",
                width = "60%",
                class = "fill-labs"
              )
            )
          ),
          fluidRow(
            column(width = 4,
              actionButton("clear_inoc_level_d",
                label = "Clear other labs",
                width = "60%",
                class = "fill-labs"
              )
            ),
            column(width = 4,
              actionButton("clear_ntubes_n",
                label = "Clear other labs",
                width = "60%",
                class = "fill-labs"
              )
            ),
            column(width = 4,
              actionButton("clear_npos_y",
                label = "Clear other labs",
                width = "60%",
                class = "fill-labs"
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
        })

      ),

      conditionalPanel(
        condition = 'input.choose_data_entry == "File upload"',
        div(id = "download_template",
          "Please",
          a("download the Excel (.xlsx) template",
            href = "MultiLab_POD_LOC_ICC_template.xlsx",
            download = NA,
            class = "template-link"
          ),
          " to your hard drive and add your data.",
          p("Then save the file and upload for processing.")
        ),
        fileInput2(  #helpers.R - with onchange event attribute
          inputId = "upload_file",
          label = "Upload Excel file (.xlsx)",
          multiple = FALSE,
          accept = ".xlsx",
          width = "65%",
          buttonLabel = span("Browse...", class = "upload-browse"),
          placeholder = "No file selected",
          onchange = "enforceFileExtension(this)"
        ),
        # #I may use a modal instead of an alert sometime in the future
        # #https://www.w3schools.com/howto/howto_css_modals.asp
        # div(id = "invalid_extension", class = "modal",
        #   div(class = "modal-content",
        #     span(class = "close", HTML("&times;")),
        #     p("Incorrect file extension")
        #   )
        # )
        h3("Data preview:"),
        shinydashboard::box(
          width = 12,
          htmlOutput("uploaded_test_portion", inline = TRUE),
          tableOutput("uploaded_data_preview")
        )
      ),

      conditionalPanel(
        condition = 'input.choose_data_entry == "Example data"',
        shinydashboard::box(
          width = 12,
          h4(
            paste(
              "Test portion size:", glob_sample_size_example,
              glob_sample_unit_example
            )
          ),
          tableOutput("example_data")
        )
      )

    ),  #end of data entry


    #--------------------------  Second column  --------------------------------

    column(width = 4, align = "left",
      fluidRow(
        column(width = 12, align = "center",
          shinyWidgets::radioGroupButtons(
            inputId = "lod_unit",
            label = "Please choose LOD unit",
            choices = c("CFU/g", "CFU/mL", "CFU/test portion"),
            size = "lg",
            individual = FALSE,
            checkIcon = list(
              yes = tags$i(
                class = "fa fa-circle"
              ),
              no = tags$i(
                class = "fa fa-circle-o"
              )
            )
          )
        )
      ),
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
        title = span("Notes", class = "notes-title"),
        solidheader = FALSE, background = "blue",
        collapsible = TRUE, collapsed = FALSE,
        HTML(
          '
          <ul class = "notes-body">
            <li class = "notes-li">
              Please do not use the back arrow in your browser. Use the
              refresh button instead.
            </li>
            <li class = "notes-li">
              See <strong>Analysis Details</strong> in the sidebar
              for information about the models and R packages used.
            </li>
            <li class = "notes-li">
              The example data are obtained from Jarvis et al. (2019) and the
              Excel tool developed by Jarvis et al. Please see
              <strong>References</strong> in the sidebar for the full citation.
            </li>
          </ul>
          '
        )
      ),

      shinydashboard::box(
        title = span("Data Entry Instructions", class = "notes-title"),
        width = 12, status = "info",
        solidheader = FALSE, background = NULL,
        collapsible = TRUE, collapsed = FALSE,
        span("Manual entry", class = "notes-subheading"),
        HTML(
          '
          <ol class = "notes-body">
            <li class = "notes-li">
              Enter the experiment name, etc. in the
              <strong>Experiment Description</strong> box.
            </li>
            <li class = "notes-li">
              In the first box under <strong>Lab-Level Data</strong>:
              <ol type = "a">
                <li class = "notes-li">
                  Change the lab name if desired.
                </li>
                <li class = "notes-li">
                  In the first column, enter the inoculation (dilution) levels
                  in CFU/g or CFU/mL. Please do not use CFU/25g, etc.
                  Click <em>Fill other labs</em> underneath the column to
                  populate the remaining labs.
                </li>
                <li class = "notes-li">
                  In the second column, enter the number of tubes inoculated at
                  each level. Again, click <em>Fill other labs</em>
                  if applicable.
                </li>
                <li class = "notes-li">
                  In the third column, enter the number of positive tubes at
                  each level.
                </li>
              </ol>
            </li>
            <li>
              Fill in the missing information for the remaining boxes (labs).
            </li>
          </ol>
          '
        ),
        span("File upload", class = "notes-subheading"),
        HTML(
          '
          <ol class = "notes-body">
            <li class = "notes-li">
              Download the Excel workbook template.
            </li>
            <li class = "notes-li">
              Add your data per the instructions in the template and save the
              file to your hard drive.
            </li>
            <li class = "notes-li">
              Upload the file and verify the data shown in the preview.
            </li>
          </ol>
          '
        ),
        span("Example data", class = "notes-subheading"),
        HTML(
          '
          <ul class = "notes-body">
            No data input required.
          </ul>
          '
        )
      ),

      shinydashboard::box(
        title = span("Analysis Instructions", class = "notes-title"),
        width = 12, status = "info",
        solidheader = FALSE, background = NULL,
        collapsible = TRUE, collapsed = FALSE,
        HTML(
          '
          <ol class = "notes-body">
            <li class = "notes-li">
              In the sidebar, choose the level for the confidence limits.
            </li>
            <li class = "notes-li">
              In the upper right corner, choose the LOD unit. Then click
              <span style = "color: white; background-color: green">
                &nbsp; Calculate &nbsp;
              </span>.
            </li>
            <li class = "notes-li">
              After the calculations are complete, you will be automatically
              re-directed to the <strong>Results</strong> page, which contains
              a button to download the results and two tabs:
            </li>
              <ol type = "a">
                <li class = "notes-li">
                  The first tab gives estimates of model parameters, ICC, and LOD.
                </li>
                <li class = "notes-li">
                  The second tab shows a plot of POD vs. inoculation level.
                </li>
              </ol>
          </ol>
          '
        )
      )

    )
  )
)


#################################  Results  ####################################

my_results <- tabItem(
  tabName = "results",

  fluidRow(
    column(width = 6,
      h2(strong("Results"))
    ),
    column(width = 3,
      br(), br(),
      shinyjs::disabled(
        shinyWidgets::downloadBttn(
          outputId = "download_results",
          label = "Download Results",
          style = "gradient",
          color = "primary",
          size = "lg",
          block = FALSE,
          no_outline = FALSE
        )
      )
    )
  ),

  tabsetPanel(
    id = "mod",
    type = "tabs",

    tabPanel(
      title = HTML(
        "<strong>",
          "Model Parameters & LOD",
        "</strong>"
      ),
      value = "model_params",
      icon = icon("table", lib = "font-awesome"),

      htmlOutput("warning_messages_results_UI"),

      br(),
      column(
        width = 12,
        shinydashboard::box(
          title = div(
            "Model Parameter Estimates",
            class = "results-title"
          ),
          width = 12,
          fluidRow(
            column(width = 12, align = "center",
              valueBoxOutput("log_mean_effect", width = 3),
              valueBoxOutput("se_log_mean_effect", width = 3),
              valueBoxOutput("sigma", width = 3),
              valueBoxOutput("ICC", width = 3)
            )
          )
        ),
        shinydashboard::box(
          title = div(
            "Level of Detection",
            class = "results-title"
          ),
          width = 9,
          fluidRow(
            column(width = 12, align = "center",
              valueBoxOutput("LOD", width = 4),
              valueBoxOutput("LOD_LCL", width = 4),
              valueBoxOutput("LOD_UCL", width = 4)
            )
          )
        )
      )
    ),

    tabPanel(
      title = HTML(
        "<strong>",
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
      "validation study for a qualitative (binary) microbiological assay.",
      "The model is fit using R statistical software."
    ),
    class = "analysis-body"
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
      "The model is then used to estimate the POD function for the assay.",
      "For the random intercept model, confidence limits for POD are obtained",
      "using model-based parametric bootstrapping; LOD and its confidence limits",
      "are then obtained using a numerical search of the POD function and its",
      "confidence limits. For the fixed effects model, LOD and its confidence",
      "limits are obtained using Equations 32 and 33 in Jarvis et al. (2019);",
      "the confidence limits for POD are obtained using the fitted POD",
      "values and a margin of error using a t critical value and the estimated",
      "standard errors from the fitted POD function."
    ),
    class = "analysis-body"
  ),

  br(),
  em(h3("ICC")),
  div(
    p(
      "This app also calculates the intra-laboratory correlation coefficient (ICC)",
      "to estimate the proportion of total variance attributable to",
      "between-laboratory variance. A small ICC value indicates that an assay",
      "has little dependence on laboratory choice and is therefore more robust."
    ),
    p(HTML(
      "For the random intercept model, the ICC is calculated using between-lab",
      "variance estimated from the model and within-lab variance of (&pi; ^ 2) / 6",
      "(Hedeker et al., 2006).",
      "For the fixed effects model, the <em>lme4</em> package",
      "(Bates et al., 2015) is used if possible (LMM approach) to estimate both",
      "the between-lab variance and within-lab variance; otherwise,",
      "the standard <em>stats</em> package is used to apply the ANCOVA method",
      "of Stanish et al. (1983)."
    )),
    class = "analysis-body"
  )
)


#############################  References  #####################################

my_references <- tabItem(
  tabName = "references",
  h2(strong("References")),
  br(),
  tags$ol(
    tags$li(
      p(HTML(
        '<div>',
          'Bates D, Maechler M, Bolker B, Walker S (2015).',
          '<strong>Fitting linear mixed-effects models using lme4.</strong>',
          '<i>Journal of Statistical Software</i>, 67(1), 1-48.',
        '</div>'
      ))
    ),
    tags$li(
      p(HTML(
        '<div>',
          'Hedeker D, Gibbons RD (2006).',
          '<strong>',
            'Longitudinal Data Analysis.',
          '</strong>',
          '<i>John Wiley and Sons, Inc.</i>, Hoboken, New Jersey.',
        '</div>'
      ))
    ),
    tags$li(
      p(HTML(
        '<div>',
          'Jarvis B, Wilrich C, Wilrich P-T (2019).',
          '<strong>',
            'Estimation of the POD function and the LOD of a binary',
            'microbiological measurement method from an interlaboratory experiment.',
          '</strong>',
          '<i>Journal of AOAC International</i>, 102(5), 1617-1623.',
        '</div>'
      ))
    ),
    tags$li(
      p(HTML(
        '<div>',
          'Nakagawa S, Johnson PCD, Schielzeth H (2017).',
          '<strong>',
            'The coefficient of determination R<sup>2</sup> and intra-class',
            'correlation coefficient from generalized linear mixed-effects',
            'models revisited and expanded.',
          '</strong>',
          '<i>Journal of the Royal Society Interface</i>, 14, 20170213.',
        '</div>'
      ))
    ),
    tags$li(
      p(HTML(
        '<div>',
          'Stanish WM, Taylor N (1983).',
          '<strong>',
            'Estimation of the intraclass correlation coefficient for the',
            'analysis of covariance model.',
          '</strong>',
          '<i>The American Statistician</i>, 37(3), 221–224.',
        '</div>'
      ))
    ),
  class = "references-body"
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

