server <- function(input, output, session) {

  observeEvent(c(input$my_tabs, input$mod), {
    shinyjs::runjs(
      'setTimeout(function() {
        // Remove focus from tab panels
        document.querySelectorAll("[role=\\042tabpanel\\042]").forEach(function(div) {
          div.removeAttribute("tabindex")
        })
        // Make sure links are focusable & do not have aria-selected attrib
        document.getElementsByTagName("a").forEach(function(a) {
          a.setAttribute("tabindex", "0")
          a.removeAttribute("aria-selected")
        })
        // Make sure disabled elements are not focusable
        document.getElementsByClassName("disabled").forEach(function(dis) {
          dis.removeAttribute("tabindex")
        })
      }, 500)'
    )
  })

  observeEvent(c(input$`choose_data_entry-radio_group`, input$`upload_file-upload_data`), {
    shinyjs::runjs(
      'setTimeout(function() {
        // Add scope to table headers
        document.getElementsByTagName("th").forEach(function(th) {
          th.setAttribute("scope", "col")
        })
      }, 500)'
    )
  })


  #-----------------------------  Data Entry  ----------------------------------

  # Choose data entry method
  choose_data_entry <- dataChoicesServer("choose_data_entry")

  observeEvent(choose_data_entry(), {
    if (choose_data_entry() == "Manual entry") {
      shinyjs::hide("upload_data_panel")
      shinyjs::hide("example_data_panel")
      shinyjs::show("manual_data_panel")
    } else if (choose_data_entry() == "File upload") {
      shinyjs::hide("manual_data_panel")
      shinyjs::hide("example_data_panel")
      shinyjs::show("upload_data_panel")
    } else if (choose_data_entry() == "Example data") {
      shinyjs::hide("manual_data_panel")
      shinyjs::hide("upload_data_panel")
      shinyjs::show("example_data_panel")
    } else {
      stop("Problem with dataChoicesServer()")
    }
  })

  # Choose LOD & confidence level
  sidebar_select <- sidebarSelectServer("sidebar_select")

  # Start "chain reaction" for calculations & take snapshot of user input values
  start_analysis <- runAnalysisServer("calculate", sidebar_select, choose_data_entry)

  # Manual data entry
  experiment_description <- experimentDescriptionServer("experiment_description",
    glob_min_labs, glob_max_labs, glob_min_levels, glob_max_levels, glob_min_size
  )
  experiment_description$iv$enable()

  data_manual <- labsServer("lab_data",
    experiment_description, glob_max_labs, glob_max_levels, glob_panel_names,
    reactive(input$fill_inoc_level), reactive(input$clear_inoc_level),
    reactive(input$fill_ntubes), reactive(input$clear_ntubes),
    reactive(input$fill_npos), reactive(input$clear_npos),
    start_analysis
  )

  # Uploaded data
  data_upload <- uploadDataServer("upload_file", start_analysis)  #see mod-data-input-upload.R

  observeEvent(c(data_upload(), choose_data_entry()), {
    shinyjs::enable(selector = "button[class*='calculate-button']")
  })

  data_example <- reactive({
    # To harmonize treatment of data in rest of app
    list(
      data_model     = reactive(dat_example),
      exp_date       = reactive("01/01/1900"),
      exp_name       = reactive("my experiment name"),
      matrix         = reactive("my food matrix"),
      microorganism  = reactive("my microorganism"),
      sample_size    = reactive(glob_sample_size_example),
      fname_uploaded = reactive(NULL),
      uploaded_data_for_preview = reactive(NULL)
    )
  })

  # Example data
  output$example_data <- renderTable({
    dat_example_ui <- data_example()$data_model()
    dat_example_ui$sample_size <- NULL
    colnames(dat_example_ui) <- c(
      "Lab ID", "Lab Name", "Level Per Portion", "Level Per Unit",
      "Inoculated Tubes", "Positive Tubes"
    )
    dat_example_ui
  }, striped = TRUE, bordered = TRUE, align = 'c', digits = 2,
    caption = "Example data"
  )

  dat <- eventReactive(start_analysis(), {
    shinyjs::disable(selector = "a[class*='download-results']")
    if (start_analysis()$choose_data_entry == "Manual entry") {
      validateDescription(experiment_description$iv$is_valid(), session)
      validateData(data_manual()$data_model(), session)
      return(data_manual())
    } else if (start_analysis()$choose_data_entry == "File upload") {
      validateData(data_upload()$data_model(), session)
      return(data_upload())
    } else if (start_analysis()$choose_data_entry == "Example data") {
      return(data_example())
    } else {
      stop("Problem with dat().")
    }
  })

  # output$data_object <- renderPrint({  #for testing only
  #   my_data <- dat()
  #   list(
  #     iv             = my_data$iv,
  #     data_model     = my_data$data_model(),
  #     exp_name       = my_data$exp_name(),
  #     exp_date       = my_data$exp_date(),
  #     matrix         = my_data$matrix(),
  #     microorganism  = my_data$microorganism(),
  #     sample_size    = my_data$sample_size(),
  #     fname_uploaded = my_data$fname_uploaded(),
  #     uploaded_data_for_preview = my_data$uploaded_data_for_preview()
  #   )
  # })


  #----------------------  Model Fitting & POD/LOD  ----------------------------

  model_fit <- eventReactive(dat(), {
  #model_fit <- eventReactive(0, {  #for testing data entry
    fitModel(dat()$data_model(), session)
  })

  output$model_results <- renderPrint(model_fit())

  # ##########################################################
  ## For testing only
  # output$test_model <- renderPrint({
  #   dat <- dat()$data_model()
  #   dat <- dat[dat$inoculum_per_unit != 0, ]
  #   x <- lme4::glmer(
  #     cbind(npos, ntest - npos) ~
  #       offset(log(sample_size)) + offset(log(inoculum_per_unit)) + (1 | lab_id),
  #     data = dat, family = binomial(link = "cloglog"),
  #     control = lme4::glmerControl(optimizer = "bobyqa"), nAGQ = 21
  #   )
  #   summary(x)
  # })
  # #############################################################

  output$warning_messages_results_UI <- renderUI({
    warnings <- model_fit()$warnings
    if (length(warnings) > 0) {
      modelAlert(warnings, session)
      warnings <- paste0("<li>", warnings, "</li>")
      warnings <- paste(warnings, collapse = "")
      tags$p(div(class = "warning",
        HTML("<ul>Warnings:", warnings, "</ul>")
      ))
    }
  })

  POD_LOD <- eventReactive(model_fit(), {
    podLod(start_analysis(), dat(), model_fit(), n_sim = 500, session)
  })


  #----------------------  Model Parameter Estimates  --------------------------

  output$log_mean_effect <- renderValueBox({
    valueBox(
      value = glob_log_mean_effect_desc,
      subtitle = HTML(
        "<span class='parameter-estimates-value'>",
          model_fit()$mu_log_char,
        "</span>"
      ),
      width = 12, icon = NULL, color = "navy"
    )
  })

  observeEvent(POD_LOD(), {
    shinyjs::runjs(
      'setTimeout(function() {
        // Change role & tabindex for MathJax spans
        const muhat = document.getElementById("log_mean_effect_desc").getElementsByClassName("MathJax")[0];
        muhat.removeAttribute("tabindex");
        muhat.setAttribute("role", "math");
        const sigmahat = document.getElementById("sigma_desc").getElementsByClassName("MathJax")[0];
        sigmahat.removeAttribute("tabindex");
        sigmahat.setAttribute("role", "math");
      }, 2000)'
    )
  })

  output$se_log_mean_effect <- renderValueBox({
    valueBox(
      value = glob_se_log_mean_effect_desc,
      subtitle = HTML(
        "<span class='parameter-estimates-value'>",
          model_fit()$mu_log_se_char,
        "</span>"
      ),
      width = 12, icon = NULL, color = "navy"
    )
  })

  output$sigma <- renderValueBox({
    valueBox(
      value = glob_sigma_desc,
      subtitle = HTML(
        "<span class='parameter-estimates-value'>",
          model_fit()$sigma_char,
        "</span>"
      ),
      width = 12, icon = NULL, color = "navy"
    )
  })

  output$ICC <- renderValueBox({
    valueBox(
      value = glob_ICC_desc,
      subtitle = HTML(
        "<span class='parameter-estimates-value'>",
          model_fit()$ICC_char,
        "</span>"
      ),
      width = 12, icon = NULL, color = "maroon"
    )
  })

  output$LOD <- renderValueBox({
    lod_perc <- start_analysis()$lod_perc
    valueBox(
      value = HTML(
        "<span class='lod-estimates-description'>",
          paste0("LOD<sub>", lod_perc, "</sub>"),
        "</span>"
      ),
      subtitle = HTML(
        "<span class='lod-estimates-value'>",
          POD_LOD()$LOD_char,
        "</span>"
      ),
      width = 12, icon = NULL, color = "blue"
    )
  })

  output$LOD_LCL <- renderValueBox({
    lod_perc   <- start_analysis()$lod_perc
    conf_level <- start_analysis()$conf_level
    valueBox(
      value = HTML(
        "<span class='lod-estimates-description'>",
          paste0("LOD<sub>", lod_perc, "</sub>"),
          conf_level, "LCL",
        "</span>"
      ),
      subtitle = HTML(
        "<span class='lod-estimates-value'>",
          POD_LOD()$LOD_L_char,
        "</span>"
      ),
      width = 12, icon = NULL, color = "blue"
    )
  })

  output$LOD_UCL <- renderValueBox({
    lod_perc   <- start_analysis()$lod_perc
    conf_level <- start_analysis()$conf_level
    valueBox(
      value = HTML(
        "<span class='lod-estimates-description'>",
          paste0("LOD<sub>", lod_perc, "</sub>"),
          conf_level, "UCL",
        "</span>"
      ),
      subtitle = HTML(
        "<span class='lod-estimates-value'>",
          POD_LOD()$LOD_U_char,
        "</span>"
      ),
      width = 12, icon = NULL, color = "blue"
    )
  })


  #-----------------------------  POD Curves  ----------------------------------

  plotCurves <- eventReactive(POD_LOD(), {
    lod_unit    <- start_analysis()$lod_unit
    conf_level  <- start_analysis()$conf_level
    lod_choice  <- start_analysis()$lod_choice
    lod_prob    <- start_analysis()$lod_prob
    lod_perc    <- start_analysis()$lod_perc
    inoc_levels <- POD_LOD()$inoc_levels
    inoc_max    <- POD_LOD()$inoc_max
    predicted_all_labs <- POD_LOD()$predicted_all_labs
    predicted_each_lab <- POD_LOD()$predicted_each_lab
    LOD           <- POD_LOD()$LOD
    LOD_rounded   <- round(LOD, digits = 3)
    LOD_L         <- POD_LOD()$LOD_L
    LOD_L_rounded <- round(LOD_L, digits = 3)
    LOD_U         <- POD_LOD()$LOD_U
    LOD_U_rounded <- round(LOD_U, digits = 3)
    sample_size <- dat()$sample_size()
    dat <- dat()$data_model()
    dat$POD <- dat$npos / dat$ntest
    conf_level_no_perc <- gsub("%", replacement = "", conf_level, fixed = TRUE)
    #https://stackoverflow.com/questions/43415217/how-do-i-add-percentage-and-fractions-to-ggplot-geom-text-label
    my_label <- paste0("LOD[", lod_perc, "] == ", LOD_rounded)
    if (lod_unit == "CFU/test portion") {
      dat$inoc_level <- dat$inoculum
      predicted_each_lab$inoc_level <- sample_size * predicted_each_lab$inoculum_per_unit
      predicted_all_labs$inoc_level <- sample_size * predicted_all_labs$inoculum_per_unit
      inoc_levels <- sample_size * inoc_levels
      inoc_max    <- max(inoc_levels)
    } else {
      dat$inoc_level <- dat$inoculum_per_unit
      predicted_each_lab$inoc_level <- predicted_each_lab$inoculum_per_unit
      predicted_all_labs$inoc_level <- predicted_all_labs$inoculum_per_unit
    }
    my_breaks_x <- unique(c(0, inoc_levels))
    if (inoc_max < 0.5) {
      inoc_incr <- .01
    } else if (inoc_max < 8) {
      inoc_incr <- .1
    } else {
      inoc_incr <- 1
    }
    my_plot <- ggplot(dat,
        aes(x = inoc_level, y = POD, color = as.factor(lab_id))
      ) +
      geom_point(size = rel(4)) +
      ggtitle("Probability / Level of Detection") +
      xlab(paste0("Inoculation level (", lod_unit, ")")) +
      scale_x_continuous(
        breaks = my_breaks_x,
        labels = c("0", as.character(my_breaks_x[-1])),
        minor_breaks = seq(from = inoc_incr, to = inoc_max, by = inoc_incr),
        limits = c(0, inoc_max), expand = c(0, 0)
      ) +
      ylab("Probability of detection (POD)") +
      scale_y_continuous(
        breaks = c(0, .25, .50, .75, 1),
        labels = c("", "0.25", "0.50", "0.75", "1"),
        minor_breaks = seq(from = 0.05, to = .95, by = .05), expand = c(0, 0)
      ) +
      labs(
        color = 'Lab ID', # change legend title
        alt = "Plot of detection probability versus inoculation level."
      )
    # Combine plots
    p.all <- my_plot +
      geom_line(data = predicted_each_lab, aes(x = inoc_level, y = POD)) +
      geom_line(data = predicted_all_labs, aes(x = inoc_level, y = mean_POD),
        linetype = "solid", color = "red", linewidth = rel(2)
      ) +
      geom_ribbon(data = predicted_all_labs,
        aes(x = inoc_level, ymin = mean_POD_L, ymax = mean_POD_U),
        fill = "blue4", alpha = 0.5, inherit.aes = FALSE
      )
    # Add LOD and CI
    vjust <- .01 * inoc_max

    p.all <- p.all +
      # Do not use aes to avoid double legend
      geom_segment(x = LOD_L, xend = LOD_L, y = 0, yend = lod_prob,
        colour = "blue", linewidth = rel(1)
      ) +
      geom_segment(x = LOD_U, xend = LOD_U, y = 0, yend = lod_prob,
        colour = "blue", linewidth = rel(1)
      ) +
      geom_segment(x = LOD, xend = LOD, y = 0, yend = lod_prob,
        colour = "blue", linewidth = rel(1)
      ) +
      geom_segment(x = 0, xend = LOD_U, y = lod_prob, yend = lod_prob,
        colour = "blue", linewidth = rel(1)
      ) +
      annotate("text", x = LOD_L - vjust, y = lod_prob / 2,
        label = LOD_L_rounded, size = rel(5), angle = 90
      ) +
      annotate("text", x = LOD_U + vjust, y = lod_prob / 2,
        label = LOD_U_rounded, size = rel(5), angle = 90
      ) +
      annotate("text", x = LOD - vjust, y = lod_prob / 2,
        label = paste0("LOD", lod_choice, " =  ", LOD_rounded),
        size = rel(5), angle = 90
      ) +
      annotate("label", x = 0.75 * inoc_max, y = 0.375,
        label = my_label, parse = TRUE,
        size = rel(8), color = "red", fontface = "bold",
        label.padding = unit(0.75, "lines"),
        label.r = unit(0.15, "lines"), label.size = 0.5
      )
    p.all <- p.all +
      theme(
        aspect.ratio = glob_plot_aspect_ratio,
        axis.title.x = element_text(
          size = rel(1.75), color = "black",
          margin = margin(t = 20, r = 0, b = 20, l = 0, unit = "pt")
        ),
        axis.title.y = element_text(
          size = rel(1.75), color = "black",
          margin = margin(t = 0, r = 20, b = 0, l = 20, unit = "pt")
        ),
        axis.text.x = element_text(
          size = rel(1.75), color = "black",
          margin = margin(t = 5, r = 0, b = 0, l = 0, unit = "pt")
        ),
        axis.text.y = element_text(
          size = rel(1.75), color = "black",
          margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt")
        ),
        axis.ticks = element_line(linewidth = rel(1.5)),
        axis.line  = element_line(linewidth = rel(1.5)),
        legend.background = element_rect(fill = "grey90"),
        legend.margin     = margin(t = 5, r = 10, b = 5, l = 10, unit = "pt"),
        legend.text       = element_text(size = rel(1.25), color = "black"),
        legend.title      = element_text(size = rel(1.5), color = "black"),
        panel.background = element_rect(fill = "grey90", color = "white"),
        panel.border = element_rect(
          linetype = "solid", linewidth = rel(3), color = "black", fill = NA
        ),
        panel.grid.major.x = element_line(linewidth = rel(1), color = "white"),
        panel.grid.major.y = element_line(linewidth = rel(2.5), color = "white"),
        panel.grid.minor   = element_line(linewidth = rel(1), color = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(
          size = rel(2.25), color = "black", hjust = 0.5,
          margin = margin(t = 20, r = 0, b = 20, l = 0, unit = "pt")
        ),
        plot.title.position = "panel",
        plot.margin = unit(rep(0.25, 4), units = "inches")
      )
    p.all
  })

  output$POD_plots <- renderPlot({
    plotCurves()
  })

  #---------------  Calculations Complete & Download Results  ------------------

  observeEvent(plotCurves(), {
    shinydashboard::updateTabItems(session, inputId = "my_tabs", selected = "results")
    modal("Calculations complete!", session,
      span("Results will be loaded momentarily.")
    )
    shinyjs::delay(3000, {
      shinyjs::enable(selector = "a[class*='download-results']")
    })
  })

  downloadResultsServer("spreadsheet",
    start_analysis, model_fit, POD_LOD, plotCurves, dat
  )

}
