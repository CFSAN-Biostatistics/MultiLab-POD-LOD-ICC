
# ----------------------  Session info  ----------------------------------------
my_R_version <- sessioninfo::platform_info()$version

my_package_info <- sessioninfo::package_info(pkgs = NULL)
my_package_info <- as.data.frame(my_package_info)
my_package_info <- my_package_info[, c("package", "loadedversion", "attached")]
rownames(my_package_info) <- NULL
# ------------------------------------------------------------------------------


server <- function(input, output, session) {

  myLODPerc <- reactive({
    switch(input$lod_choice,
      "LOD50" = "50"
    )
  })

  myLODProb <- reactive({
    switch(input$lod_choice,
      "LOD50" = 0.5
    )
  })

  myConfLevel <- reactive({
    switch(input$conf_level,
      "90%" = 0.9,
      "95%" = 0.95
    )
  })

  output$example_data <- renderTable({
    dat_example_ui
  },
    striped = TRUE,
    bordered = TRUE,
    align = 'c'
  )

  # update number of labs & inoculum levels
  observe({
    input$num_labs
    input$num_levels
    input$use_example
    validate(
      need(!is.na(input$num_labs) && !is.na(input$num_levels),
           label = "num_labs_levels"
      )
    )
    my_labs <- paste0("panel_lab", 1:input$num_labs)  #wellPanels
    # lab i, level j
    lapply(glob_panel_names, function(i) {
      if (i %in% my_labs && !input$use_example) {
        shinyjs::showElement(i)
        my_levels  <- paste0(i, "_level", 1:input$num_levels)  #panel_lab1_level1, etc.
        all_levels <- paste0(i, "_level", 1:glob_max_levels)
        lapply(all_levels, function(j) {
          if (j %in% my_levels) {
            shinyjs::showElement(j)
          } else {
            shinyjs::hideElement(j)
          }
        })
      } else {
        shinyjs::hideElement(i)
      }
    })
  })

  #fill input data for other labs when requested
  #"lab1_inoc_level1", "lab1_ntest1", etc.
  observeEvent(input$fill_inoc_level_d, {
    lab1_inoc_levels <-lapply(1:glob_max_levels, function(level) {
      input[[paste0("lab1_inoc_level", level)]]
    })
    lapply(2:glob_max_labs, function(i) {  #iterating over labs, i
      lab_name <- paste0("lab", i)
      lapply(1:glob_max_levels, function(j) {# iterating over levels, j
        input_name <- paste0(lab_name, "_inoc_level", j)
        updateNumericInput(session = session,
          inputId = input_name, value = lab1_inoc_levels[[j]]
        )
      })
    })
  })
  observeEvent(input$fill_ntubes_n, {
      lab1_ntubes <-lapply(1:glob_max_levels, function(level) {
      input[[paste0("lab1_ntest", level)]]
    })
    lapply(2:glob_max_labs, function(i) {  #iterating over labs, i
      lab_name <- paste0("lab", i)
      lapply(1:glob_max_levels, function(j) {# iterating over levels, j
        input_name <- paste0(lab_name, "_ntest", j)
        updateNumericInput(session = session,
          inputId = input_name, value = lab1_ntubes[[j]]
        )
      })
    })
  })
  #clear same inputs
  observeEvent(input$clear_inoc_level_d, {
    lapply(2:glob_max_labs, function(i) {  #iterating over labs, i
      lab_name <- paste0("lab", i)
      lapply(1:glob_max_levels, function(j) {  # iterating over levels, j
        input_name <- paste0(lab_name, "_inoc_level", j)
        updateNumericInput(session = session,
          inputId = input_name, value = 0
        )
      })
    })
  })
  observeEvent(input$clear_ntubes_n, {
    lapply(2:glob_max_labs, function(i) {  #iterating over labs, i
      lab_name <- paste0("lab", i)
      lapply(1:glob_max_levels, function(j) {  # iterating over levels, j
        input_name <- paste0(lab_name, "_ntest", j)
        updateNumericInput(session = session,
          inputId = input_name, value = 0
        )
      })
    })
  })
  observeEvent(input$clear_npos_y, {
    lapply(2:glob_max_labs, function(i) {  #iterating over labs, i
      lab_name <- paste0("lab", i)
      lapply(1:glob_max_levels, function(j) {  # iterating over levels, j
        input_name <- paste0(lab_name, "_npos", j)
        updateNumericInput(session = session,
          inputId = input_name, value = 0
        )
      })
    })
  })

  # clear results when LOD or CI level inputs change
  observeEvent(input$lod_choice, {
    output$LOD <- renderValueBox({
      my_lod_perc <- myLODPerc()  #"50", etc.
      my_lod_prob <- myLODProb()  #0.5, etc.
      valueBox(
        subtitle = "---", color = "blue", width = 12,
        value = withMathJax(HTML(
          "<span style = 'font-size:60%; font-family:Courier; color:white'>",
            paste0("LOD<sub>", my_lod_perc), "</sub>",
          "</span>"
        ))
      )
    })
    output$LOD_LCL <- renderValueBox({
      my_lod_perc <- myLODPerc()
      my_lod_prob <- myLODProb()
      valueBox(
        subtitle = "---", color = "blue", width = 12,
        value = withMathJax(HTML(
          "<span style = 'font-size:60%; font-family:Courier; color:white'>",
            paste0("LOD<sub>", my_lod_perc), "</sub>", input$conf_level, "LCL",
          "</span>"
        ))
      )
    })
    output$LOD_UCL <- renderValueBox({
      my_lod_perc <- myLODPerc()
      my_lod_prob <- myLODProb()
      valueBox(
        subtitle = "---", color = "blue", width = 12,
        value = withMathJax(HTML(
          "<span style = 'font-size:60%; font-family:Courier; color:white'>",
            paste0("LOD<sub>", my_lod_perc), "</sub>", input$conf_level, "UCL",
          "</span>"
        ))
      )
    })
    output$POD_plots <- renderPlot({})
  }, ignoreInit = TRUE)

  observeEvent(input$conf_level, {
    output$LOD_LCL <- renderValueBox({
      my_lod_perc <- myLODPerc()
      my_lod_prob <- myLODProb()
      valueBox(
        subtitle = "---", color = "blue", width = 12,
        value = withMathJax(HTML(
          "<span style = 'font-size:60%; font-family:Courier; color:white'>",
            paste0("LOD<sub>", my_lod_perc), "</sub>", input$conf_level, "LCL",
          "</span>"
        ))
      )
    })
    output$LOD_UCL <- renderValueBox({
      my_lod_perc <- myLODPerc()
      my_lod_prob <- myLODProb()
      valueBox(
        subtitle = "---", color = "blue", width = 12,
        value = withMathJax(HTML(
          "<span style = 'font-size:60%; font-family:Courier; color:white'>",
            paste0("LOD<sub>", my_lod_perc), "</sub>", input$conf_level, "UCL",
          "</span>"
        ))
      )
    })
    output$POD_plots <- renderPlot({})
  }, ignoreInit = TRUE)


  # Starts "chain reaction" for calculations & caches input values
  calculate_clicked <- eventReactive(input$calculate, {

    shinyWidgets::progressSweetAlert(session = session,
      id = "progress_data", title = "Preparing data",
      value = 0, display_pct = TRUE
    )

    iter <- input$calculate + 1
    lod_unit    <- input$lod_unit
    use_example <- input$use_example
    if (use_example) {
      exp_name      <- "N/A"
      exp_date      <- "N/A"
      microorganism <- "N/A"
      matrix        <- "N/A"
      sample_size <- glob_sample_size_example
      num_labs    <- glob_num_labs_example
      num_levels  <- glob_num_levels_example
      lab_ids   <- 1:num_labs
      lab_names <- paste("Lab", lab_ids)
    } else {
      exp_name      <- input$exp_name
      exp_date      <- input$exp_date
      microorganism <- input$microorganism
      matrix        <- input$matrix
      sample_size   <- input$sample_size
      num_labs      <- input$num_labs
      num_levels    <- input$num_levels
      lab_ids   <- 1:num_labs
      lab_names <- vapply(lab_ids, FUN.VALUE = "chr", function(x) {
        input[[paste0("lab", x)]]
      })
    }

    shinyWidgets::updateProgressBar(session = session,
      id = "progress_data", value = 10
    )

    list(
      date_time       = strftime(Sys.time(), format = "", tz = "", usetz = TRUE),
      iter            = iter,
      exp_name        = exp_name,
      microorganism   = microorganism,
      matrix          = matrix,
      exp_date        = exp_date,
      use_example     = use_example,
      sample_size     = sample_size,
      lod_unit        = lod_unit,
      num_labs        = num_labs,
      lab_names       = lab_names,
      num_levels      = num_levels,
      lod_choice      = input$lod_choice,
      lod_prob        = myLODProb(),
      lod_perc        = myLODPerc(),
      conf_level      = input$conf_level,
      conf_level_prob = myConfLevel()
    )
  })


  # Data
  dat <- eventReactive(calculate_clicked(), {

    use_example <- calculate_clicked()$use_example
    num_labs    <- calculate_clicked()$num_labs
    num_levels  <- calculate_clicked()$num_levels
    description_error <- "Problem with experiment description inputs"
    input_error       <- "Problem with lab-level input data"

    shinyWidgets::updateProgressBar(session = session,
      id = "progress_data", value = 30
    )

    desc_validate1 <- !is.na(num_labs) && is_wholenumber(num_labs)
    desc_validate2 <- !is.na(num_levels) && is_wholenumber(num_levels)
    if (!desc_validate1) {
      shinyWidgets::sendSweetAlert(session = session,
        title = description_error, type = "error",
        text = "Number of labs must be a whole number."
      )
      shinyjs::disable("download_results_bttn")
    }
    if (!desc_validate2) {
      shinyWidgets::sendSweetAlert(session = session,
        title = description_error, type = "error",
        text = "Number of levels must be a whole number."
      )
      shinyjs::disable("download_results_bttn")
    }
    validate(
      need(try(desc_validate1 && desc_validate2),
        "Error"
      )
    )
    desc_validate3 <- num_labs <= glob_max_labs
    desc_validate4 <- num_levels <= glob_max_levels
    if (!desc_validate3) {
      shinyWidgets::sendSweetAlert(session = session,
        title = description_error, type = "error",
        text = paste0("Maximum number of labs is ", glob_max_labs, ".")
      )
      shinyjs::disable("download_results_bttn")
    }
    if (!desc_validate4) {
      shinyWidgets::sendSweetAlert(session = session,
        title = description_error, type = "error",
        text = paste0("Maximum number of levels is ", glob_max_levels, ".")
      )
      shinyjs::disable("download_results_bttn")
    }
    desc_validate5 <- num_labs   >= glob_min_labs
    desc_validate6 <- num_levels >= glob_min_levels
    if (!desc_validate5) {
      shinyWidgets::sendSweetAlert(session = session,
        title = description_error, type = "error",
        text = paste0("Minimum number of labs is ", glob_min_labs, ".")
      )
      shinyjs::disable("download_results_bttn")
    }
    if (!desc_validate6) {
      shinyWidgets::sendSweetAlert(session = session,
        title = description_error, type = "error",
        text = paste0("Minimum number of levels is ", glob_min_levels, ".")
      )
      shinyjs::disable("download_results_bttn")
    }
    validate_description <-
      desc_validate3 && desc_validate4 && desc_validate5 && desc_validate6

    validate(
      need(try(validate_description),
        "Error"
      )
    )

    sample_size <- calculate_clicked()$sample_size
    if (use_example) {
      dat <- dat_example
    } else {
      ssize_validate <-
        !is.na(sample_size) && is.numeric(sample_size) && sample_size > 0
      if (!ssize_validate) {
        shinyWidgets::sendSweetAlert(session = session,
          title = description_error, type = "error",
          text = "Test portion size must be a positive number (without units)."
        )
        shinyjs::disable("download_results_bttn")
      }
      validate(
        need(try(ssize_validate),
          "Error"
        )
      )
      # inoculum level, number of inoculated tubes, number of positive tubes
      # "lab1_inoc_level1", "lab1_ntest1", "lab1_npos1", etc.
      lab_id   <- 1:num_labs
      level_id <- 1:num_levels
      # lab i, level j
      inoculum <- lapply(lab_id, FUN = function(i) {
        vapply(level_id, FUN.VALUE = 1, function(j) {
          input[[paste0("lab", i, "_inoc_level", j)]]  #input$lab1_inoc_level1, etc.
        })
      })
      inoculum <- unlist(inoculum)
      ntest <- lapply(lab_id, FUN = function(i) {
        vapply(level_id, FUN.VALUE = 1, function(j) {
          input[[paste0("lab", i, "_ntest", j)]]  #input$lab1_ntest1, etc.
        })
      })
      ntest <- unlist(ntest)

      shinyWidgets::updateProgressBar(session = session,
        id = "progress_data", value = 50
      )

      npos <- lapply(lab_id, FUN = function(i) {
        vapply(level_id, FUN.VALUE = 1, function(j) {
          input[[paste0("lab", i, "_npos", j)]]  #input$lab1_npos1, etc.
        })
      })
      npos <- unlist(npos)
      dat <- data.frame(
        lab_id = rep(lab_id, each = num_levels),
        inoculum = inoculum, ntest = ntest, npos = npos,
        sample_size = sample_size
      )
    }

    shinyWidgets::updateProgressBar(session = session,
      id = "progress_data", value = 60
    )

    sum_data <- aggregate(dat, by = list(dat$lab_id), FUN = sum)
    input_validate1 <- all(!is.na(dat))
    input_validate2 <- all(dat >= 0)
    input_validate3 <- all(sum_data$inoculum > 0)  #each lab has >=1 positive level
    input_validate4 <- all(dat$ntest > 0)
    input_validate5a <- all(sum_data$npos > 0)
    input_validate5b <- all(sum_data$ntest - sum_data$npos > 0)
    input_validate6 <- all(dat$ntest >= dat$npos)
    input_validate7 <-
      all(is_wholenumber(dat$ntest)) && all(is_wholenumber(dat$npos))

    shinyWidgets::updateProgressBar(session = session,
      id = "progress_data", value = 80
    )

    if (!input_validate1) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "Some data are missing or non-numeric."
      )
      shinyjs::disable("download_results_bttn")
    } else if (!input_validate2) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "All data must be non-negative."
      )
      shinyjs::disable("download_results_bttn")
    } else if (!input_validate3) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "Each lab must have at least 1 positive inoculum level."
      )
      shinyjs::disable("download_results_bttn")
    } else if (!input_validate4) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "Each inoculation level must have at least 1 tube inoculated."
      )
      shinyjs::disable("download_results_bttn")
    } else if (!input_validate5a) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "Each lab must have at least 1 positive tube."
      )
      shinyjs::disable("download_results_bttn")
    } else if (!input_validate5b) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "Each lab must have at least 1 negative tube."
      )
      shinyjs::disable("download_results_bttn")
    } else if (!input_validate6) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "Tubes inoculated must always be >= tubes positive."
      )
      shinyjs::disable("download_results_bttn")
    } else if (!input_validate7) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "Tubes inoculated and tubes positive must be whole numbers."
      )
      shinyjs::disable("download_results_bttn")
    }
    validate_inputs <-
      input_validate1 && input_validate2 && input_validate3 &&
         input_validate4 && input_validate5a && input_validate5b &&
         input_validate6 && input_validate7
    validate(
      need(try(validate_inputs),
        "Error"
      )
    )
    dat
  })

  ######################  end of data set creation  ############################


  model_fit <- eventReactive(dat(), {

    #https://www.gitmemory.com/issue/dreamRs/shinyWidgets/301/662290306
    shinyjs::disable("download_results_bttn")
    # updateTabItems(session = session, inputId = "my_tabs", selected = "results")
    dat <- dat()
    dat_no_zeroes <- dat[dat$inoculum != 0, ]
    fit1 <- NA
    # Step 1: catch errors and warnings
    tst_glmer1 <- tryCatch(
      lme4::glmer(cbind(npos, ntest - npos) ~
          offset(log(sample_size)) + offset(log(inoculum)) + (1 | lab_id),
        data = dat_no_zeroes, family = binomial(link = cloglog),
        control = lme4::glmerControl(optimizer = "bobyqa"), nAGQ = 21
      ),
      error = function(e) e,
      warning = function(w) w
    )
    error_glmer1   <- ifelse(is(tst_glmer1, "error"), 1, 0)    # 1 = error
    warning_glmer1 <- ifelse(is(tst_glmer1, "warning"), 1, 0)  # 1 = warning
    error_glmer2   <- 0
    warning_glmer2 <- 0
    if (error_glmer1 == 0 && warning_glmer1 == 0) {
      tst_glmer2 <- tryCatch(
        summary(
          tst_glmer1
        ),
        error = function(e) e,
        warning = function(w) w
      )
      error_glmer2   <- ifelse(is(tst_glmer2, "error"), 1, 0)    # 1 = error
      warning_glmer2 <- ifelse(is(tst_glmer2, "warning"), 1, 0)  # 1 = warning
    }
    use.glmer  <- 1
    model_type <- "random intercept"
    if (error_glmer1 || warning_glmer1 || error_glmer2 || warning_glmer2) {
      use.glmer <- 0
      model_type <- "fixed effects"
    }

    # ##################  for testing only  ######################################
    # use.glmer <- 0
    # model_type <- "fixed effects"

    # if (input$choose_model == "fixed effects") {
    #   use.glmer <- 0
    #   model_type <- "fixed effects"
    # } else if (input$choose_model == "random intercept") {
    #   use.glmer <- 1
    # }
    # ############################################################################

    shinyWidgets::updateProgressBar(session = session,
      id = "progress_data", value = 100
    )
    shinyWidgets::closeSweetAlert(session = session)

    # Step 2: fit model
    if (use.glmer == 1) {

      shinyWidgets::progressSweetAlert(session = session,
        id = "progress_alert", title = "Fitting random intercept model",
        value = 0, display_pct = TRUE
      )
      # GHQ
      fit1 <- lme4::glmer(cbind(npos, ntest - npos) ~
          offset(log(sample_size)) + offset(log(inoculum)) + (1 | lab_id),
        data = dat_no_zeroes, family = binomial(link = cloglog),
        control = lme4::glmerControl(optimizer = "bobyqa"), nAGQ = 20
      )
      ##################
      ## fixed effect ##
      ##################
      summary_fit1 <- summary(fit1)
      coef_fit1    <- coef(summary_fit1)
      mu_log       <- coef_fit1["(Intercept)", "Estimate"]    #log mean effect
      sd.mu_log    <- coef_fit1["(Intercept)", "Std. Error"]  #SE of log mean effect
      mu           <- exp(mu_log)                             #mean effect
      ######################################
      ## random effect standard deviation ##
      ######################################
      if (as.numeric(summary_fit1$varcor) < 0) {
        sigma <- NA
      } else {
        sigma <- sqrt(as.numeric(summary_fit1$varcor))  #SD of log lab effects
      }
      ###################
      ## calculate ICC ##
      ###################
      vc <- lme4::VarCorr(fit1)  ## default print method: standard dev and corr
      ## both variance and std.dev.
      #print(vc,comp=c("Variance","Std.Dev."))
      vc2 <- as.data.frame(vc)
      var.between <- var.lab <- vc2[1, "vcov"]
      var.within  <- var.residual <- (pi ^ 2) / 6
      var.total   <- var.between + var.within
      ICC <- var.between / var.total
    }  #end of use.glmer == 1

    warning_messages_results_UI   <- ""
    warning_messages_results_xlsx <- ""

    if (use.glmer == 0) {

      # Alternative method when the first method fails: use glm.
      #   No random effects are estimated.
      shinyWidgets::progressSweetAlert(session = session,
        id = "progress_alert",
        title = "Fitting fixed effects model",
        value = 0, display_pct = TRUE
      )
      shinyalert::shinyalert(
        title = "Warning",
        text = paste(
          "A random intercept model cannot be used for this data.<br>",
          "A model with only <em>fixed effects</em> is used instead."
        ),
        html = TRUE, type = "warning", timer = 0
      )
      warning_messages_results_UI <- paste0(
        "<br><strong>Warnings: </strong><br>",
        "<ul>",
          "<li>",
            "A random intercept model cannot be used for this data. ",
            "A model with only <em>fixed effects</em> is used instead.",
          "</li>"
      )
      warning_messages_results_xlsx <- c(
        "",
        "Warnings: ",
        paste0(
          "A random intercept model cannot be used for this data. ",
          "A model with only fixed effects is used instead."
        )
      )
      tst_glm <- tryCatch(
        stats::glm(cbind(npos, ntest - npos) ~
            offset(log(sample_size)) + offset(log(inoculum)),
          data = dat_no_zeroes, family = binomial(link = cloglog)
        ),
        error = function(e) e,
        warning = function(w) w
      )
      error_glm   <- ifelse(is(tst_glm, "error"), 1, 0)    # 1 = error
      warning_glm <- ifelse(is(tst_glm, "warning"), 1, 0)  # 1 = warning
      if (error_glm || warning_glm) {
        warning_messages_results_UI <- paste0(
          warning_messages_results_UI,
          "<li>",
            "POD and LOD estimates may be inaccurate because of a ",
            "lack of variation in the outcome or an issue with the model.",
          "</li>"
        )
        warning_messages_results_xlsx <- c(
          warning_messages_results_xlsx,
          paste0(
            "POD and LOD estimates may be inaccurate because of a ",
            "lack of variation in the outcome or an issue with the model."
          )
        )
        shinyalert::shinyalert(
          title = "Warning",
          text = paste0(
            "POD and LOD estimates may be inaccurate because of a lack of ",
            "variation in the outcome or an issue with the model."
          ),
          type = "warning", timer = 0
        )
      }
      # no random effects
      fit1 <- stats::glm(cbind(npos, ntest - npos) ~
          offset(log(sample_size)) + offset(log(inoculum)),
        data = dat_no_zeroes, family = binomial(link = cloglog)
      )
      # fixed effect
      summary_fit1 <- summary(fit1)
      coef_fit1    <- coef(summary_fit1)
      mu_log       <- coef_fit1[1]  #log mean effect
      sd.mu_log    <- coef_fit1[2]  #SE of log mean effect
      mu           <- exp(mu_log)   #mean effect
      # lme4::lmer() (i.e., LMM) for ICC only
      # Un-grouping the data (1 row per test tube)
      y1 <- data.frame(
        lapply(dat, function(x) rep(x, dat$npos)),
        y = 1
      )
      y1 <- y1[, c("lab_id", "inoculum", "y")]
      y0 <- data.frame(
        lapply(dat, function(x) rep(x, dat$ntest - dat$npos)),
        y = 0
      )
      y0 <- y0[, c("lab_id", "inoculum", "y")]
      my_linear_data <- rbind(y1, y0)
      tst_lmer1 = tryCatch(
        lme4::lmer(y ~ (1 | lab_id) + inoculum,
          data = my_linear_data, REML = TRUE,
          control = lme4::lmerControl(
            check.conv.singular = lme4::.makeCC(
              action = "ignore", tol = 1e-4
            )
          )
        ),
        error = function(e) e,
        warning = function(w) w
      )
      error_lmer1   <- ifelse(is(tst_lmer1, "error"), 1, 0)  # 1 = error
      warning_lmer1 <- ifelse(is(tst_lmer1, "warning"), 1, 0)
      error_lmer2   <- 0
      warning_lmer2 <- 0
      if (error_lmer1 == 0 && warning_lmer1 == 0) {
        tst_lmer2 <- tryCatch(
          summary(tst_lmer1),
          error = function(e) e,
          warning = function(w) w
        )
        error_lmer2   <- ifelse(is(tst_lmer2, "error"), 1, 0)  # 1 = error
        warning_lmer2 <- ifelse(is(tst_lmer2, "warning"), 1, 0)
      }
      #if (FALSE) {   #to force ANCOVA estimate for testing
      if (!(error_lmer1 || warning_lmer1 || error_lmer2 || warning_lmer2)) {
        # LMM approach for ICC only
        shinyalert::shinyalert(
          title = "Warning",
          text = "ICC is calculated based on a <em>linear mixed effects</em> model.",
          html = TRUE,
          type = "warning",
          timer = 0
        )
        warning_messages_results_UI <- paste0(
          warning_messages_results_UI,
          "<li>",
            "ICC is calculated based on a <em>linear mixed effects</em> model.",
          "</li>"
        )
        warning_messages_results_xlsx <- c(
          warning_messages_results_xlsx,
          "ICC is calculated based on a linear mixed effects model."
        )
        fit_lmer <- lme4::lmer(y ~ (1 | lab_id) + inoculum,
          data = my_linear_data, REML = TRUE,
          control = lme4::lmerControl(
            check.conv.singular = lme4::.makeCC(
              action = "ignore", tol = 1e-4
            )
          )
        )
        vc <- lme4::VarCorr(fit_lmer)
        vc <- as.data.frame(vc)
        var_between <- vc[vc$grp == "lab_id", "vcov"]
        var_within  <- vc[vc$grp == "Residual", "vcov"]
        var_total   <- var_between + var_within
        ICC <- var_between / var_total
        if (var_between < 0) {
          sigma <- NA
        } else {
          sigma <- sqrt(var_between)  #SD of log lab effects (random effects distribution)
        }
      } else {
        # ANCOVA approach for ICC when LMM fails
        shinyalert::shinyalert(
          title = "Warning",
          text = "ICC is calculated using an <em>analysis of covariance</em> approach.",
          html = TRUE, type = "warning", timer = 0
        )
        warning_messages_results_UI <- paste0(
          warning_messages_results_UI,
          "<li>",
            "ICC is calculated using an <em>analysis of covariance</em> approach.",
          "</li>"
        )
        warning_messages_results_xlsx <- c(
          warning_messages_results_xlsx,
          "ICC is calculated using an analysis of covariance approach."
        )
        # use the method in Stanish and Taylor (1983)
        fit_lm <- stats::lm(
          y ~ as.factor(lab_id) + I(inoculum - mean(inoculum)),
          data = my_linear_data
        )
        n_labs <- length(unique(dat$lab_id))
        ns <- aggregate(
          y ~ as.factor(lab_id), data = my_linear_data,
          FUN = length
        )[, "y"]
        n0 <- (1 / (n_labs - 1)) * (sum(ns) - sum(ns ^ 2) / sum(ns))
        grand_mean_inoculum <- mean(my_linear_data$inoculum)
        data_inoculum_lab <- aggregate(
          inoculum ~ lab_id, data = my_linear_data,
          FUN = mean
        )
        colnames(data_inoculum_lab)[2] <- "mean_inoculum"
        numer <- sum(ns ^ 2 * (data_inoculum_lab$mean_inoculum - grand_mean_inoculum) ^ 2)
        denom <- sum((my_linear_data$y - grand_mean_inoculum) ^ 2)
        n01 <- (1 / (n_labs - 1)) * ((n_labs - 1) * n0 - (numer / denom))  #Eq 2.4
        fit_anova   <- anova(fit_lm)
        var_within  <- fit_anova$"Mean Sq"[3]  #MS Residuals
        MS_labs     <- fit_anova$"Mean Sq"[1]  #MS Labs
        var_between <- (MS_labs - var_within) / n01
        var_total   <- var_within + var_between
        ICC         <- var_between / var_total
        if (var_between < 0) {
          sigma <- NA
        } else {
          sigma <- sqrt(var_between)  #SD of log lab effects (random effects distribution)
        }
      }  #end of ANCOVA
    }  #end of use.glmer == 0

    mu_char        <- formatC(mu, digits = 3, format = "f")
    mu_log_char    <- formatC(mu_log, digits = 3, format = "f")
    sd.mu_log_char <- formatC(sd.mu_log, digits = 3, format = "f")
    if (is.na(ICC)) {
      ICC_char <- "N/A"
    } else {
      ICC_char <- formatC(ICC, digits = 3, format = "f")
    }
    if (is.na(sigma)) {
      sigma_char <- "N/A"
    } else {
      sigma_char <- formatC(sigma, digits = 3, format = "f")
    }
    output$warning_messages_results_UI <- renderUI({
      tags$p(div(HTML(
        "<span style = 'font-size:150%; color:red'>",
        warning_messages_results_UI,
        "</ul>"
      )))
    })
    list(
      dat_no_zeroes = dat_no_zeroes, model_type = model_type, fit1 = fit1,
      warning_messages_results_xlsx = warning_messages_results_xlsx,
      mu = mu, mu_char = mu_char,
      mu_log = mu_log, mu_log_char = mu_log_char,
      sd.mu_log = sd.mu_log, sd.mu_log_char = sd.mu_log_char,
      sigma = sigma, sigma_char = sigma_char,
      ICC = ICC, ICC_char = ICC_char
    )
  })
  ######################  end of model fitting  ################################


  POD_LOD <- eventReactive(model_fit(), {
    dat <- dat()
    inoc_levels <- sort(unique(dat$inoculum))
    sample_size     <- calculate_clicked()$sample_size
    lod_unit        <- calculate_clicked()$lod_unit
    lod_prob        <- calculate_clicked()$lod_prob
    conf_level_prob <- calculate_clicked()$conf_level_prob
    lab_names       <- calculate_clicked()$lab_names
    my_alpha        <- 1 - conf_level_prob
    dat_no_zeroes <- model_fit()$dat_no_zeroes
    model_type    <- model_fit()$model_type
    fit1          <- model_fit()$fit1
    mu            <- model_fit()$mu
    sd.mu_log     <- model_fit()$sd.mu_log

    shinyWidgets::updateProgressBar(session = session,
      id = "progress_alert", value = 10
    )

    if (model_type == "random intercept") {

      ####################################################
      ## find effect of each lab and its standard error ##
      ####################################################
      #  Individual lab effect equals fixed effect + random effect by labID.
      #    (i.e., overall effect + deviation)
      #  Standard error for individual lab effect (intercept) equals sqrt of
      #    variance for mean lab effect + variance for random effects.
      # random effects w/ conditional sd
      dd <- as.data.frame(lme4::ranef(fit1))
      lab_effects <- data.frame(
        labID            = dd$grp,
        lab_names        = lab_names,
        estimated.effect = lme4::fixef(fit1)[['(Intercept)']] + dd$condval,
        SE               = sqrt(sd.mu_log ^ 2 + dd$condsd ^ 2)
      )
      ###############
      ## find d0.5 ##
      ###############
      # find LOD
      # see 'helpers.R'
      tst_LOD <- tryCatch(
        LODpoint(model = fit1, value = lod_prob,
          sample_size = sample_size, inoculum = dat$inoculum
        ),
        error = function(e) e,
        warning = function(w) w
      )
      is_LOD_error <- is(tst_LOD, "error")
      if (is_LOD_error) {
        shinyWidgets::sendSweetAlert(session = session,
          title = "LOD Calculation Error", type = "error",
          text = "Possibly due to zero-probability predictions."
        )
      }
      validate(
        need(try(!is_LOD_error),
          "LOD Error"
        )
      )
      LOD <- LODpoint(model = fit1, value = lod_prob,
        sample_size = sample_size, inoculum = dat$inoculum
      )

      shinyWidgets::updateProgressBar(session = session,
        id = "progress_alert", value = 40
      )

      ############################################################
      ## POD for each lab (i.e., include RE).                   ##
      ## 95% confidence interval for each lab was not estimated ##
      ############################################################
      ds <- c(LOD, seq(from = 1e-9, to = max(dat$inoculum), by = 1e-3))
      ds <- sort(unique(ds))
      newdata <- expand.grid(lab_id = unique(dat$lab_id), inoculum = ds)
      newdata <- data.frame(sample_size = sample_size, newdata)
      df.predicted1 <- newdata  #for individual lab PODs
      predict.fun1 <- function(model) {
        # individual lab PODs
        # This is predict.merMod
        predict(model, newdata = df.predicted1, re.form = NULL, type = "response")
      }
      df.predicted1$POD <- predict.fun1(fit1)  #predicted lab PODs using the model
      #####################################
      ## population level POD and 95% CI ##
      #####################################
      ds <- c(1e-9, LOD, seq(from = min(1e-4, LOD), to = max(dat$inoculum), by = 1e-5))
      ds <- sort(unique(ds))
      df.predicted <- data.frame(
        sample_size = sample_size, lab_id = NA, inoculum = ds
      )
      predict.fun <- function(model) {
        # mean POD
        # This is predict.merMod
        # i.e., fitted values, unconditional (level-0)
        predict(model, newdata = df.predicted, re.form = NA, type = "response")
      }
      df.predicted$meanPOD <- predict.fun(fit1)  #predicted mean POD using the model
      ## get 95% CI for mean POD
      #https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html
      #  from link above - Step 3a: lme4::bootMer() method 1
      ####Return predicted values from bootstrap
      mySumm <- function(.) {
        predict(., newdata = df.predicted, re.form = NA, type = "response")
      }
      ####Collapse bootstrap into 95% PI
      sumBoot <- function(merBoot) {
        return(
          data.frame(
            lwr = apply(merBoot$t, MARGIN = 2,
                        FUN = function(x) {
                          as.numeric(quantile(x, probs = my_alpha / 2, na.rm = TRUE))
                        }
                  ),
            upr = apply(merBoot$t, MARGIN = 2,
                        FUN = function(x) {
                          as.numeric(quantile(x, probs = 1 - (my_alpha / 2), na.rm = TRUE))
                        }
                  )
          )
        )
      }
      ##lme4::bootMer() method
      boot1 <- lme4::bootMer(fit1, FUN = mySumm, nsim = 500, seed = 27181,
                             re.form = NA, type = "parametric")

      shinyWidgets::updateProgressBar(session = session,
        id = "progress_alert", value = 80
      )

      PI.boot1 <- sumBoot(boot1)
      meanPOD.L <- PI.boot1[, 1]
      meanPOD.U <- PI.boot1[, 2]
      # gather results
      df.predicted <- cbind(df.predicted, meanPOD.L, meanPOD.U)
      #######################
      ## find 95% CI LOD   ##
      #######################
      #find intersections of horizontal probability line and POD interval curves
      LOD.L <- ds[which.min(abs(meanPOD.U - lod_prob))]  #lod_prob=0.5 for LOD50
      LOD.U <- ds[which.min(abs(meanPOD.L - lod_prob))]

      shinyWidgets::updateProgressBar(session = session,
        id = "progress_alert", value = 90
      )

    } else if (model_type == "fixed effects") {

      # individual intercept and its SE for each lab
      fit2 <- stats::glm(cbind(npos, ntest - npos) ~
          0 + offset(log(sample_size)) + as.factor(lab_id) + offset(log(inoculum)),
        data = dat_no_zeroes, family = binomial(link = cloglog)
      )
      summary_fit2 <- summary(fit2)
      coef_fit2    <- coef(summary_fit2)
      lab_effects <- data.frame(
        labID            = unique(dat_no_zeroes$lab_id),
        lab_names        = lab_names,
        estimated.effect = as.numeric(coef_fit2[, "Estimate"]),
        SE               = as.numeric(coef_fit2[, "Std. Error"])
      )
      rownames(lab_effects) <- NULL

      shinyWidgets::updateProgressBar(session = session,
        id = "progress_alert", value = 50
      )

      ##################################
      ## Estimate LOD50 (d0.5) and CI ##
      ##################################
      # equations (32), (33) from Jarvis et al. 2019
      my_df <- length(unique(dat_no_zeroes$lab_id)) - 1
      t_critval <- qt(p = 1 - (my_alpha / 2), df = my_df)
      LOD <- -log(1 - lod_prob) / (sample_size * mu)
      exp_t_sd.mu <- exp(t_critval * sd.mu_log)
      LOD.U <- LOD * exp_t_sd.mu
      LOD.L <- LOD / exp_t_sd.mu

      #########################
      ## Estimate POD and CI
      #########################
      # POD for each lab
      # confidence interval for each lab was not estimated
      ds <- c(LOD, seq(from = 1e-9, to = max(dat$inoculum), by = 1e-3))
      ds <- sort(unique(ds))
      newdata <- expand.grid(lab_id = unique(dat$lab_id), inoculum = ds)
      newdata <- data.frame(sample_size = sample_size, newdata)
      df.predicted1 <- newdata  #for each lab
      predict.fun <- function(model) {
        #for each lab
        predict(model, newdata = df.predicted1, type = "response")
      }
      df.predicted1$POD <- predict.fun(fit2)
      ################################
      # population level POD and CI
      ################################
      ds <- c(LOD, seq(from = 1e-9, to = max(dat$inoculum), by = 1e-4))
      ds <- sort(unique(ds))
      df.predicted <- data.frame(
        sample_size = sample_size, lab_id = NA, inoculum = ds
      )
      predict.fun <- function(model) {
        predict(model, newdata = df.predicted, type = "response", se.fit = TRUE)
      }
      preds <- predict.fun(fit1)  #for mean POD
      df.predicted$meanPOD <- preds$fit
      ME <- t_critval * preds$se.fit
      df.predicted$meanPOD.U <- preds$fit + ME
      df.predicted$meanPOD.L <- preds$fit - ME
      shinyWidgets::updateProgressBar(session = session,
        id = "progress_alert", value = 90
      )
    } else {
      stop("Problem with model_type")
    }  #end of fixed effects

    ######################################################
    ## LOD and its confidence interval per test portion ##
    ######################################################
    if (lod_unit == "CFU/test portion") {
      LOD   <- LOD * sample_size
      LOD.L <- LOD.L * sample_size
      LOD.U <- LOD.U * sample_size
    }
    LOD_char   <- formatC(LOD, digits = 3, format = "f")
    LOD.L_char <- formatC(LOD.L, digits = 3, format = "f")
    LOD.U_char <- formatC(LOD.U, digits = 3, format = "f")
    list(
      df.predicted = df.predicted, df.predicted1 = df.predicted1,
      lab_effects = lab_effects, inoc_levels = inoc_levels,
      LOD = LOD, LOD.L = LOD.L, LOD.U = LOD.U,
      LOD_char = LOD_char, LOD.L_char = LOD.L_char, LOD.U_char = LOD.U_char
    )
  })

  ######################  end of POD_LOD  ######################################


  #-------------------------  displayed  results  ------------------------------

  observeEvent(POD_LOD(), {

    # shinyjs::delay(2000, {
    #   updateTabItems(session = session, inputId = "my_tabs", selected = "results")
    #   shinyjs::delay(3000,
    #     shinyjs::enable("download_results_bttn")
    #   )
    # })

    output$log_mean_effect <- renderValueBox({
      valueBox(
        value = withMathJax(HTML(
          "<span style = 'font-size:55%; font-family:Courier; color:white'>",
            paste0("Mean Lab Effect \\((", "\\widehat{\\mu}", "\\))"),
          "</span>"
        )),
        subtitle = tags$p(HTML(
          "<span style = 'font-size:150%; font-family:Courier; color:white'>",
            "<strong>", model_fit()$mu_log_char, "</strong>",
          "</span>"
        )),
        width = 12, icon = NULL, color = "navy"
      )
    })

    output$se_log_mean_effect <- renderValueBox({
      valueBox(
        value = withMathJax(HTML(
          "<span style = 'font-size:54%; font-family:Courier; color:white'>",
            "SE of Mean Lab Effect",
          "</span>"
        )),
        subtitle = tags$p(HTML(
          "<span style = 'font-size:150%; font-family:Courier; color:white'>",
            "<strong>", model_fit()$sd.mu_log_char, "</strong>",
           "</span>"
        )),
        width = 12, icon = NULL, color = "navy"
      )
    })

    output$ICC <- renderValueBox({
      valueBox(
        value = withMathJax(HTML(
          "<span style = 'font-size:60%; font-family:Courier; color:white'>",
            "ICC",
          "</span>"
        )),
        subtitle = tags$p(HTML(
          "<span style = 'font-size:150%; font-family:Courier; color:white'>",
            "<strong>", model_fit()$ICC_char, "</strong>",
          "</span>"
        )),
        width = 12, icon = NULL, color = "maroon"
      )
    })

    output$LOD <- renderValueBox({
      valueBox(
        value = withMathJax(HTML(
          "<span style = 'font-size:60%; font-family:Courier; color:white'>",
            paste0("LOD<sub>", calculate_clicked()$lod_perc, "</sub>"),
          "</span>"
        )),
        subtitle = tags$p(HTML(
          "<span style = 'font-size:150%; font-family:Courier; color:white'>",
            "<strong>", POD_LOD()$LOD_char, "</strong>",
          "</span>"
        )),
        width = 12, icon = NULL, color = "blue"
      )
    })

    output$LOD_LCL <- renderValueBox({
      valueBox(
        value = withMathJax(HTML(
          "<span style = 'font-size:60%; font-family:Courier; color:white'>",
            paste0("LOD<sub>", calculate_clicked()$lod_perc), "</sub>",
            calculate_clicked()$conf_level, "LCL",
          "</span>"
        )),
        subtitle = tags$p(HTML(
          "<span style = 'font-size:150%; font-family:Courier; color:white'>",
            "<strong>", POD_LOD()$LOD.L_char, "</strong>",
          "</span>"
        )),
        width = 12, icon = NULL, color = "blue"
      )
    })

    output$LOD_UCL <- renderValueBox({
      valueBox(
        value = withMathJax(HTML(
          "<span style = 'font-size:60%; font-family:Courier; color:white'>",
            paste0("LOD<sub>", calculate_clicked()$lod_perc), "</sub>",
            calculate_clicked()$conf_level, "UCL",
          "</span>"
        )),
        subtitle = tags$p(HTML(
          "<span style = 'font-size:150%; font-family:Courier; color:white'>",
            "<strong>", POD_LOD()$LOD.U_char, "</strong>",
          "</span>"
        )),
        width = 12, icon = NULL, color = "blue"
      )
    })

  })
  ########################  end of displayed results  ##########################


  plotCurves <- eventReactive(POD_LOD(), {

    inoc_levels <- POD_LOD()$inoc_levels
    inoc_max <- max(inoc_levels)
    sample_size <- calculate_clicked()$sample_size
    lod_unit    <- calculate_clicked()$lod_unit
    conf_level  <- calculate_clicked()$conf_level
    lod_choice  <- calculate_clicked()$lod_choice
    lod_prob    <- calculate_clicked()$lod_prob
    lod_perc    <- calculate_clicked()$lod_perc
    df.predicted  <- POD_LOD()$df.predicted
    df.predicted1 <- POD_LOD()$df.predicted1
    LOD           <- POD_LOD()$LOD
    LOD_rounded   <- round(LOD, digits = 3)
    LOD.L         <- POD_LOD()$LOD.L
    LOD.L_rounded <- round(LOD.L, digits = 3)
    LOD.U         <- POD_LOD()$LOD.U
    LOD.U_rounded <- round(LOD.U, digits = 3)
    dat     <- dat()
    dat$POD <- dat$npos / dat$ntest
    conf_level_no_perc <- gsub("%", replacement = "", conf_level, fixed = TRUE)
    #https://stackoverflow.com/questions/43415217/how-do-i-add-percentage-and-fractions-to-ggplot-geom-text-label
    my_label <- paste0("LOD[", lod_perc, "] == ", LOD_rounded)
    if (lod_unit == "CFU/test portion") {
      dat$inoculum           <- dat$inoculum * sample_size
      df.predicted1$inoculum <- df.predicted1$inoculum * sample_size
      df.predicted$inoculum  <- df.predicted$inoculum * sample_size
      inoc_levels <- inoc_levels * sample_size
      inoc_max    <- max(inoc_levels)
    }
    my_breaks_x <- unique(c(0, inoc_levels))
    if (inoc_max < 0.5) {
      inoc_incr <- .01
    } else if (inoc_max < 8) {
      inoc_incr <- .1
    } else {
      inoc_incr <- 1
    }

    my_plot <- ggplot(data = dat,
      aes(x = inoculum, y = POD, color = as.factor(lab_id))
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
      labs(color = 'Lab ID') # change legend title

    # combine plots
    p.all <- my_plot +
      geom_line(data = df.predicted1, aes(x = inoculum, y = POD)) +
      geom_line(data = df.predicted, aes(x = inoculum, y = meanPOD),
        linetype = "solid", color = "red", size = rel(2)
      ) +
      geom_ribbon(data = df.predicted,
        aes(x = inoculum, ymin = meanPOD.L, ymax = meanPOD.U),
        fill = "blue4", alpha = 0.5, inherit.aes = FALSE
      )

    # add LOD and CI
    vjust <- .01 * inoc_max

    p.all <- p.all +
      # do not use aes to avoid double legend
      geom_segment(x = LOD.L, xend = LOD.L, y = 0, yend = lod_prob,
        colour = "blue", size = rel(1)
      ) +
      geom_segment(x = LOD.U, xend = LOD.U, y = 0, yend = lod_prob,
        colour = "blue", size = rel(1)
      ) +
      geom_segment(x = LOD, xend = LOD, y = 0, yend = lod_prob,
        colour = "blue", size = rel(1)
      ) +
      geom_segment(x = 0, xend = LOD.U, y = lod_prob, yend = lod_prob,
        colour = "blue", size = rel(1)
      ) +
      annotate("text", x = LOD.L - vjust, y = lod_prob / 2,
        label = LOD.L_rounded, size = rel(5), angle = 90
      ) +
      annotate("text", x = LOD.U + vjust, y = lod_prob / 2,
        label = LOD.U_rounded, size = rel(5), angle = 90
      ) +
      annotate("text", x = LOD - vjust, y = lod_prob / 2,
        label = paste(lod_choice, "= ", LOD_rounded),
        size = rel(5), angle = 90
      ) +
      annotate("label", x = 0.75 * max(dat$inoculum), y = 0.375,
        label = my_label, parse = TRUE,
        size = rel(8), color = "red", fontface = "bold",
        label.padding = unit(0.75, "lines"),
        label.r = unit(0.15, "lines"), label.size = 0.5
      )

    p.all +
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
        axis.ticks = element_line(size = rel(1.5)),
        axis.line = element_line(size = rel(1.5)),
        legend.background = element_rect(fill = "grey90"),
        legend.margin     = margin(t = 5, r = 10, b = 5, l = 10, unit = "pt"),
        legend.text       = element_text(size = rel(1.25), color = "black"),
        legend.title      = element_text(size = rel(1.5), color = "black"),
        panel.background = element_rect(fill = "grey90", color = "white"),
        panel.border = element_rect(
          linetype = "solid", size = rel(3), color = "black", fill = NA
        ),
        panel.grid.major.x = element_line(size = rel(1), color = "white"),
        panel.grid.major.y = element_line(size = rel(2.5), color = "white"),
        panel.grid.minor   = element_line(size = rel(1), color = "white"),
        plot.background = element_rect(fill = "white"),
        plot.title = element_text(
          size = rel(2.25), color = "black", hjust = 0.5,
          margin = margin(t = 20, r = 0, b = 20, l = 0, unit = "pt")
        ),
        plot.title.position = "panel",
        plot.margin = unit(rep(0.25, 4), units = "inches")
      )
  })

  #########################  end of plot curves  ###############################


  observeEvent(plotCurves(), {
    output$POD_plots <- renderPlot({
      plotCurves()
    })
    shinyjs::delay(2000, {
      updateTabItems(session = session, inputId = "my_tabs", selected = "results")
      shinyjs::delay(3000,
        shinyjs::enable("download_results_bttn")
      )
    })
    shinyWidgets::updateProgressBar(session = session,
      id = "progress_alert", value = 100
    )
    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(session = session,
      title = "Calculations complete", type = "success"
    )
  })


  #https://mastering-shiny.org/action-transfer.html
  output$download_results <- downloadHandler(
    filename = function() {
      paste0(
        "MultiLab_POD_LOC_ICC_results_",
        gsub(":", replacement = "_", calculate_clicked()$date_time, fixed = TRUE),
        ".xlsx"
      )
    },
    content = function(file) {
      shinybusy::show_modal_spinner(
        spin = "double-bounce", color = "#112446",
        text = "Creating your report...", session = session
      )
      on.exit(
        shinybusy::remove_modal_spinner(session = session),
        add = TRUE
      )
      #to make sure we have read/write permissions
      temp_path <- file.path(tempdir(), "temp_file.xlsx")
      file.copy(from = "temp_file.xlsx", to = temp_path, overwrite = TRUE)
      workbook <- openxlsx::createWorkbook()
      colnames_style <- openxlsx::createStyle(textDecoration = "bold")
      openxlsx::addWorksheet(workbook, sheetName = "Experiment Description")
      openxlsx::freezePane(workbook, sheet = "Experiment Description", firstRow = TRUE)
      calc <- calculate_clicked()
      my_exp_description <-
        c("EXPERIMENT DESCRIPTION/",
          paste0("Experiment name:  ", calc$exp_name),
          paste0("Experiment date:  ", calc$exp_date),
          paste0("Microorganism:  ",   calc$microorganism),
          paste0("Food matrix:  ",     calc$matrix),
          paste0("Test portion size (g or mL):  ", calc$sample_size),
          paste0("LOD unit:  ",        calc$lod_unit),
          paste0("How many labs?  ",   calc$num_labs),
          paste0("How many inoculum levels?  ", calc$num_levels),
          paste0("Use example data?  ", calc$use_example)
        )
      openxlsx::conditionalFormatting(workbook, sheet = "Experiment Description",
        cols = 1, rows = 1:100,
        type = "contains", rule = "EXPERIMENT DESCRIPTION/",
        style = colnames_style
      )
      openxlsx::setColWidths(workbook, sheet = "Experiment Description",
        cols = 1, widths = 100
      )
      openxlsx::writeData(workbook, sheet = "Experiment Description", x = my_exp_description)
      openxlsx::addWorksheet(workbook, sheetName = "Analysis")
      openxlsx::freezePane(workbook, sheet = "Analysis", firstRow = TRUE)
      my_analysis <-
        c("ANALYSIS/",
          paste0("Analysis date:  ",    calc$date_time),
          paste0("LOD choice:  ",       calc$lod_choice),
          paste0("Confidence level:  ", calc$conf_level),
          paste0("Model used:  ",       model_fit()$model_type)
        )
      openxlsx::conditionalFormatting(workbook, sheet = "Analysis",
        cols = 1, rows = 1:100,
        type = "contains", rule = "ANALYSIS/",
        style = colnames_style
      )
      openxlsx::setColWidths(workbook, sheet = "Analysis", cols = 1, widths = 100)
      openxlsx::writeData(workbook, sheet = "Analysis", x = my_analysis)
      openxlsx::addWorksheet(workbook, sheetName = "App Information")
      openxlsx::freezePane(workbook, sheet = "App Information", firstRow = TRUE)
      my_app_info <-
        c(
          "APP INFORMATION/",
          paste0("App name:  ",    glob_app_title),
          paste0("App version:  ", glob_app_version),
          my_R_version
        )
      openxlsx::conditionalFormatting(workbook, sheet = "App Information",
        cols = 1, rows = 1:100,
        type = "contains", rule = "APP INFORMATION/",
        style = colnames_style
      )
      openxlsx::setColWidths(workbook, sheet = "App Information",
        cols = 1, widths = 100
      )
      openxlsx::writeData(workbook, sheet = "App Information", x = my_app_info)
      openxlsx::addWorksheet(workbook, sheetName = "Results")
      openxlsx::freezePane(workbook, sheet = "Results", firstRow = TRUE)
      my_results <-
        c("RESULTS/",
          paste0("Mean Lab Effect:  ",       model_fit()$mu_log_char),
          paste0("SE of Mean Lab Effect:  ", model_fit()$sd.mu_log_char),
          paste0("ICC:  ",                   model_fit()$ICC_char),
          paste0("Standard Deviation of Lab Effects:  ", model_fit()$sigma_char),
          paste0(calc$lod_choice, ":  ",               POD_LOD()$LOD_char),
          paste0(calc$lod_choice, " (lower limit):  ", POD_LOD()$LOD.L_char),
          paste0(calc$lod_choice, " (upper limit):  ", POD_LOD()$LOD.U_char),
          paste0("Confidence level:  ", calc$conf_level),
          model_fit()$warning_messages_results_xlsx
        )
      openxlsx::conditionalFormatting(workbook, sheet = "Results",
        cols = 1, rows = 1:100,
        type = "contains", rule = "RESULTS/",
        style = colnames_style
      )
      openxlsx::setColWidths(workbook, sheet = "Results", cols = 1, widths = 100)
      openxlsx::writeData(workbook, sheet = "Results", x = my_results)
      openxlsx::addWorksheet(workbook, sheetName = "POD Curves")
      #https://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
      plot_width  <- 10
      plot_height <- glob_plot_aspect_ratio * plot_width
      ggsave(filename = "my_plot.png", path = tempdir(),
        plot = plotCurves(), device = "png",
        scale = 2,
        width = plot_width, height = plot_height, units = "in",
        dpi = 600
      )
      openxlsx::insertImage(workbook, sheet = "POD Curves",
        file = paste0(tempdir(), "/", "my_plot.png"),
        width = plot_width, height = plot_height, units = "in",
        startRow = 2, startCol = 2,
        dpi = 600
      )
      openxlsx::addWorksheet(workbook, sheetName = "Lab Effects")
      openxlsx::freezePane(workbook, sheet = "Lab Effects", firstRow = TRUE)
      my_lab_effects <- POD_LOD()$lab_effects
      my_lab_effects$lab_names <- calc$lab_names
      my_lab_effects <- dplyr::rename(my_lab_effects,
        "Lab ID"           = labID,
        "Lab Name"         = lab_names,
        "Estimated effect" = estimated.effect,
        "Standard error"   = SE
      )
      openxlsx::setColWidths(workbook, sheet = "Lab Effects",
        cols = 1:ncol(my_lab_effects), widths = c(8, 16, 16)
      )
      openxlsx::writeData(workbook, sheet = "Lab Effects",
        x = my_lab_effects, headerStyle = colnames_style
      )
      openxlsx::addWorksheet(workbook, sheetName = "Data")
      openxlsx::freezePane(workbook, sheet = "Data", firstRow = TRUE)
      my_data <- dplyr::rename(dat(),
        "Lab ID"            = lab_id,
        "Inoculation level" = inoculum,
        "Tubes tested"      = ntest,
        "Tubes positive"    = npos,
        "Test portion size" = sample_size
      )
      openxlsx::setColWidths(workbook, sheet = "Data",
        cols = 1:ncol(my_data), widths = c(8, 15, 12, 13, 11)
      )
      openxlsx::writeData(workbook, sheet = "Data",
        x = my_data, headerStyle = colnames_style
      )
      openxlsx::addWorksheet(workbook, sheetName = "R Packages")
      openxlsx::freezePane(workbook, sheet = "R Packages", firstRow = TRUE)
      openxlsx::setColWidths(workbook, sheet = "R Packages",
        cols = 1:ncol(my_package_info), widths = c(18, 14, 11)
      )
      openxlsx::writeData(workbook, sheet = "R Packages",
        x = my_package_info, headerStyle = colnames_style
      )
      openxlsx::saveWorkbook(wb = workbook, file = file, overwrite = TRUE)
    },
    contentType = "xlsx"
  )

}

