# "This app implements the random intercept complementary log-log model",
# "suggested by Jarvis et al. (2019) to estimate probability of",
# "detection (POD) and level of detection (LOD) from a multi-laboratory",
# "validation study for a qualitative microbiological assay. This app also",
# "calculates the intraclass correlation coefficient (ICC) to estimate",
# "the proportion of total variance attributable to between-laboratory",
# "variance."

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
      "LOD25" = "25",
      "LOD50" = "50",
      "LOD75" = "75"
    )
  })

  myLODProb <- reactive({
    switch(input$lod_choice,
      "LOD25" = 0.25,
      "LOD50" = 0.5,
      "LOD75" = 0.75
    )
  })

  myConfLevel <- reactive({
    switch(input$conf_level,
      "80%" = 0.8,
      "90%" = 0.9,
      "95%" = 0.95,
      "99%" = 0.99
    )
  })

  ############################  Calculator  ####################################

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
    iter <- input$calculate + 1
    list(
      date_time       = strftime(Sys.time(), format = "", tz = "", usetz = TRUE),
      iter            = iter,
      exp_name        = input$exp_name,
      microorganism   = input$microorganism,
      matrix          = input$matrix,
      exp_date        = input$exp_date,
      sample_size     = input$sample_size,
      sample_unit     = input$sample_unit,
      num_labs        = input$num_labs,
      num_levels      = input$num_levels,
      use_example     = input$use_example,
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

    desc_validate1 <- !is.na(num_labs) && is_wholenumber(num_labs)
    desc_validate2 <- !is.na(num_levels) && is_wholenumber(num_levels)
    if (!desc_validate1) {
      shinyWidgets::sendSweetAlert(session = session,
        title = description_error, type = "error",
        text = "Number of labs must be a whole number."
      )
    }
    if (!desc_validate2) {
      shinyWidgets::sendSweetAlert(session = session,
        title = description_error, type = "error",
        text = "Number of levels must be a whole number."
      )
    }
    validate(
      need(try(desc_validate1 && desc_validate2),
        "Error"
      )
    )
    desc_validate3 <- num_labs   <= glob_max_labs
    desc_validate4 <- num_levels <= glob_max_levels
    if (!desc_validate3) {
      shinyWidgets::sendSweetAlert(session = session,
        title = description_error, type = "error",
        text = paste0("Maximum number of labs is ", glob_max_labs, ".")
      )
    }
    if (!desc_validate4) {
      shinyWidgets::sendSweetAlert(session = session,
        title = description_error, type = "error",
        text = paste0("Maximum number of levels is ", glob_max_levels, ".")
      )
    }
    desc_validate5 <- num_labs   >= glob_min_labs
    desc_validate6 <- num_levels >= glob_min_levels
    if (!desc_validate5) {
      shinyWidgets::sendSweetAlert(session = session,
        title = description_error, type = "error",
        text = paste0("Minimum number of labs is ", glob_min_labs, ".")
      )
    }
    if (!desc_validate6) {
      shinyWidgets::sendSweetAlert(session = session,
        title = description_error, type = "error",
        text = paste0("Minimum number of levels is ", glob_min_levels, ".")
      )
    }
    validate_description <-
      desc_validate3 && desc_validate4 && desc_validate5 && desc_validate6

    validate(
      need(try(validate_description),
        "Error"
      )
    )

    if (use_example) {
      dat <- dat_example
      sample_size <- glob_sample_size_example
    } else {
      sample_size <- calculate_clicked()$sample_size
      ssize_validate <-
        !is.na(sample_size) && is.numeric(sample_size) && sample_size > 0
      if (!ssize_validate) {
        shinyWidgets::sendSweetAlert(session = session,
          title = description_error, type = "error",
          text = "Sample size must be a positive number (without units)."
        )
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

    sum_data <- aggregate(dat, by = list(dat$lab_id), FUN = sum)

    input_validate1 <- all(!is.na(dat))
    input_validate2 <- all(dat >= 0)
    input_validate3 <- all(sum_data$inoculum > 0)  #each lab has >=1 positive level
    input_validate4 <- all(dat$ntest > 0)
    input_validate5 <- all(sum_data$npos > 0)
    input_validate6 <- all(dat$ntest >= dat$npos)
    input_validate7 <-
      all(is_wholenumber(dat$ntest)) && all(is_wholenumber(dat$npos))

    if (!input_validate1) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "Some data are missing or non-numeric."
      )
    } else if (!input_validate2) {
            shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "All data must be non-negative."
      )
    } else if (!input_validate3) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "Each lab must have at least 1 positive inoculum level."
      )
    } else if (!input_validate4) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "Each inoculation level must have at least 1 tube inoculated."
      )
    } else if (!input_validate5) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "Each lab must have at least 1 positive tube."
      )
    } else if (!input_validate6) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "Tubes inoculated must always be >= tubes positive."
      )
    } else if (!input_validate7) {
      shinyWidgets::sendSweetAlert(session = session,
        title = input_error, type = "error",
        text = "Tubes inoculated and tubes positive must be whole numbers."
      )
    }

    validate_inputs <-
      input_validate1 && input_validate2 && input_validate3 &&
         input_validate4 && input_validate5 && input_validate6 &&
         input_validate7

    validate(
      need(try(validate_inputs),
        "Error"
      )
    )

    dat
  })

  # Run calculations
  model_fit <- eventReactive(dat(), {

    output$warning_message1 <- renderUI("")
    output$warning_message2 <- renderUI("")
    #https://www.gitmemory.com/issue/dreamRs/shinyWidgets/301/662290306
    shinyjs::disable("download_results_bttn")
    updateTabItems(session = session, inputId = "my_tabs", selected = "results")

    dat <- dat()
    dat_no_zeroes <- dat[dat$inoculum != 0, ]

    fit1 <- NA; fit2 <- NA; my_alpha <- NA
    sample_size     <- unique(dat$sample_size)
    lod_choice      <- calculate_clicked()$lod_choice
    lod_prob        <- calculate_clicked()$lod_prob
    lod_perc        <- calculate_clicked()$lod_perc
    conf_level      <- calculate_clicked()$conf_level
    conf_level_prob <- calculate_clicked()$conf_level_prob

    # step 1: catch errors and warnings

    tst <- tryCatch(
      lme4::glmer(cbind(npos, ntest - npos) ~
          offset(log(sample_size)) + offset(log(inoculum)) + (1 | lab_id),
        data = dat_no_zeroes, family = binomial(link = cloglog),
        control = lme4::glmerControl(optimizer = "bobyqa"), nAGQ = 21
      ),
      error = function(e) e,
      warning = function(w) w
    )

    error1   <- ifelse(is(tst, "error"), 1, 0)    # 1 = error
    warning1 <- ifelse(is(tst, "warning"), 1, 0)  # 1 = warning

    error2   <- 0
    warning2 <- 0

    if (error1 == 0 && warning1 == 0) {
      tst2 <- tryCatch(
        summary(
          tst
        ),
        error = function(e) e,
        warning = function(w) w
      )
      error2   <- ifelse(is(tst2, "error"), 1, 0)    # 1 = error
      warning2 <- ifelse(is(tst2, "warning"), 1, 0)  # 1 = warning
    }

    use.glmer  <- 1
    model_type <- "random intercept"
    if (error1 || warning1 || error2 || warning2) {
      use.glmer <- 0
      model_type <- "fixed effects"
    }

    # ##################  for testing only  ######################################
    # if (input$choose_model == "fixed effects") {
    #   use.glmer <- 0
    #   model_type <- "fixed effects"
    # } else if (input$choose_model == "random intercept") {
    #   use.glmer <- 1
    # }
    # ############################################################################

    # step 2: fit model

    if (use.glmer == 1) {

      shinyWidgets::progressSweetAlert(session = session,
        id = "progress_alert",
        title = "Fitting random intercept model ... (2 or 3 minutes)",
        value = 0, display_pct = TRUE
      )

      # GHQ
      fit1 <- lme4::glmer(cbind(npos, ntest - npos) ~
          offset(log(sample_size)) + offset(log(inoculum)) + (1 | lab_id),
        data = dat_no_zeroes, family = binomial(link = cloglog),
        control = lme4::glmerControl(optimizer = "bobyqa"), nAGQ = 20
      )

      # Table 2
      # fixed effect
      summary_fit1 <- summary(fit1)
      coef_fit1    <- coef(summary_fit1)
      mu_log       <- coef_fit1[1]  #log mean effect
      sd.mu_log    <- coef_fit1[2]  #SD of log mean effect
      mu           <- exp(mu_log)   #mean effect

      # random effect standard deviation
      sigma <- sqrt(as.numeric(summary_fit1$varcor))  #SD of log lab effects

      # calculate ICC
      ICC <- performance::icc(fit1)
      ICC <- ICC$ICC_adjusted

      # Find effect of each lab and its standard error
      #
      #  Individual lab effect equals fixed effect + random effect by labID.
      #    (i.e., overall effect + deviation)
      #  Standard error for individual lab effect (intercept) equals sqrt of
      #    variance for mean lab effect + variance for random effects.

      # Table 3
      dd <- as.data.frame(lme4::ranef(fit1))  # random effects w/ conditional sd
      lab_effects <- data.frame(
        labID            = dd$grp,
        estimated.effect = lme4::fixef(fit1)[['(Intercept)']] + dd$condval,
        SE               = sqrt(sd.mu_log ^ 2 + dd$condsd ^ 2)
      )

      # find d0.5 (LOD50, or 25 or 75)
      # see 'helpers.R'
      LOD <- LODpoint(model = fit1, value = lod_prob,
        sample_size = sample_size, inoculum = dat$inoculum
      )

      shinyWidgets::updateProgressBar(session = session,
        id = "progress_alert", value = 10
      )

      # find CI of d0.5 (LOD50/25/75)
      LOD.L <- LODlimit(fit1, value = lod_prob,
        sample_size = sample_size, inoculum = dat$inoculum,
        limit = "lower", conf_level = conf_level_prob,
        num_sim = 200L, seed = 1234
      )

      shinyWidgets::updateProgressBar(session = session,
        id = "progress_alert", value = 40
      )

      LOD.U <- LODlimit(fit1, value = lod_prob,
        sample_size = sample_size, inoculum = dat$inoculum,
        limit = "upper", conf_level = conf_level_prob,
        num_sim = 200L, seed = 1234
      )

      shinyWidgets::updateProgressBar(session = session,
        id = "progress_alert", value = 80
      )
    }

    # alternative method when the first method fails: use glm
    #   no random effects are estimated

    lack_of_variation <- FALSE
    if (use.glmer == 0) {

      shinyWidgets::progressSweetAlert(session = session,
        id = "progress_alert",
        title = "Fitting fixed effects model ...",
        value = 0, display_pct = TRUE
      )

      shinyalert::shinyalert(
        title = "Warning",
        text = paste(
          "A random intercept model cannot be used for this data.<br>",
          "A model with only <em>fixed effects</em> is used instead."
        ),
        html = TRUE,
        type = "warning",
        timer = 0
      )

      output$warning_message1 <- renderUI({
        tags$p(div(HTML(
          "<span style = 'font-size:150%; color:red'>",
          "<br><b>Warning: </b>",
          "A random intercept model cannot be used for this data.",
          "A model with only <em>fixed effects</em> is used instead."
        )))
      })

      tst3 <- tryCatch(
        glm(cbind(npos, ntest - npos) ~
            offset(log(sample_size)) + offset(log(inoculum)),
          data = dat_no_zeroes, family = binomial(link = cloglog)
        ),
        error = function(e) e,
        warning = function(w) w
      )

      error3   <- ifelse(is(tst3, "error"), 1, 0)   # 1 = error
      warning3 <- ifelse(is(tst3, "warning"), 1, 0) # 1 = warning

      if (error3 || warning3) {
        lack_of_variation <- TRUE
        output$warning_message2 <- renderUI({
          tags$p(div(HTML(
            "<span style = 'font-size:150%; color:red'>",
            "<b>Warning: </b>",
            "POD and LOD estimates may be inaccurate because of a",
            "lack of variation in the outcome or an issue with the model."
          )))
        })
        shinyalert::shinyalert(
          title = "Warning",
          text = paste(
            "POD and LOD estimates may be inaccurate because of a lack of",
            "variation in the outcome or an issue with the model."
          ),
          type = "warning",
          timer = 0
        )
      }

      fit1 <- glm(cbind(npos, ntest - npos) ~
          offset(log(sample_size)) + offset(log(inoculum)),
        data = dat_no_zeroes, family = binomial(link = cloglog)
      )

      # fixed effect
      summary_fit1 <- summary(fit1)
      coef_fit1    <- coef(summary_fit1)
      mu_log       <- coef_fit1[1]  #log mean effect
      sd.mu_log    <- coef_fit1[2]  #SD of log mean effect
      mu           <- exp(mu_log)   #mean effect

      # random effect standard deviation
      sigma <- "N/A"  #SD of log lab effects

      # ICC

      ## use Donner's method to calculate ICC (anova estimate):
      don <- aod::donner(
        cbind(npos, ntest - npos) ~ factor(inoculum),
        data = dat
      )
      ICC <- don@rho

      # individual intercept and its SE for each lab
      fit2 <- glm(cbind(npos, ntest - npos) ~
          0 + offset(log(sample_size)) + as.factor(lab_id) + offset(log(inoculum)),
        data = dat_no_zeroes, family = binomial(link = cloglog)
      )

      summary_fit2 <- summary(fit2)
      coef_fit2    <- coef(summary_fit2)

      lab_effects <- data.frame(
        labID            = unique(dat_no_zeroes$lab_id),
        estimated.effect = as.numeric(coef_fit2[, "Estimate"]),
        SE               = as.numeric(coef_fit2[, "Std. Error"])
      )
      rownames(lab_effects) <- NULL

      # Estimate d0.5 (LOD50/25/75) and CI

      # equations (32), (33) from Jarvis et al. 2019
      my_alpha  <- 1 - conf_level_prob
      my_df     <- length(unique(dat_no_zeroes$lab_id)) - 1
      t_critval <- qt(p = 1 - (my_alpha / 2), df = my_df)
      LOD <- -log(1 - lod_prob) / (sample_size * mu)

      shinyWidgets::updateProgressBar(session = session,
        id = "progress_alert", value = 20
      )

      exp_t_sd.mu <- exp(t_critval * sd.mu_log)
      LOD.U <- LOD * exp_t_sd.mu

      shinyWidgets::updateProgressBar(session = session,
        id = "progress_alert", value = 40
      )

      LOD.L <- LOD / exp_t_sd.mu
    }  #end of 'if (use.glmer == 0)'


    shinyWidgets::updateProgressBar(session = session,
      id = "progress_alert", value = 90
    )

    lab_effects$estimated.effect <- round(lab_effects$estimated.effect, 5)
    lab_effects$SE               <- round(lab_effects$SE, 5)

    mu_char        <- formatC(mu, digits = 3, format = "f")
    mu_log_char    <- formatC(mu_log, digits = 3, format = "f")
    sd.mu_log_char <- formatC(sd.mu_log, digits = 3, format = "f")

    LOD_char       <- formatC(LOD, digits = 3, format = "f")
    LOD.L_char     <- formatC(LOD.L, digits = 3, format = "f")
    LOD.U_char     <- formatC(LOD.U, digits = 3, format = "f")

    ICC_char       <- formatC(ICC, digits = 3, format = "f")

    if (use.glmer == 1) {
      sigma_char <- formatC(sigma, digits = 3, format = "f")
    } else if (use.glmer == 0) {
      sigma_char <- sigma
    } else {
      stop("Problem with 'use.glmer'.")
    }

    list(
      model_type = model_type, fit1 = fit1, fit2 = fit2,
      lack_of_variation = lack_of_variation,
      mu = mu, mu_char = mu_char,
      mu_log = mu_log, mu_log_char = mu_log_char,
      sd.mu_log = sd.mu_log, sd.mu_log_char = sd.mu_log_char,
      sigma = sigma, sigma_char = sigma_char,
      ICC = ICC, ICC_char = ICC_char,
      lab_effects = lab_effects,
      LOD = LOD, LOD_char = LOD_char,
      LOD.L = LOD.L, LOD.L_char = LOD.L_char,
      LOD.U = LOD.U, LOD.U_char = LOD.U_char,
      sample_size = sample_size,
      lod_choice = lod_choice,  #"LOD50", etc.
      lod_perc = lod_perc,      #"50", etc.
      lod_prob = lod_prob,      #0.5, etc.
      conf_level = conf_level, conf_level_prob = conf_level_prob,
      alpha = my_alpha
    )
  })

  #------------------------------  Results  ------------------------------------

  observeEvent(model_fit(), {

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
            paste0("LOD<sub>", model_fit()$lod_perc, "</sub>"),
          "</span>"
        )),
        subtitle = tags$p(HTML(
          "<span style = 'font-size:150%; font-family:Courier; color:white'>",
            "<strong>", model_fit()$LOD_char, "</strong>",
          "</span>"
        )),
        width = 12, icon = NULL, color = "blue"
      )
    })

    output$LOD_LCL <- renderValueBox({
      valueBox(
        value = withMathJax(HTML(
          "<span style = 'font-size:60%; font-family:Courier; color:white'>",
            paste0("LOD<sub>", model_fit()$lod_perc), "</sub>",
            model_fit()$conf_level, "LCL",
          "</span>"
        )),
        subtitle = tags$p(HTML(
          "<span style = 'font-size:150%; font-family:Courier; color:white'>",
            "<strong>", model_fit()$LOD.L_char, "</strong>",
          "</span>"
        )),
        width = 12, icon = NULL, color = "blue"
      )
    })

    output$LOD_UCL <- renderValueBox({
      valueBox(
        value = withMathJax(HTML(
          "<span style = 'font-size:60%; font-family:Courier; color:white'>",
            paste0("LOD<sub>", model_fit()$lod_perc), "</sub>",
            model_fit()$conf_level, "UCL",
          "</span>"
        )),
        subtitle = tags$p(HTML(
          "<span style = 'font-size:150%; font-family:Courier; color:white'>",
            "<strong>", model_fit()$LOD.U_char, "</strong>",
          "</span>"
        )),
        width = 12, icon = NULL, color = "blue"
      )
    })

  })

  # POD curves
  POD_dfs <- eventReactive(model_fit(), {
    dat <- dat()
    inoc_levels <- sort(unique(dat$inoculum))
    sample_size <- model_fit()$sample_size
    fit1  <- model_fit()$fit1
    fit2  <- model_fit()$fit2
    LOD   <- model_fit()$LOD
    LOD.L <- model_fit()$LOD.L
    LOD.U <- model_fit()$LOD.U
    if (model_fit()$model_type == "random intercept") {
      # POD and lower/upper limits for CI interval
      ds <- seq(from = 0, to = max(dat$inoculum), by = 0.001)
      newdata <- expand.grid(lab_id = unique(dat$lab_id), inoculum = unique(ds))
      newdata <- data.frame(sample_size = sample_size, newdata)
      # POD for each lab (i.e., include RE).
      # confidence interval for each lab was not estimated
      df.predicted1 <- newdata
      predict.fun <- function(model) {
        # This is predict.merMod
        predict(model, newdata = df.predicted1, re.form = NULL,
          type = "response"
        )
      }
      df.predicted1$POD <- predict.fun(fit1)
      # population level POD and 95% CI
      # predict works, but including zero causes problem when calculating
      #   confint(glmm.boots)
      ds <- c(LOD, seq(from = 0.001, to = max(dat$inoculum), by = 0.001))
      df.predicted <- data.frame(
        sample_size = sample_size, lab_id = NA, inoculum = ds
      )
      predict.fun <- function(model) {
        # This is predict.merMod
        # i.e., fitted values, unconditional (level-0)
        predict(model, newdata = df.predicted, re.form = NA,
          type = "response"
        )
      }
      df.predicted$meanPOD <- predict.fun(fit1)
      # Make predictions in 200 bootstraps of the fitted model. Use these to get
      #   confidence intervals.
      glmm.boots <- lme4::bootMer(fit1, FUN = predict.fun,
        nsim = 200, seed = 1234  #I set the seed to get repeatable results
      )
      confint_glmm.boots <- confint(glmm.boots)
      df.predicted <- cbind(df.predicted,
        meanPOD.L = confint_glmm.boots[, 1],
        meanPOD.U = confint_glmm.boots[, 2]
      )
    } else if (model_fit()$model_type == "fixed effects") {
      # Estimate POD and lower and upper CI  interval
      ds <- c(LOD, seq(from = 0, to = max(dat$inoculum), by = 0.001))
      newdata <- expand.grid(lab_id = unique(dat$lab_id), inoculum = unique(ds))
      newdata <- data.frame(sample_size = sample_size, newdata)
      df.predicted1 <- newdata
      # POD for each lab
      # confidence interval for each lab was not estimated
      predict.fun <- function(model) {
        predict(model, newdata = df.predicted1, type = "response")
      }
      df.predicted1$POD <- predict.fun(fit2)
      # population level POD and CI
      ds <- c(0, seq(from = 0.001, to = max(dat$inoculum), by = 0.001))
      df.predicted <- data.frame(
        sample_size = sample_size, lab_id = NA, inoculum = ds
      )
      predict.fun <- function(model) {
        predict(model, newdata = df.predicted, type = "response", se.fit = TRUE)
      }
      preds <- predict.fun(fit1)
      df.predicted$meanPOD <- preds$fit
      norm_critval <- qnorm(p = 1 - (model_fit()$alpha / 2))
      ME <- norm_critval * preds$se.fit
      df.predicted$meanPOD.U <- preds$fit + ME
      df.predicted$meanPOD.L <- preds$fit - ME
    } else {
      stop("Problem with model_fit()$model_type")
    }
    shinyWidgets::updateProgressBar(session = session,
      id = "progress_alert", value = 60
    )
    list(
      df.predicted = df.predicted, df.predicted1 = df.predicted1,
      inoc_levels = inoc_levels
    )
  })

  plotCurves <- function() {

    inoc_levels <- POD_dfs()$inoc_levels
    inoc_max    <- max(inoc_levels)
    if (inoc_max < 0.5) {
      inoc_incr <- .01
    } else if (inoc_max < 8) {
      inoc_incr <- .1
    } else {
      inoc_incr <- 1
    }
    if (calculate_clicked()$use_example) {
      sample_unit <- glob_sample_unit_example
    } else {
      sample_unit <- calculate_clicked()$sample_unit
    }
    df.predicted  <- POD_dfs()$df.predicted
    df.predicted1 <- POD_dfs()$df.predicted1
    conf_level    <- model_fit()$conf_level
    lod_prob      <- model_fit()$lod_prob
    lod_perc      <- model_fit()$lod_perc
    LOD           <- model_fit()$LOD
    LOD_rounded   <- round(LOD, digits = 3)
    LOD.L         <- model_fit()$LOD.L
    LOD.L_rounded <- round(LOD.L, digits = 3)
    LOD.U         <- model_fit()$LOD.U
    LOD.U_rounded <- round(LOD.U, digits = 3)
    dat           <- dat()
    dat$POD       <- dat$npos / dat$ntest

    conf_level_no_perc <- gsub("%", replacement = "", conf_level, fixed = TRUE)
    #https://stackoverflow.com/questions/43415217/how-do-i-add-percentage-and-fractions-to-ggplot-geom-text-label
    my_label    <- paste0("LOD[", lod_perc, "] == ", LOD_rounded)
    my_breaks_x <- unique(c(0, inoc_levels))

    my_plot <- ggplot(data = dat,
      aes(x = inoculum, y = POD, colour = as.factor(lab_id))
    ) +
      geom_point(size = rel(4)) +
      ggtitle("Probability / Level of Detection") +
      xlab(paste0("Inoculation level (CFU/", sample_unit, ")")) +
      scale_x_continuous(
        breaks = my_breaks_x,
        labels = c("0", as.character(my_breaks_x[-1])),
        minor_breaks = seq(from = inoc_incr, to = inoc_max, by = inoc_incr),
        limits = c(0, inoc_max),
        expand = c(0, 0)
      ) +
      ylab("Probability of detection (POD)") +
      scale_y_continuous(
        breaks = c(0, .25, .50, .75, 1),
        labels = c("", "0.25", "0.50", "0.75", "1"),
        minor_breaks = seq(from = 0.05, to = .95, by = .05),
        expand = c(0, 0)
      ) +
      labs(colour = 'Lab ID') # change legend title


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

    # add LOD50 = d0.5 (or LOD25 or 75) and confidence intervals
    vjust <- .01 * inoc_max

    p.all <- p.all +
      # do not use aes to avoid double legend
      geom_segment(x = LOD.L, xend = LOD.L,
        y = 0, yend = lod_prob,
        colour = "blue", size = rel(1)
      ) +
      geom_segment(x = LOD.U, xend = LOD.U,
        y = 0, yend = lod_prob,
        colour = "blue", size = rel(1)
      ) +
      geom_segment(x = LOD, xend = LOD,
        y = 0, yend = lod_prob,
        colour = "blue", size = rel(1)
      ) +
      geom_segment(x = 0, xend = LOD.U,
        y = lod_prob, yend = lod_prob,
        colour = "blue", size = rel(1)
      ) +
      annotate("text", x = LOD.L - vjust, y = lod_prob / 2,
        label = LOD.L_rounded, size = rel(5), angle = 90
      ) +
      annotate("text", x = LOD.U + vjust, y = lod_prob / 2,
        label = LOD.U_rounded, size = rel(5), angle = 90
      ) +
      annotate("text", x = LOD - vjust, y = lod_prob / 2,
        label = paste(model_fit()$lod_choice, "= ", LOD_rounded),
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
        aspect.ratio = .45,
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
        axis.ticks = element_line(
          size = rel(1.5)
        ),
        axis.line = element_line(
          size = rel(1.5)
        ),

        legend.background = element_rect(
          fill = "grey90"
        ),
        legend.margin = margin(
          t = 5, r = 10, b = 5, l = 10, unit = "pt"
        ),
        legend.text = element_text(
          size = rel(1.25), color = "black"
        ),
        legend.title = element_text(
          size = rel(1.5), color = "black"
        ),

        panel.background = element_rect(
          fill = "grey90", color = "white"
        ),
        panel.border = element_rect(
          linetype = "solid", color = "black",
          size = rel(3), fill = NA
        ),
        panel.grid.major.x = element_line(
          colour = "white",
          size = rel(1)
        ),
        panel.grid.major.y = element_line(
          colour = "white",
          size = rel(2.5)
        ),
        panel.grid.minor = element_line(
          colour = "white",
          size = rel(1)
        ),

        plot.background = element_rect(
          fill = "white"
        ),
        plot.title = element_text(
          size = rel(2.25),
          hjust = 0.5,
          margin = margin(t = 20, r = 0, b = 20, l = 0, unit = "pt")
        ),
        plot.title.position = "panel",
        plot.margin = unit(rep(0.25, 4), units = "inches")
      )
  }

  observeEvent(POD_dfs(), {
    output$POD_plots <- renderPlot({
      plotCurves()
    })
    shinyWidgets::updateProgressBar(session = session,
      id = "progress_alert", value = 100
    )
    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(session = session,
      title = "Calculations complete", type = "success"
    )
    shinyjs::enable("download_results_bttn")
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
      file.copy(from = "temp_file.xlsx", to = temp_path,
        overwrite = TRUE
      )

      workbook <- openxlsx::createWorkbook()
      colnames_style <- openxlsx::createStyle(textDecoration = "bold")

      openxlsx::addWorksheet(workbook, sheetName = "Description")
      calc <- calculate_clicked()
      if (calc$use_example) {
        my_exp_description <-
          c("EXPERIMENT DESCRIPTION/",
            "Experiment name:  N/A",
            "Experiment date:  N/A",
            "Microorganism:  N/A",
            "Food matrix:  N/A",
            paste0("Sample size:  ",              glob_sample_size_example),
            paste0("Sample unit:  ",              glob_sample_unit_example),
            paste0("How many labs?  ",            glob_num_labs_example),
            paste0("How many inoculum levels?  ", glob_num_levels_example),
            paste0("Use example data? ",          calc$use_example)
          )
      } else {
        my_exp_description <-
          c("EXPERIMENT DESCRIPTION/",
            paste0("Experiment name:  ",          calc$exp_name),
            paste0("Experiment date:  ",          calc$exp_date),
            paste0("Microorganism:  ",            calc$microorganism),
            paste0("Food matrix:  ",              calc$matrix),
            paste0("Sample size:  ",              calc$sample_size),
            paste0("Sample unit:  ",              calc$sample_unit),
            paste0("How many labs?  ",            calc$num_labs),
            paste0("How many inoculum levels?  ", calc$num_levels),
            paste0("Use example data?: ",         calc$use_example)
          )
      }
      my_analysis <-
        c("ANALYSIS/",
          paste0("Analysis date:  ",    calc$date_time),
          paste0("LOD choice:  ",       calc$lod_choice),
          paste0("Confidence level:  ", calc$conf_level),
          paste0("Model used:  ",       model_fit()$model_type)
        )
      my_app_info <-
        c(
          "APP INFORMATION/",
          paste0("App name:  ",    glob_app_title),
          paste0("App version:  ", glob_app_version),
          my_R_version
        )
      my_description <- c(
        my_exp_description, "\n",
        my_analysis, "\n",
        my_app_info
      )
      openxlsx::conditionalFormatting(workbook, sheet = "Description",
        cols = 1, rows = 1:100,
        type = "contains", rule = "EXPERIMENT DESCRIPTION/",
        style = colnames_style
      )
      openxlsx::conditionalFormatting(workbook, sheet = "Description",
        cols = 1, rows = 1:100,
        type = "contains", rule = "ANALYSIS/",
        style = colnames_style
      )
      openxlsx::conditionalFormatting(workbook, sheet = "Description",
        cols = 1, rows = 1:100,
        type = "contains", rule = "APP INFORMATION/",
        style = colnames_style
      )
      openxlsx::setColWidths(workbook, sheet = "Description", cols = 1,
        widths = 100
      )
      openxlsx::writeData(workbook, sheet = "Description", x = my_description)

      openxlsx::addWorksheet(workbook, sheetName = "Results")

      if (model_fit()$model_type == "fixed effects") {
        fixed_effects_warning <- paste(
          "Warning: A random intercept model could not be used for this data.",
          "A model with only fixed effects was used instead."
        )
      } else {
        fixed_effects_warning <- ""
      }
      if (model_fit()$lack_of_variation) {
        lack_of_variation_warning <- paste(
          "Warning: POD and LOD estimates may be inaccurate because of a",
          "lack of variation in the outcome or an issue with the model."
        )
      } else {
        lack_of_variation_warning <- ""
      }

      my_results <-
        c("RESULTS/",
          paste0("Mean Lab Effect:  ",       model_fit()$mu_log_char),
          paste0("SE of Mean Lab Effect:  ", model_fit()$sd.mu_log_char),
          paste0("ICC:  ",                   model_fit()$ICC_char),
          paste0(calc$lod_choice, ":  ",     model_fit()$LOD_char),
          paste0(calc$lod_choice, " (lower limit):  ", model_fit()$LOD.L_char),
          paste0(calc$lod_choice, " (upper limit):  ", model_fit()$LOD.U_char),
          paste0(fixed_effects_warning),
          paste0(lack_of_variation_warning)
        )
      openxlsx::conditionalFormatting(workbook, sheet = "Results",
        cols = 1, rows = 1:100,
        type = "contains", rule = "RESULTS/",
        style = colnames_style
      )
      openxlsx::setColWidths(workbook, sheet = "Results", cols = 1,
        widths = 100
      )
      openxlsx::writeData(workbook, sheet = "Results", x = my_results)

      openxlsx::addWorksheet(workbook, sheetName = "POD Curves")
      #https://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
      plot_width  <- 10
      plot_height <- 0.45 * plot_width  #aspect_ratio = 0.45

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
      my_lab_effects <- dplyr::rename(model_fit()$lab_effects,
        "Lab ID"           = labID,
        "Estimated effect" = estimated.effect,
        "Standard error"   = SE
      )
      openxlsx::setColWidths(workbook, sheet = "Lab Effects",
        cols = 1:ncol(my_lab_effects),
        widths = c(8, 16, 16)
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
        "Sample size"       = sample_size
      )
      openxlsx::setColWidths(workbook, sheet = "Data", cols = 1:ncol(my_data),
        widths = c(8, 15, 12, 13, 11)
      )
      openxlsx::writeData(workbook, sheet = "Data",
        x = my_data, headerStyle = colnames_style
      )

      openxlsx::addWorksheet(workbook, sheetName = "R Packages")
      openxlsx::freezePane(workbook, sheet = "R Packages", firstRow = TRUE)
      openxlsx::setColWidths(workbook, sheet = "R Packages",
        cols = 1:ncol(my_package_info),
        widths = c(18, 14, 11)
      )
      openxlsx::writeData(workbook, sheet = "R Packages",
        x = my_package_info, headerStyle = colnames_style
      )

      openxlsx::saveWorkbook(wb = workbook, file = file, overwrite = TRUE)
    },

    contentType = "xlsx"
  )

}

