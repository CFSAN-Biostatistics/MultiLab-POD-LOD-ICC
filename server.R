
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


  # update number of labs & inoculum levels
  observeEvent(c(input$num_labs, input$num_levels), {
    req(input$num_labs > 0, input$num_levels > 0)
    num_labs   <- input$num_labs
    num_levels <- input$num_levels
    my_labs <- glob_panel_names[1:num_labs]  #wellPanels
    # lab i, level j
    lapply(glob_panel_names, function(i) {
      if (i %in% my_labs) {
        shinyjs::showElement(i)
        my_levels  <- paste0(i, "_level", 1:num_levels)  #panel_lab1_level1, etc.
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


  output$example_data <- renderTable({
    dat_example_ui
    }, striped = TRUE, bordered = TRUE, align = 'c'
  )

  observeEvent(input$upload_file, {
    dat_uploaded()  #because reactive() is lazy
  })

  dat_uploaded <- reactive({
    #https://mastering-shiny.org/action-transfer.html
    req(input$upload_file)

    #In case the .includes() JS method is unsupported in browser. (see jscript.js)
    fname_uploaded <- input$upload_file$name
    validateUploadExtension(fname_uploaded, session = session)

    workbook <- input$upload_file$datapath

    validateUploadSheetNames(workbook, session = session)  #validate-inputs.R

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

    validateUploadMinimumLabs(counts, session = session)  #validate-inputs.R
    validateUploadDimensions(counts, inoc_levels, session = session)  #validate-inputs.R

    counts_n <- counts %>%
      dplyr::select(Lab_Number, Lab_Name, starts_with("n", ignore.case = FALSE)) %>%
      tidyr::pivot_longer(
        cols = starts_with("n", ignore.case = FALSE),
        names_to = "var_n",
        values_to = "n"
      ) %>%
      dplyr::rename(d = var_n)
    counts_n$d <- sub("n", replacement = "d", x = counts_n$d, ignore.case = FALSE)

    counts_y <- counts %>%
      dplyr::select(Lab_Number, Lab_Name, starts_with("y", ignore.case = FALSE)) %>%
      tidyr::pivot_longer(
        cols = starts_with("y", ignore.case = FALSE),
        names_to = "var_y",
        values_to = "y"
      ) %>%
      dplyr::rename(d = var_y)
    counts_y$d <- sub("y", replacement = "d", x = counts_y$d, ignore.case = FALSE)

    counts2 <- dplyr::left_join(counts_n, counts_y, by = c("Lab_Number", "Lab_Name", "d"))

    dils <- data.frame(
      d = colnames(inoc_levels),
      inoc_level = as.numeric(inoc_levels)
    )

    validateUploadDilutions(dils, session = session)  #validate-inputs.R

    counts2 <- dplyr::left_join(counts2, dils, by = "d") %>%
      dplyr::select(Lab_Number:d, inoc_level, n, y)

    counts2$sample_size <- description$`Test_portion_size_(g_or_mL)`
    counts2 <- counts2 %>%
      dplyr::select(-Lab_Name, -d) %>%
      dplyr::rename(
        lab_id   = Lab_Number,
        inoculum = inoc_level,
        ntest    = n,
        npos     = y
      ) %>%
      as.data.frame()

    counts2$lab_id      <- as.integer(counts2$lab_id)
    counts2$inoculum    <- as.numeric(counts2$inoculum)
    counts2$ntest       <- as.integer(counts2$ntest)
    counts2$npos        <- as.integer(counts2$npos)
    counts2$sample_size <- as.numeric(counts2$sample_size)

    validateData(counts2, session = session)

    return(
      list(description = description, inoc_levels = inoc_levels,
           data_input = counts, data_model = counts2,
           fname_uploaded = fname_uploaded
      )
    )
  })

  uploaded_data_for_preview <- reactive({
    req(dat_uploaded())
    dat <- dat_uploaded()$data_model
    num_dils <- length(unique(dat$inoculum))
    dat$sample_size <- NULL
    dat$lab_id <- paste("Lab", dat$lab_id)
    dat$lab_id[1:nrow(dat) %% num_dils != 1] <- ""
    colnames(dat) <- c("Lab Name", "Inoculation Level", "Inoculated Tubes", "Positive Tubes")
    return(dat)
  })

  output$uploaded_test_portion <- reactive({
    req(dat_uploaded())
    test_portion_size <- dat_uploaded()$description$`Test_portion_size_(g_or_mL)`
    paste("<h4>Test portion size:", test_portion_size, "g or mL</h4>")
  })

  output$uploaded_data_preview <- renderTable({
    req(uploaded_data_for_preview())
    uploaded_data_for_preview()
    }, striped = TRUE, bordered = TRUE, align = 'c'
  )


  # Starts "chain reaction" for calculations & caches input values
  calculate_clicked <- eventReactive(input$calculate, {

    choose_data_entry <- input$choose_data_entry
    if (choose_data_entry == "Example data") {
      fname_uploaded <- NA
      exp_name      <- "N/A"
      exp_date      <- "N/A"
      microorganism <- "N/A"
      matrix        <- "N/A"
      sample_size <- glob_sample_size_example
      num_labs    <- glob_num_labs_example
      num_levels  <- glob_num_levels_example
      lab_ids   <- 1:num_labs
      lab_names <- paste("Lab", lab_ids)
    } else if (choose_data_entry == "Manual entry") {
      validateDescription(input$num_labs, input$num_levels, input$sample_size,
                          session = session)  #validate-inputs.R
      fname_uploaded <- NA
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
    } else if (choose_data_entry == "File upload") {
      req(dat_uploaded())
      fname_uploaded <- dat_uploaded()$fname_uploaded
      exp_desc    <- dat_uploaded()$description
      inoc_levels <- dat_uploaded()$inoc_levels
      data_input  <- dat_uploaded()$data_input
      exp_name      <- exp_desc$Experiment_name
      exp_date      <- exp_desc$Experiment_date
      microorganism <- exp_desc$Microorganism
      matrix        <- exp_desc$Food_matrix
      sample_size   <- exp_desc$`Test_portion_size_(g_or_mL)`
      num_labs      <- max(data_input$Lab_Number)
      num_levels    <- ncol(inoc_levels)
      lab_ids       <- data_input$Lab_Number
      lab_names     <- data_input$Lab_Name
    } else {
      stop("Problem in calculate_clicked().")
    }

    list(
      date_time       = strftime(Sys.time(), format = "", tz = "", usetz = TRUE),
      fname_uploaded  = fname_uploaded,
      exp_name        = exp_name,
      microorganism   = microorganism,
      matrix          = matrix,
      exp_date        = exp_date,
      choose_data_entry = choose_data_entry,
      sample_size     = sample_size,
      lod_unit        = input$lod_unit,
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

    choose_data_entry <- calculate_clicked()$choose_data_entry
    num_labs    <- calculate_clicked()$num_labs
    num_levels  <- calculate_clicked()$num_levels
    sample_size <- calculate_clicked()$sample_size

    if (choose_data_entry == "Example data") {
      return(dat_example)
    } else if (choose_data_entry == "Manual entry") {
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
      validateData(dat, session = session)
      return(dat)
    } else if (choose_data_entry == "File upload") {
      req(dat_uploaded())
      return(dat_uploaded()$data_model)
    } else {
      stop("Problem in dat().")
    }
  })


  ######################  end of data set creation  ############################


  model_fit <- eventReactive(dat(), {

    #https://www.gitmemory.com/issue/dreamRs/shinyWidgets/301/662290306
    shinyjs::disable("download_results_bttn")
    dat <- dat()
    dat_no_zeroes <- dat[dat$inoculum != 0, ]
    warning_messages_results_UI   <- ""
    warning_messages_results_xlsx <- ""

    # Step 1: Choose model - catch errors and warnings
    model_type <- chooseModel(dat_no_zeroes)  #random intercept or fixed effects?

    #message(model_type); model_type <- "fixed effects"  #for testing only

    # Step 2: fit model
    if (model_type == "random intercept") {
      shinyWidgets::progressSweetAlert(session = session,
        id = "progress_alert", title = "Fitting random intercept model",
        value = 0, display_pct = TRUE
      )
      model_fitted <- fitRandomIntercept(dat_no_zeroes)
      mu        <- model_fitted$mu
      mu_log    <- model_fitted$mu_log
      mu_log_se <- model_fitted$mu_log_se
      sigma     <- model_fitted$sigma
      ICC       <- model_fitted$ICC
    } else if (model_type == "fixed effects") {
      # Alternative method when the first method fails: use glm.
      #   No random effects are estimated.
      shinyWidgets::progressSweetAlert(session = session,
        id = "progress_alert", title = "Fitting fixed effects model",
        value = 0, display_pct = TRUE
      )
      shinyalert::shinyalert(
        title = "Warning",
        text = span(HTML(
          "A random intercept model cannot be used for this data.",
          "A model with only <em>fixed effects</em> is used instead."
        ), class = "alert-text"),
        html = TRUE, type = "warning", timer = 0,
        confirmButtonCol = "#003152"
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
      error_glm   <- methods::is(tst_glm, "error")
      warning_glm <- methods::is(tst_glm, "warning")
      problem_glm <- error_glm || warning_glm
      if (problem_glm) {
        shinyalert::shinyalert(
          title = "Warning",
          text = span(HTML(
            "POD and LOD estimates may be inaccurate because of a lack of ",
            "variation in the outcome or an issue with the model."
          ), class = "alert-text"),
          html = TRUE,
          type = "warning", timer = 0,
          confirmButtonCol = "#003152"
        )
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
      }
      model_fitted <- fitFixedEffects(dat_no_zeroes)
      mu        <- model_fitted$mu
      mu_log    <- model_fitted$mu_log
      mu_log_se <- model_fitted$mu_log_se
      # Attempt to use LMM to estimate ICC
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
      icc_method <- chooseFixedMethodICC(my_linear_data)

      #message(icc_method); icc_method <- "ANCOVA"  #for testing only

      if (icc_method == "LMM") {
        shinyalert::shinyalert(
          title = "Warning",
          text = span(HTML(
            "ICC is calculated based on a <em>linear mixed effects</em> model."
          ), class = "alert-text"),
          html = TRUE, type = "warning", timer = 0,
          confirmButtonCol = "#003152"
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
        icc_sigma <- iccSigmaLmm(my_linear_data)

      } else if (icc_method == "ANCOVA") {
        shinyalert::shinyalert(
          title = "Warning",
          text = span(HTML(
            "ICC is calculated using an <em>analysis of covariance</em> approach."
          ), class = "alert-text"),
          html = TRUE, type = "warning", timer = 0,
          confirmButtonCol = "#003152"
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
        icc_sigma <- iccSigmaAncova(my_linear_data)

      } else {
        stop("Problem with 'icc_method'.")
      }
      sigma <- icc_sigma$sigma
      ICC   <- icc_sigma$ICC
    } else {
      stop("Problem with 'model_type'.")
    }
    mu_char        <- formatC(mu, digits = 3, format = "f")
    mu_log_char    <- formatC(mu_log, digits = 3, format = "f")
    mu_log_se_char <- formatC(mu_log_se, digits = 3, format = "f")
    if (is.na(sigma)) {
      sigma_char <- "N/A"
    } else {
      sigma_char <- formatC(sigma, digits = 3, format = "f")
    }
    if (is.na(ICC)) {
      ICC_char <- "N/A"
    } else {
      ICC_char <- formatC(ICC, digits = 3, format = "f")
    }
    output$warning_messages_results_UI <- renderUI({
      tags$p(div(
        class = "warning",
        HTML(
          "<span>",
          warning_messages_results_UI,
          "</ul>"
        )
      ))
    })
    list(
      dat_no_zeroes = dat_no_zeroes,
      model_type = model_type, model_fitted = model_fitted$fit,
      warning_messages_results_xlsx = warning_messages_results_xlsx,
      mu = mu, mu_char = mu_char,
      mu_log = mu_log, mu_log_char = mu_log_char,
      mu_log_se = mu_log_se, mu_log_se_char = mu_log_se_char,
      sigma = sigma, sigma_char = sigma_char,
      ICC = ICC, ICC_char = ICC_char
    )
  })

  ######################  end of model fitting  ################################


  POD_LOD <- eventReactive(model_fit(), {
    dat <- dat()
    inoc_levels <- sort(unique(dat$inoculum))
    inoc_max    <- max(inoc_levels)
    sample_size     <- calculate_clicked()$sample_size
    lod_unit        <- calculate_clicked()$lod_unit
    lod_prob        <- calculate_clicked()$lod_prob
    conf_level_prob <- calculate_clicked()$conf_level_prob
    num_labs        <- calculate_clicked()$num_labs
    lab_names       <- calculate_clicked()$lab_names
    lab_ids  <- unique(dat$lab_id)
    my_alpha <- 1 - conf_level_prob
    dat_no_zeroes <- model_fit()$dat_no_zeroes
    model_type    <- model_fit()$model_type
    fit1          <- model_fit()$model_fitted
    mu            <- model_fit()$mu
    mu_log_se     <- model_fit()$mu_log_se

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
        SE               = sqrt(mu_log_se ^ 2 + dd$condsd ^ 2)
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
      is_LOD_error <- methods::is(tst_LOD, "error")
      if (is_LOD_error) {
        shinyalert::shinyalert(
          title = "LOD Calculation Error",
          text = span(
            "Possibly due to zero-probability predictions.",
            class = "alert-text"
          ),
          html = TRUE, type = "error", timer = 0,
          confirmButtonCol = "#003152"
        )
        shinyWidgets::closeSweetAlert(session = session)
      }
      req(!is_LOD_error)
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
      ds <- c(LOD, seq(from = 1e-9, to = inoc_max, by = 1e-3))
      ds <- sort(unique(ds))
      newdata <- expand.grid(lab_id = lab_ids, inoculum = ds)
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
      ds <- c(1e-9, LOD, seq(from = min(1e-4, LOD), to = inoc_max, by = 1e-5))
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
        labID            = lab_ids,
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
      my_df <- num_labs - 1
      t_critval <- qt(p = 1 - (my_alpha / 2), df = my_df)
      LOD <- -log(1 - lod_prob) / (sample_size * mu)
      exp_t_mu_log_se <- exp(t_critval * mu_log_se)
      LOD.U <- LOD * exp_t_mu_log_se
      LOD.L <- LOD / exp_t_mu_log_se

      #########################
      ## Estimate POD and CI
      #########################
      # POD for each lab
      # confidence interval for each lab was not estimated
      ds <- c(LOD, seq(from = 1e-9, to = inoc_max, by = 1e-3))
      ds <- sort(unique(ds))
      newdata <- expand.grid(lab_id = lab_ids, inoculum = ds)
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
      ds <- c(LOD, seq(from = 1e-9, to = inoc_max, by = 1e-4))
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
      lab_effects = lab_effects, inoc_levels = inoc_levels, inoc_max = inoc_max,
      LOD = LOD, LOD.L = LOD.L, LOD.U = LOD.U,
      LOD_char = LOD_char, LOD.L_char = LOD.L_char, LOD.U_char = LOD.U_char
    )
  })

  ######################  end of POD_LOD  ######################################


  #-------------------------  displayed  results  ------------------------------

  observeEvent(POD_LOD(), {

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
      valueBox(
        value = HTML(
          "<span class='lod-estimates-description'>",
            paste0("LOD<sub>", calculate_clicked()$lod_perc, "</sub>"),
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
      valueBox(
        value = HTML(
          "<span class='lod-estimates-description'>",
            paste0("LOD<sub>", calculate_clicked()$lod_perc, "</sub>"),
            calculate_clicked()$conf_level, "LCL",
          "</span>"
        ),
        subtitle = HTML(
          "<span class='lod-estimates-value'>",
            POD_LOD()$LOD.L_char,
          "</span>"
        ),
        width = 12, icon = NULL, color = "blue"
      )
    })

    output$LOD_UCL <- renderValueBox({
      valueBox(
        value = HTML(
          "<span class='lod-estimates-description'>",
            paste0("LOD<sub>", calculate_clicked()$lod_perc, "</sub>"),
            calculate_clicked()$conf_level, "UCL",
          "</span>"
        ),
        subtitle = HTML(
          "<span class='lod-estimates-value'>",
            POD_LOD()$LOD.U_char,
          "</span>"
        ),
        width = 12, icon = NULL, color = "blue"
      )
    })

  })
  ########################  end of displayed results  ##########################


  plotCurves <- eventReactive(POD_LOD(), {
    sample_size <- calculate_clicked()$sample_size
    lod_unit    <- calculate_clicked()$lod_unit
    conf_level  <- calculate_clicked()$conf_level
    lod_choice  <- calculate_clicked()$lod_choice
    lod_prob    <- calculate_clicked()$lod_prob
    lod_perc    <- calculate_clicked()$lod_perc
    inoc_levels   <- POD_LOD()$inoc_levels
    inoc_max      <- POD_LOD()$inoc_max
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
      annotate("label", x = 0.75 * inoc_max, y = 0.375,
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
    shinyjs::delay(1000, {
      updateTabItems(session = session, inputId = "my_tabs", selected = "results")
      shinyjs::delay(3000,
        shinyjs::enable("download_results_bttn")
      )
    })
    shinyWidgets::updateProgressBar(session = session,
      id = "progress_alert", value = 100
    )
    shinyWidgets::closeSweetAlert(session = session)
    shinyalert::shinyalert(
      title = "Calculations complete!",
      text = span(
        "Results will be loaded momentarily.",
        class = "alert-text"
      ),
      html = TRUE, type = "success", timer = 0,
      confirmButtonCol = "#003152"
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
        text = span("Creating your report...", class = "creating-report"),
        session = session
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
      if (calc$choose_data_entry == "File upload") {
        fname_uploaded <- paste0(" (", calc$fname_uploaded, ")")
      } else {
        fname_uploaded <- ""
      }
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
          paste0("Data entry choice:  ", calc$choose_data_entry, fname_uploaded)
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
          paste0("SE of Mean Lab Effect:  ", model_fit()$mu_log_se_char),
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

