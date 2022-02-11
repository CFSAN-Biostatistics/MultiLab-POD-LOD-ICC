# Calculate POD & LOD

podLod <- function(run_analysis, dat_list, fitted_model, n_sim, session) {
  # run_analysis: a nonreactive list of nonreactives
  # dat_list: a nonreactive list of reactives
  # fitted_model: a nonreactive list of nonreactives
  # n_sim: a scalar numeric or integer value

  # From run_analysis
  lod_unit    <- run_analysis$lod_unit
  lod_prob    <- run_analysis$lod_prob
  alpha_level <- 1 - run_analysis$conf_level_prob

  # From dat_list
  dat <- dat_list$data_model()
  inoc_levels <- sort(unique(dat$inoculum_per_unit))
  num_levels  <- length(inoc_levels)
  inoc_max    <- max(inoc_levels)
  sample_size <- dat_list$sample_size()

  # From fitted_model
  dat_no_zeroes <- fitted_model$dat_no_zeroes
  lab_ids       <- unique(dat_no_zeroes$lab_id)
  num_labs      <- length(lab_ids)
  lab_names     <- unique(dat_no_zeroes$lab_name)
  model_type    <- fitted_model$model_type
  fit1          <- fitted_model$model_fitted
  mu            <- fitted_model$mu
  mu_log_se     <- fitted_model$mu_log_se

  if (model_type == "random intercept") {
      shinyWidgets::progressSweetAlert(session = session,
        id = "progress_alert", title = "Fitting random intercept model",
        value = 0, display_pct = TRUE
      )
      lab_effects <- labEffectsRE(fit1, lab_names, mu_log_se)
      is_LOD_error <- isErrorLODRE(fit1, lod_prob, sample_size, dat$inoculum_per_unit)
      if (is_LOD_error) {
        lodCalcErrorAlert()
        shinyWidgets::closeSweetAlert(session)
      }
      req(!is_LOD_error)
      LOD <- lodPointRE(fit1,
        value = lod_prob, sample_size = sample_size, inoculum = dat$inoculum_per_unit
      )
      predicted_each_lab <- podEachLabRE(fit1, LOD, inoc_max, lab_ids, sample_size)
      # Population-level POD and CI
      predicted_all_labs <- podAllLabsRE(fit1,
        LOD, inoc_max, lab_ids, sample_size, n_sim, alpha_level, session
      )
      LOD_CI <- lodCIRE(predicted_all_labs, lod_prob)
      shinyWidgets::closeSweetAlert(session)
  } else if (model_type == "fixed effects") {
    # Individual intercept and its SE for each lab
    fit2 <- stats::glm(cbind(npos, ntest - npos) ~
        0 + offset(log(sample_size)) + as.factor(lab_id) + offset(log(inoculum_per_unit)),
      data = dat_no_zeroes, family = binomial(link = "cloglog")
    )
    lab_effects <- labEffectsFE(fit2, lab_ids, lab_names)
    t_critval <- qt(p = 1 - (alpha_level / 2), df = num_labs - 1)
    LOD    <- lodPointFE(mu, lod_prob, sample_size)
    LOD_CI <- lodCIFE(LOD, mu_log_se, t_critval)
    predicted_each_lab <- podEachLabFE(fit2, LOD, inoc_max, lab_ids, sample_size)
    # Population-level POD and CI
    predicted_all_labs <- podAllLabsFE(fit1, LOD, inoc_max, lab_ids, sample_size, t_critval)
  } else {
    stop("Problem with model_type")
  }
  LOD_L <- LOD_CI[1]
  LOD_U <- LOD_CI[2]
  if (lod_unit == "CFU/test portion") {
    LOD   <- LOD * sample_size
    LOD_L <- LOD_L * sample_size
    LOD_U <- LOD_U * sample_size
  }
  LOD_char   <- format3(LOD)
  LOD_L_char <- format3(LOD_L)
  LOD_U_char <- format3(LOD_U)
  list(
    predicted_all_labs = predicted_all_labs, predicted_each_lab = predicted_each_lab,
    lab_effects = lab_effects, num_labs = num_labs,
    inoc_levels = inoc_levels, num_levels = num_levels, inoc_max = inoc_max,
    LOD = LOD, LOD_L = LOD_L, LOD_U = LOD_U,
    LOD_char = LOD_char, LOD_L_char = LOD_L_char, LOD_U_char = LOD_U_char
  )
}


# #-------------------------------  Testing  -------------------------------------
#
# # For testing uploaded data files
# library(shiny)
# source("R/helpers-validate-inputs.R")
# source("R/mod-data-input-upload.R")
# source("R/server-model-fit-helpers.R")
# source("R/server-model-fit.R")
# source("R/server-pod-lod-helpers.R")
# lodPodApp <- function() {
#   ui <- fluidPage(
#     shinyalert::useShinyalert(),
#     shinyjs::useShinyjs(),
#     includeCSS("www/style.css"),
#     #verbatimTextOutput("my_data"),
#     verbatimTextOutput("my_model_results"),
#     verbatimTextOutput("my_pod_results"),
#     fluidRow(align = "center", uploadDataUI("my_id"))
#   )
#   server <- function(input, output, session) {
#     my_analysis <- reactive(list(
#       lod_unit = "CFU/test portion", lod_prob = 0.50, conf_level_prob = 0.95
#     ))
#     my_data <- uploadDataServer("my_id")
#     #output$my_data <- renderPrint(my_data())
#     my_model <- eventReactive(my_data(), {
#       fitModel(my_data()$data_model(), session)
#     })
#     output$my_model_results <- renderPrint(my_model())
#     my_pod <- reactive(podLod(my_analysis(), my_data(), my_model(), 500, session))
#     output$my_pod_results <- renderPrint(str(my_pod()))
#   }
#   shinyApp(ui, server)
# }
# lodPodApp()
