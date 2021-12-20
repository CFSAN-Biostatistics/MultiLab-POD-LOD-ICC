

fitModel <- function(dat, session) {
  # Input data: a nonreactive dataframe
  warning_messages <- vector(mode = "character")
  dat_no_zeroes <- dat[dat$inoculum_per_unit != 0, ]
  model_type <- chooseModel(dat_no_zeroes)  #random intercept or fixed effects?
  #cat("model_type: ", model_type, "\n")
  #message(model_type); model_type <- "fixed effects"  #for testing only
  if (model_type == "random intercept") {
    model_fitted <- fitRandomIntercept(dat_no_zeroes)
    sigma <- model_fitted$sigma
    ICC   <- model_fitted$ICC
  } else if (model_type == "fixed effects") {
    # Alternative method when the first method fails: use glm.
    #   No random effects are estimated.
    warning_message <- paste(
      "A random intercept model cannot be used for this data.",
      "A model with only fixed effects is used instead."
    )
    warning_messages <- c(warning_messages, warning_message)
    tst_glm <- tryCatch(
      stats::glm(
        cbind(npos, ntest - npos) ~ offset(log(sample_size)) + offset(log(inoculum_per_unit)),
          data = dat_no_zeroes, family = binomial(link = "cloglog")
      ),
      error = function(e) e, warning = function(w) w
    )
    problem_glm <- methods::is(tst_glm, "error") || methods::is(tst_glm, "warning")
    if (problem_glm) {
      warning_message <- paste(
        "POD and LOD estimates may be inaccurate because of a lack of",
        "variation in the outcome or an issue with the model."
      )
      warning_messages <- c(warning_messages, warning_message)
    }
    model_fitted <- fitFixedEffects(dat_no_zeroes)
    my_linear_data <- linearData(dat)  # Use LMM to estimate ICC
    icc_method <- chooseFixedMethodICC(my_linear_data)

    #message(icc_method); icc_method <- "ANCOVA"  #for testing only

    if (icc_method == "LMM") {
      warning_message <- "ICC is calculated based on a linear mixed effects model."
      icc_sigma <- iccSigmaLmm(my_linear_data)
    } else if (icc_method == "ANCOVA") {
      warning_message <- "ICC is calculated using an analysis of covariance approach."
      icc_sigma <- iccSigmaAncova(my_linear_data)
    } else {
      stop("Problem with 'chooseFixedMethodICC()'.")
    }
    warning_messages <- c(warning_messages, warning_message)
    sigma <- icc_sigma$sigma
    ICC   <- icc_sigma$ICC
  } else {
    stop("Problem with 'model_type'.")
  }

  params_char <- paramsAsChars(model_fitted, sigma, ICC)

  list(
    dat_no_zeroes = dat_no_zeroes,
    model_type = model_type, model_fitted = model_fitted$fit,
    mu = model_fitted$mu, mu_char = params_char$mu,
    mu_log = model_fitted$mu_log, mu_log_char = params_char$mu_log,
    mu_log_se = model_fitted$mu_log_se, mu_log_se_char = params_char$mu_log_se,
    sigma = sigma, sigma_char = params_char$sigma,
    ICC = ICC, ICC_char = params_char$ICC,
    warnings = warning_messages
  )

}


# #-------------------------------  Testing  -------------------------------------
#
# # For testing uploaded data files
# library(shiny)
# source("R/helpers-model.R")
# source("R/helpers-validate-inputs.R")
# source("R/mod-data-input-upload.R")
# modelFitApp <- function() {
#   ui <- fluidPage(
#     shinyalert::useShinyalert(),
#     shinyjs::useShinyjs(),
#     includeCSS("www/style.css"),
#     #verbatimTextOutput("my_data"),
#     verbatimTextOutput("my_results"),
#     fluidRow(align = "center", uploadDataUI("my_id"))
#   )
#   server <- function(input, output, session) {
#     my_data <- uploadDataServer("my_id")
#     #output$my_data <- renderPrint(my_data())
#     my_model <- reactive(fitModel(my_data()$data_model(), session))
#     output$my_results <- renderPrint(my_model())
#   }
#   shinyApp(ui, server)
# }
# modelFitApp()
