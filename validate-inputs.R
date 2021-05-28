# Inout data validation

validateDescription <- function(num_labs, num_levels, sample_size, session) {
  # Validate inputs in the 'Experiment Description' box.
  description_error <- "Problem with experiment description inputs"

  desc_validate1 <- !is.na(num_labs) && is_wholenumber(num_labs) && num_labs > 0
  if (!desc_validate1) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = description_error,
      text = "Number of labs must be a positive whole number."
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(desc_validate1)

  desc_validate2 <- !is.na(num_levels) && is_wholenumber(num_levels) && num_levels > 0
  if (!desc_validate2) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = description_error,
      text = "Number of levels must be a positive whole number."
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(desc_validate2)

  desc_validate3 <- num_labs <= glob_max_labs
  if (!desc_validate3) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = description_error,
      text = paste0("Maximum number of labs is ", glob_max_labs, ".")
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(desc_validate3)

  desc_validate4 <- num_levels <= glob_max_levels
  if (!desc_validate4) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = description_error,
      text = paste0("Maximum number of levels is ", glob_max_levels, ".")
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(desc_validate4)

  desc_validate5 <- num_labs >= glob_min_labs
  if (!desc_validate5) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = description_error,
      text = paste0("Minimum number of labs is ", glob_min_labs, ".")
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(desc_validate5)

  desc_validate6 <- num_levels >= glob_min_levels
  if (!desc_validate6) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = description_error,
      text = paste0("Minimum number of levels is ", glob_min_levels, ".")
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(desc_validate6)

  desc_validate7 <- !is.na(sample_size) && is.numeric(sample_size) && sample_size > 0
  if (!desc_validate7) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = description_error,
      text = "Test portion size must be a positive number."
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(desc_validate7)
}


validateData <- function(dat, session) {
  # Validate user lab-level data.
  input_error <- "Problem with lab-level input data"
  sum_data <- stats::aggregate(dat, by = list(dat$lab_id), FUN = sum)

  input_validate1 <- all(!is.na(dat))
  if (!input_validate1) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = input_error,
      text = "Some data are missing or non-numeric."
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(input_validate1)

  input_validate2 <- all(dat >= 0)
  if (!input_validate2) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = input_error,
      text = "All data must be non-negative."
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(input_validate2)

  input_validate3 <- all(sum_data$inoculum > 0)
  if (!input_validate3) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = input_error,
      text = "Each lab must have at least 1 positive inoculum level."
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(input_validate3)

  input_validate4 <- all(dat$ntest > 0)
  if (!input_validate4) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = input_error,
      text = "Each inoculation level must have at least 1 tube inoculated."
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(input_validate4)

  input_validate5 <- all(sum_data$npos > 0)
  if (!input_validate5) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = input_error,
      text = "Each lab must have at least 1 positive tube."
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(input_validate5)

  input_validate6 <- all(sum_data$ntest - sum_data$npos > 0)
  if (!input_validate6) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = input_error,
      text = "Each lab must have at least 1 negative tube."
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(input_validate6)

  input_validate7 <- all(dat$ntest >= dat$npos)
  if (!input_validate7) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = input_error,
      text = "Tubes inoculated must always be >= tubes positive."
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(input_validate7)

  input_validate8 <- all(is_wholenumber(dat$ntest)) && all(is_wholenumber(dat$npos))
  if (!input_validate8) {
    shinyWidgets::sendSweetAlert(session = session, type = "error",
      title = input_error,
      text = "Tubes inoculated and tubes positive must be whole numbers."
    )
    shinyjs::disable("download_results-button_bttn")
  }
  req(input_validate8)
}
