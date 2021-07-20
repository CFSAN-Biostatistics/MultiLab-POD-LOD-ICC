# Input data validation

validateWithAlert <- function(validation_check, title, text, session) {
  # Helper function to validate input & create alerts.
  if (!validation_check) {
    shinyalert::shinyalert(
      title = title,
      text = span(text, class = "alert-text"),
      html = TRUE, type = "error", timer = 0,
      confirmButtonCol = "#003152"
    )
    shinyjs::disable("download_results_bttn")
  }
  req(validation_check)
}

validateDescription <- function(num_labs, num_levels, sample_size, session) {
  # Validate inputs in the 'Experiment Description' box.
  description_error <- "Experiment description error"
  validateWithAlert(
    !is.na(num_labs) && is_wholenumber(num_labs) && num_labs > 0,
    title = description_error,
    text = "Number of labs must be a positive whole number.",
    session = session
  )
  validateWithAlert(
    !is.na(num_levels) && is_wholenumber(num_levels) && num_levels > 0,
    title = description_error,
    text = "Number of levels must be a positive whole number.",
    session = session
  )
  validateWithAlert(
    num_labs <= glob_max_labs,
    title = description_error,
    text = paste0("Maximum number of labs is ", glob_max_labs, "."),
    session = session
  )
  validateWithAlert(
    num_levels <= glob_max_levels,
    title = description_error,
    text = paste0("Maximum number of levels is ", glob_max_levels, "."),
    session = session
  )
  validateWithAlert(
    num_labs >= glob_min_labs,
    title = description_error,
    text = paste0("Minimum number of labs is ", glob_min_labs, "."),
    session = session
  )
  validateWithAlert(
    num_levels >= glob_min_levels,
    title = description_error,
    text = paste0("Minimum number of levels is ", glob_min_levels, "."),
    session = session
  )
  validateWithAlert(
    !is.na(sample_size) && is.numeric(sample_size) && sample_size > 0,
    title = description_error,
    text = "Test portion size must be a positive number.",
    session = session
  )
}


validateData <- function(dat, session) {
  # Validate user lab-level data.
  input_error <- "Lab-level data error"
  sum_data <- stats::aggregate(dat, by = list(dat$lab_id), FUN = sum)
  validateWithAlert(
    all(!is.na(dat)),
    title = input_error,
    text = "Some data are missing or non-numeric.",
    session = session
  )
  validateWithAlert(
    all(dat >= 0),
    title = input_error,
    text = "All data must be non-negative.",
    session = session
  )
  dat_first_inoc <- dat[!duplicated(dat$lab_id), "inoculum", drop = TRUE]
  dat_other_inoc <- dat[duplicated(dat$lab_id), ] %>%
    dplyr::group_by(lab_id) %>%
    summarize(
      are_all_pos    = all(inoculum > 0),
      num_pos_levels = length(inoculum),
      are_all_unique = num_pos_levels == length(unique(inoculum))
    )
  validateWithAlert(
    all(dat_first_inoc == 0) && all(dat_other_inoc$are_all_pos) &&
      all(dat_other_inoc$num_pos_levels >= 2) && all(dat_other_inoc$are_all_unique),
    title = input_error,
    text = paste(
      "The first inoculation (dilution) level in each lab must be zero (0).",
      "The other levels must be a minimum of two (2) positive unique values."
    ),
    session = session
  )
  validateWithAlert(
    all(dat$ntest > 0),
    title = input_error,
    text = "Each inoculation level must have at least 1 tube inoculated.",
    session = session
  )
  validateWithAlert(
    any(dat$npos > 0),
    title = input_error,
    text = "At least 1 lab must have a positive tube.",
    session = session
  )
  validateWithAlert(
    any(dat$ntest - dat$npos > 0),
    title = input_error,
    text = "At least 1 lab must have a negative tube.",
    session = session
  )
  validateWithAlert(
    all(dat$ntest >= dat$npos),
    title = input_error,
    text = "Tubes inoculated must always be >= tubes positive.",
    session = session
  )
  validateWithAlert(
    all(is_wholenumber(dat$ntest)) && all(is_wholenumber(dat$npos)),
    title = input_error,
    text = "Tubes inoculated and tubes positive must be whole numbers.",
    session = session
  )
}


#---------------  Specifically for uploaded file  ------------------------------

validateUploadExtension <- function(file_name, session) {
  file_extension <- tools::file_ext(file_name)
  validateWithAlert(
    file_extension %in% c("xlsx", "XLSX"),
    title = "Uploaded file validation error",
    text = "Please select a file with extension .xlsx",
    session = session
  )
}

validateUploadSheetNames <- function(wb, session) {
  sheet_names <- openxlsx::getSheetNames(wb)
  validateWithAlert(
    "description" %in% sheet_names && "inoculation-levels" %in% sheet_names && "counts" %in% sheet_names,
    title = "Uploaded file validation error",
    text = "The file must contain sheets named 'description', 'inoculation-levels', and 'counts'.",
    session = session
  )
}

validateUploadTestPortionSize <- function(description_df, session) {
  test_portion_size <- description_df$`Test_portion_size_(g_or_mL)`
  validateWithAlert(
    !is.na(test_portion_size) && is.numeric(test_portion_size) && test_portion_size > 0,
    title = "Uploaded file validation error",
    text = "The test portion size must be a positive numeric value.",
    session = session
  )
}

validateUploadMinimumLabs <- function(counts_df, session) {
  validateWithAlert(
    nrow(counts_df) >= 2,
    title = "Uploaded file validation error",
    text = "A minimum of two (2) labs are required.",
    session = session
  )
}

validateUploadDimensions <- function(counts_df, inoc_levels_df, session) {
  counts_columns <- ncol(dplyr::select(counts_df, dplyr::starts_with("n", ignore.case = FALSE)))
  positives_columns <- ncol(dplyr::select(counts_df, dplyr::starts_with("y", ignore.case = FALSE)))
  validateWithAlert(
    counts_columns == positives_columns,
    title = "Uploaded file validation error",
    text = "The number of columns for positive tubes does not match the number of columns for tested tubes.",
    session = session
  )
  dilutions_columns <- ncol(inoc_levels_df)
  validateWithAlert(
    counts_columns == dilutions_columns,
    title = "Uploaded file validation error",
    text = "The number of columns for tested tubes does not match the number of inoculation/dilution levels.",
    session = session
  )
}

validateUploadDilutions <- function(dils_df, session) {
  validateWithAlert(
    !any(is.na(dils_df$inoc_level)),
    title = "Uploaded file validation error",
    text = "All dilution/inoculation levels must be numeric.",
    session = session
  )
  validateWithAlert(
    dils_df$inoc_level[1] == 0,
    title = "Uploaded file validation error",
    text = "The first dilution/inoculation level must be zero (0).",
    session = session
  )
  validateWithAlert(
    length(dils_df$inoc_level) >= 3,
    title = "Uploaded file validation error",
    text = "A minimum of two (2) positive dilution/inoculation levels are required.",
    session = session
  )
}

