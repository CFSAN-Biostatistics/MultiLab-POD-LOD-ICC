# Input data validation

#from help(is.integer)
is_wholenumber <- function(x, tol = .Machine$double.eps ^ 0.5) {
  abs(x - round(x)) < tol
}

validateWithAlert <- function(validation_check, title, text, session) {
  # Helper function to validate input & create alerts.
  if (!validation_check) {
    modal(title, session,
      span(text)
    )
  }
  req(validation_check)
}


#------------------------------  Manual data  ----------------------------------

validateDescription <- function(is_valid_description, session) {
  # Check validation of experiment description using results from shinyvalidate.
  validateWithAlert(is_valid_description,
    title = "Problem with Experiment Description",
    text = span("Please correct issues before continuing."),
    session = session
  )
}

validateData <- function(dat, session) {
  # Validate lab-level data
  input_error <- "Lab-level data error"
  validateWithAlert(all(!is.na(dat)),
    title = input_error, text = "Some data are missing or non-numeric.",
    session = session
  )
  validateWithAlert(all(dat >= 0),
    title = input_error, text = "All data must be non-negative.",
    session = session
  )
  validateWithAlert(all(dat$ntest > 0),
    title = input_error,
    text = "Each inoculation level must have at least 1 tube inoculated.",
    session = session
  )
  validateWithAlert(any(dat$npos > 0),
    title = input_error, text = "At least 1 lab must have a positive tube.",
    session = session
  )
  validateWithAlert(any(dat$ntest - dat$npos > 0),
    title = input_error, text = "At least 1 lab must have a negative tube.",
    session = session
  )
  validateWithAlert(all(dat$ntest >= dat$npos),
    title = input_error,
    text = "There must be at least as many inoculated tubes as positive tubes",
    session = session
  )
  validateWithAlert(all(is_wholenumber(dat$ntest)) && all(is_wholenumber(dat$npos)),
    title = input_error,
    text = "Tubes inoculated and tubes positive must be whole numbers.",
    session = session
  )
}


#----------------------------  Uploaded file  ----------------------------------

uploaded_file_error <- "Uploaded file validation error"

validateExtension <- function(file_name, session) {
  # In case the .includes() JS method is unsupported in browser. (see jscript.js)
  file_extension <- tools::file_ext(file_name)
  is_extension_valid <- file_extension %in% c("xlsx", "XLSX")
  validateWithAlert(is_extension_valid,
    title = uploaded_file_error,
    text = "Please select a file with extension .xlsx",
    session = session
  )
}

validateUploadExistence <- function(exists, session) {
  # Validate existence of uploaded data
  validateWithAlert(exists,
    title = "Uploaded data not found",
    text = "Please upload a data set.",
    session = session
  )
}

validateUploadSheetNames <- function(wb, session) {
  sheet_names <- openxlsx::getSheetNames(wb)
  correct_names <- c("description", "inoculation-levels", "counts")
  is_sheet_names_correct <- all(correct_names %in% sheet_names)
  validateWithAlert(is_sheet_names_correct,
    title = uploaded_file_error,
    text = "File must contain sheets named 'description', 'inoculation-levels', and 'counts'.",
    session = session
  )
}

validateUploadTestPortionSize <- function(description_df, session) {
  test_portion_size <- description_df$`Test_portion_size_(g_or_mL)`
  is_valid_test_portion <- !is.na(test_portion_size) &&
                           is.numeric(test_portion_size) && test_portion_size > 0
  validateWithAlert(is_valid_test_portion,
    title = uploaded_file_error,
    text = "Test portion size must be a positive numeric value.",
    session = session
  )
}

validateUploadMinimumLabs <- function(counts_df, session) {
  is_minimum_labs_correct <- nrow(counts_df) >= 2
  validateWithAlert(is_minimum_labs_correct,
    title = uploaded_file_error,
    text = "Minimum of two (2) labs required.",
    session = session
  )
}

validateUploadDimensions <- function(counts_df, inoc_levels_df, session) {
  ntest_cols <- dplyr::select(counts_df, dplyr::starts_with("n", ignore.case = FALSE))
  npos_cols  <- dplyr::select(counts_df, dplyr::starts_with("y", ignore.case = FALSE))
  counts_columns    <- ncol(ntest_cols)
  positives_columns <- ncol(npos_cols)
  is_columns_equal  <- counts_columns == positives_columns
  validateWithAlert(is_columns_equal,
    title = uploaded_file_error,
    text = "Number of columns for positive tubes does not match number of columns for tested tubes.",
    session = session
  )
  dilutions_columns <- ncol(inoc_levels_df)
  validateWithAlert(
    counts_columns == dilutions_columns,
    title = uploaded_file_error,
    text = "Number of columns for tested tubes does not match number of inoculation/dilution levels.",
    session = session
  )
}

validateUploadDilutions <- function(dils_df, session) {
  validateWithAlert(
    !any(is.na(dils_df$inoc_level)),
    title = uploaded_file_error,
    text = "All dilution/inoculation levels must be numeric.",
    session = session
  )
}

validateUploadCounts <- function(counts_df, session) {
  counts <- dplyr::select(counts_df, dplyr::starts_with("n", ignore.case = FALSE))
  validateWithAlert(
    all(mapply(is.numeric, counts)),
    title = uploaded_file_error,
    text = "All counts must be numeric.",
    session = session
  )
}
