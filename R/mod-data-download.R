
#-----------------------------  Helper functions  ------------------------------

sessionInfo2 <- function() {
  my_package_info <- sessioninfo::package_info(pkgs = NULL)
  my_package_info <- as.data.frame(my_package_info)
  my_package_info <- my_package_info[, c("package", "loadedversion", "attached")]
  rownames(my_package_info) <- NULL
  list(
    R_version = sessioninfo::platform_info()$version,
    package_info = my_package_info
  )
}


addSheet <- function(wb, sheet_name, data, cols = 1, col_widths = c(100)) {
  openxlsx::addWorksheet(wb, sheetName = sheet_name)
  openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  openxlsx::addStyle(wb, sheet = sheet_name,
    style = openxlsx::createStyle(textDecoration = "bold"), rows = 1, cols = 1:10
  )
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = cols, widths = col_widths)
  openxlsx::writeData(wb, sheet = sheet_name, x = data)
}


addSheetImage <- function(wb, sheet_name, plot, file_name,
                          plot_width = 10, aspect_ratio = 0.45) {
  #https://stackoverflow.com/questions/14810409/save-plots-made-in-a-shiny-app
  openxlsx::addWorksheet(wb, sheetName = sheet_name)
  openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  plot_height <- aspect_ratio * plot_width
  temp_dir <- tempdir()
  ggplot2::ggsave(filename = file_name,
    path = temp_dir, plot = plot,
    device = "png", scale = 2, width = plot_width, height = plot_height,
    units = "in", dpi = 600
  )
  openxlsx::insertImage(wb,
    sheet = sheet_name, file = paste0(temp_dir, "/", file_name),
    width = plot_width, height = plot_height,
    units = "in", dpi = 600, startRow = 3, startCol = 1
  )
  openxlsx::writeData(wb, sheet = sheet_name,
    x = "Plot of detection probability versus inoculation level."
  )
}


#------------------------  Download Results module  ----------------------------

#https://mastering-shiny.org/action-transfer.html

downloadResultsUI <- function(id, label = "Download Results") {
  ns <- NS(id)
  downloadButton(
    ns("download_results"), label = label, class = "download-results"
  )
}

downloadResultsServer <- function(id,
    run_analysis, model_fit, POD_LOD, plotCurves, dat,
    app_title = glob_app_title, app_version = glob_app_version) {

  stopifnot(is.reactive(run_analysis))
  stopifnot(is.reactive(model_fit))
  stopifnot(is.reactive(POD_LOD))
  stopifnot(is.reactive(plotCurves))
  stopifnot(is.reactive(dat))

  moduleServer(id,
    function(input, output, session) {
      #https://githubmemory.com/repo/daattali/shinyjs/issues/196
      my_session_info <- sessionInfo2()
      output$download_results <- downloadHandler(
        filename = function() {
          paste0(
            "MultiLab_POD_LOC_ICC_results_",
            gsub(":", "_", run_analysis()$date_time, fixed = TRUE),
            ".xlsx"
          )
        },
        content = function(file) {
          shinybusy::show_modal_spinner(
            spin = "double-bounce", color = "#112446",
            text = span("Creating your report...", class = "creating-report"),
            session
          )
          on.exit(shinybusy::remove_modal_spinner(session), add = TRUE)
          calc <- run_analysis()
          dat  <- dat()
          if (calc$choose_data_entry == "File upload") {
            fname_uploaded <- paste0(" (", dat$fname_uploaded(), ")")
          } else {
            fname_uploaded <- ""
          }
          workbook <- openxlsx::createWorkbook()
          my_exp_description <-
            c("EXPERIMENT DESCRIPTION",
              paste0("Experiment name:  ", dat$exp_name()),
              paste0("Experiment date:  ", dat$exp_date()),
              paste0("Microorganism:  ",   dat$microorganism()),
              paste0("Food matrix:  ",     dat$matrix()),
              paste0("Test portion size (g or mL):  ", dat$sample_size()),
              paste0("LOD unit:  ",                 calc$lod_unit),
              paste0("How many labs?  ",            POD_LOD()$num_labs),
              paste0("How many inoculum levels?  ", POD_LOD()$num_levels),
              paste0("Data entry choice:  ",        calc$choose_data_entry, fname_uploaded)
            )
          addSheet(workbook,
            sheet_name = "Experiment Description", data = my_exp_description
          )
          my_analysis <-
            c("ANALYSIS",
              paste0("Analysis date:  ",    calc$date_time),
              paste0("LOD choice:  ",       paste0("LOD", calc$lod_choice)),
              paste0("Confidence level:  ", calc$conf_level),
              paste0("Model used:  ",       model_fit()$model_type)
            )
          addSheet(workbook, sheet_name = "Analysis", data = my_analysis)
          my_app_info <-
            c("APP INFORMATION",
              paste0("App name:  ",    app_title),
              paste0("App version:  ", app_version),
              my_session_info$R_version
            )
          addSheet(workbook, sheet_name = "App Information", data = my_app_info)
          warning_prefix <- if (length(model_fit()$warnings) > 0) {
            c("", "Warnings: ")
          } else {
            ""
          }
          my_results <-
            c("RESULTS",
              paste0("Mean Lab Effect:  ",       model_fit()$mu_log_char),
              paste0("SE of Mean Lab Effect:  ", model_fit()$mu_log_se_char),
              paste0("ICC:  ",                   model_fit()$ICC_char),
              paste0("Standard Deviation of Lab Effects:  ", model_fit()$sigma_char),
              paste0("LOD unit:  ",                 calc$lod_unit),
              paste0("LOD", calc$lod_choice, ":  ",               POD_LOD()$LOD_char),
              paste0("LOD", calc$lod_choice, " (lower limit):  ", POD_LOD()$LOD_L_char),
              paste0("LOD", calc$lod_choice, " (upper limit):  ", POD_LOD()$LOD_U_char),
              paste0("Confidence level:  ", calc$conf_level),
              c(warning_prefix, model_fit()$warnings)
            )
          addSheet(workbook, sheet_name = "Results", data = my_results)
          addSheetImage(workbook, sheet_name = "POD Curves",
            plot = plotCurves(), file_name = "my_plot.png"
          )
          my_lab_effects <- POD_LOD()$lab_effects
          my_lab_effects <- dplyr::rename(my_lab_effects,
            "Lab ID" = lab_id, "Lab Name" = lab_name,
            "Estimated effect" = estimated_effect, "Standard error" = SE
          )
          addSheet(workbook, sheet_name = "Lab Effects", data = my_lab_effects,
            cols = 1:ncol(my_lab_effects), col_widths = c(8, 16, 16, 16)
          )
          my_data <- dplyr::rename(dat()$data_model(),
            "Lab ID" = lab_id, "Lab Name" = lab_name,
            "Level Per Portion" = inoculum, "Level Per Unit" = inoculum_per_unit,
            "Inoculated Tubes" = ntest, "Positive Tubes" = npos,
            "Test Portion Size" = sample_size
          )
          addSheet(workbook, sheet_name = "Data", data = my_data,
            cols = 1:ncol(my_data), col_widths = c(8, 11, 15, 15, 15, 15, 15)
          )
          my_package_info <- my_session_info$package_info
          addSheet(workbook, sheet_name = "R Packages", data = my_package_info,
            cols = 1:ncol(my_package_info), col_widths = c(18, 14, 11)
          )
          openxlsx::saveWorkbook(wb = workbook, file = file, overwrite = TRUE)
        },
        contentType = "xlsx"
      )

    }
  )
}

